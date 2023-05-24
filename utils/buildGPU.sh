#!/bin/bash

## Small script for building the GPU version of sod2d.
## Runs CMake and make using default options on the root file.
## Assumes that env is appropriatedly set, see the wiki for SOD2D.

## Usage: ./buildCPU.sh [numThreads] [isMN] [setTPP] [setTMP]
## [numThreads] : Number of threads to use for compilation.
## [isMN]       : If set to 1, will use the MN compiler, otherwise the default.
## [setTPP]     : If set to 1, will activate compilation of postproc tool, 0 turns it off.
## [setTMP]     : If set to 1, will activate compilation of test tool, 0 turns it off.

## If not enough arguments are passed, print help message and exit
if [ $# -lt 4 ]; then
    echo "Usage: ./buildCPU.sh [numThreads] [isP9] [setTPP] [setTMP]"
    echo "  [numThreads] : Number of threads to use for compilation."
    echo "  [isMN]       : If set to 1, will use the MN compiler, otherwise the default."
    echo "  [setTPP]     : If set to 1, will activate compilation of postproc tool, 0 turns it off."
    echo "  [setTMP]     : If set to 1, will activate compilation of test tool, 0 turns it off."
    exit 1
fi

## Verify number of threads passed
if [ $1 -lt 1 ]; then
    echo "Error: Number of threads must be greater than 0."
    exit 1
fi

## Create a string for the build options already containing the GPU related options
buildOptions="-DUSE_GPU=ON -DUSE_MEM_MANAGED=ON"

## Verify the MN activation
if [ $2 -eq 1 ]; then
    echo "Activating P9 compilation..."
    buildOptions="$buildOptions -DUSE_PCPOWER=ON"
    setP9="ON"
elif [ $2 -eq 0 ]; then
    echo "Activating default machine compilation..."
    buildOptions="$buildOptions -DUSE_PCPOWER=OFF"
    setP9="OFF"
else
    echo "Error: Invalid value for P9 activation."
    exit 1
fi

## Verify the postproc tool activation
if [ $3 -eq 1 ]; then
    echo "Requesting build of postproc tool..."
    buildOptions="$buildOptions -DTOOL_POSTPROC=ON"
    setTPP="ON"
elif [ $3 -eq 0 ]; then
    echo "Building without postproc tool..."
    buildOptions="$buildOptions -DTOOL_POSTPROC=OFF"
    setTPP="OFF"
else
    echo "Error: Invalid value for postproc tool activation."
    exit 1
fi

## Verify the mesh partitioner toool activation
if [ $4 -eq 1 ]; then
    echo "Requesting build of mesh partition tool..."
    buildOptions="$buildOptions -DTOOL_MESHPART=ON"
    setTMP="ON"
elif [ $4 -eq 0 ]; then
    echo "Building without mesh partition tool..."
    buildOptions="$buildOptions -DTOOL_MESHPART=OFF"
    setTMP="OFF"
else
    echo "Error: Invalid value for test tool activation."
    exit 1
fi

## Start of building process

## Check that build_gpu directory already exists
if [ ! -d "build_gpu" ]; then
    ## Directory does not exist, create it and cd into it
    echo "Creating build_gpu directory..."
    mkdir -p build_gpu && cd build_gpu
    ## Run CMake wiith the defined options
    echo "Configuring CMake with options: $buildOptions"
    cmake "$buildOptions" ..
    ## Run make with the number of threads passed
    echo "Building with $1 threads..."
    make -j $1
else
    ## Directory exists, go to it
    echo "build_gpu directory already exists, checking options..."
    cd build_gpu
    ## If a CMakeCache.txt file exists, just run Make
    if [ -f "CMakeCache.txt" ]; then
        ## Already configured
        echo "Verifying if any options have changed..."
        ## Extract the USE_MN, TOOL_POSTPROC and TOOL_MESHPART options from the CMakeCache.txt file
        p9Option=$(grep USE_PCPOWER CMakeCache.txt | cut -d '=' -f 2)
        postprocOption=$(grep TOOL_POSTPROC CMakeCache.txt | cut -d '=' -f 2)
        meshpartOption=$(grep TOOL_MESHPART CMakeCache.txt | cut -d '=' -f 2)
        ## Compare the MN option with the existing one
        if [ "$p9Option" != "$setP9" ]; then
            ## Machiine type is different, exit with error
            echo "ERROR: Machine type is different from the one used to configure CMake prevviously."
            exit 1
        fi
        ## If the tool activations are different, reconfigure CMake
        if [ "$postprocOption" != "$setTPP" ] || [ "$meshpartOption" != "$setTMP" ]; then
            echo "Options have changed, reconfiguring..."
            ## Run CMake wiith the defined options
            echo "Configuring CMake with options: $buildOptions"
            cmake $buildOptions ..
        fi
        ## Run make with the number of threads passed
        echo "Building with $1 threads..."
        make -j $1
    else
        ## Run CMake wiith the defined options
        echo "Configuring CMake with options: $buildOptions"
        cmake $buildOptions ..
        ## Run make with the number of threads passed
        echo "Building with $1 threads..."
        make -j $1
    fi
fi