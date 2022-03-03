# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.18

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Disable VCS-based implicit rules.
% : %,v


# Disable VCS-based implicit rules.
% : RCS/%


# Disable VCS-based implicit rules.
% : RCS/%,v


# Disable VCS-based implicit rules.
% : SCCS/s.%


# Disable VCS-based implicit rules.
% : s.%


.SUFFIXES: .hpux_make_needs_suffix_list


# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/lucas/sod2d_github

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/lucas/sod2d_github/build

# Include any dependencies generated for this target.
include unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/depend.make

# Include the progress variables for this target.
include unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/progress.make

# Include the compile flags for this target's objects.
include unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/flags.make

unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/__/__/src/elem_diffu.f90.o: unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/flags.make
unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/__/__/src/elem_diffu.f90.o: ../src/elem_diffu.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/lucas/sod2d_github/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/__/__/src/elem_diffu.f90.o"
	cd /home/lucas/sod2d_github/build/unitt/unitt_mom_diffu && /opt/nvidia/hpc_sdk/Linux_x86_64/22.2/compilers/bin/nvfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/lucas/sod2d_github/src/elem_diffu.f90 -o CMakeFiles/unitt_mom_diffu.dir/__/__/src/elem_diffu.f90.o

unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/__/__/src/elem_diffu.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/unitt_mom_diffu.dir/__/__/src/elem_diffu.f90.i"
	cd /home/lucas/sod2d_github/build/unitt/unitt_mom_diffu && /opt/nvidia/hpc_sdk/Linux_x86_64/22.2/compilers/bin/nvfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/lucas/sod2d_github/src/elem_diffu.f90 > CMakeFiles/unitt_mom_diffu.dir/__/__/src/elem_diffu.f90.i

unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/__/__/src/elem_diffu.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/unitt_mom_diffu.dir/__/__/src/elem_diffu.f90.s"
	cd /home/lucas/sod2d_github/build/unitt/unitt_mom_diffu && /opt/nvidia/hpc_sdk/Linux_x86_64/22.2/compilers/bin/nvfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/lucas/sod2d_github/src/elem_diffu.f90 -o CMakeFiles/unitt_mom_diffu.dir/__/__/src/elem_diffu.f90.s

unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/unitt_mom_diffu.f90.o: unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/flags.make
unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/unitt_mom_diffu.f90.o: ../unitt/unitt_mom_diffu/unitt_mom_diffu.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/lucas/sod2d_github/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/unitt_mom_diffu.f90.o"
	cd /home/lucas/sod2d_github/build/unitt/unitt_mom_diffu && /opt/nvidia/hpc_sdk/Linux_x86_64/22.2/compilers/bin/nvfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/lucas/sod2d_github/unitt/unitt_mom_diffu/unitt_mom_diffu.f90 -o CMakeFiles/unitt_mom_diffu.dir/unitt_mom_diffu.f90.o

unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/unitt_mom_diffu.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/unitt_mom_diffu.dir/unitt_mom_diffu.f90.i"
	cd /home/lucas/sod2d_github/build/unitt/unitt_mom_diffu && /opt/nvidia/hpc_sdk/Linux_x86_64/22.2/compilers/bin/nvfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/lucas/sod2d_github/unitt/unitt_mom_diffu/unitt_mom_diffu.f90 > CMakeFiles/unitt_mom_diffu.dir/unitt_mom_diffu.f90.i

unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/unitt_mom_diffu.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/unitt_mom_diffu.dir/unitt_mom_diffu.f90.s"
	cd /home/lucas/sod2d_github/build/unitt/unitt_mom_diffu && /opt/nvidia/hpc_sdk/Linux_x86_64/22.2/compilers/bin/nvfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/lucas/sod2d_github/unitt/unitt_mom_diffu/unitt_mom_diffu.f90 -o CMakeFiles/unitt_mom_diffu.dir/unitt_mom_diffu.f90.s

# Object files for target unitt_mom_diffu
unitt_mom_diffu_OBJECTS = \
"CMakeFiles/unitt_mom_diffu.dir/__/__/src/elem_diffu.f90.o" \
"CMakeFiles/unitt_mom_diffu.dir/unitt_mom_diffu.f90.o"

# External object files for target unitt_mom_diffu
unitt_mom_diffu_EXTERNAL_OBJECTS =

unitt/unitt_mom_diffu/unitt_mom_diffu: unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/__/__/src/elem_diffu.f90.o
unitt/unitt_mom_diffu/unitt_mom_diffu: unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/unitt_mom_diffu.f90.o
unitt/unitt_mom_diffu/unitt_mom_diffu: unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/build.make
unitt/unitt_mom_diffu/unitt_mom_diffu: unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/lucas/sod2d_github/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Linking Fortran executable unitt_mom_diffu"
	cd /home/lucas/sod2d_github/build/unitt/unitt_mom_diffu && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/unitt_mom_diffu.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/build: unitt/unitt_mom_diffu/unitt_mom_diffu

.PHONY : unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/build

unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/clean:
	cd /home/lucas/sod2d_github/build/unitt/unitt_mom_diffu && $(CMAKE_COMMAND) -P CMakeFiles/unitt_mom_diffu.dir/cmake_clean.cmake
.PHONY : unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/clean

unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/depend:
	cd /home/lucas/sod2d_github/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/lucas/sod2d_github /home/lucas/sod2d_github/unitt/unitt_mom_diffu /home/lucas/sod2d_github/build /home/lucas/sod2d_github/build/unitt/unitt_mom_diffu /home/lucas/sod2d_github/build/unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : unitt/unitt_mom_diffu/CMakeFiles/unitt_mom_diffu.dir/depend

