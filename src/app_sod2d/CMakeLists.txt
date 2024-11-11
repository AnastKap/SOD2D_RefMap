project(sod2d)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

if(SHARED_LIBS)
   file(GLOB_RECURSE SRC_FILES ${CMAKE_CURRENT_LIST_DIR}/*.f90)
else()
   file(GLOB SRC_FILES ${CMAKE_CURRENT_LIST_DIR}/../lib_sod2d/sources/*.f90 
                       ${CMAKE_CURRENT_LIST_DIR}/../lib_sod2d_incomp/sources/*.f90 
                       ${CMAKE_CURRENT_LIST_DIR}/../lib_mainBaseClass/sources/*.f90 
                       ${CMAKE_CURRENT_LIST_DIR}/sources/*.f90)
endif()
include_directories(${CMAKE_BINARY_DIR}/modules)

add_executable(${PROJECT_NAME} ${SRC_FILES})   # Build executable from listed sources
set_property(TARGET ${PROJECT_NAME} PROPERTY LINKER_LANGUAGE Fortran)

if(STORE_INTERMEDIATE_FILES)
   # Generate .dot files for CFG visualization after each object file
   # building (The NVIDIA opt tool must be used, otherwise the 
   # debugging meta-data in the LLVM IR are not recognized)
   add_custom_command(
      TARGET ${PROJECT_NAME}
      POST_BUILD
      COMMAND for i in `find . -name *.dot`\; do rm $$i \; done
      COMMAND for i in `find . -regex .*.gpu`\; do opt -dot-cfg-only -o /dev/null $$i 2> /dev/null \; done
      COMMAND for i in `find . -regex .*-openmp-.*.ll`\; do opt --passes="dot-cfg-only" -o /dev/null $$i 2> /dev/null \; done || exit 0
      COMMENT "Creating CFG graphs of GPU kernels"
   )
endif()

if(SHARED_LIBS)
   target_link_libraries(${PROJECT_NAME} lib_mainBaseClass)
else()
   set_mpi()
   set_hdf5()
endif()

install(TARGETS ${PROJECT_NAME} DESTINATION bin)
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} DESTINATION include)