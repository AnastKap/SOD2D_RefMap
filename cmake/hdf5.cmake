message("-- Configuring HDF5, if appropriate...")
if (USE_HDF5)
	#if(USE_PCPOWER)
	#  message("-- Configuring HDF5 in CTE-Power...")
	#  set(HDF5_ROOT "/apps/HDF5/1.12.0/NVIDIA-COMP/NVIDIA-MPI/")
	#elseif(USE_MN)
	#  message("-- Configuring HDF5 in MareNostrum...")
	#  set(HDF5_ROOT "/apps/HDF5/1.10.5/INTEL/IMPI/")
	#else()
	#  message("-- Configuring HDF5 in Local...")
	#  #set(HDF5_ROOT "/home/lucas/Libraries/HDF5_NVHPC/")
	#endif()
	set(HDF5_PREFER_PARALLEL TRUE)
	find_package(HDF5 REQUIRED COMPONENTS Fortran C HL)
	if (NOT HDF5_FOUND)
		#do nothing
		message(FATAL_ERROR "HDF5 not found!")
	endif()
endif()

function(set_hdf5)
	if (USE_HDF5)
		include_directories(${HDF5_INCLUDE_DIRS})
		# If CMake version older than 3.20, use the following line
		if(${CMAKE_VERSION} VERSION_LESS "3.20.0")
			target_link_libraries(${PROJECT_NAME} hdf5 hdf5_hl hdf5_fortran hdf5_hl_fortran)
		else()
			target_link_libraries(${PROJECT_NAME} HDF5::HDF5)
		endif()
	else()
		message("  -- Not using HDF5...")
		add_definitions(-DNO_HDF5)
	endif()
endfunction()
