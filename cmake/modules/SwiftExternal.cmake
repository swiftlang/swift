
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  include(ExternalProject)

  if(SWIFT_BUILD_SOURCEKIT)
    ExternalProject_Add(libdispatch
                        SOURCE_DIR
                          "${SWIFT_PATH_TO_LIBDISPATCH_SOURCE}"
                        BINARY_DIR
                          "${SWIFT_PATH_TO_LIBDISPATCH_BUILD}"
                        CMAKE_ARGS
                          -DCMAKE_C_COMPILER=${PATH_TO_CLANG_BUILD}/bin/clang
                          -DCMAKE_CXX_COMPILER=${PATH_TO_CLANG_BUILD}/bin/clang++
                          -DCMAKE_MAKE_PROGRAM=${CMAKE_MAKE_PROGRAM}
                          -DCMAKE_SWIFT_COMPILER=$<TARGET_FILE:swift>c
                          -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
                          -DENABLE_SWIFT=YES
                        BUILD_BYPRODUCTS
                          ${SWIFT_PATH_TO_LIBDISPATCH_BUILD}/src/${CMAKE_SHARED_LIBRARY_PREFIX}dispatch${CMAKE_SHARED_LIBRARY_SUFFIX}
                          ${SWIFT_PATH_TO_LIBDISPATCH_BUILD}/${CMAKE_STATIC_LIBRARY_PREFIX}BlocksRuntime${CMAKE_STATIC_LIBRARY_SUFFIX}
                        STEP_TARGETS
                          configure
                        BUILD_ALWAYS
                          1)

    # CMake does not like the addition of INTERFACE_INCLUDE_DIRECTORIES without
    # the directory existing.  Just create the location which will be populated
    # during the installation.
    ExternalProject_Get_Property(libdispatch install_dir)
    file(MAKE_DIRECTORY ${install_dir}/include)

    # TODO(compnerd) this should be taken care of by the
    # INTERFACE_INCLUDE_DIRECTORIES below
    include_directories(AFTER
                          ${SWIFT_PATH_TO_LIBDISPATCH_SOURCE}/src/BlocksRuntime
                          ${SWIFT_PATH_TO_LIBDISPATCH_SOURCE})
    add_dependencies(libdispatch
                       swift
                       copy_shim_headers
                       swiftCore-${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}-${SWIFT_HOST_VARIANT_ARCH}
                       swiftSwiftOnoneSupport-${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}-${SWIFT_HOST_VARIANT_ARCH}
                       swiftCore-swiftmodule-${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}-${SWIFT_HOST_VARIANT_ARCH}
                       swiftSwiftOnoneSupport-swiftmodule-${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}-${SWIFT_HOST_VARIANT_ARCH})

    ExternalProject_Get_Property(libdispatch install_dir)
    add_library(dispatch SHARED IMPORTED)
    set_target_properties(dispatch
                          PROPERTIES
                            IMPORTED_LOCATION
                              ${SWIFT_PATH_TO_LIBDISPATCH_BUILD}/src/${CMAKE_SHARED_LIBRARY_PREFIX}dispatch${CMAKE_SHARED_LIBRARY_SUFFIX}
                            INTERFACE_INCLUDE_DIRECTORIES
                              ${install_dir}/include
                            IMPORTED_LINK_INTERFACE_LIBRARIES
                              swiftCore-${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}-${SWIFT_HOST_VARIANT_ARCH})

    add_library(BlocksRuntime STATIC IMPORTED)
    set_target_properties(BlocksRuntime
                          PROPERTIES
                            IMPORTED_LOCATION
                              ${SWIFT_PATH_TO_LIBDISPATCH_BUILD}/${CMAKE_STATIC_LIBRARY_PREFIX}BlocksRuntime${CMAKE_STATIC_LIBRARY_SUFFIX}
                            INTERFACE_INCLUDE_DIRECTORIES
                              ${SWIFT_PATH_TO_LIBDISPATCH_SOURCE}/src/BlocksRuntime)
  endif()
endif()
