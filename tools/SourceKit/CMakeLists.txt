include(CheckSymbolExists)

# Append our own modules to the module path.
list(APPEND CMAKE_MODULE_PATH
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

if (DARWIN_TOOLCHAIN_VERSION)
  set(SOURCEKIT_VERSION_STRING "${DARWIN_TOOLCHAIN_VERSION}")
else()
  set(SOURCEKIT_VERSION_STRING "${SWIFT_COMPILER_VERSION}")
endif()

set(SOURCEKIT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(SOURCEKIT_BINARY_DIR ${SWIFT_BINARY_DIR})

set(SOURCEKIT_RUNTIME_OUTPUT_INTDIR "${SWIFT_RUNTIME_OUTPUT_INTDIR}")
set(SOURCEKIT_LIBRARY_OUTPUT_INTDIR "${SWIFT_LIBRARY_OUTPUT_INTDIR}")

check_symbol_exists(dispatch_block_create "dispatch/dispatch.h" HAVE_DISPATCH_BLOCK_CREATE)
configure_file(
  ${SOURCEKIT_SOURCE_DIR}/include/SourceKit/Config/config.h.cmake
  ${SOURCEKIT_BINARY_DIR}/include/SourceKit/Config/config.h)

set(SOURCEKIT_DEPLOYMENT_OS "${SWIFT_HOST_VARIANT}")
set(SOURCEKIT_DEPLOYMENT_TARGET "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_DEPLOYMENT_VERSION}")

STRING(TOLOWER "${SWIFT_PRIMARY_VARIANT_SDK}" SOURCEKIT_PLATFORM_NAME)
# Remove '_simulator' from the name if it exists
STRING(REPLACE "_simulator" "" SOURCEKIT_PLATFORM_NAME "${SOURCEKIT_PLATFORM_NAME}")

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin" AND NOT CMAKE_CROSSCOMPILING)
  set(CMAKE_OSX_SYSROOT "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_PATH}")
  set(CMAKE_OSX_ARCHITECTURES "${SWIFT_HOST_VARIANT_ARCH}")
  set(CMAKE_OSX_DEPLOYMENT_TARGET "")
endif()

# If we were don't have XPC, just build inproc.
if(NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin" OR NOT HAVE_XPC_H)
  set(SWIFT_SOURCEKIT_USE_INPROC_LIBRARY TRUE)
endif()

# Now include AddSwiftSourceKit
include(AddSwiftSourceKit)

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  # Choose a deployment target if none was set.
  set(SOURCEKIT_DEPLOYMENT_TARGET "" CACHE STRING
      "Deployment target for SourceKit.")
  if (NOT SOURCEKIT_DEPLOYMENT_TARGET)
    execute_process(COMMAND sw_vers -productVersion
                    OUTPUT_VARIABLE SOURCEKIT_DEPLOYMENT_TARGET
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    string(REGEX MATCH "[0-9]+\\.[0-9]+" SOURCEKIT_DEPLOYMENT_TARGET "${SOURCEKIT_DEPLOYMENT_TARGET}")
  endif()

  # Sadly there are two OS naming conventions.
  # xcrun SDK name:   macosx iphoneos iphonesimulator (+ "internal" or version)
  # -mOS-version-min: macosx ios      ios-simulator

  if (SOURCEKIT_DEPLOYMENT_OS MATCHES "^iphoneos")
    set(version_min_os "ios")
    set(triple_os "ios")
    set(SOURCEKIT_DEFAULT_TARGET_SDK "IOS")
  elseif (SOURCEKIT_DEPLOYMENT_OS MATCHES "^iphonesimulator")
    set(version_min_os "ios-simulator")
    set(triple_os "ios")
    set(SOURCEKIT_DEFAULT_TARGET_SDK "IOS_SIMULATOR")
  elseif (SOURCEKIT_DEPLOYMENT_OS MATCHES "^macosx")
    set(version_min_os "macosx")
    set(triple_os "macosx")
    set(SOURCEKIT_DEFAULT_TARGET_SDK "OSX")
  else()
    message(FATAL_ERROR "Can't understand SOURCEKIT_DEPLOYMENT_OS '${SOURCEKIT_DEPLOYMENT_OS}';")
  endif()

  # Add deployment target to C/C++ compiler and linker flags.
  # FIXME: CMAKE_OSX_DEPLOYMENT_TARGET falls over when used for iOS versions.
  if (XCODE)
    if (${SOURCEKIT_DEPLOYMENT_OS} MATCHES "^macosx")
      set(CMAKE_XCODE_ATTRIBUTE_MACOSX_DEPLOYMENT_TARGET ${SOURCEKIT_DEPLOYMENT_TARGET})
    else()
      set(CMAKE_XCODE_ATTRIBUTE_IPHONEOS_DEPLOYMENT_TARGET ${SOURCEKIT_DEPLOYMENT_TARGET})
    endif()
  else()
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m${version_min_os}-version-min=${SOURCEKIT_DEPLOYMENT_TARGET}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m${version_min_os}-version-min=${SOURCEKIT_DEPLOYMENT_TARGET}")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -m${version_min_os}-version-min=${SOURCEKIT_DEPLOYMENT_TARGET}")
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -m${version_min_os}-version-min=${SOURCEKIT_DEPLOYMENT_TARGET}")
  endif()
endif()

include_directories(BEFORE
  ${SOURCEKIT_BINARY_DIR}/include
  ${SOURCEKIT_SOURCE_DIR}/include
  ${SOURCEKIT_BINARY_DIR}/tools/SourceKit/include
)

if(NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  string(TOUPPER "${CMAKE_SYSTEM_NAME}" SOURCEKIT_DEFAULT_TARGET_SDK)
  set(SOURCEKIT_NEED_EXPLICIT_LIBDISPATCH TRUE)

  if(SWIFT_BUILD_SOURCEKIT)
    include(ExternalProject)
    ExternalProject_Add(libdispatch
                        SOURCE_DIR
                          "${SWIFT_PATH_TO_LIBDISPATCH_SOURCE}"
                        CMAKE_ARGS
                          -DCMAKE_AR=${CMAKE_AR}
                          -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                          -DCMAKE_C_COMPILER=${PATH_TO_CLANG_BUILD}/bin/clang
                          -DCMAKE_C_FLAGS=${CMAKE_C_FLAGS}
                          -DCMAKE_CXX_COMPILER=${PATH_TO_CLANG_BUILD}/bin/clang++
                          -DCMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS}
                          -DCMAKE_MAKE_PROGRAM=${CMAKE_MAKE_PROGRAM}
                          -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
                          -DCMAKE_LINKER=${CMAKE_LINKER}
                          -DCMAKE_RANLIB=${CMAKE_RANLIB}
                          -DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}
                          -DENABLE_SWIFT=NO
                          -DENABLE_TESTING=NO
                        INSTALL_COMMAND
                          # NOTE(compnerd) provide a custom install command to
                          # ensure that we strip out the DESTDIR environment
                          # from the sub-build
                          ${CMAKE_COMMAND} -E env --unset=DESTDIR ${CMAKE_COMMAND} --build . --target install
                        BUILD_BYPRODUCTS
                          <INSTALL_DIR>/lib/${CMAKE_SHARED_LIBRARY_PREFIX}dispatch${CMAKE_SHARED_LIBRARY_SUFFIX}
                          <INSTALL_DIR>/lib/${CMAKE_IMPORT_LIBRARY_PREFIX}dispatch${CMAKE_IMPORT_LIBRARY_SUFFIX}
                          <INSTALL_DIR>/lib/${CMAKE_SHARED_LIBRARY_PREFIX}BlocksRuntime${CMAKE_SHARED_LIBRARY_SUFFIX}
                          <INSTALL_DIR>/lib/${CMAKE_IMPORT_LIBRARY_PREFIX}BlocksRuntime${CMAKE_IMPORT_LIBRARY_SUFFIX}
                        BUILD_ALWAYS
                          1)

    ExternalProject_Get_Property(libdispatch install_dir)

    # CMake does not like the addition of INTERFACE_INCLUDE_DIRECTORIES without
    # the directory existing.  Just create the location which will be populated
    # during the installation.
    file(MAKE_DIRECTORY ${install_dir}/include)

    add_library(dispatch SHARED IMPORTED)
    set_target_properties(dispatch
                          PROPERTIES
                            IMPORTED_LOCATION
                              ${install_dir}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}dispatch${CMAKE_SHARED_LIBRARY_SUFFIX}
                            IMPORTED_IMPLIB
                              ${install_dir}/lib/${CMAKE_IMPORT_LIBRARY_PREFIX}dispatch${CMAKE_IMPORT_LIBRARY_SUFFIX}
                            INTERFACE_INCLUDE_DIRECTORIES
                              ${install_dir}/include)

    add_library(BlocksRuntime SHARED IMPORTED)
    set_target_properties(BlocksRuntime
                          PROPERTIES
                            IMPORTED_LOCATION
                              ${install_dir}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}BlocksRuntime${CMAKE_SHARED_LIBRARY_SUFFIX}
                            IMPORTED_IMPLIB
                              ${install_dir}/lib/${CMAKE_IMPORT_LIBRARY_PREFIX}BlocksRuntime${CMAKE_IMPORT_LIBRARY_SUFFIX}
                            INTERFACE_INCLUDE_DIRECTORIES
                              ${SWIFT_PATH_TO_LIBDISPATCH_SOURCE}/src/BlocksRuntime)

    swift_install_in_component(sourcekit-inproc
                               FILES
                                 $<TARGET_FILE:dispatch>
                                 $<TARGET_FILE:BlocksRuntime>
                               DESTINATION
                                 lib${LLVM_LIBDIR_SUFFIX})

    # FIXME(compnerd) this should be taken care of by the
    # INTERFACE_INCLUDE_DIRECTORIES above
    include_directories(AFTER
                          ${SWIFT_PATH_TO_LIBDISPATCH_SOURCE}/src/BlocksRuntime
                          ${SWIFT_PATH_TO_LIBDISPATCH_SOURCE})
  endif()
endif()

add_subdirectory(include)
add_subdirectory(lib)
add_subdirectory(tools)
