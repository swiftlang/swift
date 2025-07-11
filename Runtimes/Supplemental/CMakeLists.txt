cmake_minimum_required(VERSION 3.29)
# TODO before requiring CMake 4.1 or later
# and/or enforcing CMP0195, please check/update
# the implementation  of `emit_swift_interface`
# in `EmitSwiftInterface.cmake`
# to ensure it keeps laying down nested swiftmodule folders

project(SwiftRuntime LANGUAGES Swift C CXX)

include(ExternalProject)
include(GNUInstallDirs)

set(SwiftRuntime_SWIFTC_SOURCE_DIR "${PROJECT_SOURCE_DIR}/../../")

foreach(lib ${Swift_ENABLE_RUNTIMES})
  string(TOLOWER ${lib} name)
  set(SwiftRuntime_ENABLE_${name} YES)
endforeach()

if(SwiftCore_DIR)
  set(SwiftCore_DIR_FLAG "-DSwiftCore_DIR=${SwiftCore_DIR}")
endif()

if(CMAKE_MAKE_PROGRAM)
  set(MAKE_PROGRAM_FLAG "-DCMAKE_MAKE_PROGRAM=${CMAKE_MAKE_PROGRAM}")
endif()

set(COMMON_OPTIONS
  -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
  -DSwift_SDKROOT=${Swift_SDKROOT}
  -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
  -DCMAKE_INSTALL_LIBDIR=${CMAKE_INSTALL_LIBDIR}
  -DCMAKE_INSTALL_NAME_DIR=${CMAKE_INSTALL_NAME_DIR}
  -DCMAKE_BUILD_WITH_INSTALL_NAME_DIR=${CMAKE_BUILD_WITH_INSTALL_NAME_DIR}
  -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
  -DCMAKE_COLOR_DIAGNOSTICS=${CMAKE_COLOR_DIAGNOSTICS}
  -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
  -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
  -DCMAKE_Swift_COMPILER=${CMAKE_Swift_COMPILER}
  -DCMAKE_C_COMPILER_TARGET=${CMAKE_C_COMPILER_TARGET}
  -DCMAKE_CXX_COMPILER_TARGET=${CMAKE_CXX_COMPILER_TARGET}
  -DCMAKE_Swift_COMPILER_TARGET=${CMAKE_Swift_COMPILER_TARGET}
  -DCMAKE_FIND_PACKAGE_PREFER_CONFIG=${CMAKE_FIND_PACKAGE_PREFER_CONFIG}
  ${SwiftCore_DIR_FLAG}
  ${MAKE_PROGRAM_FLAG})

# StringProcessing
if(SwiftRuntime_ENABLE_stringprocessing)
  ExternalProject_Add(StringProcessing
    PREFIX "StringProcessing"
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/StringProcessing"
    INSTALL_DIR "${CMAKE_INSTALL_PREFIX}"
    INSTALL_COMMAND ""
    # To ensure incremental builds work as expected
    BUILD_ALWAYS 1
    CMAKE_ARGS
      ${COMMON_OPTIONS})
endif()

# Synchronization
if(SwiftRuntime_ENABLE_synchronization)
  ExternalProject_Add(Synchronization
    PREFIX "Synchronization"
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/Synchronization"
    INSTALL_DIR "${CMAKE_INSTALL_PREFIX}"
    INSTALL_COMMAND ""
    # To ensure incremental builds work as expected
    BUILD_ALWAYS 1
    CMAKE_ARGS
      ${COMMON_OPTIONS})
endif()
