cmake_minimum_required(VERSION 3.26...3.29)
# TODO before requiring CMake 4.1 or later
# and/or enforcing CMP0195, please check/update
# the implementation  of `emit_swift_interface`
# in `EmitSwiftInterface.cmake`
# to ensure it keeps laying down nested swiftmodule folders

set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

list(APPEND CMAKE_MODULE_PATH
  "${CMAKE_SOURCE_DIR}/cmake/modules"
  "${CMAKE_SOURCE_DIR}/../cmake/modules")

if(POLICY CMP0157 AND CMAKE_Swift_COMPILER_USE_OLD_DRIVER)
  cmake_policy(SET CMP0157 OLD)
endif()

include(SwiftProjectVersion)
project(SwiftToolchain
  LANGUAGES ASM
  VERSION ${SWIFT_RUNTIME_VERSION})


set(${PROJECT_NAME}_VENDOR_MODULE_DIR "${CMAKE_SOURCE_DIR}/cmake/modules/vendor"
  CACHE FILEPATH "Location for private build system extension")

include(GNUInstallDirs)

include(PlatformInfo)
include(CatalystSupport)

set(SwiftToolchain_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

if(SwiftToolchain_ENABLE_CLIENT_RETAIN_RELEASE)
  add_subdirectory(ClientRetainRelease)
endif()

install(CODE "message(\"Ensuring the install target is created even if nothing builds\")")