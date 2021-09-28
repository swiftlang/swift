#===--- CMakeLists.txt - Back-deployed concurrency support library -------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

cmake_minimum_required(VERSION 3.19.6)

# This is always build standalone
include("${CMAKE_CURRENT_SOURCE_DIR}/../../../cmake/modules/StandaloneOverlay.cmake")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/modules")
set(SWIFT_STDLIB_STABLE_ABI TRUE)
set(SWIFT_STDLIB_SINGLE_THREADED_RUNTIME FALSE)
set(SWIFT_CONCURRENCY_GLOBAL_EXECUTOR "dispatch")
include(AddSwiftStdlib)

# Don't build the libraries for 32-bit iOS targets; there is no back-deployment
# to them.
list(REMOVE_ITEM SWIFT_SDK_IOS_ARCHITECTURES "armv7" "armv7s")
list(REMOVE_ITEM SWIFT_SDK_IOS_SIMULATOR_ARCHITECTURES "i386")

# The back-deployed library can only be shared.
list(APPEND SWIFT_STDLIB_LIBRARY_BUILD_TYPES SHARED)

# Link aginst the libswiftCore in the SDK. This intentionally avoids using
# the locally-built libswiftCore.
foreach(sdk ${SWIFT_SDKS})
    set(sdk_name ${SWIFT_SDK_${sdk}_LIB_SUBDIR})
    set(swift_core_target "swiftCore-${sdk_name}")
    add_library(${swift_core_target} SHARED IMPORTED GLOBAL)
    set_property(TARGET ${swift_core_target}
      PROPERTY IMPORTED_LOCATION "${SWIFT_SDK_${sdk}_PUBLIC_PATH}/usr/lib/swift/libswiftCore.tbd")
    foreach(arch in ${SWIFT_SDK_${sdk}_ARCHITECTURES})
      add_library("${swift_core_target}-${arch}" ALIAS "${swift_core_target}")
    endforeach()
endforeach()

# Build the concurrency library for back deployment.
add_compile_definitions(SWIFT_CONCURRENCY_BACK_DEPLOYMENT)
set(swift_concurrency_install_component back-deployment)
set(swift_concurrency_options
  BACK_DEPLOYMENT_LIBRARY 5.5
  DARWIN_INSTALL_NAME_DIR "@rpath")
set(swift_concurrency_extra_sources
  "../BackDeployConcurrency/Exclusivity.cpp"
  "../BackDeployConcurrency/Metadata.cpp")
set(swift_concurrency_async_fp_mode "never")

add_subdirectory(../Concurrency stdlib/public/BackDeployConcurrency)
