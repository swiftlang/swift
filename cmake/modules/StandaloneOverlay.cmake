# CMAKE_SOURCE_DIR is the directory that cmake got initially invoked on.
# CMAKE_CURRENT_SOURCE_DIR is the current directory. If these are equal, it's
# a top-level build of the CMAKE_SOURCE_DIR. Otherwise, define a guard variable
# and return.
if(DEFINED SWIFT_MASTER_LOADED
  OR NOT ${CMAKE_CURRENT_SOURCE_DIR} STREQUAL ${CMAKE_SOURCE_DIR})
  set(SWIFT_MASTER_LOADED TRUE)
  return()
endif()

set(CMAKE_INSTALL_PREFIX "${SWIFT_DEST_ROOT}${TOOLCHAIN_DIR}/usr")

# Only happens if it's called from a top-level cmake invocation.
set(BUILD_STANDALONE TRUE)
set(SWIFT_STDLIB_BUILD_TYPE "Release")
set(SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES "SHARED")
set(SWIFT_INSTALL_COMPONENTS "sdk-overlay" CACHE STRING "")
set(SWIFT_DARWIN_DEPLOYMENT_VERSION_OSX "10.9" CACHE STRING "")
set(SWIFT_DARWIN_DEPLOYMENT_VERSION_IOS "7.0" CACHE STRING "")
set(SWIFT_DARWIN_DEPLOYMENT_VERSION_TVOS "9.0" CACHE STRING "")
set(SWIFT_DARWIN_DEPLOYMENT_VERSION_WATCHOS "2.0" CACHE STRING "")

set(SWIFT_SOURCE_DIR "${SWIFT_SOURCE_ROOT}/src/swift" CACHE PATH "")
set(SWIFT_PATH_TO_LLVM_SOURCE "${SWIFT_SOURCE_ROOT}/src/llvm" CACHE PATH "")
set(SWIFT_NATIVE_SWIFT_TOOLS_PATH "${TOOLCHAIN_DIR}/usr/bin" CACHE PATH "")
set(SWIFT_SDKS ${SWIFT_HOST_VARIANT_SDK})

set(LLVM_CMAKE_MODULES_PATH "${SWIFT_PATH_TO_LLVM_SOURCE}/cmake/modules")

list(APPEND CMAKE_MODULE_PATH
  "${PROJECT_SOURCE_DIR}/../../../../cmake/modules"
  ${LLVM_CMAKE_MODULES_PATH}
)

set(SWIFT_DARWIN_XCRUN_TOOLCHAIN "XcodeDefault" CACHE STRING
    "The name of the toolchain to pass to 'xcrun'")

include(SwiftToolchainUtils)
if(NOT SWIFT_LIPO)
  find_toolchain_tool(SWIFT_LIPO "${SWIFT_DARWIN_XCRUN_TOOLCHAIN}" lipo)
endif()

include(AddLLVM)
include(SwiftUtils)
include(SwiftSharedCMakeConfig)
include(AddSwift)
include(SwiftHandleGybSources)
include(SwiftConfigureSDK)
include(SwiftSource)
include(SwiftComponents)
include(DarwinSDKs)

# These variables should be passed as -D variables to cmake.
# e.g. cmake -G Ninja -DSWIFT_HOST_VARIANT_SDK=OSX ..
precondition(CMAKE_INSTALL_PREFIX)
precondition(SWIFT_SOURCE_ROOT)
precondition(SWIFT_DEST_ROOT)
precondition(SWIFT_HOST_VARIANT_SDK)
precondition(TOOLCHAIN_DIR)

# Without this line, installing components is broken. This needs refactoring.
swift_configure_components()

precondition(unknown_sdks NEGATE MESSAGE "Unknown SDKs: ${unknown_sdks}")
precondition(SWIFT_CONFIGURED_SDKS MESSAGE "No SDKs selected.")
precondition(SWIFT_HOST_VARIANT_SDK MESSAGE "No SDK for host tools.")

# ARCH is set somewhere later.
#precondition(SWIFT_HOST_VARIANT_ARCH MESSAGE "No arch for host tools")
