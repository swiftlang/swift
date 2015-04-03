# Variable that tracks the set of configured SDKs.
#
# Each element in this list is an SDK for which the various
# SWIFT_SDK_${name}_* variables are defined. Swift libraries will be
# built for each variant.
set(SWIFT_CONFIGURED_SDKS)

# Report the given SDK to the user.
function(_report_sdk prefix)
  message(STATUS "${SWIFT_SDK_${prefix}_NAME} SDK:")
  message(STATUS "  Path: ${SWIFT_SDK_${prefix}_PATH}")
  message(STATUS "  Version: ${SWIFT_SDK_${prefix}_VERSION}")
  message(STATUS "  Build number: ${SWIFT_SDK_${prefix}_BUILD_NUMBER}")
  message(STATUS "  Deployment version: ${SWIFT_SDK_${prefix}_DEPLOYMENT_VERSION}")
  message(STATUS "  Library subdir: ${SWIFT_SDK_${prefix}_LIB_SUBDIR}")
  message(STATUS "  Version min name: ${SWIFT_SDK_${prefix}_VERSION_MIN_NAME}")
  message(STATUS "  Triple name: ${SWIFT_SDK_${prefix}_TRIPLE_NAME}")
  message(STATUS "  Architectures: ${SWIFT_SDK_${prefix}_ARCHITECTURES}")

  foreach(arch ${SWIFT_SDK_${prefix}_ARCHITECTURES})
    message(STATUS
        "  Triple for ${arch} is ${SWIFT_SDK_${prefix}_ARCH_${arch}_TRIPLE}")
  endforeach()

  message(STATUS "")
endfunction()

# Configure an SDK
#
# Usage:
#   configure_sdk_darwin(
#     prefix             # Prefix to use for SDK variables (e.g., OSX)
#     name               # Display name for this SDK
#     deployment_version # Deployment version
#     xcrun_name         # SDK name to use with xcrun
#     version_min_name   # The name used in the -mOS-version-min flag
#     triple_name        # The name used in Swift's -triple
#     architectures      # A list of architectures this SDK supports
#     internal           # Whether the prefer the internal SDK, if present
#   )
#
# Sadly there are three OS naming conventions.
# xcrun SDK name:   macosx iphoneos iphonesimulator (+ "internal" or version)
# -mOS-version-min: macosx ios      ios-simulator
# swift -triple:    macosx ios      ios
#
# This macro attempts to configure a given SDK. When successful, it
# defines a number of variables:
#
#   SWIFT_SDK_${prefix}_NAME                Display name for the SDK
#   SWIFT_SDK_${prefix}_PATH                Path to the SDK
#   SWIFT_SDK_${prefix}_VERSION             SDK version number (e.g., 10.9, 7.0)
#   SWIFT_SDK_${prefix}_BUILD_NUMBER        SDK build number (e.g., 14A389a)
#   SWIFT_SDK_${prefix}_DEPLOYMENT_VERSION  Deployment version (e.g., 10.9, 7.0)
#   SWIFT_SDK_${prefix}_LIB_SUBDIR          Library subdir for this SDK
#   SWIFT_SDK_${prefix}_VERSION_MIN_NAME    Version min name for this SDK
#   SWIFT_SDK_${prefix}_TRIPLE_NAME         Triple name for this SDK
#   SWIFT_SDK_${prefix}_ARCHITECTURES       Architectures (as a list)
#   SWIFT_SDK_${prefix}_ARCH_${ARCH}_TRIPLE Triple name
macro(configure_sdk_darwin
    prefix name deployment_version xcrun_name
    version_min_name triple_name architectures internal)
  # Note: this has to be implemented as a macro because it sets global
  # variables.

  # Find the SDK
  set(SWIFT_SDK_${prefix}_PATH "" CACHE PATH "Path to the ${name} SDK")

  if(NOT SWIFT_SDK_${prefix}_PATH)
    if(${internal})
      execute_process(
          COMMAND "xcrun" "--sdk" "${xcrun_name}.internal" "--show-sdk-path"
          OUTPUT_VARIABLE SWIFT_SDK_${prefix}_PATH
          OUTPUT_STRIP_TRAILING_WHITESPACE)
    endif()
  endif()

  if(NOT SWIFT_SDK_${prefix}_PATH)
    execute_process(
        COMMAND "xcrun" "--sdk" "${xcrun_name}" "--show-sdk-path"
        OUTPUT_VARIABLE SWIFT_SDK_${prefix}_PATH
        OUTPUT_STRIP_TRAILING_WHITESPACE)
  endif()

  if(NOT EXISTS "${SWIFT_SDK_${prefix}_PATH}/System/Library/Frameworks/module.map")
    message(FATAL_ERROR "${name} SDK not found at SWIFT_SDK_${prefix}_PATH.")
  endif()

  # Determine the SDK version we found.
  execute_process(
    COMMAND "defaults" "read" "${SWIFT_SDK_${prefix}_PATH}/SDKSettings.plist" "Version"
      OUTPUT_VARIABLE SWIFT_SDK_${prefix}_VERSION
      OUTPUT_STRIP_TRAILING_WHITESPACE)

  execute_process(
    COMMAND "xcodebuild" "-sdk" "${SWIFT_SDK_${prefix}_PATH}" "-version" "ProductBuildVersion"
      OUTPUT_VARIABLE SWIFT_SDK_${prefix}_BUILD_NUMBER
      OUTPUT_STRIP_TRAILING_WHITESPACE)

  # Set other variables.
  set(SWIFT_SDK_${prefix}_NAME "${name}")
  set(SWIFT_SDK_${prefix}_DEPLOYMENT_VERSION "${deployment_version}")
  set(SWIFT_SDK_${prefix}_LIB_SUBDIR "${xcrun_name}")
  set(SWIFT_SDK_${prefix}_VERSION_MIN_NAME "${version_min_name}")
  set(SWIFT_SDK_${prefix}_TRIPLE_NAME "${triple_name}")
  set(SWIFT_SDK_${prefix}_ARCHITECTURES "${architectures}")

  foreach(arch ${architectures})
    set(SWIFT_SDK_${prefix}_ARCH_${arch}_TRIPLE
        "${arch}-apple-${SWIFT_SDK_${prefix}_TRIPLE_NAME}${SWIFT_SDK_${prefix}_DEPLOYMENT_VERSION}")
  endforeach()

  # Add this to the list of known SDKs.
  list(APPEND SWIFT_CONFIGURED_SDKS "${prefix}")

  _report_sdk("${prefix}")
endmacro()

macro(configure_sdk_unix
    prefix name lib_subdir triple_name arch triple)
  # Note: this has to be implemented as a macro because it sets global
  # variables.

  set(SWIFT_SDK_${prefix}_NAME "${name}")
  set(SWIFT_SDK_${prefix}_PATH "/")
  set(SWIFT_SDK_${prefix}_VERSION "don't use")
  set(SWIFT_SDK_${prefix}_BUILD_NUMBER "don't use")
  set(SWIFT_SDK_${prefix}_DEPLOYMENT_VERSION "don't use")
  set(SWIFT_SDK_${prefix}_LIB_SUBDIR "${lib_subdir}")
  set(SWIFT_SDK_${prefix}_VERSION_MIN_NAME "")
  set(SWIFT_SDK_${prefix}_TRIPLE_NAME "${triple_name}")
  set(SWIFT_SDK_${prefix}_ARCHITECTURES "${arch}")

  set(SWIFT_SDK_${prefix}_ARCH_${arch}_TRIPLE "${triple}")

  # Add this to the list of known SDKs.
  list(APPEND SWIFT_CONFIGURED_SDKS "${prefix}")

  _report_sdk("${prefix}")
endmacro()

# Configure a variant of a certain SDK
#
# In addition to the SDK and architecture, a variant determines build settings.
#
# FIXME: this is not wired up with anything yet.
function(configure_target_variant prefix name sdk build_config lib_subdir)
  set(SWIFT_VARIANT_${prefix}_NAME               ${name})
  set(SWIFT_VARIANT_${prefix}_SDK_PATH           ${SWIFT_SDK_${sdk}_PATH})
  set(SWIFT_VARIANT_${prefix}_VERSION            ${SWIFT_SDK_${sdk}_VERSION})
  set(SWIFT_VARIANT_${prefix}_BUILD_NUMBER       ${SWIFT_SDK_${sdk}_BUILD_NUMBER})
  set(SWIFT_VARIANT_${prefix}_DEPLOYMENT_VERSION ${SWIFT_SDK_${sdk}_DEPLOYMENT_VERSION})
  set(SWIFT_VARIANT_${prefix}_LIB_SUBDIR         "${lib_subdir}/${SWIFT_SDK_${sdk}_LIB_SUBDIR}")
  set(SWIFT_VARIANT_${prefix}_VERSION_MIN_NAME   ${SWIFT_SDK_${sdk}_VERSION_MIN_NAME})
  set(SWIFT_VARIANT_${prefix}_TRIPLE_NAME        ${SWIFT_SDK_${sdk}_TRIPLE_NAME})
  set(SWIFT_VARIANT_${prefix}_ARCHITECTURES      ${SWIFT_SDK_${sdk}_ARCHITECTURES})
endfunction()

