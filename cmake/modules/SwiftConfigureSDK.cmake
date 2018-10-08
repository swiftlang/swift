# Variable that tracks the set of configured SDKs.
#
# Each element in this list is an SDK for which the various
# SWIFT_SDK_${name}_* variables are defined. Swift libraries will be
# built for each variant.
set(SWIFT_CONFIGURED_SDKS)

include(SwiftWindowsSupport)
include(SwiftAndroidSupport)

# Report the given SDK to the user.
function(_report_sdk prefix)
  message(STATUS "${SWIFT_SDK_${prefix}_NAME} SDK:")
  if("${prefix}" STREQUAL "WINDOWS")
    message(STATUS "  UCRT Version: $ENV{UCRTVersion}")
    message(STATUS "  UCRT SDK Dir: $ENV{UniversalCRTSdkDir}")
    message(STATUS "  VC Dir: $ENV{VCToolsInstallDir}")
    if("${CMAKE_BUILD_TYPE}" STREQUAL "DEBUG")
      message(STATUS "  ${CMAKE_BUILD_TYPE} VC++ CRT: MDd")
    else()
      message(STATUS "  ${CMAKE_BUILD_TYPE} VC++ CRT: MD")
    endif()

    foreach(arch ${SWIFT_SDK_${prefix}_ARCHITECTURES})
      swift_windows_include_for_arch(${arch} ${arch}_INCLUDE)
      swift_windows_lib_for_arch(${arch} ${arch}_LIB)
      message(STATUS "  ${arch} INCLUDE: ${${arch}_INCLUDE}")
      message(STATUS "  ${arch} LIB: ${${arch}_LIB}")
    endforeach()
  elseif("${prefix}" STREQUAL "ANDROID")
    message(STATUS " NDK Dir: $ENV{SWIFT_ANDROID_NDK_PATH}")
    foreach(arch ${SWIFT_SDK_${prefix}_ARCHITECTURES})
      swift_android_include_for_arch(${arch} ${arch}_INCLUDE)
      swift_android_lib_for_arch(${arch} ${arch}_LIB)
      message(STATUS "  ${arch} INCLUDE: ${${arch}_INCLUDE}")
      message(STATUS "  ${arch} LIB: ${${arch}_LIB}")
    endforeach()
  else()
    foreach(arch ${SWIFT_SDK_${prefix}_ARCHITECTURES})
      message(STATUS "  ${arch} Path: ${SWIFT_SDK_${prefix}_ARCH_${arch}_PATH}")
    endforeach()
  endif()
  if(prefix IN_LIST SWIFT_APPLE_PLATFORMS)
    message(STATUS "  Version: ${SWIFT_SDK_${prefix}_VERSION}")
    message(STATUS "  Build number: ${SWIFT_SDK_${prefix}_BUILD_NUMBER}")
    message(STATUS "  Deployment version: ${SWIFT_SDK_${prefix}_DEPLOYMENT_VERSION}")
  endif()
  message(STATUS "  Library subdir: ${SWIFT_SDK_${prefix}_LIB_SUBDIR}")
  if(prefix IN_LIST SWIFT_APPLE_PLATFORMS)
    message(STATUS "  Version min name: ${SWIFT_SDK_${prefix}_VERSION_MIN_NAME}")
    message(STATUS "  Triple name: ${SWIFT_SDK_${prefix}_TRIPLE_NAME}")
  endif()
  message(STATUS "  Architectures: ${SWIFT_SDK_${prefix}_ARCHITECTURES}")
  if(NOT prefix IN_LIST SWIFT_APPLE_PLATFORMS)
    if(SWIFT_BUILD_STDLIB)
      foreach(arch ${SWIFT_SDK_${prefix}_ARCHITECTURES})
        message(STATUS "  ICU i18n INCLUDE (${arch}): ${SWIFT_${prefix}_${arch}_ICU_I18N_INCLUDE}")
        message(STATUS "  ICU i18n LIB (${arch}): ${SWIFT_${prefix}_${arch}_ICU_I18N}")
        message(STATUS "  ICU unicode INCLUDE (${arch}): ${SWIFT_${prefix}_${arch}_ICU_UC_INCLUDE}")
        message(STATUS "  ICU unicode LIB (${arch}): ${SWIFT_${prefix}_${arch}_ICU_UC}")
      endforeach()
    endif()
  endif()
  message(STATUS "  Object Format: ${SWIFT_SDK_${prefix}_OBJECT_FORMAT}")

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
#   )
#
# Sadly there are three OS naming conventions.
# xcrun SDK name:   macosx iphoneos iphonesimulator (+ version)
# -mOS-version-min: macosx ios      ios-simulator
# swift -triple:    macosx ios      ios
#
# This macro attempts to configure a given SDK. When successful, it
# defines a number of variables:
#
#   SWIFT_SDK_${prefix}_NAME                Display name for the SDK
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
    version_min_name triple_name architectures)
  # Note: this has to be implemented as a macro because it sets global
  # variables.

  # Find the SDK
  set(SWIFT_SDK_${prefix}_PATH "" CACHE PATH "Path to the ${name} SDK")

  if(NOT SWIFT_SDK_${prefix}_PATH)
    execute_process(
        COMMAND "xcrun" "--sdk" "${xcrun_name}" "--show-sdk-path"
        OUTPUT_VARIABLE SWIFT_SDK_${prefix}_PATH
        OUTPUT_STRIP_TRAILING_WHITESPACE)
    if(NOT EXISTS "${SWIFT_SDK_${prefix}_PATH}/System/Library/Frameworks/module.map")
      message(FATAL_ERROR "${name} SDK not found at ${SWIFT_SDK_${prefix}_PATH}.")
    endif()
  endif()

  if(NOT EXISTS "${SWIFT_SDK_${prefix}_PATH}/System/Library/Frameworks/module.map")
    message(FATAL_ERROR "${name} SDK not found at ${SWIFT_SDK_${prefix}_PATH}.")
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
  set(SWIFT_SDK_${prefix}_OBJECT_FORMAT "MACHO")

  foreach(arch ${architectures})
    # On Darwin, all archs share the same SDK path.
    set(SWIFT_SDK_${prefix}_ARCH_${arch}_PATH "${SWIFT_SDK_${prefix}_PATH}")

    set(SWIFT_SDK_${prefix}_ARCH_${arch}_TRIPLE
        "${arch}-apple-${SWIFT_SDK_${prefix}_TRIPLE_NAME}")
  endforeach()

  # Add this to the list of known SDKs.
  list(APPEND SWIFT_CONFIGURED_SDKS "${prefix}")

  _report_sdk("${prefix}")
endmacro()

macro(configure_sdk_unix name architectures)
  # Note: this has to be implemented as a macro because it sets global
  # variables.

  string(TOUPPER ${name} prefix)
  string(TOLOWER ${name} platform)

  set(SWIFT_SDK_${prefix}_NAME "${name}")
  set(SWIFT_SDK_${prefix}_LIB_SUBDIR "${platform}")
  set(SWIFT_SDK_${prefix}_ARCHITECTURES "${architectures}")
  if("${prefix}" STREQUAL "CYGWIN")
    set(SWIFT_SDK_${prefix}_OBJECT_FORMAT "COFF")
  else()
    set(SWIFT_SDK_${prefix}_OBJECT_FORMAT "ELF")
  endif()

  foreach(arch ${architectures})
    if("${prefix}" STREQUAL "ANDROID")
      if("${arch}" STREQUAL "armv7")
        set(SWIFT_SDK_ANDROID_ARCH_${arch}_NDK_TRIPLE "arm-linux-androideabi")
        set(SWIFT_SDK_ANDROID_ARCH_${arch}_ALT_SPELLING "arm")
        set(SWIFT_SDK_ANDROID_ARCH_${arch}_PATH "${SWIFT_ANDROID_NDK_PATH}/platforms/android-${SWIFT_ANDROID_API_LEVEL}/arch-arm")
        set(SWIFT_SDK_ANDROID_ARCH_${arch}_TRIPLE "armv7-none-linux-androideabi")
      elseif("${arch}" STREQUAL "aarch64")
        set(SWIFT_SDK_ANDROID_ARCH_${arch}_NDK_TRIPLE "aarch64-linux-android")
        set(SWIFT_SDK_ANDROID_ARCH_${arch}_ALT_SPELLING "aarch64")
        set(SWIFT_SDK_ANDROID_ARCH_${arch}_PATH "${SWIFT_ANDROID_NDK_PATH}/platforms/android-${SWIFT_ANDROID_API_LEVEL}/arch-arm64")
        set(SWIFT_SDK_ANDROID_ARCH_${arch}_TRIPLE "aarch64-unknown-linux-android")
      else()
        message(FATAL_ERROR "unknown arch for android SDK: ${arch}")
      endif()

      # Get the prebuilt suffix to create the correct toolchain path when using the NDK
      if("${CMAKE_HOST_SYSTEM_NAME}" STREQUAL "Darwin")
        set(_swift_android_prebuilt_build "darwin-x86_64")
      elseif("${CMAKE_HOST_SYSTEM_NAME}" STREQUAL "Linux")
        set(_swift_android_prebuilt_build "linux-x86_64")
      endif()
      set(SWIFT_SDK_ANDROID_ARCH_${arch}_NDK_PREBUILT_PATH
          "${SWIFT_ANDROID_NDK_PATH}/toolchains/${SWIFT_SDK_ANDROID_ARCH_${arch}_NDK_TRIPLE}-${SWIFT_ANDROID_NDK_GCC_VERSION}/prebuilt/${_swift_android_prebuilt_build}")
    else()
      if(NOT SWIFT_SDK_${prefix}_ARCH_${arch}_PATH)
        set(SWIFT_SDK_${prefix}_ARCH_${arch}_PATH "/")
      endif()

      if("${prefix}" STREQUAL "LINUX")
        if(arch MATCHES "(armv6|armv7)")
          set(SWIFT_SDK_LINUX_ARCH_${arch}_TRIPLE "${arch}-unknown-linux-gnueabihf")
        elseif(arch MATCHES "(aarch64|i686|powerpc64|powerpc64le|s390x|x86_64)")
          set(SWIFT_SDK_LINUX_ARCH_${arch}_TRIPLE "${arch}-unknown-linux-gnu")
        else()
          message(FATAL_ERROR "unknown arch for ${prefix}: ${arch}")
        endif()
      elseif("${prefix}" STREQUAL "FREEBSD")
        if(arch STREQUAL x86_64)
          message(FATAL_ERROR "unsupported arch for FreeBSD: ${arch}")
        endif()

        if(CMAKE_HOST_SYSTEM_NAME NOT STREQUAL FreeBSD)
          message(WARNING "CMAKE_SYSTEM_VERSION will not match target")
        endif()

        string(REPLACE "[-].*" "" freebsd_system_version ${CMAKE_SYSTEM_VERSION})
        message(STATUS "FreeBSD Version: ${freebsd_system_version}")

        set(SWIFT_SDK_FREEBSD_ARCH_x86_64_TRIPLE "x86_64-unknown-freebsd${freebsd_system_version}")
      elseif("${prefix}" STREQUAL "CYGWIN")
        if(NOT arch STREQUAL x86_64)
          message(FATAL_ERROR "unsupported arch for cygwin: ${arch}")
        endif()
        set(SWIFT_SDK_CYGWIN_ARCH_x86_64_TRIPLE "x86_64-unknown-windows-cygnus")
      elseif("${prefix}" STREQUAL "HAIKU")
        if(NOT arch STREQUAL x86_64)
          message(FATAL_ERROR "unsupported arch for Haiku: ${arch}")
        endif()
        set(SWIFT_SDK_HAIKU_ARCH_x86_64_TRIPLE "x86_64-unknown-haiku")
      else()
        message(FATAL_ERROR "unknown Unix OS: ${prefix}")
      endif()
    endif()
  endforeach()

  # Add this to the list of known SDKs.
  list(APPEND SWIFT_CONFIGURED_SDKS "${prefix}")

  _report_sdk("${prefix}")
endmacro()

macro(configure_sdk_windows name environment architectures)
  # Note: this has to be implemented as a macro because it sets global
  # variables.

  string(TOUPPER ${name} prefix)
  string(TOLOWER ${name} platform)

  set(SWIFT_SDK_${prefix}_NAME "${sdk_name}")
  set(SWIFT_SDK_${prefix}_LIB_SUBDIR "windows")
  set(SWIFT_SDK_${prefix}_ARCHITECTURES "${architectures}")
  set(SWIFT_SDK_${prefix}_OBJECT_FORMAT "COFF")

  foreach(arch ${architectures})
    if(arch STREQUAL armv7)
      set(SWIFT_SDK_${prefix}_ARCH_${arch}_TRIPLE
          "thumbv7-unknown-windows-${environment}")
    else()
      set(SWIFT_SDK_${prefix}_ARCH_${arch}_TRIPLE
          "${arch}-unknown-windows-${environment}")
    endif()
    # NOTE: set the path to / to avoid a spurious `--sysroot` from being passed
    # to the driver -- rely on the `INCLUDE` AND `LIB` environment variables
    # instead.
    set(SWIFT_SDK_${prefix}_ARCH_${arch}_PATH "/")
  endforeach()

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

