#===--- CMakeLists.txt - Platform differentiation library ----------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

if("${SWIFT_STDLIB_BUILD_TYPE}" STREQUAL "Debug")
  set(bin_dir_path_extension "Debug")
else()
  set(bin_dir_path_extension ".")
endif()

set(swift_lib_products_dir 
  "${SWIFT_BINARY_DIR}/${bin_dir_path_extension}/lib/swift/${SWIFT_HOST_VARIANT}")

if("${SWIFT_HOST_VARIANT_SDK}" IN_LIST SWIFT_DARWIN_PLATFORMS)
  set(platform_lib_name Darwin)
elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "LINUX")
  set(platform_lib_name Glibc)
elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "FREEBSD")
  set(platform_lib_name Glibc)
elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "OPENBSD")
  set(platform_lib_name Glibc)
elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "CYGWIN")
  set(platform_lib_name Glibc)
elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "HAIKU")
  set(platform_lib_name Glibc)
elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "ANDROID")
  set(platform_lib_name Glibc)
elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "WASI")
  set(platform_lib_name WASILibc)
elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "WINDOWS")
  set(platform_lib_name CRT)
else()
  message(FATAL_ERROR "Unsupported platform")
endif()

file(WRITE 
  "${swift_lib_products_dir}/${platform_lib_name}.swiftcrossimport/_Differentiation.swiftoverlay" 
  "version: 1\nmodules:\n- name: _${platform_lib_name}_Differentiation\n")

add_swift_target_library(swift_Darwin_Differentiation ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  GYB_SOURCES
    TgmathDerivatives.swift.gyb

  SWIFT_MODULE_DEPENDS_OSX Darwin _Differentiation
  SWIFT_MODULE_DEPENDS_IOS Darwin _Differentiation
  SWIFT_MODULE_DEPENDS_TVOS Darwin _Differentiation
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin _Differentiation

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS ALL_APPLE_PLATFORMS FREESTANDING
  INSTALL_IN_COMPONENT sdk-overlay)

add_swift_target_library(swift_Glibc_Differentiation ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  GYB_SOURCES
    TgmathDerivatives.swift.gyb

  SWIFT_MODULE_DEPENDS_LINUX Glibc _Differentiation
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc _Differentiation
  SWIFT_MODULE_DEPENDS_OPENBSD Glibc _Differentiation
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc _Differentiation
  SWIFT_MODULE_DEPENDS_HAIKU Glibc _Differentiation

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS ANDROID CYGWIN FREEBSD OPENBSD LINUX HAIKU
  INSTALL_IN_COMPONENT sdk-overlay)

add_swift_target_library(swift_WASILibc_Differentiation ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  GYB_SOURCES
    TgmathDerivatives.swift.gyb

  SWIFT_MODULE_DEPENDS_WASI WASILibc _Differentiation

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS WASI
  INSTALL_IN_COMPONENT sdk-overlay)

add_swift_target_library(swift_CRT_Differentiation ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  GYB_SOURCES
    TgmathDerivatives.swift.gyb

  SWIFT_MODULE_DEPENDS_WINDOWS CRT _Differentiation

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS WINDOWS
  INSTALL_IN_COMPONENT sdk-overlay)
