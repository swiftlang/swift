set(swift_platform_name)
set(swift_platform_flags)
set(swift_platform_sources
    Platform.swift
    TiocConstants.swift
    tgmath.swift.gyb)

add_swift_target_library(swiftDarwin ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    Darwin.swift.gyb
    ${swift_platform_sources}
    POSIXError.swift
    MachError.swift

    SWIFT_COMPILE_FLAGS -Xfrontend -disable-objc-attr-requires-foundation-module "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    TARGET_SDKS ALL_APPLE_PLATFORMS
    API_NOTES_NON_OVERLAY)

add_swift_target_library(swiftGlibc ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    Glibc.swift.gyb
    ${swift_platform_sources}

    SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    TARGET_SDKS ANDROID CYGWIN FREEBSD LINUX HAIKU
    DEPENDS glibc_modulemap)

add_swift_target_library(swiftMSVCRT ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    msvcrt.swift
    ${swift_platform_sources}

    SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    TARGET_SDKS WINDOWS)

set(glibc_modulemap_target_list)
foreach(sdk ${SWIFT_SDKS})
  if(NOT "${sdk}" STREQUAL "LINUX" AND
     NOT "${sdk}" STREQUAL "FREEBSD" AND
     NOT "${sdk}" STREQUAL "ANDROID" AND
     NOT "${sdk}" STREQUAL "CYGWIN" AND
     NOT "${sdk}" STREQUAL "HAIKU")
    continue()
  endif()

  foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
    set(arch_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${arch}")
    set(module_dir "${SWIFTLIB_DIR}/${arch_subdir}")

    if("${sdk}" STREQUAL "HAIKU")
      # Haiku doesn't have /usr/include, instead the headers are located
      # at /system/develop/headers.
      set(GLIBC_INCLUDE_PATH "/system/develop/headers/posix")
      set(GLIBC_ARCH_INCLUDE_PATH "/system/develop/headers/posix")
      set(GLIBC_SYSROOT_RELATIVE_ARCH_INCLUDE_PATH "/system/develop/headers/")
    elseif("${sdk}" STREQUAL "ANDROID")
      set(GLIBC_SYSROOT_RELATIVE_INCLUDE_PATH "${SWIFT_ANDROID_NDK_PATH}/sysroot/usr/include")
      set(GLIBC_SYSROOT_RELATIVE_ARCH_INCLUDE_PATH "${SWIFT_ANDROID_NDK_PATH}/sysroot/usr/include")
    else()
      # Determine the location of glibc headers based on the target.
      set(GLIBC_SYSROOT_RELATIVE_INCLUDE_PATH "/usr/include")
      set(GLIBC_SYSROOT_RELATIVE_ARCH_INCLUDE_PATH "${GLIBC_SYSROOT_RELATIVE_INCLUDE_PATH}")
    endif()

    # Some SDKs place their headers in architecture-specific subfolders.
    if((${sdk} STREQUAL "LINUX" OR ${sdk} STREQUAL "FREEBSD") AND CMAKE_LIBRARY_ARCHITECTURE)
      set(GLIBC_SYSROOT_RELATIVE_ARCH_INCLUDE_PATH "${GLIBC_SYSROOT_RELATIVE_ARCH_INCLUDE_PATH}/${CMAKE_LIBRARY_ARCHITECTURE}")
    endif()

    set(GLIBC_INCLUDE_PATH "${GLIBC_SYSROOT_RELATIVE_INCLUDE_PATH}")
    set(GLIBC_ARCH_INCLUDE_PATH "${GLIBC_SYSROOT_RELATIVE_ARCH_INCLUDE_PATH}")

    if(NOT "${SWIFT_SDK_${sdk}_ARCH_${arch}_PATH}" STREQUAL "/" AND NOT "${sdk}" STREQUAL "ANDROID")
      set(GLIBC_INCLUDE_PATH "${SWIFT_SDK_${sdk}_ARCH_${arch}_PATH}${GLIBC_INCLUDE_PATH}")
      set(GLIBC_ARCH_INCLUDE_PATH "${SWIFT_SDK_${sdk}_ARCH_${arch}_PATH}${GLIBC_ARCH_INCLUDE_PATH}")
    endif()

    set(glibc_modulemap_source "glibc.modulemap.gyb")
    set(glibc_modulemap_out "${module_dir}/glibc.modulemap")

    # Configure the module map based on the target. Each platform needs to
    # reference different headers, based on what's available in their glibc.
    # This is the 'glibc.modulemap' in the 'resource-dir', so
    # it's the one we'll look at during the build process.
    handle_gyb_source_single(glibc_modulemap_target
        SOURCE "${glibc_modulemap_source}"
        OUTPUT "${glibc_modulemap_out}"
        FLAGS
            "-DCMAKE_SDK=${sdk}"
            "-DGLIBC_INCLUDE_PATH=${GLIBC_INCLUDE_PATH}"
            "-DGLIBC_ARCH_INCLUDE_PATH=${GLIBC_ARCH_INCLUDE_PATH}")

    list(APPEND glibc_modulemap_target_list ${glibc_modulemap_target})

    # If this SDK is a target for a non-native host, create a native modulemap
    # without a sysroot prefix. This is the one we'll install instead.
    if(NOT "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_ARCH_${arch}_PATH}" STREQUAL "/")

      set(glibc_sysroot_relative_modulemap_out "${module_dir}/sysroot-relative-modulemaps/glibc.modulemap")
      handle_gyb_source_single(glibc_modulemap_native_target
          SOURCE "${glibc_modulemap_source}"
          OUTPUT "${glibc_sysroot_relative_modulemap_out}"
          FLAGS
              "-DCMAKE_SDK=${sdk}"
              "-DGLIBC_INCLUDE_PATH=${GLIBC_SYSROOT_RELATIVE_INCLUDE_PATH}"
              "-DGLIBC_ARCH_INCLUDE_PATH=${GLIBC_SYSROOT_RELATIVE_ARCH_INCLUDE_PATH}")

      list(APPEND glibc_modulemap_target_list ${glibc_modulemap_native_target})
      set(glibc_modulemap_out ${glibc_sysroot_relative_modulemap_out})
    endif()

    # FIXME: When SDK is a cross-compile target (SDK != Host), the generated
    #        modulemap will be relative to the Host, with hardcoded paths.
    #        It is not relocatable to the target platform itself.
    #        This affects any cross-comipled targets that use glibc.modulemap.

    swift_install_in_component(sdk-overlay glibc
        FILES "${glibc_modulemap_out}"
        DESTINATION "lib/swift/${arch_subdir}")

  endforeach()
endforeach()
add_custom_target(glibc_modulemap DEPENDS ${glibc_modulemap_target_list})
set_property(TARGET glibc_modulemap PROPERTY FOLDER "Miscellaneous")
