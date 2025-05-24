set(swift_platform_sources
    Platform.swift
    TiocConstants.swift)
set(swift_platform_gyb_sources
    tgmath.swift.gyb)

set(swift_platform_compile_flags)
if(SWIFT_ENABLE_REFLECTION)
  list(APPEND swift_platform_compile_flags "-DSWIFT_ENABLE_REFLECTION")
endif()

set(swiftGlibc_target_sdks CYGWIN FREEBSD OPENBSD LINUX HAIKU)
if(SWIFT_FREESTANDING_FLAVOR STREQUAL "linux")
  set(swiftGlibc_target_sdks CYGWIN FREEBSD OPENBSD LINUX HAIKU FREESTANDING)
endif()
add_swift_target_library(swiftGlibc ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    ${swift_platform_sources}
    POSIXError.swift

    GYB_SOURCES
      ${swift_platform_gyb_sources}
      Glibc.swift.gyb

    SWIFT_COMPILE_FLAGS
      ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}
      ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
      ${swift_platform_compile_flags}
    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    TARGET_SDKS "${swiftGlibc_target_sdks}"
    INSTALL_IN_COMPONENT sdk-overlay
    DEPENDS glibc_modulemap)

add_swift_target_library(swiftMusl ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    ${swift_platform_sources}
    POSIXError.swift

    GYB_SOURCES
      ${swift_platform_gyb_sources}
      Musl.swift.gyb

    SWIFT_COMPILE_FLAGS
      ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}
      ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
      ${swift_platform_compile_flags}
    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    TARGET_SDKS LINUX_STATIC
    INSTALL_IN_COMPONENT sdk-overlay
    DEPENDS musl_modulemap)

add_swift_target_library(swiftWASILibc ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    ${swift_platform_sources}
    POSIXError.swift

    GYB_SOURCES
      ${swift_platform_gyb_sources}
      WASILibc.swift.gyb

    SWIFT_COMPILE_FLAGS
      ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}
      ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
      ${swift_platform_compile_flags}
    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    TARGET_SDKS WASI
    INSTALL_IN_COMPONENT sdk-overlay
    DEPENDS wasilibc_modulemap)

add_swift_target_library(swiftCRT ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    ucrt.swift
    ${swift_platform_sources}
    POSIXError.swift

    GYB_SOURCES
      ${swift_platform_gyb_sources}

    SWIFT_COMPILE_FLAGS
      ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}
      ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
      -Xcc -D_USE_MATH_DEFINES
      ${swift_platform_compile_flags}
    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    TARGET_SDKS WINDOWS
    INSTALL_IN_COMPONENT sdk-overlay)

foreach(sdk ${SWIFT_SDKS})
  # Add more platforms supporting Musl to these checks,
  if(NOT "${sdk}" STREQUAL "LINUX_STATIC")
    continue()
  endif()

  set(musl_modulemap "musl.modulemap")
  set(musl_header "SwiftMusl.h")

  foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
    set(arch_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${arch}")
    set(module_dir "${SWIFTLIB_DIR}/${arch_subdir}")
    set(module_dir_static "${SWIFTSTATICLIB_DIR}/${arch_subdir}")

    set(musl_modulemap_out "${module_dir}/${musl_modulemap}")
    set(musl_modulemap_out_static "${module_dir_static}/${musl_modulemap}")
    set(musl_header_out "${module_dir}/${musl_header}")
    set(musl_header_out_static "${module_dir_static}/${musl_header}")

    add_custom_command_target(
      copy_musl_modulemap
      COMMAND
        "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir}
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy"
          ${CMAKE_CURRENT_LIST_DIR}/${musl_modulemap}
          ${musl_modulemap_out}
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy"
          ${CMAKE_CURRENT_LIST_DIR}/${musl_header}
          ${musl_header_out}
      OUTPUT ${musl_modulemap_out} ${musl_header_out}
      COMMENT "Copying Musl modulemap to resources")

    list(APPEND musl_modulemap_target_list ${copy_musl_modulemap})

    if(SWIFT_BUILD_STATIC_STDLIB OR SWIFT_SDK_${sdk}_STATIC_ONLY)
      add_custom_command_target(
        copy_musl_modulemap_static
        COMMAND
          "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir_static}
        COMMAND
          "${CMAKE_COMMAND}" "-E" "copy"
            ${CMAKE_CURRENT_LIST_DIR}/${musl_modulemap}
            ${musl_modulemap_out_static}
        COMMAND
          "${CMAKE_COMMAND}" "-E" "copy"
            ${CMAKE_CURRENT_LIST_DIR}/${musl_header}
            ${musl_header_out_static}
        OUTPUT ${musl_modulemap_out_static} ${musl_header_out_static}
        COMMENT "Copying Musl modulemap to static resources")

      list(APPEND musl_modulemap_target_list ${copy_musl_modulemap_static})

      swift_install_in_component(FILES "${musl_modulemap_out_static}"
                                 DESTINATION "lib/swift_static/${arch_subdir}"
                                 COMPONENT sdk-overlay)
      swift_install_in_component(FILES "${musl_header_out_static}"
                                 DESTINATION "lib/swift_static/${arch_subdir}"
                                 COMPONENT sdk-overlay)
    endif()

    swift_install_in_component(FILES "${musl_modulemap_out}"
                               DESTINATION "lib/swift/${arch_subdir}"
                               COMPONENT sdk-overlay)
    swift_install_in_component(FILES "${musl_header_out}"
                               DESTINATION "lib/swift/${arch_subdir}"
                               COMPONENT sdk-overlay)
  endforeach()
endforeach()
add_custom_target(musl_modulemap DEPENDS ${musl_modulemap_target_list})
set_property(TARGET musl_modulemap PROPERTY FOLDER "Miscellaneous")
add_dependencies(sdk-overlay musl_modulemap)

# Add a system 'math' module overlay, which applies only to platforms that
# have a top level '_math' module.
add_swift_target_library(swift_math ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    Math.swift

    SWIFT_COMPILE_FLAGS
      ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}
      ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
      ${swift_platform_compile_flags}
    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    TARGET_SDKS "ANDROID"
    INSTALL_IN_COMPONENT sdk-overlay
    DEPENDS android_modulemap)

add_swift_target_library(swiftAndroid ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    Android.swift
    ${swift_platform_sources}
    POSIXError.swift

    GYB_SOURCES
      ${swift_platform_gyb_sources}

    SWIFT_COMPILE_FLAGS
      ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}
      ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
      ${swift_platform_compile_flags}
    LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
    TARGET_SDKS "ANDROID"
    INSTALL_IN_COMPONENT sdk-overlay
    DEPENDS android_modulemap)

set(glibc_modulemap_target_list)
foreach(sdk ${SWIFT_SDKS})
  if(NOT "${sdk}" STREQUAL "LINUX" AND
     NOT "${sdk}" STREQUAL "FREEBSD" AND
     NOT "${sdk}" STREQUAL "OPENBSD" AND
     NOT "${sdk}" STREQUAL "CYGWIN" AND
     NOT "${sdk}" STREQUAL "HAIKU")
    continue()
  endif()

  foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
    set(arch_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${arch}")
    set(module_dir "${SWIFTLIB_DIR}/${arch_subdir}")
    set(module_dir_static "${SWIFTSTATICLIB_DIR}/${arch_subdir}")

    set(glibc_modulemap_source "glibc.modulemap.gyb")
    set(glibc_modulemap_out "${module_dir}/glibc.modulemap")
    set(glibc_modulemap_out_static "${module_dir_static}/glibc.modulemap")

    # Configure the module map based on the target. Each platform needs to
    # reference different headers, based on what's available in their glibc.
    # This is the 'glibc.modulemap' in the 'resource-dir', so
    # it's the one we'll look at during the build process.
    handle_gyb_source_single(glibc_modulemap_target
        SOURCE "${glibc_modulemap_source}"
        OUTPUT "${glibc_modulemap_out}"
        FLAGS
            "-DCMAKE_SDK=${sdk}")

    list(APPEND glibc_modulemap_target_list ${glibc_modulemap_target})

    set(glibc_header_out "${module_dir}/SwiftGlibc.h")
    set(glibc_header_out_static "${module_dir_static}/SwiftGlibc.h")
    handle_gyb_source_single(glibc_header_target
        SOURCE "SwiftGlibc.h.gyb"
        OUTPUT "${glibc_header_out}"
        FLAGS "-DCMAKE_SDK=${sdk}")
    list(APPEND glibc_modulemap_target_list ${glibc_header_target})

    if(SWIFT_BUILD_STATIC_STDLIB OR SWIFT_SDK_${sdk}_STATIC_ONLY)
      add_custom_command_target(
        copy_glibc_modulemap_header_static
        COMMAND
          "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir_static}
        COMMAND
          "${CMAKE_COMMAND}" "-E" "copy"
            ${glibc_modulemap_out} ${glibc_modulemap_out_static}
        COMMAND
          "${CMAKE_COMMAND}" "-E" "copy"
            ${glibc_header_out} ${glibc_header_out_static}
        OUTPUT ${glibc_modulemap_out_static} ${glibc_header_out_static}
        DEPENDS
          "${glibc_modulemap_target}"
          "${glibc_header_target}"
        COMMENT "Copying Glibc modulemap and header to static resources")

      list(APPEND glibc_modulemap_target_list
        ${copy_glibc_modulemap_header_static})
    endif()

    # If this SDK is a target for a non-native host, create a native modulemap
    # without a sysroot prefix. This is the one we'll install instead.
    if(NOT "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_ARCH_${arch}_PATH}" STREQUAL "/")
      set(glibc_sysroot_relative_modulemap_out "${module_dir}/sysroot-relative-modulemaps/glibc.modulemap")

      handle_gyb_source_single(glibc_modulemap_native_target
        SOURCE "${glibc_modulemap_source}"
        OUTPUT "${glibc_sysroot_relative_modulemap_out}"
        FLAGS "-DCMAKE_SDK=${sdk}")

      list(APPEND glibc_modulemap_target_list ${glibc_modulemap_native_target})
      set(glibc_modulemap_out ${glibc_sysroot_relative_modulemap_out})
    endif()

    # FIXME: When SDK is a cross-compile target (SDK != Host), the generated
    #        modulemap will be relative to the Host, with hardcoded paths.
    #        It is not relocatable to the target platform itself.
    #        This affects any cross-compiled targets that use glibc.modulemap.

    swift_install_in_component(FILES "${glibc_modulemap_out}"
                               DESTINATION "lib/swift/${arch_subdir}"
                               COMPONENT sdk-overlay)
    swift_install_in_component(FILES "${glibc_header_out}"
                               DESTINATION "lib/swift/${arch_subdir}"
                               COMPONENT sdk-overlay)

    if(SWIFT_BUILD_STATIC_STDLIB OR SWIFT_SDK_${sdk}_STATIC_ONLY)
      swift_install_in_component(FILES "${glibc_modulemap_out}"
                                 DESTINATION "lib/swift_static/${arch_subdir}"
                                 COMPONENT sdk-overlay)
      swift_install_in_component(FILES "${glibc_header_out}"
                                 DESTINATION "lib/swift_static/${arch_subdir}"
                                 COMPONENT sdk-overlay)
    endif()
  endforeach()
endforeach()
add_custom_target(glibc_modulemap DEPENDS ${glibc_modulemap_target_list})
set_property(TARGET glibc_modulemap PROPERTY FOLDER "Miscellaneous")
add_dependencies(sdk-overlay glibc_modulemap)

set(android_modulemap_target_list)
if("ANDROID" IN_LIST SWIFT_SDKS)
  set(android_modulemap_source "android.modulemap")
  set(android_ndk_header_source "SwiftAndroidNDK.h")
  set(android_bionic_header_source "SwiftBionic.h")

  foreach(arch ${SWIFT_SDK_ANDROID_ARCHITECTURES})
    set(arch_subdir "${SWIFT_SDK_ANDROID_LIB_SUBDIR}/${arch}")
    set(module_dir "${SWIFTLIB_DIR}/${arch_subdir}")
    set(module_dir_static "${SWIFTSTATICLIB_DIR}/${arch_subdir}")

    add_custom_command_target(
      copy_android_modulemap_resource
      COMMAND
        "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir} ${module_dir_static}
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
          "${CMAKE_CURRENT_SOURCE_DIR}/${android_modulemap_source}" ${module_dir}
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
          "${CMAKE_CURRENT_SOURCE_DIR}/${android_modulemap_source}" ${module_dir_static}
      OUTPUT ${module_dir}/${android_modulemap_source} ${module_dir_static}/${android_modulemap_source}
      COMMENT "Copying Android modulemap to resource directories")
    add_custom_command_target(
      copy_android_ndk_header_resource
      COMMAND
        "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir} ${module_dir_static}
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
          "${CMAKE_CURRENT_SOURCE_DIR}/${android_ndk_header_source}" ${module_dir}
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
          "${CMAKE_CURRENT_SOURCE_DIR}/${android_ndk_header_source}" ${module_dir_static}
      OUTPUT ${module_dir}/${android_ndk_header_source} ${module_dir_static}/${android_ndk_header_source}
      COMMENT "Copying Android NDK header to resource directories")
    add_custom_command_target(
      copy_android_bionic_header_resource
      COMMAND
        "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir} ${module_dir_static}
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
          "${CMAKE_CURRENT_SOURCE_DIR}/${android_bionic_header_source}" ${module_dir}
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
          "${CMAKE_CURRENT_SOURCE_DIR}/${android_bionic_header_source}" ${module_dir_static}
      OUTPUT ${module_dir}/${android_bionic_header_source} ${module_dir_static}/${android_bionic_header_source}
      COMMENT "Copying Android NDK header to resource directories")

    add_dependencies(sdk-overlay ${copy_android_modulemap_resource}
                                 ${copy_android_ndk_header_resource}
                                 ${copy_android_bionic_header_resource})
    list(APPEND android_modulemap_target_list ${copy_android_modulemap_resource}
                                              ${copy_android_ndk_header_resource}
                                              ${copy_android_bionic_header_resource})

    swift_install_in_component(FILES "${android_modulemap_source}"
                                     "${android_ndk_header_source}"
                                     "${android_bionic_header_source}"
                               DESTINATION "lib/swift/${arch_subdir}"
                               COMPONENT sdk-overlay)
    if(SWIFT_BUILD_STATIC_STDLIB)
      swift_install_in_component(FILES "${android_modulemap_source}"
                                       "${android_ndk_header_source}"
                                       "${android_bionic_header_source}"
                                 DESTINATION "lib/swift_static/${arch_subdir}"
                                 COMPONENT sdk-overlay)
    endif()
  endforeach()

  add_custom_command_target(
    copy_android_posix_filesystem_apinotes_resource
    COMMAND
      ${CMAKE_COMMAND} -E make_directory "${SWIFTLIB_DIR}/apinotes"
    COMMAND
      ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_CURRENT_SOURCE_DIR}/posix_filesystem.apinotes" "${SWIFTLIB_DIR}/apinotes"
    COMMAND
      ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_CURRENT_SOURCE_DIR}/spawn.apinotes" "${SWIFTLIB_DIR}/apinotes"
    OUTPUT
      "${SWIFTLIB_DIR}/apinotes/posix_filesystem.apinotes"
      "${SWIFTLIB_DIR}/apinotes/spawn.apinotes"
    COMMENT
      "Copying Android API Notes to resource directories")
  add_dependencies(sdk-overlay ${copy_android_posix_filesystem_apinotes_resource})
  list(APPEND android_modulemap_target_list ${copy_android_posix_filesystem_apinotes_resource})
  swift_install_in_component(FILES posix_filesystem.apinotes spawn.apinotes
                             DESTINATION "lib/swift/apinotes"
                             COMPONENT sdk-overlay)
endif()
add_custom_target(android_modulemap DEPENDS ${android_modulemap_target_list})
set_property(TARGET android_modulemap PROPERTY FOLDER "Miscellaneous")
add_dependencies(sdk-overlay android_modulemap)

set(wasilibc_modulemap_target_list)
if("WASI" IN_LIST SWIFT_SDKS)
  set(wasilibc_modulemap_source "wasi-libc.modulemap")
  foreach(arch ${SWIFT_SDK_WASI_ARCHITECTURES})
    set(arch_subdir "${SWIFT_SDK_WASI_LIB_SUBDIR}/${arch}")
    set(module_dir "${SWIFTLIB_DIR}/${arch_subdir}")
    set(module_dir_static "${SWIFTSTATICLIB_DIR}/${arch_subdir}")

    add_custom_command_target(
      copy_wasilibc_modulemap_resource
      COMMAND
        "${CMAKE_COMMAND}" "-E" "make_directory" ${module_dir} ${module_dir_static}
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
          "${CMAKE_CURRENT_SOURCE_DIR}/${wasilibc_modulemap_source}" ${module_dir}
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
          "${CMAKE_CURRENT_SOURCE_DIR}/${wasilibc_modulemap_source}" ${module_dir_static}
      OUTPUT ${module_dir}/${wasilibc_modulemap_source} ${module_dir_static}/${wasilibc_modulemap_source}
      COMMENT "Copying WASILibc modulemap to resource directories")
    add_dependencies(sdk-overlay ${copy_wasilibc_modulemap_resource})
    list(APPEND wasilibc_modulemap_target_list ${copy_wasilibc_modulemap_resource})

    swift_install_in_component(FILES "${wasilibc_modulemap_source}"
                               DESTINATION "lib/swift/${arch_subdir}"
                               COMPONENT sdk-overlay)
    if(SWIFT_BUILD_STATIC_STDLIB)
      swift_install_in_component(FILES "${wasilibc_modulemap_source}"
                                 DESTINATION "lib/swift_static/${arch_subdir}"
                                 COMPONENT sdk-overlay)
    endif()

    set(wasilibc_apinotes_source "SwiftWASILibc.apinotes")
    add_custom_command_target(
      copy_wasilibc_apinotes_resource
      COMMAND
        "${CMAKE_COMMAND}" "-E" "make_directory" ${SWIFTLIB_DIR}/apinotes ${SWIFTSTATICLIB_DIR}/apinotes
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
          "${CMAKE_CURRENT_SOURCE_DIR}/${wasilibc_apinotes_source}" ${SWIFTLIB_DIR}/apinotes
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
          "${CMAKE_CURRENT_SOURCE_DIR}/${wasilibc_apinotes_source}" ${SWIFTSTATICLIB_DIR}/apinotes
      OUTPUT
        ${SWIFTLIB_DIR}/apinotes/${wasilibc_apinotes_source}
        ${SWIFTSTATICLIB_DIR}/apinotes/${wasilibc_apinotes_source}
      COMMENT "Copying WASILibc API notes to resource directories")

    list(APPEND wasilibc_modulemap_target_list ${copy_wasilibc_apinotes_resource})
    add_dependencies(sdk-overlay ${copy_wasilibc_apinotes_resource})
    swift_install_in_component(FILES "${wasilibc_apinotes_source}"
                               DESTINATION "lib/swift/apinotes"
                               COMPONENT sdk-overlay)
    if(SWIFT_BUILD_STATIC_STDLIB)
      swift_install_in_component(FILES "${wasilibc_apinotes_source}"
                                 DESTINATION "lib/swift_static/apinotes"
                                 COMPONENT sdk-overlay)
    endif()

  endforeach()
endif()
add_custom_target(wasilibc_modulemap DEPENDS ${wasilibc_modulemap_target_list})
set_property(TARGET wasilibc_modulemap PROPERTY FOLDER "Miscellaneous")
add_dependencies(sdk-overlay wasilibc_modulemap)

if(WINDOWS IN_LIST SWIFT_SDKS)
  swift_install_in_component(FILES
      ucrt.modulemap
      vcruntime.apinotes
      vcruntime.modulemap
      winsdk.modulemap
    DESTINATION "share"
    COMPONENT sdk-overlay)
endif()
