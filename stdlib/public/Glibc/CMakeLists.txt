set(sources
    Glibc.swift
    Misc.c
)

# When cross-compiling the stdlib on Unix platforms, we'll need a separate
# glibc for each target.
foreach(SDK ${SWIFT_SDKS})
  foreach(arch ${SWIFT_SDK_${SDK}_ARCHITECTURES})
    # Don't generate Glibc module maps for Darwin targets.
    if("${SDK}" STREQUAL "LINUX" OR "${SDK}" STREQUAL "FREEBSD")
      set(output_dir "${SWIFTLIB_DIR}/${SWIFT_SDK_${SDK}_LIB_SUBDIR}/${arch}")

      # Determine the location of glibc based on the target.
      set(GLIBC_INCLUDE_PATH "/usr/include")
      set(GLIBC_ARCH_INCLUDE_PATH "${GLIBC_INCLUDE_PATH}")
      if(("${SDK}" STREQUAL "LINUX" OR "${SDK}" STREQUAL "FREEBSD") AND CMAKE_LIBRARY_ARCHITECTURE)
        # FIXME: Some distributions install headers in
        #        "/usr/include/x86_64-linux-gnu/sys/...". Here we use the host
        #        machine's path, regardless of the SDK target we're building for.
        #        This will break if cross-compiling from a distro that uses the
        #        architecture as part of the path to a distro that does not.
        set(GLIBC_ARCH_INCLUDE_PATH "${GLIBC_INCLUDE_PATH}/${CMAKE_LIBRARY_ARCHITECTURE}")
      endif()

      # Configure the module map based on the target. Each platform needs to
      # reference different headers, based on what's available in their glibc.
      set(modulemap_path "${CMAKE_CURRENT_BINARY_DIR}/${SWIFT_SDK_${SDK}_LIB_SUBDIR}/${arch}/module.map")
      if("${SDK}" STREQUAL "FREEBSD")
        configure_file(module.freebsd.map.in "${modulemap_path}" @ONLY)
      else()
        configure_file(module.map.in "${modulemap_path}" @ONLY)
      endif()

      set(VARIANT_SUFFIX "-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-${arch}")
      add_custom_command_target(unused_var
          COMMAND
              "${CMAKE_COMMAND}" "-E" "make_directory" "${output_dir}"
          COMMAND
              "${CMAKE_COMMAND}" "-E" "copy_if_different"
              "${modulemap_path}"
              "${output_dir}/glibc.modulemap"
          CUSTOM_TARGET_NAME "copy_glibc_module${VARIANT_SUFFIX}"
          OUTPUT "${output_dir}/glibc.modulemap" "${output_dir}"
          DEPENDS "${modulemap_path}"
          COMMENT "Copying Glibc module to ${output_dir}")

      swift_install_in_component(stdlib
          FILES "${output_dir}/glibc.modulemap"
          DESTINATION "${output_dir}")

      add_swift_library(swiftGlibc IS_SDK_OVERLAY
          ${sources}
          FILE_DEPENDS "copy_glibc_module${VARIANT_SUFFIX}" "${output_dir}"
          TARGET_SDKS "${SDK}"
          INSTALL_IN_COMPONENT stdlib-experimental)
    endif()
  endforeach()
endforeach()
