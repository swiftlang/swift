set(sources
    Glibc.swift
    Misc.c
)

set(output_dir "${SWIFTLIB_DIR}/glibc")

# Set correct paths to glibc headers
set(GLIBC_INCLUDE_PATH "/usr/include")
if(CMAKE_LIBRARY_ARCHITECTURE)
  set(GLIBC_ARCH_INCLUDE_PATH "${GLIBC_INCLUDE_PATH}/${CMAKE_LIBRARY_ARCHITECTURE}")
else()
  set(GLIBC_ARCH_INCLUDE_PATH "${GLIBC_INCLUDE_PATH}")
endif()
if (NOT EXISTS "${GLIBC_ARCH_INCLUDE_PATH}/sys")
  message(FATAL_ERROR "Glibc headers were not found.")
endif()

# Generate module.map
configure_file(module.map.in "${CMAKE_CURRENT_BINARY_DIR}/module.map" @ONLY)

add_custom_command_target(unused_var
    COMMAND
        "${CMAKE_COMMAND}" "-E" "make_directory" "${output_dir}"
    COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
        "${CMAKE_CURRENT_BINARY_DIR}/module.map"
        "${output_dir}/module.map"
    CUSTOM_TARGET_NAME "copy_glibc_module"
    OUTPUT "${output_dir}/module.map" "${output_dir}"
    DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/module.map"
    COMMENT "Copying Glibc module to ${output_dir}")

swift_install_in_component(stdlib
    FILES "${output_dir}/module.map"
    DESTINATION "lib/swift/glibc")

add_swift_library(swiftGlibc IS_SDK_OVERLAY
    ${sources}
    FILE_DEPENDS copy_glibc_module "${output_dir}"
    INSTALL_IN_COMPONENT stdlib-experimental)
