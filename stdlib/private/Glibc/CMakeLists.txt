set(sources
  module.map
  )
set(output_dir "${SWIFTLIB_DIR}/glibc")

set(commands
    COMMAND ${CMAKE_COMMAND} "-E" "make_directory" "${output_dir}")
set(outputs)
foreach(input ${sources})
  list(APPEND commands
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
        "${CMAKE_CURRENT_SOURCE_DIR}/${input}"
        "${output_dir}/${input}")
  list(APPEND outputs "${output_dir}/${input}")
endforeach()
# Put the output dir itself last so that it isn't considered the primary output.
list(APPEND outputs "${output_dir}")
add_custom_command_target(unused_var
    ${commands}
    CUSTOM_TARGET_NAME "copy_glibc_module"
    OUTPUT "${outputs}"
    DEPENDS "${sources}"
    COMMENT "Copying Glibc module to ${output_dir}")

swift_install_in_component(stdlib-experimental
    FILES ${sources}
    DESTINATION "lib/swift/glibc")

add_swift_library(swiftGlibc IS_SDK_OVERLAY
  Glibc.swift
  FILE_DEPENDS copy_glibc_module "${SWIFTLIB_DIR}/glibc"
  INSTALL_IN_COMPONENT stdlib-experimental)

