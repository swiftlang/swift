# These API notes are obsolete, but are still required for older SDKs.
# ClangImporter uses them selectively with a unique -iapinotes-modules
# for each one.
set(sources
  Dispatch.apinotes
  os.apinotes
)
set(subdirectories
  apinotes-dispatch
  apinotes-os
)

set(output_dirs)
set(outputs)
foreach(input subdirectory IN ZIP_LISTS sources subdirectories)
  set(output_dir "${SWIFTLIB_DIR}/${subdirectory}")

  add_custom_command(
      OUTPUT "${output_dir}"
      COMMAND ${CMAKE_COMMAND} "-E" "make_directory" "${output_dir}")
  add_custom_command(
      OUTPUT "${output_dir}/${input}"
      DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${input}" "${output_dir}"
      COMMAND
        "${CMAKE_COMMAND}" "-E" "copy_if_different"
        "${CMAKE_CURRENT_SOURCE_DIR}/${input}" "${output_dir}/")

  list(APPEND output_dirs "${output_dir}")
  list(APPEND outputs "${output_dir}/${input}")

  swift_install_in_component(FILES "${input}"
                             DESTINATION "lib/swift/${subdirectory}"
                             COMPONENT compiler)
endforeach()

add_custom_target("copy_apinotes" ALL
    DEPENDS "${outputs}" "${output_dirs}"
    COMMENT "Copying API notes to ${SWIFTLIB_DIR}"
    SOURCES "${sources}")

add_dependencies(compiler copy_apinotes)
