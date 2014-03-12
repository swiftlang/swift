set(sources shims.h module.map)
set(output_dir "${SWIFT_LIBRARY_OUTPUT_INTDIR}/swift/shims")

set(commands)
set(outputs)
foreach(input ${sources})
  list(APPEND outputs "${output_dir}/${input}")
  list(APPEND commands
    COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${input}"
    "${output_dir}/${input}")
endforeach()

add_custom_command(OUTPUT "${output_dir}" ${outputs}
  COMMAND ${CMAKE_COMMAND} -E make_directory "${output_dir}"
  ${commands}
  DEPENDS ${sources}
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  COMMENT "Copying SwiftShims module to ${output_dir}"
  )

add_custom_target(
  copy_shim_headers
  DEPENDS "${output_dir}" ${outputs}
  )

install(FILES ${sources} DESTINATION "lib/swift/shims")
