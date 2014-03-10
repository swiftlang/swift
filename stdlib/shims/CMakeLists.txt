set(sources shims.h module.map)

add_custom_target(copy_shim_headers ALL
  COMMAND ${CMAKE_COMMAND} -E make_directory
    "${SWIFT_LIBRARY_OUTPUT_INTDIR}/swift/shims"
  SOURCES ${sources})

foreach(input ${sources})
  add_custom_command(TARGET copy_shim_headers
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${input}
      "${SWIFT_LIBRARY_OUTPUT_INTDIR}/swift/shims/${input}"
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
endforeach()

install(FILES ${sources} DESTINATION "lib/swift/shims")
