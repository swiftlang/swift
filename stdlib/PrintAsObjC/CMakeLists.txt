set(sources swift.h module.map)

add_custom_target(copy_headers ALL
  COMMAND ${CMAKE_COMMAND} -E make_directory
    "${SWIFT_INCLUDE_OUTPUT_INTDIR}/swift"
  SOURCES ${sources})

foreach(input ${sources})
  add_custom_command(TARGET copy_headers
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${input}
      "${SWIFT_INCLUDE_OUTPUT_INTDIR}/swift/${input}"
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
endforeach()

install(FILES ${sources} DESTINATION "include/swift")
