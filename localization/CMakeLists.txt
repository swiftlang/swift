set(diagnostic_witness "${CMAKE_BINARY_DIR}/share/swift/diagnostics/generated")

add_custom_command(
  OUTPUT
    ${diagnostic_witness}
  COMMAND
    ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/diagnostics/ ${CMAKE_BINARY_DIR}/share/swift/diagnostics/
  COMMAND
    "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/swift-def-to-strings-converter"
      --output-directory ${CMAKE_BINARY_DIR}/share/swift/diagnostics/
  COMMAND
    "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/swift-serialize-diagnostics"
      --input-file-path ${CMAKE_BINARY_DIR}/share/swift/diagnostics/en.strings
      --output-directory ${CMAKE_BINARY_DIR}/share/swift/diagnostics/
  COMMAND
    ${CMAKE_COMMAND} -E touch ${diagnostic_witness}
  DEPENDS
    swift-def-to-strings-converter
    swift-serialize-diagnostics
    # Add files in diagnostics subdirectory when they're created
)

add_custom_target(diagnostic-database DEPENDS ${diagnostic_witness})

add_dependencies(swift-frontend diagnostic-database)

swift_install_in_component(
  DIRECTORY ${CMAKE_BINARY_DIR}/share/swift/diagnostics/
  DESTINATION "share/swift/diagnostics"
  COMPONENT compiler
  FILES_MATCHING
  PATTERN "*.db"
  PATTERN "*.yaml"
  PATTERN "*.strings"
)
