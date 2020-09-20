add_custom_target(diagnostic-database)

add_custom_command(TARGET diagnostic-database
  COMMAND
    ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/diagnostics/ ${CMAKE_BINARY_DIR}/share/swift/diagnostics/
  COMMAND
    "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/swift-def-to-yaml-converter"
      --output-directory ${CMAKE_BINARY_DIR}/share/swift/diagnostics/
  COMMAND
    "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/swift-serialize-diagnostics"
      --input-file-path ${CMAKE_BINARY_DIR}/share/swift/diagnostics/en.yaml
      --output-directory ${CMAKE_BINARY_DIR}/share/swift/diagnostics/
)

add_dependencies(swift-frontend diagnostic-database)
add_dependencies(diagnostic-database swift-serialize-diagnostics)
add_dependencies(diagnostic-database swift-def-to-yaml-converter)

swift_install_in_component(
  DIRECTORY ${CMAKE_BINARY_DIR}/share/swift/diagnostics/
  DESTINATION "share/swift/diagnostics"
  COMPONENT compiler)
