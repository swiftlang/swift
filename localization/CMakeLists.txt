add_custom_target(diagnostic-translation-database)

add_custom_command(
  TARGET diagnostic-translation-database
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DEPENDS diagnostics
  COMMAND "${CMAKE_COMMAND}" -E copy_directory diagnostics/ "${SWIFT_BINARY_DIR}/share/swift/diagnostics/")

add_custom_command(
  TARGET diagnostic-translation-database
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMAND swift-serialize-diagnostics
    --input-file-path diagnostics/en.yaml
    --output-directory ${SWIFT_BINARY_DIR}/share/swift/diagnostics)

add_dependencies(swift-frontend diagnostic-translation-database)

swift_install_in_component(
  DIRECTORY diagnostics
  DESTINATION "share/swift"
  COMPONENT compiler)
