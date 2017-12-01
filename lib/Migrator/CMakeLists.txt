set(datafiles
  macos.json
  ios.json
  tvos.json
  watchos.json
  overlay.json
)
set(SWIFTLIB_DIR
    "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/swift")
set(output_dir "${SWIFTLIB_DIR}/migrator")

add_custom_command(
    OUTPUT "${output_dir}"
    COMMAND ${CMAKE_COMMAND} "-E" "make_directory" "${output_dir}")

set(outputs)

foreach(input ${datafiles})
  add_custom_command(
      OUTPUT "${output_dir}/${input}"
      DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${input}"
      COMMAND
        "${CMAKE_COMMAND}" "-E" "create_symlink"
        "${CMAKE_CURRENT_SOURCE_DIR}/${input}"
        "${output_dir}/${input}")
  list(APPEND outputs "${output_dir}/${input}")
endforeach()
list(APPEND outputs "${output_dir}")

add_custom_target("symlink_migrator_data"
    DEPENDS "${output_dir}" "${outputs}"
    COMMENT "Symlinking migrator data to ${output_dir}")

swift_install_in_component(compiler
  FILES ${datafiles}
DESTINATION "lib/swift/migrator")

add_swift_library(swiftMigrator STATIC
  APIDiffMigratorPass.cpp
  EditorAdapter.cpp
  FixitApplyDiagnosticConsumer.cpp
  Migrator.cpp
  MigrationState.cpp
  RewriteBufferEditsReceiver.cpp
  TupleSplatMigratorPass.cpp
  TypeOfMigratorPass.cpp
  DEPENDS
    swift-syntax-generated-headers
  LINK_LIBRARIES swiftSyntax swiftIDE)

add_dependencies(swiftMigrator
  "symlink_migrator_data")
