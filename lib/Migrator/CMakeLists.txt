set(datafiles
  macos4.json
  ios4.json
  tvos4.json
  watchos4.json
  overlay4.json
)

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  set(cmake_symlink_option copy_if_different)
else()
  set(cmake_symlink_option create_symlink)
endif()

add_custom_target(swift-migrator-data ALL
                  COMMAND
                    ${CMAKE_COMMAND} -E make_directory ${SWIFTLIB_DIR}/migrator
                  COMMAND
                    ${CMAKE_COMMAND} -E ${cmake_symlink_option} ${datafiles} ${SWIFTLIB_DIR}/migrator/
                  WORKING_DIRECTORY
                    ${CMAKE_CURRENT_SOURCE_DIR}
                  DEPENDS
                    ${datafiles})

swift_install_in_component(compiler
                           FILES ${datafiles}
                           DESTINATION "lib/swift/migrator"
                           COMPONENT swift-migrator-data)
add_llvm_install_targets(install-swift-migrator-data
                         COMPONENT swift-migrator-data)

add_swift_library(swiftMigrator STATIC
  APIDiffMigratorPass.cpp
  EditorAdapter.cpp
  FixitApplyDiagnosticConsumer.cpp
  Migrator.cpp
  MigrationState.cpp
  RewriteBufferEditsReceiver.cpp
  LINK_LIBRARIES swiftSyntax swiftIDE)
add_dependencies(swiftMigrator swift-migrator-data)

