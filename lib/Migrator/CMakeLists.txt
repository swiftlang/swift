add_swift_library(swiftMigrator STATIC
  FixitApplyDiagnosticConsumer.cpp
  Migrator.cpp
  MigrationState.cpp
  Replacement.cpp

  LINK_LIBRARIES swiftSyntax)

