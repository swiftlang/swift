if (SWIFT_SWIFT_PARSER)
  add_library(swiftASTGen
    Sources/ASTGen/ASTGen.swift
    Sources/ASTGen/Decls.swift
    Sources/ASTGen/Exprs.swift
    Sources/ASTGen/Literals.swift
    Sources/ASTGen/Misc.swift
    Sources/ASTGen/Stmts.swift
  )

  # Link against the SwiftSyntax parser and libraries it depends on. The actual
  # formulation of this is a hack to work around a CMake bug in Ninja file
  # generation that results in multiple Ninja targets producing the same file in
  # a downstream SourceKit target. This should be expressed as:
  #
  #   target_link_libraries(swiftASTGen
  #     PRIVATE
  #     SwiftSyntax::SwiftCompilerSupport
  #   )
  target_link_libraries(swiftASTGen
    PRIVATE
    $<TARGET_OBJECTS:SwiftSyntax::SwiftBasicFormat>
    $<TARGET_OBJECTS:SwiftSyntax::SwiftParser>
    $<TARGET_OBJECTS:SwiftSyntax::SwiftDiagnostics>
    $<TARGET_OBJECTS:SwiftSyntax::SwiftSyntax>
    $<TARGET_OBJECTS:SwiftSyntax::SwiftOperators>
    $<TARGET_OBJECTS:SwiftSyntax::SwiftCompilerSupport>
  )

  target_include_directories(swiftASTGen PUBLIC
    "${SWIFT_PATH_TO_EARLYSWIFTSYNTAX_BUILD_DIR}/swift")

  add_dependencies(swiftASTGen swiftAST)
endif()
