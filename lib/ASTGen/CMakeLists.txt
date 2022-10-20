if (SWIFT_SWIFT_PARSER)
  add_library(swiftASTGen STATIC
    Sources/ASTGen/ASTGen.swift
    Sources/ASTGen/Decls.swift
    Sources/ASTGen/Exprs.swift
    Sources/ASTGen/Literals.swift
    Sources/ASTGen/Misc.swift
    Sources/ASTGen/SourceFile.swift
    Sources/ASTGen/Stmts.swift
  )

  # Set the appropriate target triple.
  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
    set(DEPLOYMENT_VERSION "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_DEPLOYMENT_VERSION}")
  endif()

  if(SWIFT_HOST_VARIANT_SDK STREQUAL ANDROID)
    set(DEPLOYMENT_VERSION ${SWIFT_ANDROID_API_LEVEL})
  endif()

  get_target_triple(target target_variant "${SWIFT_HOST_VARIANT_SDK}" "${SWIFT_HOST_VARIANT_ARCH}"
    MACCATALYST_BUILD_FLAVOR ""
    DEPLOYMENT_VERSION "${DEPLOYMENT_VERSION}")

    target_compile_options(swiftASTGen PRIVATE $<$<COMPILE_LANGUAGE:Swift>:-target;${target}>)

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

    swiftAST
  )

  target_include_directories(swiftASTGen PUBLIC
    "${SWIFT_PATH_TO_EARLYSWIFTSYNTAX_BUILD_DIR}/swift")

  add_dependencies(swiftASTGen
    SwiftSyntax::SwiftBasicFormat
    SwiftSyntax::SwiftParser
    SwiftSyntax::SwiftDiagnostics
    SwiftSyntax::SwiftSyntax
    SwiftSyntax::SwiftOperators
    SwiftSyntax::SwiftCompilerSupport
    swiftAST
  )

  set_property(GLOBAL APPEND PROPERTY SWIFT_EXPORTS swiftASTGen)
endif()
