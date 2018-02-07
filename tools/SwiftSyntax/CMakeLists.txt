set(EXTRA_COMPILE_FLAGS "-F" "${SWIFT_LIBRARY_OUTPUT_INTDIR}")
set(EXTRA_LINKER_FLAGS "-Xlinker" "-rpath" "-Xlinker" "${SWIFT_LIBRARY_OUTPUT_INTDIR}"
    "-Xlinker" "-F" "-Xlinker" "${SWIFT_LIBRARY_OUTPUT_INTDIR}")

add_swift_library(swiftSwiftSyntax SHARED
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftSyntax.swift
  AtomicCache.swift
  Diagnostic.swift
  DiagnosticConsumer.swift
  DiagnosticEngine.swift
  JSONDiagnosticConsumer.swift
  PrintingDiagnosticConsumer.swift
  RawSyntax.swift
  SourcePresence.swift
  SwiftcInvocation.swift
  Syntax.swift
  SyntaxData.swift
  SyntaxChildren.swift
  SyntaxCollections.swift.gyb
  SyntaxBuilders.swift.gyb
  SyntaxFactory.swift.gyb
  SyntaxKind.swift.gyb
  SyntaxNodes.swift.gyb
  SyntaxRewriter.swift.gyb
  TokenKind.swift.gyb
  Trivia.swift

  SWIFT_COMPILE_FLAGS ${EXTRA_COMPILE_FLAGS}
  LINK_FLAGS ${EXTRA_LINKER_FLAGS}
  SWIFT_MODULE_DEPENDS SwiftSourceKit
  INSTALL_IN_COMPONENT swift-syntax
  TARGET_SDKS OSX
  IS_STDLIB)
