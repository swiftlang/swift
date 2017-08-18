add_swift_library(swiftSwiftSyntax SHARED
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftSyntax.swift
  AtomicCache.swift
  BitstreamWriter.swift
  BitCodeAbbrev.swift
  Diagnostic.swift
  DiagnosticConsumer.swift
  DiagnosticEngine.swift
  PrintingDiagnosticConsumer.swift
  RawSyntax.swift
  SerializedDiagnosticConsumer.swift
  SourcePresence.swift
  SwiftcInvocation.swift
  Syntax.swift
  SyntaxData.swift
  SyntaxChildren.swift
  SyntaxCollection.swift
  SyntaxBuilders.swift.gyb
  SyntaxFactory.swift.gyb
  SyntaxKind.swift.gyb
  SyntaxNodes.swift.gyb
  SyntaxRewriter.swift.gyb
  TokenKind.swift.gyb
  Trivia.swift

  SWIFT_MODULE_DEPENDS Foundation
  INSTALL_IN_COMPONENT swift-syntax
  TARGET_SDKS OSX
  IS_STDLIB)
