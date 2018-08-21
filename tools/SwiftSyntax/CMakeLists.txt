add_swift_library(swiftSwiftSyntax SHARED
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftSyntax.swift
  AbsolutePosition.swift
  AtomicCache.swift
  ByteTreeDeserialization.swift
  Diagnostic.swift
  DiagnosticConsumer.swift
  DiagnosticEngine.swift
  JSONDiagnosticConsumer.swift
  PrintingDiagnosticConsumer.swift
  RawSyntax.swift
  SourcePresence.swift
  SourceLength.swift
  SwiftcInvocation.swift
  Syntax.swift
  SyntaxData.swift
  SyntaxChildren.swift
  SyntaxCollections.swift.gyb
  SyntaxClassifier.swift.gyb
  SyntaxBuilders.swift.gyb
  SyntaxFactory.swift.gyb
  SyntaxKind.swift.gyb
  SyntaxNodes.swift.gyb
  SyntaxRewriter.swift.gyb
  TokenKind.swift.gyb
  Trivia.swift.gyb

  SWIFT_MODULE_DEPENDS Foundation
  INSTALL_IN_COMPONENT swift-syntax
  TARGET_SDKS OSX
  IS_STDLIB)
