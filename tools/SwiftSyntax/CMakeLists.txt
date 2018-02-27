set(swiftsyntax_compile_flags "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}")

# FIXME: This is _awful_. These pieces could/should be sunk into
#        `add_swift_library`
if("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "LINUX")
  LIST(APPEND swiftsyntax_compile_flags
    -I"${SWIFT_PATH_TO_LIBDISPATCH_SOURCE}"
    -I"${SWIFT_PATH_TO_LIBDISPATCH_BUILD}/src"
    -I"${SWIFT_PATH_TO_LIBDISPATCH_BUILD}/src/swift"
    -I"${FOUNDATION_BUILD_DIR}"
    -I"${FOUNDATION_BUILD_DIR}/usr/lib/swift"
    -I"${FOUNDATION_BUILD_DIR}/usr/lib/swift/CoreFoundation"

    -L"${SWIFT_PATH_TO_LIBDISPATCH_BUILD}/src"
    -L"${SWIFT_PATH_TO_LIBDISPATCH_BUILD}/src/swift"
    -L"${FOUNDATION_BUILD_DIR}"
    -L"${FOUNDATION_BUILD_DIR}/usr/lib/swift"
    -L"${FOUNDATION_BUILD_DIR}/usr/lib/swift/CoreFoundation"

    -lFoundation
    -lCoreFoundation
    -ldispatch)
endif()

add_swift_library(swiftSwiftSyntax SHARED
  # This file should be listed first.  Module name is inferred from the
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
  Trivia.swift.gyb

  SWIFT_COMPILE_FLAGS "${swiftsyntax_compile_flags}"
  SWIFT_MODULE_DEPENDS_OSX Foundation
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  INSTALL_IN_COMPONENT swift-syntax
  IS_STDLIB)

