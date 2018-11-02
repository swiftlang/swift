set(SWIFT_SYNTAX_PATH "${SWIFT_LIBRARY_OUTPUT_INTDIR}/swift/macosx/x86_64")
set(EXTRA_COMPILE_FLAGS "-I" "${SWIFT_SYNTAX_PATH}")
set(EXTRA_LINKER_FLAGS "-Xlinker" "-rpath" "-Xlinker" "${SWIFT_SYNTAX_PATH}")

add_swift_host_tool(swift-swiftsyntax-test
  swift-syntax-test.swift
  DEPENDS swiftSwiftSyntax
)
