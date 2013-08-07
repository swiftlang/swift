set(LLVM_LINK_COMPONENTS
    core)
add_swift_unittest(SwiftParseTests
  LexerTests.cpp
)

target_link_libraries(SwiftParseTests
   curses swiftParse swiftSIL swiftSema swiftClangImporter
)
