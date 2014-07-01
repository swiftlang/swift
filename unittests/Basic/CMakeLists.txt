set(GENERATED_TESTS UnicodeGraphemeBreakTest.cpp.gyb)

handle_gyb_sources(${CMAKE_SIZEOF_VOID_P} GENERATED_TESTS)

add_swift_unittest(SwiftBasicTests
  SourceManager.cpp
  TreeScopedHashTableTests.cpp
  StringExtrasTest.cpp
  SuccessorMapTest.cpp
  Unicode.cpp

  ${GENERATED_TESTS}
  )

target_link_libraries(SwiftBasicTests
  swiftBasic
  clangBasic
  )
