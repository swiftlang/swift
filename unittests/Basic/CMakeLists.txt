add_swift_unittest(SwiftBasicTests
  SourceManager.cpp
  TreeScopedHashTableTests.cpp
  StringExtrasTest.cpp
  SuccessorMapTest.cpp
  Unicode.cpp
  )

target_link_libraries(SwiftBasicTests
  swiftBasic
  clangBasic
  )
