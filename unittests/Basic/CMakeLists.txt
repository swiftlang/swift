add_swift_unittest(SwiftBasicTests
  SourceManager.cpp
  TreeScopedHashTableTests.cpp
  )
target_link_libraries(SwiftBasicTests curses)
