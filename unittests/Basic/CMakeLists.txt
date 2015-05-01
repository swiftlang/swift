set(generated_tests UnicodeGraphemeBreakTest.cpp.gyb)

handle_gyb_sources(
    gyb_dependency_targets
    generated_tests
    ${SWIFT_HOST_VARIANT_ARCH})

add_swift_unittest(SwiftBasicTests
  ADTTests.cpp
  ClusteredBitVectorTest.cpp
  Demangle.cpp
  EditorPlaceholderTest.cpp
  FileSystemTests.cpp
  SourceManager.cpp
  TreeScopedHashTableTests.cpp
  StringExtrasTest.cpp
  SuccessorMapTest.cpp
  Unicode.cpp

  ${generated_tests}
  )

add_dependencies(SwiftBasicTests "${gyb_dependency_targets}")

target_link_libraries(SwiftBasicTests
  swiftBasic
  clangBasic
  )
