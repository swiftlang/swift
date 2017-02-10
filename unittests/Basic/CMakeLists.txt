set(generated_tests UnicodeGraphemeBreakTest.cpp.gyb)

handle_gyb_sources(
    gyb_dependency_targets
    generated_tests
    ${SWIFT_HOST_VARIANT_ARCH})

add_swift_unittest(SwiftBasicTests
  ValueEnumeratorTest.cpp
  OptionSetTest.cpp
  RangeTest.cpp
  BlotMapVectorTest.cpp
  ClusteredBitVectorTest.cpp
  DemangleTest.cpp
  EditorPlaceholderTest.cpp
  EncodedSequenceTest.cpp
  FileSystemTest.cpp
  ImmutablePointerSetTest.cpp
  PointerIntEnumTest.cpp
  PrefixMapTest.cpp
  SourceManagerTest.cpp
  StringExtrasTest.cpp
  SuccessorMapTest.cpp
  ThreadSafeRefCntPointerTest.cpp
  TransformArrayRefTest.cpp
  TreeScopedHashTableTest.cpp
  UnicodeTest.cpp
  ${generated_tests}
  )

add_dependencies(SwiftBasicTests "${gyb_dependency_targets}")

target_link_libraries(SwiftBasicTests
  swiftBasic
  clangBasic
  )
