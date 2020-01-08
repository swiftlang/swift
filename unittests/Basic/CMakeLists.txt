set(generated_tests UnicodeGraphemeBreakTest.cpp.gyb)

handle_gyb_sources(
    gyb_dependency_targets
    generated_tests
    ${SWIFT_HOST_VARIANT_ARCH})

add_swift_unittest(SwiftBasicTests
  BlotMapVectorTest.cpp
  CacheTest.cpp
  ClusteredBitVectorTest.cpp
  DemangleTest.cpp
  DiverseStackTest.cpp
  EditorPlaceholderTest.cpp
  EncodedSequenceTest.cpp
  ExponentialGrowthAppendingBinaryByteStreamTests.cpp
  FileSystemTest.cpp
  ImmutablePointerSetTest.cpp
  JSONSerialization.cpp
  OptionSetTest.cpp
  OwnedStringTest.cpp
  PointerIntEnumTest.cpp
  PrefixMapTest.cpp
  RangeTest.cpp
  SourceManagerTest.cpp
  STLExtrasTest.cpp
  StringExtrasTest.cpp
  SuccessorMapTest.cpp
  ThreadSafeRefCntPointerTest.cpp
  TransformRangeTest.cpp
  TreeScopedHashTableTest.cpp
  UnicodeTest.cpp
  ValueEnumeratorTest.cpp
  ${generated_tests}
  )

add_dependencies(SwiftBasicTests "${gyb_dependency_targets}")

target_link_libraries(SwiftBasicTests
  PRIVATE
  swiftBasic
  clangBasic
  LLVMTestingSupport
  )
