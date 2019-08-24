add_swift_unittest(SourceKitSupportTests
  FuzzyStringMatcherTest.cpp
  ImmutableTextBufferTest.cpp
  )

target_link_libraries(SourceKitSupportTests
  PRIVATE
  SourceKitSupport
  )
