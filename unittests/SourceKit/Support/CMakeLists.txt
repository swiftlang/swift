add_swift_unittest(SourceKitSupportTests
  ImmutableTextBufferTest.cpp
  )

target_link_libraries(SourceKitSupportTests
  PRIVATE
  SourceKitSupport
  )
