add_swift_unittest(SwiftMetadataReaderTests
  MetadataReaderTest.cpp)
target_link_libraries(SwiftMetadataReaderTests
  PRIVATE
  swiftBasic
  swiftDemangling
  )
