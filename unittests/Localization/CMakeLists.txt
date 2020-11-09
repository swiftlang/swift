add_swift_unittest(swiftLocalizationTests
  DefToYAMLConverterTests.cpp
  SerializationTests.cpp)

target_link_libraries(swiftLocalizationTests
  PRIVATE
    swiftLocalization)

target_compile_definitions(swiftLocalizationTests PRIVATE
  SWIFTLIB_DIR=\"${SWIFTLIB_DIR}\")
