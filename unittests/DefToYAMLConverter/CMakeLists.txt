add_swift_unittest(DefToYAMLConverterTest
  DefToYAMLConverterTest.cpp)

target_link_libraries(DefToYAMLConverterTest
  PRIVATE
    swiftLocalization)

target_compile_definitions(DefToYAMLConverterTest PRIVATE
  SWIFTLIB_DIR=\"${SWIFTLIB_DIR}\")
