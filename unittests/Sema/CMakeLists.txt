
add_swift_unittest(swiftSemaTests
  SemaFixture.cpp
  BindingInferenceTests.cpp
  ConstraintGenerationTests.cpp
  ConstraintGraphTests.cpp
  ConstraintSimplificationTests.cpp
  ConstraintSystemDumpTests.cpp
  UnresolvedMemberLookupTests.cpp
  PlaceholderTypeInferenceTests.cpp
  SolutionFilteringTests.cpp
  KeypathFunctionConversionTests.cpp)

target_link_libraries(swiftSemaTests
  PRIVATE
    swiftAST
    swiftClangImporter
    swiftSIL
    swiftSema
    swiftSerialization)

target_compile_definitions(swiftSemaTests PRIVATE
  SWIFTLIB_DIR=\"${SWIFTLIB_DIR}\")
