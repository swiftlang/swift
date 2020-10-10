
add_swift_unittest(swiftSemaTests
  SemaFixture.cpp)

target_link_libraries(swiftSemaTests
  PRIVATE
    swiftSema)
