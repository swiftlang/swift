add_swift_unittest(SwiftSILTests
  SILBitfieldTest.cpp
)

target_link_libraries(SwiftSILTests
   PRIVATE
   swiftSIL
)
