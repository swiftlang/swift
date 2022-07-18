add_swift_unittest(SwiftRemangleTests
  RemangleTest.cpp
  )

target_link_libraries(SwiftRemangleTests
  PRIVATE
  swiftBasic
  swiftDemangling
  LLVMTestingSupport
  )
