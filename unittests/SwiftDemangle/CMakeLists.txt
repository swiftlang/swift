if(TARGET swiftDemangle)
  add_swift_unittest(SwiftDemangleTests IS_TARGET_TEST
    DemangleTest.cpp
  )
  target_link_libraries(SwiftDemangleTests
    PRIVATE
    swiftDemangle
  )
endif()
