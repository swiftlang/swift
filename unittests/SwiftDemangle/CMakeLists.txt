if(TARGET swiftDemangle)
  add_swift_unittest(SwiftDemangleTests
    DemangleTest.cpp
    )

  target_link_libraries(SwiftDemangleTests
    swiftDemangle
    )
endif()
