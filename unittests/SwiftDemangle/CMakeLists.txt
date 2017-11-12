if(TARGET swiftDemangle)
  add_swift_unittest(SwiftDemangleTests
    DemangleTest.cpp
  )
  set_target_properties(SwiftDemangleTests
    PROPERTIES BUILD_WITH_INSTALL_RPATH On INSTALL_RPATH "${SWIFT_LIBRARY_OUTPUT_INTDIR}"
  )
  
  target_link_libraries(SwiftDemangleTests
    swiftDemangle
  )
endif()
