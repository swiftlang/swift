if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  add_swift_unittest(FunctionNameDemangleTests
    DemangleTest.cpp
    )

  target_link_libraries(FunctionNameDemangleTests
    functionNameDemangle
    )
endif()
