if(TARGET functionNameDemangle)
  add_swift_unittest(FunctionNameDemangleTests
    DemangleTest.cpp
    )

  target_link_libraries(FunctionNameDemangleTests
    functionNameDemangle
    )
endif()
