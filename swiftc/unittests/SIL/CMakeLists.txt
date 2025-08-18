# SIL unit tests
add_executable(SwiftcSILTests
    SILFunctionTest.cpp
)

target_link_libraries(SwiftcSILTests
    PRIVATE
    SwiftcSIL
    SwiftcAST
    SwiftcBasic
    ${GTEST_LIBRARIES}
    pthread
)

target_include_directories(SwiftcSILTests PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${GTEST_INCLUDE_DIRS}
)

# Add test to CTest
add_test(NAME SwiftcSILTests COMMAND SwiftcSILTests)