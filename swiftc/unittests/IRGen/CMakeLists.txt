# IR Generation unit tests
add_executable(SwiftcIRGenTests
    IRGenTest.cpp
)

target_link_libraries(SwiftcIRGenTests
    PRIVATE
    SwiftcIRGen
    SwiftcSIL
    SwiftcAST
    SwiftcBasic
    ${GTEST_LIBRARIES}
    pthread
)

target_include_directories(SwiftcIRGenTests PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${GTEST_INCLUDE_DIRS}
)

# Add test to CTest
add_test(NAME SwiftcIRGenTests COMMAND SwiftcIRGenTests)