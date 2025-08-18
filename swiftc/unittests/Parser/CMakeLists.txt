# Parser unit tests
add_executable(SwiftcParserTests
    ParserTest.cpp
    ExpressionParserTest.cpp
)

target_link_libraries(SwiftcParserTests
    PRIVATE
    SwiftcParser
    SwiftcLexer
    SwiftcAST
    SwiftcBasic
    ${GTEST_LIBRARIES}
    pthread
)

target_include_directories(SwiftcParserTests PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${GTEST_INCLUDE_DIRS}
)

# Add test to CTest
add_test(NAME SwiftcParserTests COMMAND SwiftcParserTests)