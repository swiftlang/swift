# Lexer unit tests
add_executable(SwiftcLexerTests
    LexerTest.cpp
    TokenizerTest.cpp
    KeywordTest.cpp
    OperatorTest.cpp
)

target_link_libraries(SwiftcLexerTests
    PRIVATE
    SwiftcLexer
    SwiftcBasic
    ${GTEST_LIBRARIES}
    pthread
)

target_include_directories(SwiftcLexerTests PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${GTEST_INCLUDE_DIRS}
)

# Add test to CTest
add_test(NAME SwiftcLexerTests COMMAND SwiftcLexerTests)