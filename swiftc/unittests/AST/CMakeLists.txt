# AST unit tests
add_executable(SwiftcASTTests
    ASTNodeTest.cpp
    ExprTest.cpp
    DeclTest.cpp
)

target_link_libraries(SwiftcASTTests
    PRIVATE
    SwiftcAST
    SwiftcBasic
    ${GTEST_LIBRARIES}
    pthread
)

target_include_directories(SwiftcASTTests PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${GTEST_INCLUDE_DIRS}
)

# Add test to CTest
add_test(NAME SwiftcASTTests COMMAND SwiftcASTTests)