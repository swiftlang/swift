# Swift Compiler Unit Tests
cmake_minimum_required(VERSION 3.20)

# Find required packages
find_package(GTest REQUIRED)

# Include directories
include_directories(${CMAKE_SOURCE_DIR}/include)
include_directories(${GTEST_INCLUDE_DIRS})

# Add subdirectories for each component
add_subdirectory(Basic)
add_subdirectory(Lexer)
add_subdirectory(Parser)
add_subdirectory(AST)
add_subdirectory(Sema)
add_subdirectory(SIL)
add_subdirectory(IRGen)
add_subdirectory(Runtime)

# Create a target to run all unit tests
add_custom_target(check-unittests
    COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure
    DEPENDS 
        SwiftcBasicTests
        SwiftcLexerTests
        SwiftcParserTests
        SwiftcASTTests
        SwiftcSemaTests
        SwiftcSILTests
        SwiftcIRGenTests
        SwiftcRuntimeTests
    COMMENT "Running all Swift compiler unit tests"
)

# Enable testing
enable_testing()