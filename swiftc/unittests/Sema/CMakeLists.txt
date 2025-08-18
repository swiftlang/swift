# Semantic Analysis unit tests
add_executable(SwiftcSemaTests
    TypeCheckerTest.cpp
)

target_link_libraries(SwiftcSemaTests
    PRIVATE
    SwiftcSema
    SwiftcAST
    SwiftcBasic
    ${GTEST_LIBRARIES}
    pthread
)

target_include_directories(SwiftcSemaTests PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${GTEST_INCLUDE_DIRS}
)

# Add test to CTest
add_test(NAME SwiftcSemaTests COMMAND SwiftcSemaTests)