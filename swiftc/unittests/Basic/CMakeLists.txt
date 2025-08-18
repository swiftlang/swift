# Basic library unit tests - only include files that exist
add_executable(SwiftcBasicTests
    SourceManagerTest.cpp
    DiagnosticTest.cpp
    StringExtrasTest.cpp
    FileSystemTest.cpp
    UnicodeTest.cpp
    RangeTest.cpp
    STLExtrasTest.cpp
    OptionSetTest.cpp
    EnumMapTest.cpp
)

target_link_libraries(SwiftcBasicTests
    PRIVATE
    swiftcBasic
    gtest
    gtest_main
    pthread
)

target_include_directories(SwiftcBasicTests PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${GTEST_INCLUDE_DIRS}
)

# Add test to CTest
add_test(NAME SwiftcBasicTests COMMAND SwiftcBasicTests)