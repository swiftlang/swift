# Runtime unit tests
add_executable(SwiftcRuntimeTests
    ARCTest.cpp
    RefCountTest.cpp
    WeakReferenceTest.cpp
    CycleDetectionTest.cpp
)

target_link_libraries(SwiftcRuntimeTests
    PRIVATE
    SwiftcRuntime
    SwiftcBasic
    ${GTEST_LIBRARIES}
    pthread
)

target_include_directories(SwiftcRuntimeTests PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${GTEST_INCLUDE_DIRS}
)

# Add test to CTest
add_test(NAME SwiftcRuntimeTests COMMAND SwiftcRuntimeTests)