cmake_minimum_required(VERSION 3.20)

# Unified swiftc frontend - Simple, Powerful, All LLVM Targets
add_executable(swiftc
    main.cpp
    CompilerDriver.cpp
    CommandLine.cpp
    BuildSystem.cpp
    TargetManager.cpp
    OptimizationManager.cpp
    DiagnosticManager.cpp
)

target_link_libraries(swiftc
    swiftcBasic
    swiftcLexer
    swiftcAST
    swiftcParser
    swiftcSema
    swiftcSIL
    swiftcIRGen
    ${llvm_libs}
)

# Set compiler flags
target_compile_features(swiftc PRIVATE cxx_std_20)
set_target_properties(swiftc PROPERTIES
    OUTPUT_NAME "swiftc"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

# Include directories
target_include_directories(swiftc PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${LLVM_INCLUDE_DIRS}
)
