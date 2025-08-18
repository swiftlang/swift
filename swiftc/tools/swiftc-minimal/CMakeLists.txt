# Minimal Swift Compiler - No LLVM backend, just parsing and AST
add_executable(swiftc-minimal
    main.cpp
)

target_link_libraries(swiftc-minimal
    PRIVATE
    swiftcBasic
    swiftcLexer
    swiftcParser
    swiftcAST
    ${llvm_libs}
)

target_include_directories(swiftc-minimal PRIVATE
    ${CMAKE_SOURCE_DIR}/include
)