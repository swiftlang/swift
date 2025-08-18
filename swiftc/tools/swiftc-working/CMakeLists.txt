# Working Swift Compiler with Standard Library Support
add_executable(swiftc-working
    main.cpp
)

target_link_libraries(swiftc-working
    PRIVATE
    swiftcBasic
    swiftcLexer
    swiftcParser
    swiftcAST
    # Only link essential LLVM components to avoid linker issues
    LLVMCore
    LLVMSupport
    LLVMIRReader
    LLVMMC
)

target_include_directories(swiftc-working PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${LLVM_INCLUDE_DIRS}
)