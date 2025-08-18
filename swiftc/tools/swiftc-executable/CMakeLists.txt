# Executable Swift Compiler with JIT Execution
add_executable(swiftc-executable
    main.cpp
)

target_link_libraries(swiftc-executable
    PRIVATE
    swiftcBasic
    swiftcLexer
    swiftcParser
    swiftcAST
    # LLVM components for execution
    LLVMCore
    LLVMSupport
    LLVMIRReader
    LLVMMC
    LLVMExecutionEngine
    LLVMMCJIT
    LLVMTarget
    LLVMCodeGen
    LLVMAsmPrinter
    LLVMX86CodeGen
    LLVMX86AsmParser
    LLVMX86Desc
    LLVMX86Info
)

target_include_directories(swiftc-executable PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${LLVM_INCLUDE_DIRS}
)