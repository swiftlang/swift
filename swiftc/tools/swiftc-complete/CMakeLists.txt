# Complete Swift Compiler with Full Standard Library Support
add_executable(swiftc-complete
    main.cpp
)

target_link_libraries(swiftc-complete
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

target_include_directories(swiftc-complete PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${LLVM_INCLUDE_DIRS}
)