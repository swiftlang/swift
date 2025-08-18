# Ultimate Swift Compiler with Full Language Support
add_executable(swiftc-ultimate
    main.cpp
)

target_link_libraries(swiftc-ultimate
    PRIVATE
    swiftcBasic
    swiftcLexer
    swiftcParser
    swiftcAST
    # Only link essential LLVM components
    LLVMCore
    LLVMSupport
    LLVMIRReader
    LLVMMC
)

target_include_directories(swiftc-ultimate PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${LLVM_INCLUDE_DIRS}
)