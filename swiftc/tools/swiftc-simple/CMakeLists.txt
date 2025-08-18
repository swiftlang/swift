set(SOURCES
  main.cpp
)

add_executable(swiftc-simple ${SOURCES})

target_include_directories(swiftc-simple PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(swiftc-simple
  swiftcBasic
  swiftcLexer
  swiftcParser
  swiftcAST
  swiftcSema
  ${llvm_libs}
  LLVMX86CodeGen
  LLVMX86AsmParser
  LLVMAArch64CodeGen
  LLVMAArch64AsmParser
)