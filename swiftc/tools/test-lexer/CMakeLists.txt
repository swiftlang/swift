set(SOURCES
  main.cpp
)

add_executable(test-lexer ${SOURCES})

target_include_directories(test-lexer PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(test-lexer
  swiftcBasic
  swiftcLexer
  ${llvm_libs}
)