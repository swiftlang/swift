set(SOURCES
  Token.cpp
  Lexer.cpp
)

add_library(swiftcLexer STATIC ${SOURCES})

target_include_directories(swiftcLexer PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(swiftcLexer swiftcBasic ${llvm_libs})