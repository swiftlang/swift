set(SOURCES
  Parser.cpp
)

add_library(swiftcParser STATIC ${SOURCES})

target_include_directories(swiftcParser PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(swiftcParser swiftcBasic swiftcLexer swiftcAST ${llvm_libs})