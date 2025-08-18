set(SOURCES
  main.cpp
)

add_executable(test-parser ${SOURCES})

target_include_directories(test-parser PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(test-parser
  swiftcBasic
  swiftcLexer
  swiftcAST
  swiftcParser
  ${llvm_libs}
)