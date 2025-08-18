set(SOURCES
  main.cpp
)

add_executable(ultimate-parser ${SOURCES})

target_include_directories(ultimate-parser PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(ultimate-parser
  swiftcBasic
  swiftcLexer
  ${llvm_libs}
)