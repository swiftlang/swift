set(SOURCES
  main.cpp
)

add_executable(robust-parser ${SOURCES})

target_include_directories(robust-parser PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(robust-parser
  swiftcBasic
  swiftcLexer
  ${llvm_libs}
)