set(SOURCES
  main.cpp
)

add_executable(swiftc-binary ${SOURCES})

target_include_directories(swiftc-binary PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(swiftc-binary
  swiftcBasic
  swiftcLexer
  swiftcParser
  swiftcAST
  swiftcSema
  ${llvm_libs}
  zstd::libzstd_shared
)