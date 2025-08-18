set(SOURCES
  main.cpp
)

add_executable(swiftc ${SOURCES})

target_include_directories(swiftc PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(swiftc
  swiftcBasic
  swiftcLexer
  swiftcParser
  swiftcAST
  swiftcSema
  swiftcSIL
  swiftcIRGen
  ${llvm_libs}
)