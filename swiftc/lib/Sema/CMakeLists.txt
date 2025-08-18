set(SOURCES
  TypeChecker.cpp
)

add_library(swiftcSema STATIC ${SOURCES})

target_include_directories(swiftcSema PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(swiftcSema swiftcBasic swiftcAST ${llvm_libs})