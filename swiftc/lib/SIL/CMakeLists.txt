set(SOURCES
  SILInstruction.cpp
  SILFunction.cpp
  SILGen.cpp
)

add_library(swiftcSIL STATIC ${SOURCES})

target_include_directories(swiftcSIL PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(swiftcSIL swiftcBasic swiftcAST swiftcSema ${llvm_libs})