set(SOURCES
  ASTNode.cpp
  Expr.cpp
  Stmt.cpp
  Decl.cpp
  Type.cpp
)

add_library(swiftcAST STATIC ${SOURCES})

target_include_directories(swiftcAST PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/../../include
)

target_link_libraries(swiftcAST swiftcBasic ${llvm_libs})