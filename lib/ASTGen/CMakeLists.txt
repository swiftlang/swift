add_subdirectory(${SWIFT_SOURCE_DIR}/../swift-syntax ${CMAKE_CURRENT_BINARY_DIR}/swift-syntax)

add_library(ASTGen
        ASTGen.swift)

target_compile_options(ASTGen PUBLIC
        "-emit-module-interface")

target_link_libraries(ASTGen PUBLIC
  SwiftSyntax
  SwiftParser
  SwiftDiagnostics)
  
target_include_directories(SwiftParser PUBLIC
  "${CMAKE_CURRENT_BINARY_DIR}/../Parser/swift-syntax/SwiftSyntax"
  "${CMAKE_CURRENT_BINARY_DIR}/../Parser/swift-syntax/SwiftDiagnostics"
  "${CMAKE_CURRENT_BINARY_DIR}/../Parser/swift-syntax/SwiftParser")

set_property(GLOBAL APPEND PROPERTY SWIFTSYNTAX_EXPORTS ASTGen)

install(TARGETS ASTGen
  EXPORT SwiftSyntaxTargets
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  RUNTIME DESTINATION bin)

add_dependencies(ASTGen swiftAST)