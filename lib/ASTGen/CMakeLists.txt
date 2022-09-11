# TODO: why do we need these on Linux?
set(CMAKE_Swift_ARCHIVE_CREATE "<CMAKE_AR> crs <TARGET> <OBJECTS>")
set(CMAKE_Swift_ARCHIVE_FINISH "")

if (SWIFT_SWIFT_PARSER)
    add_library(ASTGen
            Sources/ASTGen/ASTGen.swift
            Sources/ASTGen/Decls.swift
            Sources/ASTGen/Exprs.swift
            Sources/ASTGen/Literals.swift
            Sources/ASTGen/Misc.swift
            Sources/ASTGen/Stmts.swift)

    target_link_libraries(ASTGen PUBLIC
            SwiftSyntax
            SwiftParser
            SwiftDiagnostics)

    target_link_directories(ASTGen PUBLIC
            "${SWIFT_PATH_TO_EARLYSWIFTSYNTAX_BUILD_DIR}/lib")

    target_include_directories(ASTGen PUBLIC
            "${SWIFT_PATH_TO_EARLYSWIFTSYNTAX_BUILD_DIR}/swift")
else()
    add_library(ASTGen
            Sources/ASTGen/DummyASTGen.swift)
endif()

set_target_properties(ASTGen PROPERTIES LINKER_LANGUAGE Swift)

target_compile_options(ASTGen PUBLIC
        "-emit-module-interface"
        "-g")

add_dependencies(ASTGen swiftAST)