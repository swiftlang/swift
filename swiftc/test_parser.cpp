#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Parser/Parser.h"
#include "swiftc/Basic/Diagnostic.h"
#include <iostream>
#include <fstream>
#include <sstream>

int main() {
    // Read the ComprehensiveExample.swift file
    std::ifstream file("/workspace/swiftc/ComprehensiveExample.swift");
    if (!file.is_open()) {
        std::cerr << "Error: Could not open ComprehensiveExample.swift" << std::endl;
        return 1;
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    
    // Create diagnostic engine
    swiftc::DiagnosticEngine diags;
    
    // Create lexer
    swiftc::Lexer lexer(source, swiftc::SourceLoc(1), diags);
    
    // Create parser
    swiftc::Parser parser(lexer, diags);
    
    // Parse the source file
    std::cout << "Parsing ComprehensiveExample.swift..." << std::endl;
    auto decls = parser.parseSourceFile();
    
    std::cout << "Successfully parsed " << decls.size() << " top-level declarations!" << std::endl;
    
    // Print basic info about each declaration
    for (size_t i = 0; i < decls.size(); ++i) {
        auto& decl = decls[i];
        std::cout << "Declaration " << (i+1) << ": ";
        
        switch (decl->getKind()) {
        case swiftc::NodeKind::FuncDecl:
            std::cout << "Function";
            break;
        case swiftc::NodeKind::VarDecl:
            std::cout << "Variable";
            break;
        case swiftc::NodeKind::ClassDecl:
            std::cout << "Class";
            break;
        case swiftc::NodeKind::StructDecl:
            std::cout << "Struct";
            break;
        case swiftc::NodeKind::EnumDecl:
            std::cout << "Enum";
            break;
        case swiftc::NodeKind::ProtocolDecl:
            std::cout << "Protocol";
            break;
        case swiftc::NodeKind::TypeAliasDecl:
            std::cout << "TypeAlias";
            break;
        case swiftc::NodeKind::PrecedenceGroupDecl:
            std::cout << "PrecedenceGroup";
            break;
        case swiftc::NodeKind::OperatorDecl:
            std::cout << "Operator";
            break;
        default:
            std::cout << "Other";
            break;
        }
        std::cout << std::endl;
    }
    
    return 0;
}