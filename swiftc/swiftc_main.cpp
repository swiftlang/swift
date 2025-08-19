#include "swiftc/Driver/Driver.h"
#include "swiftc/Basic/Diagnostic.h"
#include <iostream>
#include <string>

using namespace swiftc;

void printUsage(const char* programName) {
    std::cout << "Usage: " << programName << " [options] <input.swift>" << std::endl;
    std::cout << "Options:" << std::endl;
    std::cout << "  -o <output>    Specify output executable name" << std::endl;
    std::cout << "  -h, --help     Show this help message" << std::endl;
    std::cout << "  --version      Show compiler version" << std::endl;
    std::cout << "  --verbose      Enable verbose output" << std::endl;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printUsage(argv[0]);
        return 1;
    }
    
    std::string inputFile;
    std::string outputFile;
    bool verbose = false;
    
    // Parse command line arguments
    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        
        if (arg == "-h" || arg == "--help") {
            printUsage(argv[0]);
            return 0;
        } else if (arg == "--version") {
            std::cout << "SwiftC Compiler v1.0" << std::endl;
            std::cout << "Complete Swift compilation pipeline" << std::endl;
            return 0;
        } else if (arg == "--verbose") {
            verbose = true;
        } else if (arg == "-o" && i + 1 < argc) {
            outputFile = argv[++i];
        } else if (arg.find(".swift") != std::string::npos) {
            inputFile = arg;
        } else {
            std::cerr << "Unknown option: " << arg << std::endl;
            return 1;
        }
    }
    
    if (inputFile.empty()) {
        std::cerr << "Error: No input file specified" << std::endl;
        printUsage(argv[0]);
        return 1;
    }
    
    if (outputFile.empty()) {
        // Default output name
        size_t dotPos = inputFile.find_last_of('.');
        if (dotPos != std::string::npos) {
            outputFile = inputFile.substr(0, dotPos);
        } else {
            outputFile = inputFile + "_out";
        }
    }
    
    std::cout << "=== SwiftC Complete Compiler Pipeline ===" << std::endl;
    std::cout << "Input:  " << inputFile << std::endl;
    std::cout << "Output: " << outputFile << std::endl;
    std::cout << std::endl;
    
    if (verbose) {
        std::cout << "🔧 Verbose mode enabled" << std::endl;
    }
    
    // Create and run the compiler driver
    Driver driver;
    
    if (verbose) {
        driver.printCompilerInfo();
        std::cout << std::endl;
    }
    
    bool success = driver.compileFile(inputFile, outputFile);
    
    if (success) {
        std::cout << std::endl;
        std::cout << "🎉 COMPILATION SUCCESSFUL!" << std::endl;
        std::cout << "✅ All compiler phases completed successfully" << std::endl;
        std::cout << "📦 Executable created: " << outputFile << std::endl;
        
        if (verbose) {
            std::cout << std::endl;
            std::cout << "📊 Compilation Statistics:" << std::endl;
            std::cout << "  ✅ Lexical Analysis: Complete" << std::endl;
            std::cout << "  ✅ Syntax Analysis: Complete" << std::endl;
            std::cout << "  ✅ Semantic Analysis: Complete" << std::endl;
            std::cout << "  ✅ SIL Generation: Complete" << std::endl;
            std::cout << "  ✅ SIL Optimization: Complete" << std::endl;
            std::cout << "  ✅ LLVM IR Generation: Complete" << std::endl;
            std::cout << "  ✅ LLVM Optimization: Complete" << std::endl;
            std::cout << "  ✅ Code Generation: Complete" << std::endl;
            std::cout << "  ✅ Linking: Complete" << std::endl;
        }
        
        return 0;
    } else {
        std::cout << std::endl;
        std::cout << "❌ COMPILATION FAILED!" << std::endl;
        
        // Print any diagnostics
        auto& diags = driver.getDiagnostics();
        if (diags.hasErrors()) {
            std::cout << "Errors encountered:" << std::endl;
            for (const auto& diag : diags.getDiagnostics()) {
                if (diag.Level == DiagnosticLevel::Error) {
                    std::cout << "  Error: " << diag.Message << std::endl;
                }
            }
        }
        
        return 1;
    }
}