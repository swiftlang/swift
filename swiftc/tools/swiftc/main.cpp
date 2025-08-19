#include "CompilerDriver.h"
#include "CommandLine.h"
#include "DiagnosticManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>

using namespace swiftc;

int main(int argc, char *argv[]) {
    // Initialize LLVM with pretty stack traces
    llvm::InitLLVM initLLVM(argc, argv);
    
    // Enable pretty stack traces for crashes
    llvm::PrettyStackTraceProgram X(argc, argv);
    
    // Create diagnostic manager
    DiagnosticManager diags;
    
    // Parse command line
    CommandLineParser parser;
    if (!parser.parse(argc, argv)) {
        diags.error("Failed to parse command line arguments");
        diags.printErrors();
        return 1;
    }
    
    // Create and run compiler driver
    CompilerDriver driver(diags);
    
    // Execute the requested command
    if (!driver.execute(parser.getCommand(), parser.getCompileOptions(), parser.getBuildOptions())) {
        diags.printErrors();
        return 1;
    }
    
    // Always print diagnostics (for help, version, etc.)
    diags.printDiagnostics();
    
    return 0;
}
