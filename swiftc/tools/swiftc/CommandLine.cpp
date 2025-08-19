#include "CommandLine.h"
#include <iostream>

namespace swiftc {

CommandLineParser::CommandLineParser()
    : outputFile("o", llvm::cl::desc("Output file"), llvm::cl::value_desc("file"))
    , targetTriple("target", llvm::cl::desc("Target triple"), llvm::cl::value_desc("triple"))
    , emitLLVM("emit-llvm", llvm::cl::desc("Emit LLVM IR"))
    , emitSIL("emit-sil", llvm::cl::desc("Emit Swift Intermediate Language"))
    , emitAssembly("S", llvm::cl::desc("Emit assembly"))
    , emitObject("c", llvm::cl::desc("Emit object file"))
    , emitExecutable("", llvm::cl::desc("Emit executable (default)"))
    , optimize("O", llvm::cl::desc("Optimization level"), llvm::cl::value_desc("level"))
    , debugInfo("g", llvm::cl::desc("Generate debug info"))
    , warningsAsErrors("Werror", llvm::cl::desc("Treat warnings as errors"))
    , verbose("v", llvm::cl::desc("Verbose output"))
    , buildType("build-type", llvm::cl::desc("Build type"), llvm::cl::init("debug"), llvm::cl::value_desc("type"))
    , clean("clean", llvm::cl::desc("Clean build directory"))
    , parallel("parallel", llvm::cl::desc("Enable parallel builds"), llvm::cl::init(true))
    , inputFiles(llvm::cl::Positional, llvm::cl::desc("<input files>"), llvm::cl::OneOrMore)
    , includePaths("I", llvm::cl::desc("Include path"), llvm::cl::value_desc("path"))
    , libraryPaths("L", llvm::cl::desc("Library path"), llvm::cl::value_desc("path"))
    , libraries("l", llvm::cl::desc("Link library"), llvm::cl::value_desc("library"))
    , defineFlags("D", llvm::cl::desc("Define macro"), llvm::cl::value_desc("macro"))
    , currentCommand(Command::Compile)
    , commandName("compile")
    , projectPath(".") {
    
    setupOptions();
}

CommandLineParser::~CommandLineParser() = default;

void CommandLineParser::setupOptions() {
    // Hide the default help option since we handle it ourselves
    llvm::cl::HideUnrelatedOptions(llvm::cl::getGeneralCategory());
}

bool CommandLineParser::parse(int argc, char *argv[]) {
    if (argc < 2) {
        // No arguments provided, show help
        currentCommand = Command::Help;
        return true;
    }

    // Parse the command first
    std::string cmd = argv[1];
    if (!parseCommand(cmd)) {
        return false;
    }

    // For help and version, we don't need to parse further options
    if (currentCommand == Command::Help || currentCommand == Command::Version) {
        return true;
    }

    // Parse LLVM command line options
    std::vector<const char*> llvmArgs;
    llvmArgs.push_back(argv[0]); // program name
    
    // Add all arguments starting from the command
    for (int i = 1; i < argc; ++i) {
        llvmArgs.push_back(argv[i]);
    }

    // Parse with LLVM
    llvm::cl::ParseCommandLineOptions(llvmArgs.size(), llvmArgs.data());
    
    // Convert LLVM options to our structs
    convertOptions();
    
    return true;
}

bool CommandLineParser::parseCommand(const std::string& cmd) {
    if (cmd == "compile" || cmd == "c") {
        currentCommand = Command::Compile;
        commandName = "compile";
    } else if (cmd == "build" || cmd == "b") {
        currentCommand = Command::Build;
        commandName = "build";
    } else if (cmd == "run" || cmd == "r") {
        currentCommand = Command::Run;
        commandName = "run";
    } else if (cmd == "test" || cmd == "t") {
        currentCommand = Command::Test;
        commandName = "test";
    } else if (cmd == "init" || cmd == "i") {
        currentCommand = Command::Init;
        commandName = "init";
    } else if (cmd == "check" || cmd == "k") {
        currentCommand = Command::Check;
        commandName = "check";
    } else if (cmd == "help" || cmd == "h" || cmd == "--help" || cmd == "-h") {
        currentCommand = Command::Help;
        commandName = "help";
    } else if (cmd == "version" || cmd == "--version" || cmd == "-v") {
        currentCommand = Command::Version;
        commandName = "version";
    } else {
        // Default to compile if no recognized command
        currentCommand = Command::Compile;
        commandName = "compile";
        
        // Check if this looks like a file path (ends with .swift)
        if (cmd.find(".swift") != std::string::npos) {
            // This is a file, not a command, so we're in compile mode
            return true;
        }
    }
    
    return true;
}

void CommandLineParser::convertOptions() {
    // This will be called after LLVM parsing to convert the options
    // to our internal structs
}

CompileOptions CommandLineParser::getCompileOptions() const {
    CompileOptions opts;
    
    // Convert LLVM options to our struct
    if (!outputFile.empty()) {
        opts.outputFile = outputFile;
    }
    
    if (!targetTriple.empty()) {
        opts.targetTriple = targetTriple;
    }
    
    opts.emitLLVM = emitLLVM;
    opts.emitSIL = emitSIL;
    opts.emitAssembly = emitAssembly;
    opts.emitObject = emitObject;
    opts.emitExecutable = emitExecutable;
    opts.optimize = optimize;
    opts.debugInfo = debugInfo;
    opts.warningsAsErrors = warningsAsErrors;
    opts.verbose = verbose;
    
    // Convert lists by copying values
    opts.inputFiles.clear();
    for (const auto& file : inputFiles) {
        opts.inputFiles.push_back(file);
    }
    
    opts.includePaths.clear();
    for (const auto& path : includePaths) {
        opts.includePaths.push_back(path);
    }
    
    opts.libraryPaths.clear();
    for (const auto& path : libraryPaths) {
        opts.libraryPaths.push_back(path);
    }
    
    opts.libraries.clear();
    for (const auto& lib : libraries) {
        opts.libraries.push_back(lib);
    }
    
    opts.defineFlags.clear();
    for (const auto& flag : defineFlags) {
        opts.defineFlags.push_back(flag);
    }
    
    return opts;
}

BuildOptions CommandLineParser::getBuildOptions() const {
    BuildOptions opts;
    
    opts.projectPath = projectPath;
    opts.buildType = buildType;
    opts.clean = clean;
    opts.parallel = parallel;
    opts.verbose = verbose;
    
    if (!targetTriple.empty()) {
        opts.targetTriple = targetTriple;
    }
    
    return opts;
}

} // namespace swiftc
