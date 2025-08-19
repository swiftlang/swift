#pragma once

#include <string>
#include <vector>
#include <memory>
#include "llvm/Support/CommandLine.h"

namespace swiftc {

/// Supported commands
enum class Command {
    Compile,    // Compile Swift source files
    Build,      // Build a Swift project
    Run,        // Run a compiled Swift program
    Test,       // Run Swift tests
    Init,       // Initialize a new Swift project
    Check,      // Type check Swift source files
    Help,       // Show help information
    Version     // Show version information
};

/// Compilation options
struct CompileOptions {
    std::vector<std::string> inputFiles;
    std::string outputFile;
    std::string targetTriple;
    bool emitLLVM = false;
    bool emitSIL = false;
    bool emitAssembly = false;
    bool emitObject = false;
    bool emitExecutable = false;
    bool optimize = false;
    bool debugInfo = false;
    std::vector<std::string> includePaths;
    std::vector<std::string> libraryPaths;
    std::vector<std::string> libraries;
    std::vector<std::string> defineFlags;
    bool warningsAsErrors = false;
    bool verbose = false;
};

/// Build options
struct BuildOptions {
    std::string projectPath;
    std::string buildType = "debug";
    bool clean = false;
    bool parallel = true;
    std::string targetTriple;
    bool verbose = false;
};

/// Command line parser using LLVM's cl library
class CommandLineParser {
private:
    // LLVM command line options
    llvm::cl::opt<std::string> outputFile;
    llvm::cl::opt<std::string> targetTriple;
    llvm::cl::opt<bool> emitLLVM;
    llvm::cl::opt<bool> emitSIL;
    llvm::cl::opt<bool> emitAssembly;
    llvm::cl::opt<bool> emitObject;
    llvm::cl::opt<bool> emitExecutable;
    llvm::cl::opt<bool> optimize;
    llvm::cl::opt<bool> debugInfo;
    llvm::cl::opt<bool> warningsAsErrors;
    llvm::cl::opt<bool> verbose;
    llvm::cl::opt<std::string> buildType;
    llvm::cl::opt<bool> clean;
    llvm::cl::opt<bool> parallel;
    
    // Positional arguments
    llvm::cl::list<std::string> inputFiles;
    llvm::cl::list<std::string> includePaths;
    llvm::cl::list<std::string> libraryPaths;
    llvm::cl::list<std::string> libraries;
    llvm::cl::list<std::string> defineFlags;
    
    // Command tracking
    Command currentCommand;
    std::string commandName;
    std::string projectPath;

public:
    CommandLineParser();
    ~CommandLineParser();

    /// Parse command line arguments
    bool parse(int argc, char *argv[]);

    /// Get the parsed command
    Command getCommand() const { return currentCommand; }

    /// Get compilation options
    CompileOptions getCompileOptions() const;

    /// Get build options
    BuildOptions getBuildOptions() const;

    /// Get command name (e.g., "compile", "build")
    std::string getCommandName() const { return commandName; }

    /// Get project path for build/init commands
    std::string getProjectPath() const { return projectPath; }

private:
    /// Parse command from first argument
    bool parseCommand(const std::string& cmd);
    
    /// Setup LLVM command line options
    void setupOptions();
    
    /// Convert LLVM options to our structs
    void convertOptions();
};

} // namespace swiftc
