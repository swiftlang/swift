#ifndef SWIFTC_COMPILERDRIVER_H
#define SWIFTC_COMPILERDRIVER_H

#include "CommandLine.h"
#include "swiftc/Basic/LLVM.h"
#include <memory>

namespace swiftc {

class DiagnosticManager;
class BuildSystem;
class TargetManager;
class OptimizationManager;

/// Main compiler driver that orchestrates the entire compilation process
class CompilerDriver {
    DiagnosticManager* diags;
    std::unique_ptr<BuildSystem> buildSystem;
    std::unique_ptr<TargetManager> targetManager;
    std::unique_ptr<OptimizationManager> optManager;

public:
    CompilerDriver(DiagnosticManager& diagnosticManager);
    ~CompilerDriver();

    /// Execute the requested command
    bool execute(Command command, const CompileOptions& compileOpts, const BuildOptions& buildOpts);

private:
    /// Execute compile command
    bool executeCompile(const CompileOptions& opts);
    
    /// Execute build command
    bool executeBuild(const BuildOptions& opts);
    
    /// Execute run command
    bool executeRun(const BuildOptions& opts);
    
    /// Execute test command
    bool executeTest(const BuildOptions& opts);
    
    /// Execute init command
    bool executeInit(const BuildOptions& opts);
    
    /// Execute check command
    bool executeCheck(const CompileOptions& opts);
    
    /// Execute help command
    bool executeHelp();
    
    /// Execute version command
    bool executeVersion();
};

} // namespace swiftc

#endif // SWIFTC_COMPILERDRIVER_H
