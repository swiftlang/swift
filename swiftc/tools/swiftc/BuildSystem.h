#ifndef SWIFTC_BUILDSYSTEM_H
#define SWIFTC_BUILDSYSTEM_H

#include "swiftc/Basic/LLVM.h"
#include <string>
#include <vector>
#include <memory>
#include <map>

namespace swiftc {

class DiagnosticManager;
class TargetManager;

/// Build configuration
struct BuildConfig {
    std::string projectPath;
    std::string target;
    std::string mode;  // debug, release, profile
    int optimizationLevel;
    bool clean;
    bool verbose;
    int parallelJobs;
    std::vector<std::string> sourceFiles;
    std::vector<std::string> includePaths;
    std::vector<std::string> libraryPaths;
    std::vector<std::string> libraries;
};

/// Build result
struct BuildResult {
    bool success = false;
    std::string outputPath;
    std::vector<std::string> objectFiles;
    std::vector<std::string> dependencies;
    std::string errorMessage;
};

/// Project information
struct ProjectInfo {
    std::string name;
    std::string type;  // executable, library
    std::string version;
    std::string sourcePath;
    std::vector<std::string> sourceFiles;
    std::vector<std::string> dependencies;
    bool isValid = false;
};

/// Build system for managing project compilation
class BuildSystem {
    std::unique_ptr<DiagnosticManager> diags;
    TargetManager* targetManager;
    std::map<std::string, ProjectInfo> projects;

public:
    BuildSystem(DiagnosticManager& diagnosticManager, TargetManager& targetMgr);
    ~BuildSystem();

    /// Build a project
    BuildResult buildProject(const BuildConfig& config);

    /// Build a single file
    BuildResult buildFile(const std::string& filePath, const BuildConfig& config);

    /// Clean build artifacts
    bool cleanProject(const std::string& projectPath);

    /// Discover project information
    ProjectInfo discoverProject(const std::string& path);

    /// Get project dependencies
    std::vector<std::string> getProjectDependencies(const std::string& projectPath);

    /// Check if project needs rebuilding
    bool needsRebuild(const std::string& projectPath, const BuildConfig& config);

    /// Get build cache directory
    std::string getBuildCacheDir(const std::string& projectPath) const;

    /// Get output directory
    std::string getOutputDir(const std::string& projectPath, const BuildConfig& config) const;

    /// Validate build configuration
    bool validateBuildConfig(const BuildConfig& config);

private:
    /// Parse project configuration
    bool parseProjectConfig(const std::string& path, ProjectInfo& project);

    /// Find source files
    void findSourceFiles(const std::string& path, std::vector<std::string>& files);

    /// Compile source file to object file
    bool compileFile(const std::string& sourceFile, const std::string& objectFile, const BuildConfig& config);

    /// Link object files
    bool linkObjects(const std::vector<std::string>& objectFiles, const std::string& outputPath, const BuildConfig& config);

    /// Check file timestamps
    bool isFileNewer(const std::string& file1, const std::string& file2) const;

    /// Create build directories
    bool createBuildDirectories(const std::string& projectPath, const BuildConfig& config);

    /// Get compiler flags
    std::vector<std::string> getCompilerFlags(const BuildConfig& config);

    /// Get linker flags
    std::vector<std::string> getLinkerFlags(const BuildConfig& config);
};

} // namespace swiftc

#endif // SWIFTC_BUILDSYSTEM_H
