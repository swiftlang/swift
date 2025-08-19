#ifndef SWIFTC_OPTIMIZATIONMANAGER_H
#define SWIFTC_OPTIMIZATIONMANAGER_H

#include "swiftc/Basic/LLVM.h"
#include <string>
#include <vector>
#include <memory>
#include <map>

namespace swiftc {

/// Optimization level configuration
enum class OptimizationLevel {
    None = 0,      // -O0: No optimizations
    Basic = 1,     // -O1: Basic optimizations
    Standard = 2,  // -O2: Standard optimizations
    Aggressive = 3 // -O3: Aggressive optimizations
};

/// Build mode configuration
enum class BuildMode {
    Debug,         // Debug build with assertions
    Release,       // Release build with optimizations
    Profile,       // Profile-guided optimization
    Size           // Size-optimized build
};

/// Optimization configuration
struct OptimizationConfig {
    OptimizationLevel level = OptimizationLevel::None;
    BuildMode mode = BuildMode::Debug;
    bool linkTimeOptimization = false;
    bool crossModuleOptimization = false;
    bool vectorize = true;
    bool unroll = true;
    bool inlineFunctions = true;
    bool tailCallElimination = true;
    std::vector<std::string> customPasses;
    std::vector<std::string> disabledPasses;
};

/// Manager for controlling LLVM optimization passes
class OptimizationManager {
    OptimizationConfig currentConfig;
    bool initialized = false;

public:
    OptimizationManager();
    ~OptimizationManager();

    /// Initialize optimization manager
    bool initialize();

    /// Configure optimization level
    void configureOptimization(OptimizationLevel level);

    /// Configure build mode
    void configureBuildMode(BuildMode mode);

    /// Set custom optimization configuration
    void setOptimizationConfig(const OptimizationConfig& config);

    /// Get current optimization configuration
    const OptimizationConfig& getOptimizationConfig() const { return currentConfig; }

    /// Get optimization statistics
    std::map<std::string, int> getOptimizationStats() const;

    /// Enable specific optimization pass
    void enablePass(const std::string& passName);

    /// Disable specific optimization pass
    void disablePass(const std::string& passName);

    /// Reset to default optimization configuration
    void resetToDefaults();

    /// Get available optimization passes
    std::vector<std::string> getAvailablePasses() const;

    /// Get enabled optimization passes
    std::vector<std::string> getEnabledPasses() const;

    /// Get disabled optimization passes
    std::vector<std::string> getDisabledPasses() const;

private:
    /// Setup optimization passes for given level
    void setupOptimizationPasses(OptimizationLevel level);

    /// Setup build mode specific passes
    void setupBuildModePasses(BuildMode mode);

    /// Add standard optimization passes
    void addStandardPasses();

    /// Add size optimization passes
    void addSizeOptimizationPasses();

    /// Add performance optimization passes
    void addPerformanceOptimizationPasses();

    /// Add profile-guided optimization passes
    void addProfileGuidedOptimizationPasses();

    /// Add link-time optimization passes
    void addLinkTimeOptimizationPasses();

    /// Validate pass configuration
    bool validatePassConfiguration() const;
};

} // namespace swiftc

#endif // SWIFTC_OPTIMIZATIONMANAGER_H
