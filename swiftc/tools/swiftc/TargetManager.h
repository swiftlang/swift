#ifndef SWIFTC_TARGETMANAGER_H
#define SWIFTC_TARGETMANAGER_H

#include "swiftc/Basic/LLVM.h"
#include <string>
#include <vector>
#include <memory>

namespace llvm {
class TargetMachine;
class Target;
class Triple;
} // namespace llvm

namespace swiftc {

/// Target information for cross-compilation
struct TargetInfo {
    std::string triple;
    std::string cpu;
    std::string features;
    std::string abi;
    bool isValid = false;
};

/// Manager for handling all LLVM targets and cross-compilation
class TargetManager {
    std::string currentTarget;
    std::vector<std::string> supportedTargets;
    std::unique_ptr<llvm::TargetMachine> targetMachine;

public:
    TargetManager();
    ~TargetManager();

    /// Initialize target manager with all LLVM targets
    bool initialize();

    /// Set the target for compilation
    bool setTarget(const std::string& targetTriple);

    /// Get current target
    std::string getCurrentTarget() const { return currentTarget; }

    /// Get all supported targets
    const std::vector<std::string>& getSupportedTargets() const { return supportedTargets; }

    /// Check if target is supported
    bool isTargetSupported(const std::string& targetTriple) const;

    /// Get target machine for current target
    llvm::TargetMachine* getTargetMachine() const { return targetMachine.get(); }

    /// Get target information
    TargetInfo getTargetInfo(const std::string& targetTriple) const;

    /// List all available targets
    void listTargets() const;

    /// Get default target for host system
    std::string getHostTarget() const;

    /// Validate target triple format
    bool validateTargetTriple(const std::string& triple) const;

    /// Get target-specific compiler flags
    std::vector<std::string> getTargetFlags(const std::string& targetTriple) const;

    /// Get target-specific linker flags
    std::vector<std::string> getLinkerFlags(const std::string& targetTriple) const;

    /// Get LLVM version string
    std::string getLLVMVersion() const;

    /// Get count of supported targets
    size_t getSupportedTargetCount() const { return supportedTargets.size(); }

private:
    /// Discover all available LLVM targets
    void discoverTargets();

    /// Create target machine for given triple
    bool createTargetMachine(const std::string& targetTriple);

    /// Parse target triple components
    bool parseTargetTriple(const std::string& triple, llvm::Triple& parsedTriple) const;

    /// Get target-specific CPU features
    std::string getTargetFeatures(const std::string& targetTriple) const;

    /// Get target-specific ABI
    std::string getTargetABI(const std::string& targetTriple) const;
};

} // namespace swiftc

#endif // SWIFTC_TARGETMANAGER_H
