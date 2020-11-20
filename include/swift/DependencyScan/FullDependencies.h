//===------------------ FullDependencies.h - Swift Compiler ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEPENDENCY_FULL_DEPENDENCIES_H
#define SWIFT_DEPENDENCY_FULL_DEPENDENCIES_H

#include "swift/AST/ModuleDependencies.h"
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

// This file describes the format for in-memory dependency scanning result
// produced by the Swift dependency scanner. 

namespace swift {
namespace dependencies {

/// Swift modules to be built from a module interface, may have a bridging
/// header.
struct SwiftTextualModuleDetails {
  /// The module interface from which this module was built, if any.
  std::string ModuleInterfacePath;

  /// The paths of potentially ready-to-use compiled modules for the interface.
  std::vector<std::string> CompiledModuleCandidates;

  /// The bridging header, if any.
  std::string BridgingHeaderPath;

  /// The source files referenced by the bridging header.
  std::vector<std::string> BridgingSourceFiles;

  /// (Clang) modules on which the bridging header depends.
  std::vector<std::string> BridgingModuleDependencies;

  /// Options to the compile command required to build this module interface
  std::vector<std::string> CommandLine;

  /// To build a PCM to be used by this Swift module, we need to append these
  /// arguments to the generic PCM build arguments reported from the dependency
  /// graph.
  std::vector<std::string> ExtraPcmArgs;

  /// The hash value that will be used for the generated module
  std::string ContextHash;

  /// A flag to indicate whether or not this module is a framework.
  bool IsFramework = false;
};

/// Swift modules with only a binary module file.
struct SwiftBinaryModuleDetails {
  /// The path to the pre-compiled binary module
  std::string CompiledModulePath;

  /// The path to the .swiftModuleDoc file.
  std::string ModuleDocPath;

  /// The path to the .swiftSourceInfo file.
  std::string ModuleSourceInfoPath;
};

/// Swift placeholder modules carry additional details that specify their
/// module doc path and source info paths.
struct SwiftPlaceholderModuleDetails {
  /// The path to the pre-compiled binary module
  std::string CompiledModulePath;

  /// The path to the .swiftModuleDoc file.
  std::string ModuleDocPath;

  /// The path to the .swiftSourceInfo file.
  std::string ModuleSourceInfoPath;
};

/// Clang modules are built from a module map file.
struct ClangModuleDetails {
  /// The path to the module map used to build this module.
  std::string ModuleMapPath;

  /// Set of PCM Arguments of depending modules which
  /// are covered by the directDependencies info of this module
  // TODO: Should this be modeled here or in the clients?
  // const std::set<std::string> DependenciesCapturedPCMArgs;

  /// clang-generated context hash
  std::string ContextHash;

  /// Options to the compile command required to build this clang modulemap
  std::vector<std::string> CommandLine;
};

struct ModuleDetails {
  ModuleDetails(SwiftTextualModuleDetails Details)
      : DetailsKind(ModuleDependenciesKind::SwiftTextual), Value(Details) {}
  ModuleDetails(SwiftBinaryModuleDetails Details)
      : DetailsKind(ModuleDependenciesKind::SwiftBinary), Value(Details) {}
  ModuleDetails(SwiftPlaceholderModuleDetails Details)
      : DetailsKind(ModuleDependenciesKind::SwiftPlaceholder), Value(Details) {}
  ModuleDetails(ClangModuleDetails Details)
      : DetailsKind(ModuleDependenciesKind::Clang), Value(Details) {}

  ~ModuleDetails() {
    switch (DetailsKind) {
    case ModuleDependenciesKind::SwiftTextual:
      (&Value.TextualDetails)->~SwiftTextualModuleDetails();
      break;
    case ModuleDependenciesKind::SwiftBinary:
      (&Value.BinaryDetails)->~SwiftBinaryModuleDetails();
      break;
    case ModuleDependenciesKind::SwiftPlaceholder:
      (&Value.PlaceholderDetails)->~SwiftPlaceholderModuleDetails();
      break;
    case ModuleDependenciesKind::Clang:
      (&Value.ClangDetails)->~ClangModuleDetails();
      break;
    }
  }

  ModuleDetails(const ModuleDetails &src) :
    DetailsKind(src.DetailsKind) {
    switch (src.DetailsKind) {
    case ModuleDependenciesKind::SwiftBinary:
        new (&Value.BinaryDetails) auto(src.Value.BinaryDetails);
        break;
    case ModuleDependenciesKind::SwiftTextual:
      new (&Value.TextualDetails) auto(src.Value.TextualDetails);
      break;
    case ModuleDependenciesKind::SwiftPlaceholder:
      new (&Value.PlaceholderDetails) auto(src.Value.PlaceholderDetails);
      break;
    case ModuleDependenciesKind::Clang:
      new (&Value.ClangDetails) auto(src.Value.ClangDetails);
      break;
    }
  }

  const SwiftTextualModuleDetails *getAsSwiftTextualModule() const {
    return DetailsKind == ModuleDependenciesKind::SwiftTextual
               ? &Value.TextualDetails
               : nullptr;
  }
  const SwiftPlaceholderModuleDetails *
  getAsPlaceholderDependencyModule() const {
    return DetailsKind == ModuleDependenciesKind::SwiftPlaceholder
               ? &Value.PlaceholderDetails
               : nullptr;
  }
  const SwiftBinaryModuleDetails *getAsSwiftBinaryModule() const {
    return DetailsKind == ModuleDependenciesKind::SwiftBinary
               ? &Value.BinaryDetails
               : nullptr;
  }
  const ClangModuleDetails *getAsClangModule() const {
    return DetailsKind == ModuleDependenciesKind::Clang ? &Value.ClangDetails
                                                        : nullptr;
  }

private:
  ModuleDependenciesKind DetailsKind;

  union ModuleDetailsValue {
    SwiftTextualModuleDetails TextualDetails;
    SwiftBinaryModuleDetails BinaryDetails;
    SwiftPlaceholderModuleDetails PlaceholderDetails;
    ClangModuleDetails ClangDetails;

    ModuleDetailsValue(SwiftTextualModuleDetails Details)
        : TextualDetails(Details) {}
    ModuleDetailsValue(SwiftBinaryModuleDetails Details)
        : BinaryDetails(Details) {}
    ModuleDetailsValue(SwiftPlaceholderModuleDetails Details)
        : PlaceholderDetails(Details) {}
    ModuleDetailsValue(ClangModuleDetails Details) : ClangDetails(Details) {}

    ModuleDetailsValue() {}
    ~ModuleDetailsValue() {}
  } Value;
};

struct ModuleInfo {
  /// The ID of the module this info describes
  ModuleDependencyID ID;

  /// The path for the module.
  std::string ModulePath;

  /// The source files used to build this module.
  std::vector<std::string> SourceFiles;

  /// The set of direct module dependencies of this module.
  std::vector<ModuleDependencyID> DirectDependencies;

  /// Specific details of a particular kind of module.
  ModuleDetails Details;
};

struct FullDependencies {
  /// The name of the main module for this dependency graph (root node)
  const std::string MainModuleName;

  /// The complete set of modules discovered
  std::vector<ModuleInfo> Modules;

  FullDependencies(std::string MainModuleName)
      : MainModuleName(MainModuleName), Modules() {}
};
} // namespace dependencies
} // namespace swift

#endif // SWIFT_DEPENDENCY_FULL_DEPENDENCIES_H
