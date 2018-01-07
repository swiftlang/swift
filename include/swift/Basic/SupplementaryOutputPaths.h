//
//  OutputPaths.h
//  Swift
//
//  Created by David Ungar on 12/22/17.
//

#ifndef OutputPaths_h
#define OutputPaths_h

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/Optional.h"

#include <string>
#include <vector>

namespace swift {
struct SupplementaryOutputPaths {
  /// The path to which we should emit an Objective-C header for the module.
  std::string ObjCHeaderOutputPath;

  /// The path to which we should emit a serialized module.
  std::string ModuleOutputPath;

  /// The path to which we should emit a module documentation file.
  std::string ModuleDocOutputPath;

  /// The path to which we should output a Make-style dependencies file.
  std::string DependenciesFilePath;

  /// The path to which we should output a Swift reference dependencies file.
  std::string ReferenceDependenciesFilePath;

  /// Path to a file which should contain serialized diagnostics for this
  /// frontend invocation.
  std::string SerializedDiagnosticsPath;

  /// The path to which we should output a loaded module trace file.
  std::string LoadedModuleTracePath;

  /// The path to which we should output a TBD file.
  std::string TBDPath;

  SupplementaryOutputPaths(std::string objCHeaderOutputPath,
                           std::string moduleOutputPath,
                           std::string moduleDocOutputPath,
                           std::string dependenciesFilePath,
                           std::string referenceDependenciesFilePath,
                           std::string serializedDiagnosticsPath,
                           std::string loadedModuleTracePath,
                           std::string tbdPath)
      : ObjCHeaderOutputPath(objCHeaderOutputPath),
        ModuleOutputPath(moduleOutputPath),
        ModuleDocOutputPath(moduleDocOutputPath),
        DependenciesFilePath(dependenciesFilePath),
        ReferenceDependenciesFilePath(referenceDependenciesFilePath),
        SerializedDiagnosticsPath(serializedDiagnosticsPath),
        LoadedModuleTracePath(loadedModuleTracePath), TBDPath(tbdPath) {}

  SupplementaryOutputPaths() = default;
  SupplementaryOutputPaths(const SupplementaryOutputPaths &) = default;

};
} // namespace swift

#endif /* OutputPaths_h */
