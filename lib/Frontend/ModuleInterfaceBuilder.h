//===----- ModuleInterfaceBuilder.h - Compiles .swiftinterface files ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_MODULEINTERFACEBUILDER_H
#define SWIFT_FRONTEND_MODULEINTERFACEBUILDER_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Frontend/Frontend.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/Serialization/SerializationOptions.h"
#include "llvm/Support/StringSaver.h"

namespace llvm {
namespace vfs {
class FileSystem;
}
}

namespace swift {

class DiagnosticEngine;
class LangOptions;
class SearchPathOptions;
class DependencyTracker;

class ModuleInterfaceBuilder {
  SourceManager &sourceMgr;
  DiagnosticEngine &diags;
  InterfaceSubContextDelegate &subASTDelegate;
  const StringRef interfacePath;
  const StringRef moduleName;
  const StringRef moduleCachePath;
  const StringRef prebuiltCachePath;
  const bool disableInterfaceFileLock;
  const SourceLoc diagnosticLoc;
  DependencyTracker *const dependencyTracker;
  SmallVector<StringRef, 3> extraDependencies;

public:
  /// Emit a diagnostic tied to this declaration.
  template<typename ...ArgTypes>
  static InFlightDiagnostic diagnose(
      DiagnosticEngine &Diags,
      SourceManager &SM,
      StringRef InterfacePath,
      SourceLoc Loc,
      Diag<ArgTypes...> ID,
      typename detail::PassArgument<ArgTypes>::type... Args) {
    if (Loc.isInvalid()) {
      // Diagnose this inside the interface file, if possible.
      Loc = SM.getLocFromExternalSource(InterfacePath, 1, 1);
    }
    return Diags.diagnose(Loc, ID, std::move(Args)...);
  }

private:
  /// Emit a diagnostic tied to this declaration.
  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(
      Diag<ArgTypes...> ID,
      typename detail::PassArgument<ArgTypes>::type... Args) const {
    return diagnose(diags, sourceMgr, interfacePath, diagnosticLoc,
                    ID, std::move(Args)...);
  }

  /// Populate the provided \p Deps with \c FileDependency entries for all
  /// dependencies \p SubInstance's DependencyTracker recorded while compiling
  /// the module, excepting .swiftmodules in \p moduleCachePath or
  /// \p prebuiltCachePath. Those have _their_ dependencies added instead, both
  /// to avoid having to do recursive scanning when rechecking this dependency
  /// in future and to make the module caches relocatable.
  bool collectDepsForSerialization(
      CompilerInstance &SubInstance,
      SmallVectorImpl<SerializationOptions::FileDependency> &Deps,
      bool IsHashBased);

  bool buildSwiftModuleInternal(StringRef OutPath, bool ShouldSerializeDeps,
                                std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
                                ArrayRef<std::string> CandidateModules);
public:
  ModuleInterfaceBuilder(SourceManager &sourceMgr, DiagnosticEngine &diags,
                            InterfaceSubContextDelegate &subASTDelegate,
                            StringRef interfacePath,
                            StringRef moduleName,
                            StringRef moduleCachePath,
                            StringRef prebuiltCachePath,
                            bool disableInterfaceFileLock = false,
                            SourceLoc diagnosticLoc = SourceLoc(),
                            DependencyTracker *tracker = nullptr)
    : sourceMgr(sourceMgr), diags(diags),
      subASTDelegate(subASTDelegate),
      interfacePath(interfacePath), moduleName(moduleName),
      moduleCachePath(moduleCachePath), prebuiltCachePath(prebuiltCachePath),
      disableInterfaceFileLock(disableInterfaceFileLock),
      diagnosticLoc(diagnosticLoc), dependencyTracker(tracker) {}

  /// Ensures the requested file name is added as a dependency of the resulting
  /// module.
  void addExtraDependency(StringRef path) {
    extraDependencies.push_back(path);
  }

  bool buildSwiftModule(StringRef OutPath, bool ShouldSerializeDeps,
                        std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
                        llvm::function_ref<void()> RemarkRebuild = nullptr,
                        ArrayRef<std::string> CandidateModules = {});
};

} // end namespace swift

#endif // defined(SWIFT_FRONTEND_MODULEINTERFACEBUILDER_H)
