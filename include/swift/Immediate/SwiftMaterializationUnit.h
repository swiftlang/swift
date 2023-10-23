//===--- SwiftMaterializationUnit.h - JIT Swift ASTs ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines the `SwiftMaterializationUnit` class, which allows you to JIT
// individual Swift AST declarations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IMMEDIATE_SWIFTMATERIALIZATIONUNIT_H
#define SWIFT_IMMEDIATE_SWIFTMATERIALIZATIONUNIT_H

#include <memory>

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/JITLink/JITLink.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/EPCIndirectionUtils.h"
#include "llvm/ExecutionEngine/Orc/IndirectionUtils.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/LazyReexports.h"
#include "llvm/ExecutionEngine/Orc/SymbolStringPool.h"

#include "swift/AST/TBDGenRequests.h"

namespace swift {

class CompilerInstance;

/// A JIT stack able to lazily JIT Swift programs
class SwiftJIT {
public:
  SwiftJIT(const SwiftJIT &) = delete;
  SwiftJIT(SwiftJIT &&) = delete;
  SwiftJIT &operator=(const SwiftJIT &) = delete;
  SwiftJIT &operator=(SwiftJIT &&) = delete;

  /// Attempt to create and initialize a new `SwiftJIT` with lazy compilation
  /// enabled and an attached generator to search for symbols defined in the
  /// current process.
  static llvm::Expected<std::unique_ptr<SwiftJIT>> Create(CompilerInstance &CI);

  /// Adds a plugin that will rename function symbols for lazy reexports.
  /// Should be called only once.
  void addRenamer();

  ~SwiftJIT();

  /// Get the dylib associated with the main program
  llvm::orc::JITDylib &getMainJITDylib();

  /// Register a the materialization unit `MU` with the `JITDylib``JD` and
  /// create lazy reexports for all functions defined in the interface of `MU`
  llvm::Error addSwift(llvm::orc::JITDylib &JD,
                       std::unique_ptr<llvm::orc::MaterializationUnit> MU);

  /// Return a linker-mangled version of `Name`
  std::string mangle(llvm::StringRef Name);

  /// Add a symbol name to the underlying `SymbolStringPool` and return
  /// a pointer to it
  llvm::orc::SymbolStringPtr intern(llvm::StringRef Name);

  /// Return a linker-mangled version of `Name` and intern the result
  llvm::orc::SymbolStringPtr mangleAndIntern(llvm::StringRef Name);

  /// Get the `IRCompileLayer` associated with this `SwiftJIT`
  llvm::orc::IRCompileLayer &getIRCompileLayer();

  /// Get the `ObjectTransformLayer` associated with this `SwiftJIT`
  llvm::orc::ObjectTransformLayer &getObjTransformLayer();

  /// Initialize the main `JITDylib`, lookup the main symbol, execute it,
  /// deinitialize the main `JITDylib`, and return the exit code of the
  /// JIT'd program
  llvm::Expected<int> runMain(llvm::ArrayRef<std::string> Args);

private:
  static llvm::Expected<std::unique_ptr<llvm::orc::LLJIT>>
  CreateLLJIT(CompilerInstance &CI);

  /// An ORC layer to rename the names of function bodies to support lazy
  /// reexports
  class Plugin : public llvm::orc::ObjectLinkingLayer::Plugin {
    void
    modifyPassConfig(llvm::orc::MaterializationResponsibility &MR,
                     llvm::jitlink::LinkGraph &G,
                     llvm::jitlink::PassConfiguration &PassConfig) override;

    llvm::Error
    notifyFailed(llvm::orc::MaterializationResponsibility &MR) override;

    llvm::Error notifyRemovingResources(llvm::orc::JITDylib &JD,
                                        llvm::orc::ResourceKey K) override;

    void notifyTransferringResources(llvm::orc::JITDylib &JD,
                                     llvm::orc::ResourceKey DstKey,
                                     llvm::orc::ResourceKey SrcKey) override;
  };

  static void handleLazyCompilationFailure();

  SwiftJIT(std::unique_ptr<llvm::orc::LLJIT> J,
           std::unique_ptr<llvm::orc::EPCIndirectionUtils> EPCIU);

  std::unique_ptr<llvm::orc::LLJIT> J;
  std::unique_ptr<llvm::orc::EPCIndirectionUtils> EPCIU;
  llvm::orc::LazyCallThroughManager &LCTM;
  std::unique_ptr<llvm::orc::IndirectStubsManager> ISM;
};

/// Lazily JITs a Swift AST using function at a time compilation
class LazySwiftMaterializationUnit : public llvm::orc::MaterializationUnit {
public:
  /// Create a new `LazySwiftMaterializationUnit` with the associated
  /// JIT stack `JIT` and compiler instance `CI`
  static std::unique_ptr<LazySwiftMaterializationUnit>
  Create(SwiftJIT &JIT, CompilerInstance &CI);

  llvm::StringRef getName() const override;

private:
  LazySwiftMaterializationUnit(SwiftJIT &JIT, CompilerInstance &CI,
                               const SymbolSourceMap *Sources,
                               llvm::orc::SymbolFlagsMap Symbols);
  void materialize(
      std::unique_ptr<llvm::orc::MaterializationResponsibility> MR) override;

  void discard(const llvm::orc::JITDylib &JD,
               const llvm::orc::SymbolStringPtr &Sym) override;

  const SymbolSourceMap *Sources;
  SwiftJIT &JIT;
  CompilerInstance &CI;
};

/// Eagerly materializes a whole `SILModule`
class EagerSwiftMaterializationUnit : public llvm::orc::MaterializationUnit {
public:
  /// Create a new `EagerSwiftMaterializationUnit` with the JIT stack `JIT`
  /// and provided compiler options
  EagerSwiftMaterializationUnit(SwiftJIT &JIT, const CompilerInstance &CI,
                                const IRGenOptions &IRGenOpts,
                                std::unique_ptr<SILModule> SM);

  StringRef getName() const override;

private:
  void materialize(
      std::unique_ptr<llvm::orc::MaterializationResponsibility> MR) override;

  /// Get the linker-level interface defined by the `SILModule` being
  /// materialized
  static MaterializationUnit::Interface
  getInterface(SwiftJIT &JIT, const CompilerInstance &CI);

  void dumpJIT(const llvm::Module &Module);

  void discard(const llvm::orc::JITDylib &JD,
               const llvm::orc::SymbolStringPtr &Sym) override;

  SwiftJIT &JIT;
  const CompilerInstance &CI;
  const IRGenOptions &IRGenOpts;
  std::unique_ptr<SILModule> SM;
};

} // end namespace swift

#endif // SWIFT_IMMEDIATE_SWIFTMATERIALIZATIONUNIT_H
