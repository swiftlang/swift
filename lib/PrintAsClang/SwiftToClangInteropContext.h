//===--- SwiftToClangInteropContext.h - Interop context ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PRINTASCLANG_SWIFTTOCLANGINTEROPCONTEXT_H
#define SWIFT_PRINTASCLANG_SWIFTTOCLANGINTEROPCONTEXT_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringSet.h"
#include <memory>

namespace swift {

class Decl;
class IRABIDetailsProvider;
class IRGenOptions;
class ModuleDecl;
class ExtensionDecl;
class NominalTypeDecl;

/// The \c SwiftToClangInteropContext class is responsible for providing
/// access to the other required subsystems of the compiler during the emission
/// of a clang header. It provides access to the other subsystems lazily to
/// ensure avoid any additional setup cost that's not required.
class SwiftToClangInteropContext {
public:
  SwiftToClangInteropContext(ModuleDecl &mod, const IRGenOptions &irGenOpts);
  ~SwiftToClangInteropContext();

  IRABIDetailsProvider &getIrABIDetails();

  // Runs the given function if we haven't emitted some context-specific stub
  // for the given concrete stub name.
  void runIfStubForDeclNotEmitted(llvm::StringRef stubName,
                                  llvm::function_ref<void(void)> function);

  /// Records that the given nominal type decl that has a clang declaration was
  /// emitted in the generated header.
  void recordEmittedClangTypeDecl(const NominalTypeDecl *typeDecl);

  inline const llvm::SetVector<const NominalTypeDecl *> &
  getEmittedClangTypeDecls() const {
    return referencedClangTypeDecls;
  }

  void recordExtensions(const NominalTypeDecl *typeDecl,
                        const ExtensionDecl *ext);

  llvm::ArrayRef<const ExtensionDecl *>
  getExtensionsForNominalType(const NominalTypeDecl *typeDecl) const;

private:
  ModuleDecl &mod;
  const IRGenOptions &irGenOpts;
  std::unique_ptr<IRABIDetailsProvider> irABIDetails;
  llvm::StringSet<> emittedStubs;
  llvm::SetVector<const NominalTypeDecl *> referencedClangTypeDecls;
  llvm::DenseMap<const NominalTypeDecl *, std::vector<const ExtensionDecl *>>
      extensions;
};

} // end namespace swift

#endif
