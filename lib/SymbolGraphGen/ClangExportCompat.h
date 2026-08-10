//===--- ClangExportCompat.h - clang::Module::ExportDecl Accessors --------===//
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
// Representation-agnostic accessors for the elements of
// clang::Module::Exports.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYMBOLGRAPHGEN_CLANGEXPORTCOMPAT_H
#define SWIFT_SYMBOLGRAPHGEN_CLANGEXPORTCOMPAT_H

#include "clang/Basic/Module.h"
#include "llvm/ADT/PointerIntPair.h"

#include <utility>

namespace swift {
namespace symbolgraphgen {

/// `clang::Module::ExportDecl` used to be a
/// `llvm::PointerIntPair<clang::Module *, 1, bool>`, and is a
/// `std::pair<clang::ModuleRef, bool>` as of
/// f68d44dce63f1a94e60410e8817e0eed70fc0578
/// (https://github.com/llvm/llvm-project/pull/194968), which introduced lazy
/// deserialization of submodules. These overloads accept either representation
/// so that this code builds against both; the `std::pair` ones are templated
/// because `clang::ModuleRef` cannot be named in the older representation.
/// They can be collapsed into direct member accesses once support for the
/// older representation is no longer needed.
/// @{

/// The exported module, or null for a wildcard export that is not scoped to a
/// submodule, e.g. `export *`.
inline clang::Module *
getExportedClangModule(const llvm::PointerIntPair<clang::Module *, 1, bool> &ED) {
  return ED.getPointer();
}

template <typename ModuleRefTy>
inline clang::Module *
getExportedClangModule(const std::pair<ModuleRefTy, bool> &ED) {
  // Note that in this representation, converting to `clang::Module *`
  // materializes the module on demand.
  return ED.first;
}

/// Whether this is a wildcard export, e.g. `export *` or `export Submodule.*`.
inline bool
isWildcardClangExport(const llvm::PointerIntPair<clang::Module *, 1, bool> &ED) {
  return ED.getInt();
}

template <typename ModuleRefTy>
inline bool isWildcardClangExport(const std::pair<ModuleRefTy, bool> &ED) {
  return ED.second;
}

/// @}

} // end namespace symbolgraphgen
} // end namespace swift

#endif // SWIFT_SYMBOLGRAPHGEN_CLANGEXPORTCOMPAT_H
