//===--- ModuleNameLookup.cpp - Name lookup within a module ----*- c++ -*--===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_MODULENAMELOOKUP_H
#define SWIFT_AST_MODULENAMELOOKUP_H

#include "swift/Basic/SourceManager.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"

namespace swift {
namespace namelookup {

/// Controls the behavior of lookupInModule().
enum class ResolutionKind {
  /// Lookup can match any number of decls, as long as they are all
  /// overloadable.
  ///
  /// If non-overloadable decls are returned, this indicates ambiguous lookup.
  Overloadable,

  /// Lookup should match a single decl.
  Exact,

  /// Lookup should match a single decl that declares a type.
  TypesOnly
};

/// Performs a lookup into the given module and, if necessary, its
/// reexports, observing proper shadowing rules.
///
/// \param module The module that will contain the name.
/// \param accessPath The import scope on \p module.
/// \param name The name to look up.
/// \param[out] decls Any found decls will be added to this vector.
/// \param lookupKind Whether this lookup is qualified or unqualified.
/// \param resolutionKind What sort of decl is expected.
/// \param topLevel If \p module should be treated as a top-level source file,
///        e.g. its private imports should be included in the search.
void lookupInModule(Module *module, Module::AccessPathTy accessPath,
                    Identifier name, SmallVectorImpl<ValueDecl *> &decls,
                    NLKind lookupKind, ResolutionKind resolutionKind,
                    bool topLevel);

/// Performs a qualified lookup into the given module and, if necessary, its
/// reexports, observing proper shadowing rules.
void lookupVisibleDeclsInModule(Module *module, Module::AccessPathTy accessPath,
                                SmallVectorImpl<ValueDecl *> &decls,
                                NLKind lookupKind,
                                ResolutionKind resolutionKind);

} // end namespace namelookup
} // end namespace swift

#endif
