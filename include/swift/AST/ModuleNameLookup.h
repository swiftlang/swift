//===--- ModuleNameLookup.h - Name lookup within a module -------*- C++ -*-===//
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
// This file defines interfaces for performing top-level name lookup into a
// set of imported modules.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_MODULE_NAME_LOOKUP_H
#define SWIFT_AST_MODULE_NAME_LOOKUP_H

#include "llvm/ADT/SmallVector.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Module.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {
class ValueDecl;

namespace namelookup {

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
/// \param moduleScopeContext The top-level context from which the lookup is
///        being performed, for checking access. This must be either a
///        FileUnit or a Module.
/// \param extraImports Private imports to include in this search.
void lookupInModule(ModuleDecl *module, ModuleDecl::AccessPathTy accessPath,
                    DeclName name, SmallVectorImpl<ValueDecl *> &decls,
                    NLKind lookupKind, ResolutionKind resolutionKind,
                    const DeclContext *moduleScopeContext,
                    ArrayRef<ModuleDecl::ImportedModule> extraImports = {});

template <typename Fn>
void forAllVisibleModules(const DeclContext *DC, const Fn &fn) {
  DeclContext *moduleScope = DC->getModuleScopeContext();
  if (auto file = dyn_cast<FileUnit>(moduleScope))
    file->forAllVisibleModules(fn);
  else
    cast<ModuleDecl>(moduleScope)
        ->forAllVisibleModules(ModuleDecl::AccessPathTy(), fn);
}

/// Performs a qualified lookup into the given module and, if necessary, its
/// reexports, observing proper shadowing rules.
void
lookupVisibleDeclsInModule(ModuleDecl *M, ModuleDecl::AccessPathTy accessPath,
                           SmallVectorImpl<ValueDecl *> &decls,
                           NLKind lookupKind,
                           ResolutionKind resolutionKind,
                           const DeclContext *moduleScopeContext,
                           ArrayRef<ModuleDecl::ImportedModule> extraImports = {});

} // end namespace namelookup

} // end namespace swift

#endif
