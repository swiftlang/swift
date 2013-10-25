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

#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/LLVM.h"

namespace swift {
namespace namelookup {

/// Performs a lookup into the given module and, if necessary, its
/// reexports, observing proper shadowing rules.
///
/// \param module The module that will contain the name.
/// \param accessPath The import scope on \p module.
/// \param name The name to look up.
/// \param[out] decls Any found decls will be added to this vector.
/// \param lookupKind Whether this lookup is qualified or unqualified.
/// \param resolutionKind What sort of decl is expected.
/// \param typeResolver The type resolver for decls that need to be
///        type-checked. This is needed for shadowing resolution.
/// \param extraImports Private imports to include in this search.
void lookupInModule(Module *module, Module::AccessPathTy accessPath,
                    Identifier name, SmallVectorImpl<ValueDecl *> &decls,
                    NLKind lookupKind, ResolutionKind resolutionKind,
                    LazyResolver *typeResolver,
                    ArrayRef<Module::ImportedModule> extraImports = {});

} // end namespace namelookup
} // end namespace swift

#endif
