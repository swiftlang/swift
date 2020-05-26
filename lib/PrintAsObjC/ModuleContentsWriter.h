//===--- ModuleContentsWriter.h - Walk a module to print ObjC ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PRINTASOBJC_MODULECONTENTSWRITER_H
#define SWIFT_PRINTASOBJC_MODULECONTENTSWRITER_H

#include "swift/AST/AttrKind.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace clang {
  class Module;
}

namespace swift {
class ModuleDecl;

using ImportModuleTy = PointerUnion<ModuleDecl*, const clang::Module*>;

/// Prints the declarations of \p M to \p os, filtering by \p minRequiredAccess
/// and collecting imports in \p imports along the way.
void printModuleContentsAsObjC(raw_ostream &os,
                               llvm::SmallPtrSetImpl<ImportModuleTy> &imports,
                               ModuleDecl &M, AccessLevel minRequiredAccess);

} // end namespace swift

#endif

