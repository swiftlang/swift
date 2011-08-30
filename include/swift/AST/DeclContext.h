//===--- DeclContext.h - Swift Language Context ASTs ------------*- C++ -*-===//
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
//
// This file defines the DeclContext class.  A DeclContext is the semantic
// construct that a declaration belongs to, such as the enclosing
// FuncExpr or declaration.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DECLCONTEXT_H
#define SWIFT_DECLCONTEXT_H

#include "llvm/ADT/PointerIntPair.h"
#include <cassert>

namespace swift {
  class FuncExpr;
  class TranslationUnitDecl;

enum class DeclContextKind {
  TranslationUnitDecl,
  FuncExpr,
  OneOfType
};

class DeclContext {
  llvm::PointerIntPair<DeclContext*, 2, unsigned> ParentAndKind;

public:
  DeclContext(DeclContextKind Kind, DeclContext *Parent)
    : ParentAndKind(Parent, static_cast<unsigned>(Kind)) {
    assert(Parent || Kind == DeclContextKind::TranslationUnitDecl);
  }

  DeclContextKind getContextKind() const {
    return DeclContextKind(ParentAndKind.getInt());
  }

  DeclContext *getParent() const {
    return ParentAndKind.getPointer();
  }
};
  
} // end namespace swift

#endif
