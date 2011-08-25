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

#include "llvm/Support/DataTypes.h"
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
  /// This is essentially a customized PointerUnion where we want to
  /// use DeclContextKind as the discriminator.
  enum : uintptr_t { KindMask = 0x3 };
  uintptr_t ParentAndKind;

public:
  DeclContext(DeclContextKind Kind, DeclContext *Parent)
    : ParentAndKind(reinterpret_cast<uintptr_t>(Parent) |
                    static_cast<uintptr_t>(Kind)) {
    assert(!(reinterpret_cast<uintptr_t>(Parent) & KindMask));
    assert(Parent || Kind == DeclContextKind::TranslationUnitDecl);
  }

  DeclContextKind getContextKind() const {
    return static_cast<DeclContextKind>(ParentAndKind & KindMask);
  }

  DeclContext *getParent() const {
    return reinterpret_cast<DeclContext*>(ParentAndKind & ~KindMask);
  }
};
  
} // end namespace swift

#endif
