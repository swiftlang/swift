//===--- IfConfigClause.h ---------------------------------------*- C++ -*-===//
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
// This file defines the IfConfigClause.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_IFCONFIGCLAUSE_H
#define SWIFT_AST_IFCONFIGCLAUSE_H

#include "llvm/ADT/ArrayRef.h"

namespace swift {
  class Expr;
  class SourceLoc;

/// This represents one part of a #if block.  If the condition field is
/// non-null, then this represents a #if or a #elseif, otherwise it represents
/// an #else block.
template <typename ElemTy>
struct IfConfigClause {
  /// The location of the #if, #elseif, or #else keyword.
  SourceLoc Loc;
  
  /// The condition guarding this #if or #elseif block.  If this is null, this
  /// is a #else clause.
  Expr *Cond;
  
  /// Elements inside the clause
  ArrayRef<ElemTy> Elements;

  /// True if this is the active clause of the #if block.
  bool isActive;

  IfConfigClause<ElemTy>(SourceLoc Loc, Expr *Cond,
                         ArrayRef<ElemTy> Elements, bool isActive)
    : Loc(Loc), Cond(Cond), Elements(Elements), isActive(isActive) {
  }
};

} // end namespace swift

#endif
