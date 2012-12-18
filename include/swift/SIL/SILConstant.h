//===--- SILConstant.h - Defines the SILConstant struct ---------*- C++ -*-===//
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
// This file defines the SILConstant struct, which is used to identify a SIL
// global identifier that can be used as the operand of a ConstantRefInst
// instruction or that can have a SIL Function associated with it.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILCONSTANT_H
#define SWIFT_SIL_SILCONSTANT_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {
  class ValueDecl;
  class CapturingExpr;

/// SILConstant - A key for referencing an entity that can be the subject of a
/// SIL ConstantRefInst or the name of a SIL Function body. This can currently
/// be either a reference to a ValueDecl for functions, methods, constructors,
/// and other named entities, or a reference to a CapturingExpr (that is, a
/// FuncExpr or ClosureExpr) for an anonymous function. In addition to the AST
/// reference, there is also an identifier for distinguishing definitions with
/// multiple associated entry points, such as a curried function.
struct SILConstant {
  typedef llvm::PointerUnion<ValueDecl*, CapturingExpr*> Loc;
  Loc loc;
  unsigned id;
  
  enum : unsigned {
    /// Getter - this id references the getter for the associated decl.
    Getter = 1U << 31U,
    /// Setter - this id references the setter for the associated decl.
    Setter = 1U << 30U,
  };
  
  SILConstant() : loc(), id(0) {}
  explicit SILConstant(Loc loc, unsigned id)
    : loc(loc), id(id) {}
  
  SILConstant(Loc loc);
  
  bool hasDecl() const { return loc.is<ValueDecl*>(); }
  bool hasExpr() const { return loc.is<CapturingExpr*>(); }
  
  ValueDecl *getDecl() const { return loc.get<ValueDecl*>(); }
  CapturingExpr *getExpr() const { return loc.get<CapturingExpr*>(); }
  
  bool operator==(SILConstant rhs) const {
    return loc.getOpaqueValue() == rhs.loc.getOpaqueValue() && id == rhs.id;
  }
  bool operator!=(SILConstant rhs) const {
    return loc.getOpaqueValue() != rhs.loc.getOpaqueValue() || id != rhs.id;
  }
    
  void print(llvm::raw_ostream &os) const;
  void dump() const;
};

} // end swift namespace

namespace llvm {

// DenseMap key support for SILConstant.
template<> struct DenseMapInfo<swift::SILConstant> {
  static swift::SILConstant getEmptyKey() {
    void *xx = DenseMapInfo<void*>::getEmptyKey();
    return swift::SILConstant(swift::SILConstant::Loc::getFromOpaqueValue(xx),
                              DenseMapInfo<unsigned>::getEmptyKey());
  }
  static swift::SILConstant getTombstoneKey() {
    void *xx = DenseMapInfo<void*>::getTombstoneKey();
    return swift::SILConstant(swift::SILConstant::Loc::getFromOpaqueValue(xx),
                              DenseMapInfo<unsigned>::getTombstoneKey());
  }
  static unsigned getHashValue(swift::SILConstant Val) {
    unsigned h1 = DenseMapInfo<void*>::getHashValue(Val.loc.getOpaqueValue());
    unsigned h2 = DenseMapInfo<unsigned>::getHashValue(Val.id);
    return h1 ^ (h2 << 9);
  }
  static bool isEqual(swift::SILConstant const &LHS,
                      swift::SILConstant const &RHS) {
    return LHS == RHS;
  }
};

} // end llvm namespace

#endif
