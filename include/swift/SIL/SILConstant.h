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
#include "llvm/Support/PrettyStackTrace.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {
  class ValueDecl;
  class CapturingExpr;
  class ASTContext;
  class ClassDecl;

/// SILConstant - A key for referencing an entity that can be the subject of a
/// SIL ConstantRefInst or the name of a SIL Function body. This can currently
/// be either a reference to a ValueDecl for functions, methods, constructors,
/// and other named entities, or a reference to a CapturingExpr (that is, a
/// FuncExpr or ClosureExpr) for an anonymous function. In addition to the AST
/// reference, there is also an identifier for distinguishing definitions with
/// multiple associated entry points, such as a curried function.
struct SILConstant {
  typedef llvm::PointerUnion<ValueDecl*, CapturingExpr*> Loc;
  
  /// Represents the "kind" of the SILConstant. For some Swift decls there
  /// are multiple SIL entry points, and the kind is used to distinguish them.
  enum class Kind : unsigned {
    /// Func - this constant references the FuncDecl or CapturingExpr in loc
    /// directly.
    Func,
    
    /// Getter - this constant references the getter for the ValueDecl in loc.
    Getter,
    /// Setter - this constant references the setter for the ValueDecl in loc.
    Setter,
    
    /// Allocator - this constant references the allocating constructor
    /// entry point of a class ConstructorDecl or the constructor of a value
    /// ConstructorDecl.
    Allocator,
    /// Initializer - this constant references the initializing constructor
    /// entry point of the class ConstructorDecl in loc.
    Initializer,
    
    /// Destructor = this constant references the destructor for the ClassDecl
    /// in loc.
    Destructor,
    
    /// GlobalAccessor - this constant references the lazy-initializing
    /// accessor for the global VarDecl in loc.
    GlobalAccessor,
    
    /// GlobalAddress - this constant references the physical address of the
    /// global VarDecl in loc.
    GlobalAddress
  };
  
  /// The ValueDecl or CapturingExpr represented by this SILConstant.
  Loc loc;
  /// The Kind of this SILConstant.
  Kind kind : 4;
  /// The uncurry level of this SILConstant.
  unsigned uncurryLevel : 16;
  
  /// A magic value for SILConstant constructors to ask for the natural uncurry
  /// level of the constant.
  enum : unsigned { ConstructAtNaturalUncurryLevel = ~0U };
  
  /// Produces a null SILConstant.
  SILConstant() : loc(), kind(Kind::Func), uncurryLevel(0) {}
  
  /// Produces a SILConstant of the given kind for the given decl.
  explicit SILConstant(ValueDecl *decl, Kind kind,
                       unsigned uncurryLevel = ConstructAtNaturalUncurryLevel);
  
  /// Produces the 'natural' SILConstant for the given ValueDecl or
  /// CapturingExpr:
  /// - If 'loc' is a func or closure, this returns a Func SILConstant.
  /// - If 'loc' is a getter or setter FuncDecl, this returns the Getter or
  ///   Setter SILConstant for the property VarDecl.
  /// - If 'loc' is a ConstructorDecl, this returns the Allocator SILConstant
  ///   for the constructor.
  /// - If 'loc' is a DestructorDecl, this returns the Destructor SILConstant
  ///   for the containing ClassDecl.
  /// - If 'loc' is a global VarDecl, this returns its GlobalAccessor
  ///   SILConstant.
  /// If the uncurry level is unspecified or specified as NaturalUncurryLevel,
  /// then the SILConstant for the natural uncurry level of the definition is
  /// used.
  explicit SILConstant(Loc loc,
                       unsigned uncurryLevel = ConstructAtNaturalUncurryLevel);
    
  bool isNull() const { return loc.isNull(); }
  
  bool hasDecl() const { return loc.is<ValueDecl*>(); }
  bool hasExpr() const { return loc.is<CapturingExpr*>(); }
  
  ValueDecl *getDecl() const { return loc.get<ValueDecl*>(); }
  CapturingExpr *getExpr() const { return loc.get<CapturingExpr*>(); }
  
  /// True if the SILConstant references a function.
  bool isFunc() const {
    return kind == Kind::Func;
  }
  /// True if the SILConstant references a property accessor.
  bool isProperty() const {
    return kind == Kind::Getter || kind == Kind::Setter;
  }
  /// True if the SILConstant references a constructor entry point.
  bool isConstructor() const {
    return kind == Kind::Allocator || kind == Kind::Initializer;
  }
  /// True if the SILConstant references a destructor.
  bool isDestructor() const {
    return kind == Kind::Destructor;
  }
  /// True if the SILConstant references a global variable accessor.
  bool isGlobal() const {
    return kind == Kind::GlobalAccessor;
  }
  
  bool operator==(SILConstant rhs) const {
    return loc.getOpaqueValue() == rhs.loc.getOpaqueValue()
      && kind == rhs.kind
      && uncurryLevel == rhs.uncurryLevel;
  }
  bool operator!=(SILConstant rhs) const {
    return loc.getOpaqueValue() != rhs.loc.getOpaqueValue()
      || kind != rhs.kind
      || uncurryLevel != rhs.uncurryLevel;
  }
  
  void print(llvm::raw_ostream &os) const;
  void dump() const;
  
  /// Produces a SILConstant from an opaque value.
  explicit SILConstant(void *opaqueLoc, Kind kind, unsigned uncurryLevel)
    : loc(Loc::getFromOpaqueValue(opaqueLoc)),
      kind(kind), uncurryLevel(uncurryLevel)
  {}
};

/// PrettyStackTraceSILConstant - Observe that we are processing a specific
/// SIL constant.
class PrettyStackTraceSILConstant : public llvm::PrettyStackTraceEntry {
  SILConstant C;
  const char *Action;
public:
  PrettyStackTraceSILConstant(const char *Action, SILConstant C)
    : C(C), Action(Action) {}
  virtual void print(llvm::raw_ostream &OS) const;
};
  
} // end swift namespace

namespace llvm {

// DenseMap key support for SILConstant.
template<> struct DenseMapInfo<swift::SILConstant> {
  using SILConstant = swift::SILConstant;
  using Kind = SILConstant::Kind;
  using Loc = SILConstant::Loc;
  using PointerInfo = DenseMapInfo<void*>;
  using UnsignedInfo = DenseMapInfo<unsigned>;

  static SILConstant getEmptyKey() {
    return SILConstant(PointerInfo::getEmptyKey(), Kind::Func, 0);
  }
  static swift::SILConstant getTombstoneKey() {
    return SILConstant(PointerInfo::getEmptyKey(), Kind::Func, 0);
  }
  static unsigned getHashValue(swift::SILConstant Val) {
    unsigned h1 = PointerInfo::getHashValue(Val.loc.getOpaqueValue());
    unsigned h2 = UnsignedInfo::getHashValue(unsigned(Val.kind));
    unsigned h3 = UnsignedInfo::getHashValue(Val.uncurryLevel);
    return h1 ^ (h2 << 4) ^ (h3 << 9);
  }
  static bool isEqual(swift::SILConstant const &LHS,
                      swift::SILConstant const &RHS) {
    return LHS == RHS;
  }
};

} // end llvm namespace

#endif
