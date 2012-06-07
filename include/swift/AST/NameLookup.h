//===--- NameLookup.h - Swift Name Lookup Routines --------------*- C++ -*-===//
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
// This file defines interfaces for performing name lookup.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_NAME_LOOKUP_H
#define SWIFT_AST_NAME_LOOKUP_H

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {
  class ASTContext;
  class DeclContext;
  class Expr;
  class ValueDecl;
  class Type;
  class TypeDecl;
  class Module;
  class TupleType;

/// MemberLookupResult - One result of member name lookup.
struct MemberLookupResult {
  /// D - The decl found or tuple element referenced.
  union {
    ValueDecl *D;
    unsigned TupleFieldNo;
  };

  /// Kind - The kind of reference.
  enum KindTy {
    /// MemberProperty - "a.x" refers to an "x" which is an instance property
    /// of "a".
    MemberProperty,

    /// MemberFunction - "a.x" refers to an "x" which is an instance function
    /// of "a".  "A.x" refers to a curried function such that "A.x(a)" is
    /// equivalent to "x.a" if "A" is the metatype of the type of "a".
    MemberFunction,

    /// MetatypeMember - "A.x" refers to an "x" which is a member of the
    /// metatype "A". "a.x" is equivalent to "A.x", where "A' is the
    /// metatype of the type of "a"; the base is evaluated and ignored.
    MetatypeMember,

    /// TupleElement - "a.x" is a direct reference to a field of a tuple.
    TupleElement,
    
    /// ExistentialMember - "a.x" refers to a member of an existential type.
    ExistentialMember
  } Kind;
  
  static MemberLookupResult getMemberProperty(ValueDecl *D) {
    MemberLookupResult R;
    R.D = D;
    R.Kind = MemberProperty;
    return R;
  }
  static MemberLookupResult getMemberFunction(ValueDecl *D) {
    MemberLookupResult R;
    R.D = D;
    R.Kind = MemberFunction;
    return R;
  }
  static MemberLookupResult getMetatypeMember(ValueDecl *D) {
    MemberLookupResult R;
    R.D = D;
    R.Kind = MetatypeMember;
    return R;
  }
  static MemberLookupResult getTupleElement(unsigned Elt) {
    MemberLookupResult R;
    R.TupleFieldNo = Elt;
    R.Kind = TupleElement;
    return R;
  }
  static MemberLookupResult getExistentialMember(ValueDecl *D) {
    MemberLookupResult R;
    R.D = D;
    R.Kind = ExistentialMember;
    return R;
  }
  
  /// \brief Determine whether this result has a declaration.
  bool hasDecl() const { return Kind != TupleElement; }
};
  
/// MemberLookup - This class implements and represents the result of performing
/// "dot" style member lookup.
class MemberLookup {
  MemberLookup(const MemberLookup&) = delete;
  void operator=(const MemberLookup&) = delete;
public:
  /// MemberLookup ctor - Lookup a member 'Name' in 'BaseTy' within the context
  /// of a given module 'M'.  This operation corresponds to a standard "dot" 
  /// lookup operation like "a.b" where 'this' is the type of 'a'.  This
  /// operation is only valid after name binding.
  MemberLookup(Type BaseTy, Identifier Name, Module &M);

  /// Results - The constructor fills this vector in with all of the results.
  /// If name lookup failed, this is empty.
  llvm::SmallVector<MemberLookupResult, 4> Results;
  
  /// isSuccess - Return true if anything was found by the name lookup.
  bool isSuccess() const { return !Results.empty(); }
  
  /// createResultAST - Build an AST to represent this lookup, with the
  /// specified base expression.
  Expr *createResultAST(Expr *Base, SourceLoc DotLoc, SourceLoc NameLoc,
                        ASTContext &Context);
  
private:
  Identifier MemberName;
  typedef llvm::SmallPtrSet<TypeDecl *, 8> VisitedSet;
  void doIt(Type BaseTy, Module &M, VisitedSet &Visited);
};

/// ConstructorLookup - This class implements and represents the result of
/// looking up a constructor for a type.
class ConstructorLookup {
  ConstructorLookup(const ConstructorLookup&) = delete;
  void operator=(const ConstructorLookup&) = delete;
public:
  /// ConstructorLookup ctor - Lookup constructors for the given type in the
  /// given module.
  ConstructorLookup(Type BaseTy, Module &M);

  /// Results - The constructor fills this vector in with all of the results.
  /// If name lookup failed, this is empty.
  llvm::SmallVector<ValueDecl*, 4> Results;
  
  /// isSuccess - Return true if anything was found by the name lookup.
  bool isSuccess() const { return !Results.empty(); }
};

/// UnqualifiedLookupResult - One result of unqualified lookup.
struct UnqualifiedLookupResult {
private:
  ValueDecl *Base;
  union {
    ValueDecl *Value;
    Module *NamedModule;
  };

public:
  /// Kind - The kind of reference.
  enum KindTy {
    /// ModuleMember - "x" refers to a value declared at module scope.
    ModuleMember,

    /// LocalDecl - "x" refers to a value declared in a local scope.
    LocalDecl,

    /// MemberProperty - "x" refers to an instance property of a type which
    /// is a containing scope.
    MemberProperty,

    /// MemberFunction - "x" refers to an instance function of a type
    /// is a containing scope.  If the lookup is inside an instance function
    /// of the containing type, it refers to the instance function; otherwise,
    /// it refers to the curried function on the metatype.
    MemberFunction,

    /// MetatypeMember - "x" refers to a member of a metatype "A", which is a
    /// referred to by BaseDecl.
    MetatypeMember,
  
    /// ExistentialMember - "x" refers to a member of an existential type,
    /// which is referred to by BaseDecl.
    ExistentialMember,

    /// ModuleName - "x" refers to a module, either the current
    /// module or an imported module.
    ModuleName
  } Kind;

  bool hasValueDecl() {
    return Kind != ModuleName;
  }

  ValueDecl *getValueDecl() {
    assert(hasValueDecl());
    return Value;
  }

  Module *getNamedModule() {
    assert(Kind == ModuleName);
    return NamedModule;
  }

  ValueDecl *getBaseDecl() {
    return Base;
  }

  static UnqualifiedLookupResult getModuleMember(ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = nullptr;
    R.Value = value;
    R.Kind = ModuleMember;
    return R;
  }

  static UnqualifiedLookupResult getLocalDecl(ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = nullptr;
    R.Value = value;
    R.Kind = LocalDecl;
    return R;
  }

  static UnqualifiedLookupResult getMemberProperty(ValueDecl *base,
                                                   ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = base;
    R.Value = value;
    R.Kind = MemberProperty;
    return R;
  }

  static UnqualifiedLookupResult getMemberFunction(ValueDecl *base,
                                                   ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = base;
    R.Value = value;
    R.Kind = MemberFunction;
    return R;
  }

  static UnqualifiedLookupResult getMetatypeMember(ValueDecl *base,
                                                   ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = base;
    R.Value = value;
    R.Kind = MetatypeMember;
    return R;
  }

  static UnqualifiedLookupResult getExistentialMember(ValueDecl *base,
                                                      ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = base;
    R.Value = value;
    R.Kind = ExistentialMember;
    return R;
  }

  static UnqualifiedLookupResult getModuleName(Module *m) {
    UnqualifiedLookupResult R;
    R.Base = nullptr;
    R.NamedModule = m;
    R.Kind = ModuleName;
    return R;
  }
};

/// UnqualifiedLookup - This class implements and represents the result of
/// performing unqualified lookup (i.e. lookup for a plain identifier).
class UnqualifiedLookup {
  UnqualifiedLookup(const MemberLookup&) = delete;
  void operator=(const MemberLookup&) = delete;
public:
  /// UnqualifiedLookup ctor - Lookup an unqualfied identifier 'Name' in the
  /// context.  If the current DeclContext is nested in a FuncExpr, the
  /// SourceLoc is used to determine which declarations in that FuncExpr's
  /// context are visible.
  UnqualifiedLookup(Identifier Name, DeclContext *DC,
                    SourceLoc Loc = SourceLoc());

  llvm::SmallVector<UnqualifiedLookupResult, 4> Results;

  /// isSuccess - Return true if anything was found by the name lookup.
  bool isSuccess() const { return !Results.empty(); }

  /// getSingleTypeResult - Get the result as a single type, or
  /// a null type if that fails.
  TypeDecl* getSingleTypeResult();
};

} // end namespace swift

#endif
