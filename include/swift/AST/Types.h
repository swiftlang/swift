//===--- Types.h - Swift Language Type ASTs ---------------------*- C++ -*-===//
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
// This file defines the TypeBase class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPES_H
#define SWIFT_TYPES_H

#include "swift/AST/Type.h"
#include "swift/AST/Identifier.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/SMLoc.h"

namespace llvm {
  class raw_ostream;
}
namespace swift {
  class ASTContext;
  class Expr;
  class Identifier;
  class TypeAliasDecl;
  class OneOfElementDecl;
  
  enum TypeKind {
    BuiltinInt1Kind,
    BuiltinInt8Kind,
    BuiltinInt16Kind,
    BuiltinInt32Kind,
    BuiltinInt64Kind,
    UnresolvedTypeKind,
    DependentTypeKind,
    NameAliasTypeKind,
    TupleTypeKind,
    OneOfTypeKind,
    FunctionTypeKind,
    ArrayTypeKind,
    
    Builtin_First = BuiltinInt1Kind,
    Builtin_Last = BuiltinInt64Kind
  };
  
/// TypeBase - Base class for all types in Swift.
class TypeBase {
  friend class ASTContext;
  TypeBase(const TypeBase&);                 // DO NOT IMPLEMENT
  void operator=(const TypeBase&);           // DO NOT IMPLEMENT
  
  /// CanonicalType - This field is always set to 'this' for canonical types,
  /// and is otherwise lazily populated by ASTContext when the canonical form of
  /// a non-canonical type is requested.
  TypeBase *CanonicalType;
protected:
  TypeBase(TypeKind kind, TypeBase *CanType = 0)
    : CanonicalType(CanType), Kind(kind) {}
public:
  /// Kind - The discriminator that indicates what subclass of type this is.
  const TypeKind Kind;

  /// isCanonical - Return true if this is a canonical type.
  bool isCanonical() const { return CanonicalType == this; }
  
  /// hasCanonicalTypeComputed - Return true if we've already computed a
  /// canonical version of this type.
  bool hasCanonicalTypeComputed() const { return CanonicalType != 0; }
  
  /// getCanonicalType - Return the canonical version of this type, which has
  /// sugar from all levels stripped off.
  TypeBase *getCanonicalType(ASTContext &Ctx);
  
  /// isEqual - Return true if these two types are equal, ignoring sugar.
  bool isEqual(Type Other, ASTContext &Ctx);
  
  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();

  /// If this type is a (potentially sugared) type of the specified kind, remove
  /// the minimal amount of sugar required to get a pointer to the type.
  template <typename T>
  T *getAs() {
    return dyn_cast<T>(getDesugaredType());
  }

  template <typename T>
  bool is() {
    return isa<T>(getDesugaredType());
  }
  
  /// getString - Return the name of the type as a string, for use in
  /// diagnostics only.
  std::string getString() const;
  
  void dump() const;
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *) { return true; }

private:
  // Make placement new and vanilla new/delete illegal for Types.
  void *operator new(size_t Bytes) throw();  // DO NOT IMPLEMENT.
  void operator delete(void *Data) throw();  // DO NOT IMPLEMENT.
  void *operator new(size_t Bytes, void *Mem) throw();  // DO NOT IMPLEMENT.
public:
  // Only allow allocation of Types using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8) throw();  
};

/// BuiltinType - Trivial builtin types.
class BuiltinType : public TypeBase {
  friend class ASTContext;
  // Builtin types are always canonical.
  BuiltinType(TypeKind kind) : TypeBase(kind, this) {}
public:
  
  void print(raw_ostream &OS) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const BuiltinType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->Kind >= Builtin_First && T->Kind <= Builtin_Last;
  }
};
  
/// UnresolvedType - This is the type that represents a use of a type that
/// wasn't forward declared.  This type exists during the parsing phase before
/// name binding, and can only be the underlying type of a TypeAliasDecl.
class UnresolvedType : public TypeBase {
  friend class ASTContext;
  // The Unresolved type is always canonical.
  UnresolvedType() : TypeBase(UnresolvedTypeKind, this) {}
public:
  
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->Kind == UnresolvedTypeKind;
  }
};

  
/// DependentType - This is a expression type whose actual kind is specified by
/// context which hasn't been provided yet.
class DependentType : public TypeBase {
  friend class ASTContext;
  // The Dependent type is always canonical.
  DependentType() : TypeBase(DependentTypeKind, this) {}
public:
  
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const DependentType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->Kind == DependentTypeKind;
  }
};

/// NameAliasType - An alias type is a name for another type, just like a
/// typedef in C.
class NameAliasType : public TypeBase {
  friend class TypeAliasDecl;
  // NameAliasType are never canonical.
  NameAliasType(TypeAliasDecl *d) : TypeBase(NameAliasTypeKind), TheDecl(d) {}
public:
  TypeAliasDecl *const TheDecl;
   
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const NameAliasType *) { return true; }
  static bool classof(const TypeBase *T) {
    return T->Kind == NameAliasTypeKind;
  }
};

/// TupleTypeElt - This represents a single element of a tuple.
class TupleTypeElt {
public:
  /// Name - An optional name for the field.
  Identifier Name;

  /// Ty - This is the type of the field, which is mandatory.
  Type Ty;

  /// Init - This is a default value for the tuple element, used if an explicit
  /// value is not specified.
  Expr *Init;
  
  TupleTypeElt(Type ty = Type(), Identifier name = Identifier(), Expr *init = 0)
    : Name(name), Ty(ty), Init(init) { }
};
  
/// TupleType - A tuple is a parenthesized list of types where each name has an
/// optional name.
///
class TupleType : public TypeBase, public llvm::FoldingSetNode {
public:
  const ArrayRef<TupleTypeElt> Fields;
  
  /// getTupleType - Return the uniqued tuple type with the specified elements.
  static TupleType *get(ArrayRef<TupleTypeElt> Fields, ASTContext &C);
  
  /// getElementType - Return the type of the specified field.
  Type getElementType(unsigned FieldNo) const {
    return Fields[FieldNo].Ty;
  }
  
  /// getNamedElementId - If this tuple has a field with the specified name,
  /// return the field index, otherwise return -1.
  int getNamedElementId(Identifier I) const;
  
  /// hasAnyDefaultValues - Return true if any of our elements has a default
  /// value.
  bool hasAnyDefaultValues() const;
  
  /// getFieldForScalarInit - If a tuple of this type can be initialized with a
  /// scalar, return the field number that the scalar is assigned to.  If not,
  /// return -1.
  int getFieldForScalarInit() const;
  
  
  /// updateInitializedElementType - This methods updates the element type and
  /// initializer for a non-canonical TupleType that has an initializer for the
  /// specified element.  This should only be used by TypeChecker.
  void updateInitializedElementType(unsigned EltNo, Type NewTy, Expr *NewInit);
  
  void print(raw_ostream &OS) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleType *) { return true; }
  static bool classof(const TypeBase *T) { return T->Kind == TupleTypeKind;}

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, Fields);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, 
                      ArrayRef<TupleTypeElt> Fields);
  
private:
  TupleType(ArrayRef<TupleTypeElt> fields, bool isCanonical)
    : TypeBase(TupleTypeKind, isCanonical ? this : 0), Fields(fields) {}
};
  
/// OneOfType - a 'oneof' type.  This represents the oneof type itself, not its
/// elements (which are OneOfElementDecl's).
class OneOfType : public TypeBase {
public:
  const SMLoc OneOfLoc;
  const ArrayRef<OneOfElementDecl*> Elements;
  
  SMLoc getLocStart() const { return OneOfLoc; }
  OneOfElementDecl *getElement(unsigned i) const {
    assert(i < Elements.size() && "Invalid index");
    return Elements[i];
  }
  
  OneOfElementDecl *getElement(Identifier Name) const;
  
  /// hasSingleElement - Return true if this is a single element oneof that has
  /// an argument type.  These are typically (but not necessarily) made with
  /// 'struct'.  Since it is unambiguous which slice is being referenced,
  /// various syntactic forms are allowed for these, like direct "foo.x" syntax.
  bool hasSingleElement() const;
  
  void print(raw_ostream &O) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const OneOfType *D) { return true; }
  static bool classof(const TypeBase *T) { return T->Kind == OneOfTypeKind; }
  
private:
  // oneof types are always canonical.
  OneOfType(SMLoc oneofloc, ArrayRef<OneOfElementDecl*> Elts)
    : TypeBase(OneOfTypeKind, this), OneOfLoc(oneofloc), Elements(Elts) {
  }
  friend class ASTContext;
};

  
/// FunctionType - A function type has a single input and result, e.g.
/// "int -> int" or (var a : int, var b : int) -> (int, int).
class FunctionType : public TypeBase {
public:
  const Type Input;
  const Type Result;
  
  /// 'Constructor' Factory Function
  static FunctionType *get(Type Input, Type Result, ASTContext &C);
  
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const FunctionType *) { return true; }
  static bool classof(const TypeBase *T) { return T->Kind == FunctionTypeKind;}
  
private:
  FunctionType(Type input, Type result);
};
  
  
/// ArrayType - An array type has a base type and either an unspecified or a
/// constant size.  For example "int[]" and "int[4]".  Array types cannot have
/// size = 0.
class ArrayType : public TypeBase {
public:
  const Type Base;
  
  /// Size - When this is zero it indicates an unsized array like "int[]".
  uint64_t Size;
  
  /// 'Constructor' Factory Function.
  /// Size=0 indicates an unspecified size array.
  static ArrayType *get(Type BaseType, uint64_t Size, ASTContext &C);
  
  void print(raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ArrayType *) { return true; }
  static bool classof(const TypeBase *T) { return T->Kind == ArrayTypeKind; }
  
private:
  ArrayType(Type base, uint64_t size);
};

} // end namespace swift

#endif
