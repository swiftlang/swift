//===--- Type.h - Swift Language Type ASTs ----------------------*- C++ -*-===//
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
// This file defines the Type class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPE_H
#define SWIFT_TYPE_H

#include "swift/AST/Identifier.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/Casting.h"

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
    BuiltinInt32Kind,
    UnresolvedTypeKind,
    DependentTypeKind,
    NameAliasTypeKind,
    TupleTypeKind,
    OneOfTypeKind,
    FunctionTypeKind,
    ArrayTypeKind,
    
    Builtin_First = BuiltinInt32Kind,
    Builtin_Last = BuiltinInt32Kind
  };
  
/// Type - Base class for all types in Swift.
class Type {
  friend class ASTContext;
  Type(const Type&);                 // DO NOT IMPLEMENT
  void operator=(const Type&);       // DO NOT IMPLEMENT
  
  /// CanonicalType - This field is always set to 'this' for canonical types,
  /// and is otherwise lazily populated by ASTContext when the canonical form of
  /// a non-canonical type is requested.
  Type *CanonicalType;
protected:
  Type(TypeKind kind, Type *CanType = 0)
    : CanonicalType(CanType), Kind(kind) {}
public:
  /// Kind - The discriminator that indicates what subclass of type this is.
  const TypeKind Kind;

  /// isCanonical - Return true if this is a canonical type.
  bool isCanonical() const { return CanonicalType == this; }
    
  /// Return true if the outermost level of this type is a sugared type.
  Type *getDesugaredType();

  /// If this type is a (potentially sugared) type of the specified kind, remove
  /// the minimal amount of sugar required to get a pointer to the type.
  template <typename T>
  T *getAs() {
    return llvm::dyn_cast<T>(getDesugaredType());
  }

  void dump() const;
  void print(llvm::raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Type *) { return true; }

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
class BuiltinType : public Type {
  friend class ASTContext;
  // Builtin types are always canonical.
  BuiltinType(TypeKind kind) : Type(kind, this) {}
public:
  
  void print(llvm::raw_ostream &OS) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const BuiltinType *) { return true; }
  static bool classof(const Type *T) {
    return T->Kind >= Builtin_First && T->Kind <= Builtin_Last;
  }
};
  
/// UnresolvedType - This is the type that represents a use of a type that
/// wasn't forward declared.  This type exists during the parsing phase before
/// name binding, and can only be the underlying type of a TypeAliasDecl.
class UnresolvedType : public Type {
  friend class ASTContext;
  // The Unresolved type is always canonical.
  UnresolvedType() : Type(UnresolvedTypeKind, this) {}
public:
  
  void print(llvm::raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const UnresolvedType *) { return true; }
  static bool classof(const Type *T) {
    return T->Kind == UnresolvedTypeKind;
  }
};

  
/// DependentType - This is a expression type whose actual kind is specified by
/// context which hasn't been provided yet.
class DependentType : public Type {
  friend class ASTContext;
  // The Dependent type is always canonical.
  DependentType() : Type(DependentTypeKind, this) {}
public:
  
  void print(llvm::raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const DependentType *) { return true; }
  static bool classof(const Type *T) {
    return T->Kind == DependentTypeKind;
  }
};

/// NameAliasType - An alias type is a name for another type, just like a
/// typedef in C.
class NameAliasType : public Type {
  friend class TypeAliasDecl;
  // NameAliasType are never canonical.
  NameAliasType(TypeAliasDecl *d) : Type(NameAliasTypeKind), TheDecl(d) {}
public:
  TypeAliasDecl *const TheDecl;
   
  void print(llvm::raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const NameAliasType *) { return true; }
  static bool classof(const Type *T) {
    return T->Kind == NameAliasTypeKind;
  }
};

/// TupleTypeElt - This represents a single element of a tuple.
class TupleTypeElt {
public:
  /// Name - An optional name for the field.
  Identifier Name;

  /// Ty - This is the type of the field, which is mandatory.
  Type *Ty;

  /// Init - This is a default value for the tuple element, used if an explicit
  /// value is not specified.
  Expr *Init;
  
  TupleTypeElt(Type *ty = 0, Identifier name = Identifier(), Expr *init = 0)
    : Name(name), Ty(ty), Init(init) { }
};
  
/// TupleType - A tuple is a parenthesized list of types where each name has an
/// optional name.
///
class TupleType : public Type, public llvm::FoldingSetNode {
public:
  const llvm::ArrayRef<TupleTypeElt> Fields;
  
  /// getElementType - Return the type of the specified field.
  Type *getElementType(unsigned FieldNo) const {
    return Fields[FieldNo].Ty;
  }
  
  /// getNamedElementId - If this tuple has a field with the specified name,
  /// return the field index, otherwise return -1.
  int getNamedElementId(Identifier I) const;
  
  void print(llvm::raw_ostream &OS) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleType *) { return true; }
  static bool classof(const Type *T) { return T->Kind == TupleTypeKind;}

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, Fields);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, 
                      llvm::ArrayRef<TupleTypeElt> Fields);
  
private:
  TupleType(llvm::ArrayRef<TupleTypeElt> fields)
    : Type(TupleTypeKind), Fields(fields) {}
  friend class ASTContext;
};
  
/// OneOfType - a 'oneof' type.  This represents the oneof type itself, not its
/// elements (which are OneOfElementDecl's).
class OneOfType : public Type {
public:
  const llvm::SMLoc OneOfLoc;
  const llvm::ArrayRef<OneOfElementDecl*> Elements;
  
  llvm::SMLoc getLocStart() const { return OneOfLoc; }
  OneOfElementDecl *getElement(unsigned i) const {
    assert(i < Elements.size() && "Invalid index");
    return Elements[i];
  }
  
  OneOfElementDecl *getElement(Identifier Name) const;
  
  void print(llvm::raw_ostream &O) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const OneOfType *D) { return true; }
  static bool classof(const Type *T) { return T->Kind == OneOfTypeKind; }
  
private:
  // oneof types are always canonical.
  OneOfType(llvm::SMLoc oneofloc, llvm::ArrayRef<OneOfElementDecl*> Elts)
    : Type(OneOfTypeKind, this), OneOfLoc(oneofloc), Elements(Elts) {
  }
  friend class ASTContext;
};

  
/// FunctionType - A function type has a single input and result, e.g.
/// "int -> int" or (var a : int, var b : int) -> (int, int).
class FunctionType : public Type {
public:
  Type *const Input;
  Type *const Result;
  
  void print(llvm::raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const FunctionType *) { return true; }
  static bool classof(const Type *T) { return T->Kind == FunctionTypeKind;}
  
private:
  FunctionType(Type *input, Type *result)
    : Type(FunctionTypeKind), Input(input), Result(result) {}
  friend class ASTContext;
};
  
  
/// ArrayType - An array type has a base type and either an unspecified or a
/// constant size.  For example "int[]" and "int[4]".  Array types cannot have
/// size = 0.
class ArrayType : public Type {
public:
  Type *const Base;
  
  /// Size - When this is zero it indicates an unsized array like "int[]".
  uint64_t Size;
  
  void print(llvm::raw_ostream &OS) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const ArrayType *) { return true; }
  static bool classof(const Type *T) { return T->Kind == ArrayTypeKind; }
  
private:
  ArrayType(Type *base, uint64_t size)
    : Type(ArrayTypeKind), Base(base), Size(size) {}
  friend class ASTContext;
};

} // end namespace swift

namespace llvm {
  static inline llvm::raw_ostream &
  operator<<(llvm::raw_ostream &OS, const swift::Type &Ty) {
    Ty.print(OS);
    return OS;
  }
}  

#endif
