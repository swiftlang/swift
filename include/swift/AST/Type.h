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

#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"

namespace llvm {
  class raw_ostream;
}
namespace swift {
  class ASTContext;
  class NamedDecl;
  
  enum TypeKind {
    // BuiltinDependentKind,
    BuiltinIntKind,
    TupleTypeKind,
    FunctionTypeKind,
    
    Builtin_First = BuiltinIntKind,
    Builtin_Last = BuiltinIntKind
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
  Type(TypeKind kind) : Kind(kind), CanonicalType(0) {}
  Type(TypeKind kind, Type *CanType) : Kind(kind), CanonicalType(CanType) {}
public:
  /// Kind - The discriminator that indicates what subclass of type this is.
  const TypeKind Kind;

  
  /// isCanonical - Return true if this is a canonical type.
  bool isCanonical() const { return CanonicalType == this; }
  
  
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
  BuiltinType(TypeKind K) : Type(K, this) {}
public:
  
  void print(llvm::raw_ostream &OS) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const BuiltinType *) { return true; }
  static bool classof(const Type *T) {
    return T->Kind >= Builtin_First && T->Kind <= Builtin_Last;
  }
};

/// TupleType - A tuple is a parenthesized list of types where each name has an
/// optional name.
///
/// FIXME: Do we want to allow default values??
/// 
class TupleType : public Type, public llvm::FoldingSetNode {
public:
  typedef llvm::PointerUnion<Type*, NamedDecl*> TypeOrDecl;
  const TypeOrDecl * const Fields;
  const unsigned NumFields;
  
  
  
  void print(llvm::raw_ostream &OS) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TupleType *) { return true; }
  static bool classof(const Type *T) { return T->Kind == TupleTypeKind;}

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, Fields, NumFields);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, 
                      const TypeOrDecl *Fields, unsigned NumFields);
  
private:
  TupleType(const TypeOrDecl *const fields, unsigned numfields)
    : Type(TupleTypeKind), Fields(fields), NumFields(numfields) {}
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
  
} // end namespace swift

namespace llvm {
  static inline llvm::raw_ostream &
  operator<<(llvm::raw_ostream &OS, const swift::Type &Ty) {
    Ty.print(OS);
    return OS;
  }
}  

#endif
