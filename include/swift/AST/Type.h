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

#include <cstddef>

namespace swift {
  class ASTContext;
  
  enum TypeKind {
    // BuiltinDependentKind,
    BuiltinVoidKind,
    BuiltinIntKind
  };
  
/// Type - Base class for all types in swift.
class Type {
  Type(const Type&);                 // DO NOT IMPLEMENT
  void operator=(const Type&);       // DO NOT IMPLEMENT
  TypeKind Kind;
protected:
  Type(TypeKind kind) : Kind(kind) {}
public:
  TypeKind getKind() const { return Kind; }
  
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
  BuiltinType(TypeKind K) : Type(K) {}
public:
  
  
};
  
} // end namespace swift

#endif
