//===--- ASTContext.h - AST Context Object ----------------------*- C++ -*-===//
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
// This file defines the ASTContext interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTCONTEXT_H
#define SWIFT_AST_ASTCONTEXT_H

#include "llvm/Support/DataTypes.h"

namespace llvm {
  class BumpPtrAllocator;
  class SourceMgr;
  class StringRef;
  template <typename T> class FoldingSet;
  template <typename T> class ArrayRef;
}

namespace swift {
  class Type;
  class OneOfType;
  class TupleType;
  class FunctionType;
  class ArrayType;
  class Identifier;
  class TupleTypeElt;
  class NamedTypeDecl;

/// ASTContext - This object creates and owns the AST objects.
class ASTContext {
  ASTContext(const ASTContext&);           // DO NOT IMPLEMENT
  void operator=(const ASTContext&);       // DO NOT IMPLEMENT
  llvm::BumpPtrAllocator *Allocator;
  
  void *IdentifierTable; // llvm::StringMap<char>
  void *TupleTypes;      // llvm::FoldingSet<TupleType>
  void *FunctionTypes;   // DenseMap<std::pair<Type*, Type*>, FunctionType*>
  void *ArrayTypes;      // DenseMap<std::pair<Type*, uint64_t>, ArrayType*>
public:
  ASTContext(llvm::SourceMgr &SourceMgr);
  ~ASTContext();
  
  /// SourceMgr - The source manager object.
  llvm::SourceMgr &SourceMgr;

  /// Allocate - Allocate memory from the ASTContext bump pointer.
  void *Allocate(unsigned long Bytes, unsigned Alignment);

  /// getIdentifier - Return the uniqued and AST-Context-owned version of the
  /// specified string.
  Identifier getIdentifier(llvm::StringRef Str);
    
  //===--------------------------------------------------------------------===//
  // Type manipulation routines.
  //===--------------------------------------------------------------------===//
  
  /// getCanonicalType - Get the canonicalized version of a type, stripping off
  /// sugar like argument names and type aliases.
  Type *getCanonicalType(Type *T);
  
  // Builtin type and simple types that are used frequently.
  Type * const TheEmptyTupleType;  /// TheEmptyTupleType - This is "()"
  Type * const TheDependentType;   /// TheDependentType - Dependent on context.
  Type * const TheInt32Type;       /// TheInt32Type - 32-bit signed integer.

  /// getTupleType - Return the uniqued tuple type with the specified elements.
  TupleType *getTupleType(llvm::ArrayRef<TupleTypeElt> Fields);
  
  /// getFunctionType - Return a uniqued function type with the specified
  /// input and result.
  FunctionType *getFunctionType(Type *Input, Type *Result);

  /// getArrayType - Return a uniqued array type with the specified base type
  /// and the specified size.  Size=0 indicates an unspecified size array.
  ArrayType *getArrayType(Type *BaseType, uint64_t Size);                          
};
  
} // end namespace swift

#endif
