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
#include "llvm/ADT/ArrayRef.h"
#include "swift/AST/Type.h"

namespace llvm {
  class BumpPtrAllocator;
  class SourceMgr;
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
  class OneOfElementDecl;

/// ASTContext - This object creates and owns the AST objects.
class ASTContext {
  ASTContext(const ASTContext&);           // DO NOT IMPLEMENT
  void operator=(const ASTContext&);       // DO NOT IMPLEMENT
  llvm::BumpPtrAllocator *Allocator;
  
  void *IdentifierTable; // llvm::StringMap<char>
  void *TupleTypes;      // llvm::FoldingSet<TupleType>
  void *FunctionTypes;   // DenseMap<std::pair<Type*, Type*>, FunctionType*>
  void *ArrayTypes;      // DenseMap<std::pair<Type*, uint64_t>, ArrayType*>
  
  bool HadError;
public:
  ASTContext(llvm::SourceMgr &SourceMgr);
  ~ASTContext();
  
  /// SourceMgr - The source manager object.
  llvm::SourceMgr &SourceMgr;

  /// Allocate - Allocate memory from the ASTContext bump pointer.
  void *Allocate(unsigned long Bytes, unsigned Alignment);

  template <typename T>
  T *Allocate(unsigned NElts) {
    T *Res = (T*)Allocate(sizeof(T)*NElts, __alignof__(T));
    for (unsigned i = 0; i != NElts; ++i)
      new (Res+i) T();
    return Res;
  }

  template <typename T, typename It>
  T *AllocateCopy(It Start, It End) {
    T *Res = (T*)Allocate(sizeof(T)*(End-Start), __alignof__(T));
    for (unsigned i = 0; Start != End; ++Start, ++i)
      new (Res+i) T(*Start);
    return Res;
  }

  template<typename T>
  llvm::ArrayRef<T> AllocateCopy(llvm::ArrayRef<T> Arr) {
    return llvm::ArrayRef<T>(AllocateCopy<T>(Arr.begin(), Arr.end()),
                             Arr.size());
  }

  template<typename T>
  llvm::ArrayRef<T> AllocateCopy(const llvm::SmallVectorImpl<T> &Vec) {
    return AllocateCopy(llvm::ArrayRef<T>(Vec));
  }

  
  /// getIdentifier - Return the uniqued and AST-Context-owned version of the
  /// specified string.
  Identifier getIdentifier(StringRef Str);
  
  /// setHadError - This is called when an error message is emitted.
  void setHadError() {
    HadError = true;
  }
  
  bool hadError() const {
    return HadError;
  }
  
  //===--------------------------------------------------------------------===//
  // Type manipulation routines.
  //===--------------------------------------------------------------------===//
  
  // Builtin type and simple types that are used frequently.
  const Type TheEmptyTupleType;  /// TheEmptyTupleType - This is "()"
  const Type TheUnresolvedType;  /// TheUnresolvedType - Not name bound.
  
  /// TheDependentType - Dependent on context.  This is given to an anonymous
  /// closure argument (e.g. $4) and to UnresolvedMemberExprs (e.g. :foo) during
  /// type checking until they are resolved to something with concrete type.
  const Type TheDependentType;
  const Type TheInt1Type;        /// TheInt1Type  - 1-bit integer.
  const Type TheInt8Type;        /// TheInt8Type  - 8-bit integer.
  const Type TheInt16Type;       /// TheInt16Type - 16-bit integer.
  const Type TheInt32Type;       /// TheInt32Type - 32-bit integer.
  const Type TheInt64Type;       /// TheInt64Type - 64-bit integer.

  /// getTupleType - Return the uniqued tuple type with the specified elements.
  TupleType *getTupleType(llvm::ArrayRef<TupleTypeElt> Fields);
  
  /// getFunctionType - Return a uniqued function type with the specified
  /// input and result.
  FunctionType *getFunctionType(Type Input, Type Result);

  /// getArrayType - Return a uniqued array type with the specified base type
  /// and the specified size.  Size=0 indicates an unspecified size array.
  ArrayType *getArrayType(Type BaseType, uint64_t Size);
  
  /// getNewOneOfType - Return a new instance of oneof type.  These are never
  /// uniqued each syntactic instance of them is semantically considered to be a
  /// different type.
  OneOfType *getNewOneOfType(SMLoc OneOfLoc,
                             llvm::ArrayRef<OneOfElementDecl*> Elements);
};
  
} // end namespace swift

#endif
