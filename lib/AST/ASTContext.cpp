//===--- ASTContext.cpp - ASTContext Implementation -----------------------===//
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
//  This file implements the ASTContext class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Type.h"
#include "llvm/Support/Allocator.h"
#include "llvm/ADT/DenseMap.h"
using namespace swift;

/// FunctionTypesMapTy - This is the actual type underlying 'FunctionTypes'.
typedef llvm::DenseMap<std::pair<Type*,Type*>, FunctionType*>FunctionTypesMapTy;

ASTContext::ASTContext(llvm::SourceMgr &sourcemgr)
  : Allocator(new llvm::BumpPtrAllocator()),
    SourceMgr(sourcemgr),
    VoidType(new (*this) BuiltinType(BuiltinVoidKind)),
    IntType(new (*this) BuiltinType(BuiltinIntKind)) {
      
  FunctionTypes = new FunctionTypesMapTy();
}

ASTContext::~ASTContext() {
  delete Allocator;
  delete (FunctionTypesMapTy*)FunctionTypes; FunctionTypes = 0;
}

void *ASTContext::Allocate(unsigned long Bytes, unsigned Alignment) {
  return Allocator->Allocate(Bytes, Alignment);
}




void TupleType::Profile(llvm::FoldingSetNodeID &ID, 
                        const TypeOrDecl *Fields, unsigned NumFields) {
  ID.AddInteger(NumFields);
  for (unsigned i = 0; i != NumFields; ++i)
    if (Type *Ty = Fields[i].dyn_cast<Type*>())
      ID.AddPointer(Ty);
    else
      ID.AddPointer(Fields[i].get<VarDecl*>());
}

/// getTupleType - Return the uniqued tuple type with the specified elements.
TupleType *ASTContext::getTupleType(const TupleType::TypeOrDecl *Fields,
                                    unsigned NumFields) {
  // Check to see if we've already seen this tuple before.
  llvm::FoldingSetNodeID ID;
  TupleType::Profile(ID, Fields, NumFields);
 
  // FIXME: This is completely bogus.  The VarDecl fields are not being unique'd
  // so they all get their own addresses.  This should unique all-type tuples
  // though.
  void *InsertPos = 0;
  if (TupleType *TT = TupleTypes.FindNodeOrInsertPos(ID, InsertPos))
    return TT;
  
  // Okay, we didn't find one.  Make a copy of the fields list into ASTContext
  // owned memory.
  TupleType::TypeOrDecl *FieldsCopy =
    (TupleType::TypeOrDecl *)Allocate(sizeof(*Fields)*NumFields, 8);
  memcpy(FieldsCopy, Fields, sizeof(*Fields)*NumFields);
  
  TupleType *New = new (*this) TupleType(FieldsCopy, NumFields);
  TupleTypes.InsertNode(New, InsertPos);
  return New;
}

/// getFunctionType - Return a uniqued function type with the specified
/// input and result.
FunctionType *ASTContext::getFunctionType(Type *Input, Type *Result) {
  FunctionType *&Entry =
    (*(FunctionTypesMapTy*)FunctionTypes)[std::make_pair(Input, Result)];
  if (Entry) return Entry;
  
  Entry = new (*this) FunctionType(Input, Result);
  return Entry;
}
