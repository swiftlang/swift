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
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
#include "llvm/Support/Allocator.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
using namespace swift;

/// IdentifierTableMapTy - This is the type underlying IdentifierTable.
typedef llvm::StringMap<char, llvm::BumpPtrAllocator&> IdentifierTableMapTy;

/// TupleTypesMapTy - This is the actual type underlying ASTContext::TupleTypes.
typedef llvm::FoldingSet<TupleType> TupleTypesMapTy;

/// FunctionTypesMapTy - This is the actual type underlying 'FunctionTypes'.
typedef llvm::DenseMap<std::pair<Type*,Type*>, FunctionType*>FunctionTypesMapTy;

ASTContext::ASTContext(llvm::SourceMgr &sourcemgr)
  : Allocator(new llvm::BumpPtrAllocator()),
    IdentifierTable(new IdentifierTableMapTy(*Allocator)),
    TupleTypes(new TupleTypesMapTy()),
    FunctionTypes(new FunctionTypesMapTy()),
    SourceMgr(sourcemgr),
    VoidType(getTupleType(0, 0)), // void is aka "()"
    IntType(new (*this) BuiltinType(BuiltinIntKind)) {
}

ASTContext::~ASTContext() {
  delete (TupleTypesMapTy*)TupleTypes; TupleTypes = 0;
  delete (FunctionTypesMapTy*)FunctionTypes; FunctionTypes = 0;
  delete (IdentifierTableMapTy*)IdentifierTable;
  delete Allocator;
}

void *ASTContext::Allocate(unsigned long Bytes, unsigned Alignment) {
  return Allocator->Allocate(Bytes, Alignment);
}

/// getIdentifier - Return the uniqued and AST-Context-owned version of the
/// specified string.
Identifier ASTContext::getIdentifier(llvm::StringRef Str) {
  // Make sure null pointers stay null.
  if (Str.empty()) return Identifier(0);
  
  IdentifierTableMapTy &Table = *((IdentifierTableMapTy*)IdentifierTable);
  return Identifier(Table.GetOrCreateValue(Str).getKeyData());
}

//===----------------------------------------------------------------------===//
// Type manipulation routines.
//===----------------------------------------------------------------------===//

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
 
  
  TupleTypesMapTy &TupleTypesMap = *(TupleTypesMapTy*)TupleTypes;
  
  // FIXME: This is completely bogus.  The VarDecl fields are not being unique'd
  // so they all get their own addresses.  This should unique all-type tuples
  // though.
  void *InsertPos = 0;
  if (TupleType *TT = TupleTypesMap.FindNodeOrInsertPos(ID, InsertPos))
    return TT;
  
  // Okay, we didn't find one.  Make a copy of the fields list into ASTContext
  // owned memory.
  TupleType::TypeOrDecl *FieldsCopy =
    (TupleType::TypeOrDecl *)Allocate(sizeof(*Fields)*NumFields, 8);
  memcpy(FieldsCopy, Fields, sizeof(*Fields)*NumFields);
  
  TupleType *New = new (*this) TupleType(FieldsCopy, NumFields);
  TupleTypesMap.InsertNode(New, InsertPos);
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
