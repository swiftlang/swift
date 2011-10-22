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
#include "swift/AST/AST.h"
#include "swift/Basic/DiagnosticEngine.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
using namespace swift;

/// IdentifierTableMapTy - This is the type underlying IdentifierTable.
typedef llvm::StringMap<char, llvm::BumpPtrAllocator&> IdentifierTableMapTy;

/// TupleTypesMapTy - This is the actual type underlying ASTContext::TupleTypes.
typedef llvm::FoldingSet<TupleType> TupleTypesMapTy;

/// FunctionTypesMapTy - This is the actual type underlying 'FunctionTypes'.
typedef llvm::DenseMap<std::pair<Type,Type>, FunctionType*> FunctionTypesMapTy;

/// ArrayTypesMapTy - This is the actual type underlying 'ArrayTypes'.
typedef llvm::DenseMap<std::pair<Type, uint64_t>, ArrayType*> ArrayTypesMapTy;

ASTContext::ASTContext(llvm::SourceMgr &sourcemgr, DiagnosticEngine &Diags)
  : Allocator(new llvm::BumpPtrAllocator()),
    IdentifierTable(new IdentifierTableMapTy(*Allocator)),
    TupleTypes(new TupleTypesMapTy()),
    FunctionTypes(new FunctionTypesMapTy()),
    ArrayTypes(new ArrayTypesMapTy()),
    SourceMgr(sourcemgr),
    Diags(Diags),
    BuiltinModule(new (*this) Module(getIdentifier("Builtin"), *this)),
    TheErrorType(new (*this) ErrorType(*this)),
    TheEmptyTupleType(TupleType::get(ArrayRef<TupleTypeElt>(), *this)),
    TheDependentType(new (*this) DependentType(*this)),
    TheFloat32Type(new (*this) BuiltinType(TypeKind::BuiltinFloat32, *this)),
    TheFloat64Type(new (*this) BuiltinType(TypeKind::BuiltinFloat64, *this)),
    TheInt1Type(new (*this) BuiltinType(TypeKind::BuiltinInt1, *this)),
    TheInt8Type(new (*this) BuiltinType(TypeKind::BuiltinInt8, *this)),
    TheInt16Type(new (*this) BuiltinType(TypeKind::BuiltinInt16, *this)),
    TheInt32Type(new (*this) BuiltinType(TypeKind::BuiltinInt32, *this)),
    TheInt64Type(new (*this) BuiltinType(TypeKind::BuiltinInt64, *this)) {
  HadError = false;
}

ASTContext::~ASTContext() {
  delete (TupleTypesMapTy*)TupleTypes; TupleTypes = 0;
  delete (FunctionTypesMapTy*)FunctionTypes; FunctionTypes = 0;
  delete (ArrayTypesMapTy*)ArrayTypes; ArrayTypes = 0;
  delete (IdentifierTableMapTy*)IdentifierTable; IdentifierTable = 0;
  delete Allocator; Allocator = 0;
}

void *ASTContext::Allocate(unsigned long Bytes, unsigned Alignment) {
  return Allocator->Allocate(Bytes, Alignment);
}

/// getIdentifier - Return the uniqued and AST-Context-owned version of the
/// specified string.
Identifier ASTContext::getIdentifier(StringRef Str) {
  // Make sure null pointers stay null.
  if (Str.empty()) return Identifier(0);
  
  IdentifierTableMapTy &Table = *((IdentifierTableMapTy*)IdentifierTable);
  return Identifier(Table.GetOrCreateValue(Str).getKeyData());
}

bool ASTContext::hadError() const {
  // FIXME: When this goes away, remove ASTContext::hadError!
  return HadError || Diags.hadAnyError();
}


//===----------------------------------------------------------------------===//
// Type manipulation routines.
//===----------------------------------------------------------------------===//

// Simple accessors.
Type ErrorType::get(ASTContext &C) { return C.TheErrorType; }
Type DependentType::get(ASTContext &C) { return C.TheDependentType; }
Type TupleType::getEmpty(ASTContext &C) { return C.TheEmptyTupleType; }

void TupleType::Profile(llvm::FoldingSetNodeID &ID,
                        ArrayRef<TupleTypeElt> Fields) {
  ID.AddInteger(Fields.size());
  for (const TupleTypeElt &Elt : Fields) {
    ID.AddPointer(Elt.Ty.getPointer());
    ID.AddPointer(Elt.Name.get());
    ID.AddPointer(Elt.Init);
  }
}

/// getTupleType - Return the uniqued tuple type with the specified elements.
TupleType *TupleType::get(ArrayRef<TupleTypeElt> Fields, ASTContext &C) {
  // Check to see if we've already seen this tuple before.
  llvm::FoldingSetNodeID ID;
  TupleType::Profile(ID, Fields);
  
  TupleTypesMapTy &TupleTypesMap = *(TupleTypesMapTy*)C.TupleTypes;
  
  // FIXME: This is pointless for types with named fields.  The ValueDecl fields
  // themselves are not unique'd so they all get their own addresses, which
  // means that we'll never get a hit here.  This should unique all-type tuples
  // though.  Likewise with default values.
  void *InsertPos = 0;
  if (TupleType *TT = TupleTypesMap.FindNodeOrInsertPos(ID, InsertPos))
    return TT;
  
  // Okay, we didn't find one.  Make a copy of the fields list into ASTContext
  // owned memory.
  TupleTypeElt *FieldsCopy =
    C.AllocateCopy<TupleTypeElt>(Fields.begin(), Fields.end());
  
  bool IsCanonical = true;   // All canonical elts means this is canonical.
  for (const TupleTypeElt &Elt : Fields)
    IsCanonical &= Elt.Ty.isNull() ? false : Elt.Ty->isCanonical();

  Fields = ArrayRef<TupleTypeElt>(FieldsCopy, Fields.size());
  
  TupleType *New = new (C) TupleType(Fields, IsCanonical ? &C : 0);
  TupleTypesMap.InsertNode(New, InsertPos);

  return New;
}

/// getNewOneOfType - Return a new instance of oneof type.  These are never
/// uniqued because the loc is generally different.
OneOfType *OneOfType::getNew(SourceLoc OneOfLoc,
                             ArrayRef<OneOfElementDecl*> InElts,
                             DeclContext *Parent) {
  ASTContext &C = Parent->getASTContext();
  
  return new (C) OneOfType(OneOfLoc, C.AllocateCopy(InElts), Parent);
}


/// FunctionType::get - Return a uniqued function type with the specified
/// input and result.
FunctionType *FunctionType::get(Type Input, Type Result, ASTContext &C) {
  FunctionType *&Entry =
    (*(FunctionTypesMapTy*)C.FunctionTypes)[std::make_pair(Input, Result)];
  if (Entry) return Entry;
  
  return Entry = new (C) FunctionType(Input, Result);
}

// If the input and result types are canonical, then so is the result.
FunctionType::FunctionType(Type input, Type result)
  : TypeBase(TypeKind::Function,
             (input->isCanonical() && result->isCanonical()) ?
               &input->getASTContext() : 0),
    Input(input), Result(result) {
}


/// getArrayType - Return a uniqued array type with the specified base type
/// and the specified size.  Size=0 indicates an unspecified size array.
ArrayType *ArrayType::get(Type BaseType, uint64_t Size, ASTContext &C) {
  ArrayType *&Entry =
    (*(ArrayTypesMapTy*)C.ArrayTypes)[std::make_pair(BaseType, Size)];
  if (Entry) return Entry;

  return Entry = new (C) ArrayType(BaseType, Size);
}

ArrayType::ArrayType(Type base, uint64_t size)
  : TypeBase(TypeKind::Array, base->isCanonical() ? &base->getASTContext() : 0),
    Base(base), Size(size) {}



/// getNew - Return a new instance of a protocol type.  These are never
/// uniqued since each syntactic instance of them is semantically considered
/// to be a different type.
ProtocolType *ProtocolType::getNew(SourceLoc ProtocolLoc,
                                   ArrayRef<ValueDecl*> Elts,
                                   DeclContext *Parent) {
  ASTContext &C = Parent->getASTContext();
  return new (C) ProtocolType(ProtocolLoc, C.AllocateCopy(Elts), Parent);
}


