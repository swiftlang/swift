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
#include "swift/AST/Decl.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Casting.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
using namespace swift;

/// IdentifierTableMapTy - This is the type underlying IdentifierTable.
typedef llvm::StringMap<char, llvm::BumpPtrAllocator&> IdentifierTableMapTy;

/// AliasTypesMapTy - This is the type underlying AliasTypes.
typedef llvm::DenseMap<Identifier, AliasType*> AliasTypesMapTy;

/// DataTypesMapTy - This is the type underlying DataTypes.
typedef llvm::DenseMap<Identifier, DataType*> DataTypesMapTy;

/// TupleTypesMapTy - This is the actual type underlying ASTContext::TupleTypes.
typedef llvm::FoldingSet<TupleType> TupleTypesMapTy;

/// FunctionTypesMapTy - This is the actual type underlying 'FunctionTypes'.
typedef llvm::DenseMap<std::pair<Type*,Type*>, FunctionType*>FunctionTypesMapTy;

/// ArrayTypesMapTy - This is the actual type underlying 'ArrayTypes'.
typedef llvm::DenseMap<std::pair<Type*, uint64_t>, ArrayType*> ArrayTypesMapTy;

ASTContext::ASTContext(llvm::SourceMgr &sourcemgr)
  : Allocator(new llvm::BumpPtrAllocator()),
    IdentifierTable(new IdentifierTableMapTy(*Allocator)),
    AliasTypes(new AliasTypesMapTy()),
    DataTypes(new DataTypesMapTy()),
    TupleTypes(new TupleTypesMapTy()),
    FunctionTypes(new FunctionTypesMapTy()),
    ArrayTypes(new ArrayTypesMapTy()),
    SourceMgr(sourcemgr),
    TheEmptyTupleType(getTupleType(0, 0)),
    TheDependentType(new (*this) DependentType()),
    TheInt32Type(new (*this) BuiltinType(BuiltinInt32Kind)) {
}

ASTContext::~ASTContext() {
  delete (AliasTypesMapTy*)AliasTypes; AliasTypes = 0;
  delete (DataTypesMapTy*)DataTypes; DataTypes = 0;
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
Identifier ASTContext::getIdentifier(llvm::StringRef Str) {
  // Make sure null pointers stay null.
  if (Str.empty()) return Identifier(0);
  
  IdentifierTableMapTy &Table = *((IdentifierTableMapTy*)IdentifierTable);
  return Identifier(Table.GetOrCreateValue(Str).getKeyData());
}


//===----------------------------------------------------------------------===//
// Type manipulation routines.
//===----------------------------------------------------------------------===//

/// getCanonicalType - Get the canonicalized version of a type, stripping off
/// sugar like argument names and type aliases.
Type *ASTContext::getCanonicalType(Type *T) {
  // If the type is itself canonical or if the canonical type was already
  // computed, just return what we have.
  if (T->CanonicalType)
    return T->CanonicalType;
  
  switch (T->Kind) {
  case BuiltinInt32Kind:
  case DependentTypeKind:
  case DataTypeKind:
    assert(0 && "These are always canonical");
  case AliasTypeKind:
    return T->CanonicalType =
      getCanonicalType(llvm::cast<AliasType>(T)->UnderlyingType);
  case TupleTypeKind: {
    llvm::SmallVector<TupleTypeElt, 8> CanElts;
    TupleType *TT = llvm::cast<TupleType>(T);
    CanElts.resize(TT->NumFields);
    for (unsigned i = 0, e = TT->NumFields; i != e; ++i) {
      CanElts[i].Name = TT->Fields[i].Name;
      CanElts[i].Ty = getCanonicalType(TT->Fields[i].Ty);
    }
    
    return T->CanonicalType = getTupleType(CanElts.data(), CanElts.size());
  }
    
  case FunctionTypeKind: {
    FunctionType *FT = llvm::cast<FunctionType>(T);
    return T->CanonicalType = getFunctionType(getCanonicalType(FT->Input),
                                              getCanonicalType(FT->Result));
  }
  case ArrayTypeKind:
    ArrayType *AT = llvm::cast<ArrayType>(T);
    return T->CanonicalType = getArrayType(getCanonicalType(AT->Base),AT->Size);
  }
  assert(0 && "Unreachable");
}


void TupleType::Profile(llvm::FoldingSetNodeID &ID, const TupleTypeElt *Fields,
                        unsigned NumFields) {
  ID.AddInteger(NumFields);
  for (unsigned i = 0; i != NumFields; ++i) {
    ID.AddPointer(Fields[i].Ty);
    ID.AddPointer(Fields[i].Name.get());
  }
}

/// getNamedType - This method does a lookup for the specified type name.  If
/// no type with the specified name exists, null is returned.
///
/// FIXME: This is a total hack since we don't have scopes for types.
/// getNamedType should eventually be replaced.
Type *ASTContext::getNamedType(Identifier Name) {
  {
    AliasTypesMapTy &Table = *((AliasTypesMapTy*)AliasTypes);
    AliasTypesMapTy::iterator It = Table.find(Name);
    if (It != Table.end())
      return It->second;
  }

  {
    DataTypesMapTy &Table = *((DataTypesMapTy*)DataTypes);
    DataTypesMapTy::iterator It = Table.find(Name);
    if (It != Table.end())
      return It->second;
  }
  return 0;
}


/// InstallAliasType - InstallAliasType a new alias type.
void ASTContext::InstallAliasType(Identifier Name, Type *Underlying) {
  AliasType *&Entry = (*(AliasTypesMapTy*)AliasTypes)[Name];
  assert(Entry == 0 && "Redefinition of alias type");
  
  Entry = new (*this) AliasType(Name, Underlying);
}

/// InstallDataType - Install the type corresponding to the specified data
/// declaration.
void ASTContext::InstallDataType(DataDecl *TheDecl) {
  assert(TheDecl->Name.get() != 0);
  DataType *&Entry = (*(DataTypesMapTy*)DataTypes)[TheDecl->Name];
  assert(Entry == 0 && "Named type redefinition");
  Entry = new (*this) DataType(TheDecl);
}

/// getTupleType - Return the uniqued tuple type with the specified elements.
TupleType *ASTContext::getTupleType(const TupleTypeElt *Fields,
                                    unsigned NumFields) {
  // Check to see if we've already seen this tuple before.
  llvm::FoldingSetNodeID ID;
  TupleType::Profile(ID, Fields, NumFields);
 
  
  TupleTypesMapTy &TupleTypesMap = *(TupleTypesMapTy*)TupleTypes;
  
  // FIXME: This is pointless for types with named fields.  The ValueDecl fields
  // themselves are not unique'd so they all get their own addresses, which
  // means that we'll never get a hit here.  This should unique all-type tuples
  // though.
  void *InsertPos = 0;
  if (TupleType *TT = TupleTypesMap.FindNodeOrInsertPos(ID, InsertPos))
    return TT;
  
  // Okay, we didn't find one.  Make a copy of the fields list into ASTContext
  // owned memory.
  TupleTypeElt *FieldsCopy =
    (TupleTypeElt *)Allocate(sizeof(*Fields)*NumFields, 8);
  
  bool IsCanonical = true;   // All canonical elts means this is canonical.
  for (unsigned i = 0, e = NumFields; i != e; ++i) {
    FieldsCopy[i] = Fields[i];
    IsCanonical &= Fields[i].Ty->isCanonical();
  }

  TupleType *New = new (*this) TupleType(FieldsCopy, NumFields);
  TupleTypesMap.InsertNode(New, InsertPos);

  // If the type is canonical then set the canonical type pointer to itself.
  if (IsCanonical)
    New->CanonicalType = New;
  
  return New;
}

/// getFunctionType - Return a uniqued function type with the specified
/// input and result.
FunctionType *ASTContext::getFunctionType(Type *Input, Type *Result) {
  FunctionType *&Entry =
    (*(FunctionTypesMapTy*)FunctionTypes)[std::make_pair(Input, Result)];
  if (Entry) return Entry;
  
  Entry = new (*this) FunctionType(Input, Result);
  
  // If the input and result types are canonical, then so is the result.
  if (Input->isCanonical() && Result->isCanonical())
    Entry->CanonicalType = Entry;
  
  return Entry;
}

/// getArrayType - Return a uniqued array type with the specified base type
/// and the specified size.  Size=0 indicates an unspecified size array.
ArrayType *ASTContext::getArrayType(Type *BaseType, uint64_t Size) {
  ArrayType *&Entry =
    (*(ArrayTypesMapTy*)FunctionTypes)[std::make_pair(BaseType, Size)];
  if (Entry) return Entry;

  Entry = new (*this) ArrayType(BaseType, Size);
  
  if (BaseType->isCanonical())
    Entry->CanonicalType = Entry;
  
  return Entry;
}
