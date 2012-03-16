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
#include "swift/AST/DiagnosticEngine.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
using namespace swift;

struct ASTContext::Implementation {
  Implementation();
  ~Implementation();

  llvm::BumpPtrAllocator Allocator; // used in later initializations
  llvm::StringMap<char, llvm::BumpPtrAllocator&> IdentifierTable;
  llvm::FoldingSet<TupleType> TupleTypes;
  llvm::DenseMap<TypeAliasDecl*, MetaTypeType*> MetaTypeTypes;
  llvm::DenseMap<Module*, ModuleType*> ModuleTypes;
  llvm::DenseMap<std::pair<Type,std::pair<Type,char>>,
                 FunctionType*> FunctionTypes;
  llvm::DenseMap<std::pair<Type, uint64_t>, ArrayType*> ArrayTypes;
  llvm::DenseMap<unsigned, BuiltinIntegerType*> IntegerTypes;
  llvm::DenseMap<Type, ParenType*> ParenTypes;
  llvm::DenseMap<std::pair<Type, LValueType::Qual::opaque_type>, LValueType*>
    LValueTypes;
};
ASTContext::Implementation::Implementation()
 : IdentifierTable(Allocator) {}
ASTContext::Implementation::~Implementation() {}

ASTContext::ASTContext(llvm::SourceMgr &sourcemgr, DiagnosticEngine &Diags)
  : Impl(*new Implementation()),
    SourceMgr(sourcemgr),
    Diags(Diags),
    TheBuiltinModule(new (*this) BuiltinModule(getIdentifier("Builtin"),*this)),
    TheErrorType(new (*this) ErrorType(*this)),
    TheEmptyTupleType(TupleType::get(ArrayRef<TupleTypeElt>(), *this)),
    TheObjectPointerType(new (*this) BuiltinObjectPointerType(*this)),
    TheDependentType(new (*this) DependentType(*this)),
    TheIEEE32Type(new (*this) BuiltinFloatType(BuiltinFloatType::IEEE32,*this)),
    TheIEEE64Type(new (*this) BuiltinFloatType(BuiltinFloatType::IEEE64,*this)),
    TheIEEE16Type(new (*this) BuiltinFloatType(BuiltinFloatType::IEEE16,*this)),
    TheIEEE80Type(new (*this) BuiltinFloatType(BuiltinFloatType::IEEE80,*this)),
    TheIEEE128Type(new (*this) BuiltinFloatType(BuiltinFloatType::IEEE128,
                                                *this)),
    ThePPC128Type(new (*this) BuiltinFloatType(BuiltinFloatType::PPC128,*this)){
}

ASTContext::~ASTContext() {
  delete &Impl;
}

void *ASTContext::Allocate(unsigned long Bytes, unsigned Alignment) {
  return Impl.Allocator.Allocate(Bytes, Alignment);
}

/// getIdentifier - Return the uniqued and AST-Context-owned version of the
/// specified string.
Identifier ASTContext::getIdentifier(StringRef Str) {
  // Make sure null pointers stay null.
  if (Str.empty()) return Identifier(0);
  
  return Identifier(Impl.IdentifierTable.GetOrCreateValue(Str).getKeyData());
}

bool ASTContext::hadError() const {
  return Diags.hadAnyError();
}


//===----------------------------------------------------------------------===//
// Type manipulation routines.
//===----------------------------------------------------------------------===//

// Simple accessors.
Type ErrorType::get(ASTContext &C) { return C.TheErrorType; }
Type DependentType::get(ASTContext &C) { return C.TheDependentType; }


BuiltinIntegerType *BuiltinIntegerType::get(unsigned BitWidth, ASTContext &C) {
  BuiltinIntegerType *&Result = C.Impl.IntegerTypes[BitWidth];
  if (Result == 0)
    Result = new (C) BuiltinIntegerType(BitWidth, C);
  return Result;
}

ParenType *ParenType::get(ASTContext &C, Type underlying) {
  ParenType *&Result = C.Impl.ParenTypes[underlying];
  if (Result == 0)
    Result = new (C) ParenType(underlying);
  return Result;
}

Type TupleType::getEmpty(ASTContext &C) { return C.TheEmptyTupleType; }

void TupleType::Profile(llvm::FoldingSetNodeID &ID,
                        ArrayRef<TupleTypeElt> Fields) {
  ID.AddInteger(Fields.size());
  for (const TupleTypeElt &Elt : Fields) {
    ID.AddPointer(Elt.getType().getPointer());
    ID.AddPointer(Elt.getName().get());
    ID.AddPointer(Elt.getInit());
  }
}

/// getTupleType - Return the uniqued tuple type with the specified elements.
TupleType *TupleType::get(ArrayRef<TupleTypeElt> Fields, ASTContext &C) {
  // Check to see if we've already seen this tuple before.
  llvm::FoldingSetNodeID ID;
  TupleType::Profile(ID, Fields);
  
  // FIXME: This is pointless for types with named fields.  The ValueDecl fields
  // themselves are not unique'd so they all get their own addresses, which
  // means that we'll never get a hit here.  This should unique all-type tuples
  // though.  Likewise with default values.
  void *InsertPos = 0;
  if (TupleType *TT = C.Impl.TupleTypes.FindNodeOrInsertPos(ID, InsertPos))
    return TT;
  
  // Okay, we didn't find one.  Make a copy of the fields list into ASTContext
  // owned memory.
  TupleTypeElt *FieldsCopy =
    C.AllocateCopy<TupleTypeElt>(Fields.begin(), Fields.end());
  
  bool IsCanonical = true;   // All canonical elts means this is canonical.
  for (const TupleTypeElt &Elt : Fields) {
    if (Elt.getType().isNull() || !Elt.getType()->isCanonical()) {
      IsCanonical = false;
      break;
    }
  }

  Fields = ArrayRef<TupleTypeElt>(FieldsCopy, Fields.size());
  
  TupleType *New = new (C) TupleType(Fields, IsCanonical ? &C : 0);
  C.Impl.TupleTypes.InsertNode(New, InsertPos);

  return New;
}

/// getNewOneOfType - Return a new instance of oneof type.  These are never
/// uniqued because the loc is generally different.
OneOfType *OneOfType::getNew(SourceLoc OneOfLoc,
                             ArrayRef<OneOfElementDecl*> InElts,
                             TypeAliasDecl *TheDecl) {
  ASTContext &C = TheDecl->getASTContext();
  
  return new (C) OneOfType(OneOfLoc, C.AllocateCopy(InElts), TheDecl);
}

// oneof types are always canonical.
OneOfType::OneOfType(SourceLoc OneOfLoc, ArrayRef<OneOfElementDecl*> Elts,
                     TypeAliasDecl *TheDecl)
  : TypeBase(TypeKind::OneOf, &TheDecl->getASTContext()),
    DeclContext(DeclContextKind::OneOfType, TheDecl->getDeclContext()),
    OneOfLoc(OneOfLoc), Elements(Elts), TheDecl(TheDecl) {
}

IdentifierType *IdentifierType::getNew(ASTContext &C,
                                       MutableArrayRef<Component> Components) {
  Components = C.AllocateCopy(Components);
  return new (C) IdentifierType(Components);
}


MetaTypeType *MetaTypeType::get(TypeAliasDecl *Type) {
  ASTContext &C = Type->getASTContext();

  MetaTypeType *&Entry = C.Impl.MetaTypeTypes[Type];
  if (Entry) return Entry;
  
  return Entry = new (C) MetaTypeType(Type, C);
}

ModuleType *ModuleType::get(Module *M) {
  ASTContext &C = M->getASTContext();
  
  ModuleType *&Entry = C.Impl.ModuleTypes[M];
  if (Entry) return Entry;
  
  return Entry = new (C) ModuleType(M, C);
}

/// FunctionType::get - Return a uniqued function type with the specified
/// input and result.
FunctionType *FunctionType::get(Type Input, Type Result, bool isAutoClosure,
                                ASTContext &C) {
  FunctionType *&Entry =
    C.Impl.FunctionTypes[std::make_pair(Input,
                                        std::make_pair(Result, 
                                                       (char)isAutoClosure))];
  if (Entry) return Entry;
  
  return Entry = new (C) FunctionType(Input, Result, isAutoClosure);
}

// If the input and result types are canonical, then so is the result.
FunctionType::FunctionType(Type input, Type result, bool isAutoClosure)
  : TypeBase(TypeKind::Function,
             (input->isCanonical() && result->isCanonical()) ?
               &input->getASTContext() : 0),
    Input(input), Result(result), AutoClosure(isAutoClosure) {
}


/// getArrayType - Return a uniqued array type with the specified base type
/// and the specified size.  Size=0 indicates an unspecified size array.
ArrayType *ArrayType::get(Type BaseType, uint64_t Size, ASTContext &C) {
  ArrayType *&Entry = C.Impl.ArrayTypes[std::make_pair(BaseType, Size)];
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
                                   TypeAliasDecl *TheDecl) {
  ASTContext &C = TheDecl->getASTContext();
  return new (C) ProtocolType(ProtocolLoc, C.AllocateCopy(Elts), TheDecl);
}

ProtocolType::ProtocolType(SourceLoc ProtocolLoc, ArrayRef<ValueDecl*> Elts,
                           TypeAliasDecl *TheDecl)
  : TypeBase(TypeKind::Protocol, &TheDecl->getASTContext()),
    DeclContext(DeclContextKind::ProtocolType, TheDecl->getDeclContext()),
    ProtocolLoc(ProtocolLoc), Elements(Elts), TheDecl(TheDecl) {
}

LValueType *LValueType::get(Type objectTy, Qual quals, ASTContext &C) {
  auto key = std::make_pair(objectTy, quals.getOpaqueData());
  auto it = C.Impl.LValueTypes.find(key);
  if (it != C.Impl.LValueTypes.end()) return it->second;

  ASTContext *canonicalContext = (objectTy->isCanonical() ? &C : nullptr);
  LValueType *type = new (C) LValueType(objectTy, quals, canonicalContext);
  C.Impl.LValueTypes.insert(std::make_pair(key, type));
  return type;
}
