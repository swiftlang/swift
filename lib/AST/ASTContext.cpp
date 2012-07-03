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
  llvm::DenseMap<TypeDecl*, MetaTypeType*> MetaTypeTypes;
  llvm::DenseMap<Module*, ModuleType*> ModuleTypes;
  llvm::DenseMap<std::pair<Type,std::pair<Type,char>>,
                 FunctionType*> FunctionTypes;
  llvm::DenseMap<std::pair<Type, uint64_t>, ArrayType*> ArrayTypes;
  llvm::DenseMap<Type, ArraySliceType*> ArraySliceTypes;
  llvm::DenseMap<unsigned, BuiltinIntegerType*> IntegerTypes;
  llvm::DenseMap<Type, ParenType*> ParenTypes;
  llvm::DenseMap<std::pair<Type, LValueType::Qual::opaque_type>, LValueType*>
    LValueTypes;
  llvm::DenseMap<std::pair<Type, Type>, SubstitutedType *> SubstitutedTypes;
  llvm::FoldingSet<ProtocolCompositionType> ProtocolCompositionTypes;
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
    TheObjCPointerType(new (*this) BuiltinObjCPointerType(*this)),
    TheRawPointerType(new (*this) BuiltinRawPointerType(*this)),
    TheUnstructuredUnresolvedType(new (*this) UnstructuredUnresolvedType(*this)),
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

  for (auto &entry : ConformsTo)
    delete const_cast<ProtocolConformance*>(entry.second);
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
Type UnstructuredUnresolvedType::get(ASTContext &C) { 
  return C.TheUnstructuredUnresolvedType; 
}


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
    ID.AddPointer(Elt.getVarargBaseTy().getPointer());
  }
}

/// getTupleType - Return the uniqued tuple type with the specified elements.
TupleType *TupleType::get(ArrayRef<TupleTypeElt> Fields, ASTContext &C) {
  bool HasAnyDefaultValues = false;
  for (const TupleTypeElt &Elt : Fields) {
    if (Elt.hasInit()) {
      HasAnyDefaultValues = true;
      break;
    }
  }

  void *InsertPos = 0;
  if (!HasAnyDefaultValues) {
    // Check to see if we've already seen this tuple before.
    llvm::FoldingSetNodeID ID;
    TupleType::Profile(ID, Fields);

    if (TupleType *TT = C.Impl.TupleTypes.FindNodeOrInsertPos(ID, InsertPos))
      return TT;
  }

  // Make a copy of the fields list into ASTContext owned memory.
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
  if (!HasAnyDefaultValues)
    C.Impl.TupleTypes.InsertNode(New, InsertPos);

  return New;
}

OneOfType::OneOfType(OneOfDecl *TheDecl, ArrayRef<Type> ArgumentTypes,
                     ASTContext &C)
  : NominalType(TypeKind::OneOf, &C, TheDecl, ArgumentTypes) { }

StructType::StructType(StructDecl *TheDecl, ArrayRef<Type> ArgumentTypes,
                       ASTContext &C)
  : NominalType(TypeKind::Struct, &C, TheDecl, ArgumentTypes) { }

ClassType::ClassType(ClassDecl *TheDecl, ArrayRef<Type> ArgumentTypes,
                    ASTContext &C)
  : NominalType(TypeKind::Class, &C, TheDecl, ArgumentTypes) { }

IdentifierType *IdentifierType::getNew(ASTContext &C,
                                       MutableArrayRef<Component> Components) {
  Components = C.AllocateCopy(Components);
  return new (C) IdentifierType(Components);
}

ProtocolCompositionType *
ProtocolCompositionType::get(ASTContext &C, SourceLoc FirstLoc,
                             ArrayRef<Type> Protocols) {
  // Check to see if we've already seen this protocol composition before.
  void *InsertPos = 0;
  llvm::FoldingSetNodeID ID;
  ProtocolCompositionType::Profile(ID, Protocols);
  if (ProtocolCompositionType *Result
        = C.Impl.ProtocolCompositionTypes.FindNodeOrInsertPos(ID, InsertPos))
    return Result;
  
  // Create a new protocol composition type.
  ProtocolCompositionType *New = new (C) ProtocolCompositionType(C, FirstLoc,
                                                                 Protocols);
  C.Impl.ProtocolCompositionTypes.InsertNode(New, InsertPos);
  return New;
}


MetaTypeType *MetaTypeType::get(TypeDecl *Type) {
  ASTContext &C = Type->getASTContext();

  MetaTypeType *&Entry = C.Impl.MetaTypeTypes[Type];
  if (Entry) return Entry;

  // FIXME: Should we build a MetaTypeType for TypeAliasDecls.  If so,
  // should it be canonical?
  bool IsCanonical = true;
  return Entry = new (C) MetaTypeType(Type, IsCanonical ? &C : 0);
}

MetaTypeType::MetaTypeType(TypeDecl *Type, ASTContext *C)
  : TypeBase(TypeKind::MetaType, C, /*Unresolved=*/false),
    TheType(Type) {
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
FunctionType::FunctionType(Type input, Type output, bool isAutoClosure)
  : AnyFunctionType(TypeKind::Function,
             (input->isCanonical() && output->isCanonical()) ?
               &input->getASTContext() : 0,
             input, output,
             (input->isUnresolvedType() || output->isUnresolvedType())),
    AutoClosure(isAutoClosure) { }


/// FunctionType::get - Return a uniqued function type with the specified
/// input and result.
PolymorphicFunctionType *PolymorphicFunctionType::get(Type input, Type output,
                                                      GenericParamList *params,
                                                      ASTContext &C) {
  // FIXME: one day we should do canonicalization properly.
  return new (C) PolymorphicFunctionType(input, output, params, C);
}

PolymorphicFunctionType::PolymorphicFunctionType(Type input, Type output,
                                                 GenericParamList *params,
                                                 ASTContext &C)
  : AnyFunctionType(TypeKind::PolymorphicFunction,
                    (input->isCanonical() && output->isCanonical()) ?&C : 0,
                    input, output,
                    (input->isUnresolvedType() || output->isUnresolvedType())),
    Params(params) { }

/// Return a uniqued array type with the specified base type and the
/// specified size.
ArrayType *ArrayType::get(Type BaseType, uint64_t Size, ASTContext &C) {
  assert(Size != 0);

  ArrayType *&Entry = C.Impl.ArrayTypes[std::make_pair(BaseType, Size)];
  if (Entry) return Entry;

  return Entry = new (C) ArrayType(BaseType, Size);
}

ArrayType::ArrayType(Type base, uint64_t size)
  : TypeBase(TypeKind::Array, 
             base->isCanonical() ? &base->getASTContext() : 0,
             base->isUnresolvedType()),
    Base(base), Size(size) {}


/// Return a uniqued array slice type with the specified base type.
ArraySliceType *ArraySliceType::get(Type base, SourceLoc loc, ASTContext &C) {
  ArraySliceType *&entry = C.Impl.ArraySliceTypes[base];
  if (entry) return entry;

  return entry = new (C) ArraySliceType(loc, base);
}

ProtocolType::ProtocolType(ProtocolDecl *TheDecl, ASTContext &Ctx)
  : NominalType(TypeKind::Protocol, &Ctx, TheDecl, ArrayRef<Type>()) { }

LValueType *LValueType::get(Type objectTy, Qual quals, ASTContext &C) {
  auto key = std::make_pair(objectTy, quals.getOpaqueData());
  auto it = C.Impl.LValueTypes.find(key);
  if (it != C.Impl.LValueTypes.end()) return it->second;

  ASTContext *canonicalContext = (objectTy->isCanonical() ? &C : nullptr);
  LValueType *type = new (C) LValueType(objectTy, quals, canonicalContext);
  C.Impl.LValueTypes.insert(std::make_pair(key, type));
  return type;
}

/// Return a uniqued substituted type.
SubstitutedType *SubstitutedType::get(Type Original, Type Replacement,
                                      ASTContext &C) {
  SubstitutedType *&Known = C.Impl.SubstitutedTypes[{Original, Replacement}];
  if (!Known)
    Known = new (C) SubstitutedType(Original, Replacement);
  return Known;
}
