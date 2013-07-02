//===--- ModuleFile.cpp - Loading a serialized module -----------*- c++ -*-===//
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

#include "ModuleFile.h"
#include "ModuleFormat.h"
#include "swift/AST/AST.h"
#include "swift/Serialization/BCReadingExtras.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace swift;
using namespace swift::serialization;

static ModuleStatus
validateControlBlock(llvm::BitstreamCursor &cursor,
                     llvm::SmallVectorImpl<uint64_t> &scratch) {
  // The control block is malformed until we've at least read a major version
  // number.
  ModuleStatus result = ModuleStatus::Malformed;

  auto next = cursor.advance();
  while (next.Kind != llvm::BitstreamEntry::EndBlock) {
    if (next.Kind == llvm::BitstreamEntry::Error)
      return ModuleStatus::Malformed;

    if (next.Kind == llvm::BitstreamEntry::SubBlock) {
      // Unknown metadata sub-block, possibly for use by a future version of the
      // module format.
      if (cursor.SkipBlock())
        return ModuleStatus::Malformed;
      next = cursor.advance();
      continue;
    }

    scratch.clear();
    StringRef blobData;
    unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
    switch (kind) {
    case control_block::METADATA: {
      uint16_t versionMajor = scratch[0];
      if (versionMajor > VERSION_MAJOR)
        return ModuleStatus::FormatTooNew;
      result = ModuleStatus::Valid;
      break;
    }
    default:
      // Unknown metadata record, possibly for use by a future version of the
      // module format.
      break;
    }

    next = cursor.advance();
  }

  return result;
}

Pattern *ModuleFile::maybeReadPattern() {
  using namespace decls_block;
  
  SmallVector<uint64_t, 8> scratch;

  auto next = DeclTypeCursor.advance();
  if (next.Kind != llvm::BitstreamEntry::Record)
    return nullptr;

  unsigned kind = DeclTypeCursor.readRecord(next.ID, scratch);
  switch (kind) {
  case decls_block::PAREN_PATTERN: {
    Pattern *subPattern = maybeReadPattern();
    assert(subPattern);

    auto result = new (ModuleContext->Ctx) ParenPattern(SourceLoc(),
                                                        subPattern,
                                                        SourceLoc());
    result->setType(subPattern->getType());
    return result;
  }
  case decls_block::TUPLE_PATTERN: {
    TypeID tupleTypeID;
    unsigned count;

    TuplePatternLayout::readRecord(scratch, tupleTypeID, count);

    SmallVector<TuplePatternElt, 8> elements;
    for ( ; count > 0; --count) {
      scratch.clear();
      next = DeclTypeCursor.advance();
      assert(next.Kind == llvm::BitstreamEntry::Record);

      kind = DeclTypeCursor.readRecord(next.ID, scratch);
      assert(kind == decls_block::TUPLE_PATTERN_ELT);

      TypeID varargsTypeID;
      TuplePatternEltLayout::readRecord(scratch, varargsTypeID);

      Pattern *subPattern = maybeReadPattern();
      assert(subPattern);

      elements.push_back(TuplePatternElt(subPattern));
      if (varargsTypeID) {
        BCOffsetRAII restoreOffset(DeclTypeCursor);
        elements.back().setVarargBaseType(getType(varargsTypeID));
      }
    }

    auto result = TuplePattern::create(ModuleContext->Ctx, SourceLoc(),
                                       elements, SourceLoc());
    {
      BCOffsetRAII restoreOffset(DeclTypeCursor);
      result->setType(getType(tupleTypeID));
    }
    return result;
  }
  case decls_block::NAMED_PATTERN: {

    DeclID varID;
    NamedPatternLayout::readRecord(scratch, varID);

    BCOffsetRAII restoreOffset(DeclTypeCursor);

    auto var = cast<VarDecl>(getDecl(varID));
    auto result = new (ModuleContext->Ctx) NamedPattern(var);
    if (var->hasType())
      result->setType(var->getType());
    return result;
  }
  case decls_block::ANY_PATTERN: {
    TypeID typeID;

    AnyPatternLayout::readRecord(scratch, typeID);
    auto result = new (ModuleContext->Ctx) AnyPattern(SourceLoc());
    {
      BCOffsetRAII restoreOffset(DeclTypeCursor);
      result->setType(getType(typeID));
    }
    return result;
  }
  case decls_block::TYPED_PATTERN: {
    TypeID typeID;

    TypedPatternLayout::readRecord(scratch, typeID);
    Pattern *subPattern = maybeReadPattern();
    assert(subPattern);

    TypeLoc typeInfo = TypeLoc::withoutLoc(getType(typeID));
    auto result = new (ModuleContext->Ctx) TypedPattern(subPattern, typeInfo);
    {
      BCOffsetRAII restoreOffset(DeclTypeCursor);
      result->setType(typeInfo.getType());
    }
    return result;
  }
  default:
    return nullptr;
  }
}

MutableArrayRef<TypeLoc> ModuleFile::getTypes(ArrayRef<uint64_t> rawTypeIDs) {
  ASTContext &ctx = ModuleContext->Ctx;
  auto result =
    MutableArrayRef<TypeLoc>(ctx.Allocate<TypeLoc>(rawTypeIDs.size()),
                             rawTypeIDs.size());

  TypeLoc *nextType = result.data();
  for (TypeID rawID : rawTypeIDs) {
    auto type = getType(rawID);
    new (nextType) auto(TypeLoc::withoutLoc(type));
    ++nextType;
  }

  return result;
}

Optional<MutableArrayRef<Decl *>> ModuleFile::readMembers() {
  using namespace decls_block;

  auto entry = DeclTypeCursor.advance();
  if (entry.Kind != llvm::BitstreamEntry::Record)
    return Nothing;

  SmallVector<uint64_t, 16> memberIDBuffer;

  unsigned kind = DeclTypeCursor.readRecord(entry.ID, memberIDBuffer);
  if (kind != DECL_CONTEXT)
    return Nothing;

  ArrayRef<uint64_t> rawMemberIDs;
  decls_block::DeclContextLayout::readRecord(memberIDBuffer, rawMemberIDs);

  if (rawMemberIDs.empty())
    return MutableArrayRef<Decl *>();

  ASTContext &ctx = ModuleContext->Ctx;
  MutableArrayRef<Decl *> members(ctx.Allocate<Decl *>(rawMemberIDs.size()),
                                  rawMemberIDs.size());
  auto nextMember = members.begin();
  for (DeclID rawID : rawMemberIDs) {
    *nextMember = getDecl(rawID);
    ++nextMember;
  }

  return members;
}

Identifier ModuleFile::getIdentifier(IdentifierID IID) {
  if (IID == 0)
    return Identifier();

  assert(IID <= Identifiers.size() && "invalid identifier ID");
  auto identRecord = Identifiers[IID-1];

  if (identRecord.Offset == 0)
    return identRecord.Ident;

  assert(!IdentifierData.empty() && "no identifier data in module");

  StringRef rawStrPtr = IdentifierData.substr(identRecord.Offset);
  size_t terminatorOffset = rawStrPtr.find('\0');
  assert(terminatorOffset != StringRef::npos &&
         "unterminated identifier string data");

  return ModuleContext->Ctx.getIdentifier(rawStrPtr.slice(0, terminatorOffset));
}

DeclContext *ModuleFile::getDeclContext(DeclID DID) {
  if (DID == 0)
    return ModuleContext;

  Decl *D = getDecl(DID);

  if (auto ND = dyn_cast<NominalTypeDecl>(D))
    return ND;
  if (auto ED = dyn_cast<ExtensionDecl>(D))
    return ED;
  if (auto CD = dyn_cast<ConstructorDecl>(D))
    return CD;
  if (auto DD = dyn_cast<DestructorDecl>(D))
    return DD;
  if (auto FD = dyn_cast<FuncDecl>(D))
    return FD->getBody();

  llvm_unreachable("unknown DeclContext kind");
}


/// Finds all value decls with the given name in the given context,
/// visible or not.
static void lookupImmediateDecls(DeclContext *DC, Identifier name,
                                 SmallVectorImpl<ValueDecl *> &results) {
  ArrayRef<Decl *> members;

  switch (DC->getContextKind()) {
  case DeclContextKind::TranslationUnit:
  case DeclContextKind::BuiltinModule:
  case DeclContextKind::SerializedModule:
  case DeclContextKind::ClangModule:
    llvm_unreachable("should not be used for module contexts");

  case DeclContextKind::CapturingExpr:
  case DeclContextKind::ConstructorDecl:
  case DeclContextKind::DestructorDecl:
    // Don't look at params or local variables.
    return;

  case DeclContextKind::NominalTypeDecl: {
    members = cast<NominalTypeDecl>(DC)->getMembers();
    break;
  }

  case DeclContextKind::ExtensionDecl: {
    members = cast<ExtensionDecl>(DC)->getMembers();
    break;
  }

  case DeclContextKind::TopLevelCodeDecl:
    llvm_unreachable("libraries do not expose their top-level code");
  }

  for (Decl *member : members) {
    if (auto value = dyn_cast<ValueDecl>(member)) {
      if (value->getName() == name)
        results.push_back(value);
    }
  }
}


Decl *ModuleFile::getDecl(DeclID DID, DeclDeserializationOptions opts) {
  if (DID == 0)
    return nullptr;

  assert(DID <= Decls.size() && "invalid decl ID");
  auto &declOrOffset = Decls[DID-1];

  if (declOrOffset.is<Decl *>())
    return declOrOffset.get<Decl *>();

  DeclTypeCursor.JumpToBit(declOrOffset.get<BitOffset>());
  auto entry = DeclTypeCursor.advance();

  if (entry.Kind != llvm::BitstreamEntry::Record) {
    // We don't know how to serialize decls represented by sub-blocks.
    error();
    return nullptr;
  }

#ifndef NDEBUG
  assert(declOrOffset.get<BitOffset>() != 0 &&
         "this decl is already being deserialized");
  declOrOffset = BitOffset(0);
#endif

  ASTContext &ctx = ModuleContext->Ctx;

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch, &blobData);

  switch (recordID) {
  case decls_block::TYPE_ALIAS_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    TypeID underlyingTypeID;
    bool isGeneric;
    bool isImplicit;
    ArrayRef<uint64_t> inheritedIDs;

    decls_block::TypeAliasLayout::readRecord(scratch, nameID, contextID,
                                             underlyingTypeID,
                                             isGeneric, isImplicit,
                                             inheritedIDs);

    auto inherited =
      MutableArrayRef<TypeLoc>(ctx.Allocate<TypeLoc>(inheritedIDs.size()),
                               inheritedIDs.size());

    TypeLoc *nextInheritedType = inherited.data();
    for (TypeID TID : inheritedIDs) {
      auto type = getType(TID);
      new (nextInheritedType) TypeLoc(TypeLoc::withoutLoc(type));
      ++nextInheritedType;
    }

    TypeLoc underlyingType = TypeLoc::withoutLoc(getType(underlyingTypeID));
    auto alias = new (ctx) TypeAliasDecl(SourceLoc(),
                                         getIdentifier(nameID),
                                         SourceLoc(),
                                         underlyingType,
                                         getDeclContext(contextID),
                                         inherited);
    declOrOffset = alias;

    if (isImplicit)
      alias->setImplicit();
    if (isGeneric)
      alias->setGenericParameter();
    break;
  }

  case decls_block::STRUCT_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;
    ArrayRef<uint64_t> inheritedIDs;

    decls_block::StructLayout::readRecord(scratch, nameID, contextID,
                                          isImplicit, inheritedIDs);

    MutableArrayRef<TypeLoc> inherited;
    {
      BCOffsetRAII restoreOffset(DeclTypeCursor);
      inherited = getTypes(inheritedIDs);
    }

    auto theStruct = new (ctx) StructDecl(SourceLoc(), getIdentifier(nameID),
                                          SourceLoc(), inherited,
                                          /*generic params=*/nullptr,
                                          getDeclContext(contextID));
    declOrOffset = theStruct;

    if (isImplicit)
      theStruct->setImplicit();

    auto members = readMembers();
    assert(members.hasValue() && "could not read struct members");
    theStruct->setMembers(members.getValue(), SourceRange());

    break;
  }

  case decls_block::CONSTRUCTOR_DECL: {
    DeclID parentID;
    bool isImplicit;
    TypeID signatureID;
    DeclID implicitThisID;

    decls_block::ConstructorLayout::readRecord(scratch, parentID, isImplicit,
                                               signatureID, implicitThisID);
    VarDecl *thisDecl;
    NominalTypeDecl *parent;
    {
      BCOffsetRAII restoreOffset(DeclTypeCursor);
      thisDecl = cast<VarDecl>(getDecl(implicitThisID, SkipContext));
      parent = cast<NominalTypeDecl>(getDeclContext(parentID));
    }

    auto ctor = new (ctx) ConstructorDecl(ctx.getIdentifier("constructor"),
                                          SourceLoc(), /*args=*/nullptr,
                                          thisDecl, /*generic params=*/nullptr,
                                          parent);
    declOrOffset = ctor;
    thisDecl->setDeclContext(ctor);

    Pattern *args = maybeReadPattern();
    assert(args && "missing arguments for constructor");
    ctor->setArguments(args);

    ctor->setType(getType(signatureID));

    if (isImplicit)
      ctor->setImplicit();
    break;
  }

  case decls_block::VAR_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit, isNeverLValue;
    TypeID typeID;
    DeclID getterID, setterID;
    DeclID overriddenID;

    decls_block::VarLayout::readRecord(scratch, nameID, contextID, isImplicit,
                                       isNeverLValue, typeID, getterID,
                                       setterID, overriddenID);

    auto DC = (opts & SkipContext) ? nullptr : getDeclContext(contextID);
    auto var = new (ctx) VarDecl(SourceLoc(), getIdentifier(nameID),
                                 getType(typeID), DC);

    // Explicitly set the getter and setter info /before/ recording the VarDecl
    // in the map. The functions will check this to know if they are getters or
    // setters.
    if (getterID || setterID) {
      var->setProperty(ctx, SourceLoc(),
                       cast_or_null<FuncDecl>(getDecl(getterID)),
                       cast_or_null<FuncDecl>(getDecl(setterID)),
                       SourceLoc());
    }

    declOrOffset = var;

    var->setNeverUsedAsLValue(isNeverLValue);
    if (isImplicit)
      var->setImplicit();

    var->setOverriddenDecl(cast_or_null<VarDecl>(getDecl(overriddenID)));
    break;
  }

  case decls_block::FUNC_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit, isNeverLValue;
    TypeID signatureID;
    bool isClassMethod;
    DeclID associatedDeclID;
    DeclID overriddenID;

    decls_block::FuncLayout::readRecord(scratch, nameID, contextID, isImplicit,
                                        isNeverLValue, signatureID,
                                        isClassMethod, associatedDeclID,
                                        overriddenID);

    DeclContext *DC;
    FunctionType *signature;
    {
      BCOffsetRAII restoreOffset(DeclTypeCursor);
      DC = getDeclContext(contextID);
      signature = cast<FunctionType>(getType(signatureID).getPointer());
    }

    auto fn = new (ctx) FuncDecl(SourceLoc(), SourceLoc(),
                                 getIdentifier(nameID), SourceLoc(),
                                 /*generic params=*/nullptr, signature,
                                 /*body=*/nullptr, DC);
    declOrOffset = fn;

    SmallVector<Pattern *, 16> patternBuf;
    while (Pattern *pattern = maybeReadPattern())
      patternBuf.push_back(pattern);

    assert(!patternBuf.empty());
    size_t patternCount = patternBuf.size() / 2;
    assert(patternCount * 2 == patternBuf.size() &&
           "two sets of patterns don't  match up");

    ArrayRef<Pattern *> patterns(patternBuf);
    ArrayRef<Pattern *> argPatterns = patterns.slice(0, patternCount);
    ArrayRef<Pattern *> bodyPatterns = patterns.slice(patternCount);

    auto body = FuncExpr::create(ctx, SourceLoc(),
                                 argPatterns, bodyPatterns,
                                 TypeLoc::withoutLoc(signature->getResult()),
                                 /*body=*/nullptr, DC);
    fn->setBody(body);

    fn->setOverriddenDecl(cast_or_null<FuncDecl>(getDecl(overriddenID)));

    fn->setNeverUsedAsLValue(isNeverLValue);
    fn->setStatic(isClassMethod);
    if (isImplicit)
      fn->setImplicit();

    if (Decl *associated = getDecl(associatedDeclID)) {
      if (auto op = dyn_cast<OperatorDecl>(associated)) {
        fn->setOperatorDecl(op);
      } else {
        bool isGetter = false;

        if (auto subscript = dyn_cast<SubscriptDecl>(associated)) {
          isGetter = (subscript->getGetter() == fn);
          assert(isGetter || subscript->getSetter() == fn);
        } else if (auto var = dyn_cast<VarDecl>(associated)) {
          isGetter = (var->getGetter() == fn);
          assert(isGetter || var->getSetter() == fn);
        } else {
          llvm_unreachable("unknown associated decl kind");
        }

        if (isGetter)
          fn->makeGetter(associated);
        else
          fn->makeSetter(associated);
      }
    }

    break;
  }

  case decls_block::PATTERN_BINDING_DECL: {
    DeclID contextID;
    bool isImplicit;

    decls_block::PatternBindingLayout::readRecord(scratch, contextID,
                                                  isImplicit);
    Pattern *pattern = maybeReadPattern();
    assert(pattern);

    auto binding = new (ctx) PatternBindingDecl(SourceLoc(), pattern,
                                                /*init=*/nullptr,
                                                getDeclContext(contextID));
    declOrOffset = binding;

    if (isImplicit)
      binding->setImplicit();

    break;
  }

  case decls_block::PROTOCOL_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;
    ArrayRef<uint64_t> inheritedIDs;

    decls_block::ProtocolLayout::readRecord(scratch, nameID, contextID,
                                            isImplicit, inheritedIDs);

    MutableArrayRef<TypeLoc> inherited;
    {
      BCOffsetRAII restoreOffset(DeclTypeCursor);
      inherited = getTypes(inheritedIDs);
    }
    auto proto = new (ctx) ProtocolDecl(getDeclContext(contextID), SourceLoc(),
                                        SourceLoc(), getIdentifier(nameID),
                                        inherited);
    declOrOffset = proto;

    if (isImplicit)
      proto->setImplicit();

    auto members = readMembers();
    assert(members.hasValue() && "could not read struct members");
    proto->setMembers(members.getValue(), SourceRange());

    break;
  }

  case decls_block::XREF: {
    uint8_t kind;
    TypeID expectedTypeID;
    ArrayRef<uint64_t> rawAccessPath;

    decls_block::XRefLayout::readRecord(scratch, kind, expectedTypeID,
                                        rawAccessPath);

    // First, find the module this reference is referring to.
    Identifier moduleName = getIdentifier(rawAccessPath.front());
    rawAccessPath = rawAccessPath.slice(1);

    Module *M;
    if (moduleName.empty()) {
      M = ctx.TheBuiltinModule;
    } else {
      // FIXME: provide a real source location.
      M = ctx.getModule(std::make_pair(moduleName, SourceLoc()), false);
    }
    assert(M && "missing dependency");

    switch (kind) {
    case XRefKind::SwiftValue: {
      // Start by looking up the top-level decl in the module.
      SmallVector<ValueDecl *, 8> values;
      M->lookupValue(Module::AccessPathTy(),
                     getIdentifier(rawAccessPath.front()),
                     NLKind::QualifiedLookup,
                     values);
      rawAccessPath = rawAccessPath.slice(1);

      // Then, follow the chain of nested ValueDecls until we run out of
      // identifiers in the access path.
      SmallVector<ValueDecl *, 8> baseValues;
      while (!rawAccessPath.empty()) {
        baseValues.swap(values);
        values.clear();
        for (auto base : baseValues) {
          // FIXME: extensions?
          if (auto nominal = dyn_cast<NominalTypeDecl>(base))
            lookupImmediateDecls(nominal,
                                 getIdentifier(rawAccessPath.front()),
                                 values);
        }
        rawAccessPath = rawAccessPath.slice(1);
      }

      // If we have a type to validate against, filter out any ValueDecls that
      // don't match that type.
      CanType expectedTy;
      Type maybeExpectedTy = getType(expectedTypeID);
      if (maybeExpectedTy)
        expectedTy = maybeExpectedTy->getCanonicalType();

      ValueDecl *result = nullptr;
      for (auto value : values) {
        if (!expectedTy || value->getType()->getCanonicalType() == expectedTy) {
          // It's an error if more than one value has the same type.
          // FIXME: Functions and constructors can overload based on parameter
          // names.
          if (result) {
            error();
            return nullptr;
          }
          result = value;
        }
      }

      // It's an error if lookup doesn't actually find anything -- that means
      // the module's out of date.
      if (!result) {
        error();
        return nullptr;
      }

      declOrOffset = result;
      break;
    }
    case XRefKind::SwiftOperator: {
      assert(rawAccessPath.size() == 1 &&
             "can't import operators not at module scope");
      Identifier opName = getIdentifier(rawAccessPath.back());

      switch (expectedTypeID) {
      case OperatorKind::Infix: {
        auto op = M->lookupInfixOperator(opName);
        declOrOffset = op.hasValue() ? op.getValue() : nullptr;
        break;
      }
      case OperatorKind::Prefix: {
        auto op = M->lookupPrefixOperator(opName);
        declOrOffset = op.hasValue() ? op.getValue() : nullptr;
        break;
      }
      case OperatorKind::Postfix: {
        auto op = M->lookupPostfixOperator(opName);
        declOrOffset = op.hasValue() ? op.getValue() : nullptr;
        break;
      }
      default:
        // Unknown operator kind.
        error();
        return nullptr;
      }
      break;
    }
    default:
      // Unknown cross-reference kind.
      error();
      return nullptr;
    }

    break;
  }
      
  default:
    // We don't know how to deserialize this kind of decl.
    error();
    return nullptr;
  }

  return declOrOffset.get<Decl *>();
}

/// Translate from the Serialization calling convention enum values to the AST
/// strongly-typed enum.
///
/// The former is guaranteed to be stable, but may not reflect this version of
/// the AST.
static Optional<swift::AbstractCC> getActualCC(uint8_t cc) {
  switch (cc) {
#define CASE(THE_CC) \
  case serialization::AbstractCC::THE_CC: \
    return swift::AbstractCC::THE_CC;
  CASE(C)
  CASE(ObjCMethod)
  CASE(Freestanding)
  CASE(Method)
#undef CASE
  default:
    return Nothing;
  }
}

Type ModuleFile::getType(TypeID TID) {
  if (TID == 0)
    return Type();

  assert(TID <= Types.size() && "invalid decl ID");
  auto &typeOrOffset = Types[TID-1];

  if (typeOrOffset.is<Type>())
    return typeOrOffset.get<Type>();

  DeclTypeCursor.JumpToBit(typeOrOffset.get<BitOffset>());
  auto entry = DeclTypeCursor.advance();

  if (entry.Kind != llvm::BitstreamEntry::Record) {
    // We don't know how to serialize types represented by sub-blocks.
    error();
    return nullptr;
  }

#ifndef NDEBUG
  assert(typeOrOffset.get<BitOffset>() != 0 &&
         "this type is already being deserialized");
  typeOrOffset = BitOffset(0);
#endif

  ASTContext &ctx = ModuleContext->Ctx;

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch, &blobData);

  switch (recordID) {
  case decls_block::NAME_ALIAS_TYPE: {
    DeclID underlyingID;
    decls_block::NameAliasTypeLayout::readRecord(scratch, underlyingID);
    auto alias = dyn_cast_or_null<TypeAliasDecl>(getDecl(underlyingID));
    if (!alias) {
      error();
      return nullptr;
    }
    typeOrOffset = alias->getDeclaredType();
    break;
  }

  case decls_block::STRUCT_TYPE: {
    DeclID structID;
    TypeID parentID;
    decls_block::StructTypeLayout::readRecord(scratch, structID, parentID);
    typeOrOffset = StructType::get(cast<StructDecl>(getDecl(structID)),
                                   getType(parentID), ctx);
    break;
  }

  case decls_block::PAREN_TYPE: {
    TypeID underlyingID;
    decls_block::ParenTypeLayout::readRecord(scratch, underlyingID);
    typeOrOffset = ParenType::get(ctx, getType(underlyingID));
    break;
  }

  case decls_block::TUPLE_TYPE: {
    // The tuple record itself is empty. Read all trailing elements.
    SmallVector<TupleTypeElt, 8> elements;
    while (true) {
      auto entry = DeclTypeCursor.advance();
      if (entry.Kind != llvm::BitstreamEntry::Record)
        break;

      scratch.clear();
      unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch,
                                                    &blobData);
      if (recordID != decls_block::TUPLE_TYPE_ELT)
        break;

      IdentifierID nameID;
      TypeID typeID;
      TypeID varargBaseID;
      decls_block::TupleTypeEltLayout::readRecord(scratch, nameID, typeID,
                                                  varargBaseID);

      {
        BCOffsetRAII restoreOffset(DeclTypeCursor);
        elements.push_back({getType(typeID), getIdentifier(nameID),
                            /*initializer=*/nullptr, getType(varargBaseID)});
      }
    }

    typeOrOffset = TupleType::get(elements, ctx);
    break;
  }

  case decls_block::IDENTIFIER_TYPE: {
    TypeID mappedID;
    decls_block::IdentifierTypeLayout::readRecord(scratch, mappedID);
    // FIXME: Actually recreate the IdentifierType instead of just aliasing the
    // underlying mapped type.
    typeOrOffset = getType(mappedID);
    break;
  }

  case decls_block::FUNCTION_TYPE: {
    TypeID inputID;
    TypeID resultID;
    uint8_t rawCallingConvention;
    bool autoClosure;
    bool thin;
    bool blockCompatible;

    decls_block::FunctionTypeLayout::readRecord(scratch, inputID, resultID,
                                                rawCallingConvention,
                                                autoClosure, thin,
                                                blockCompatible);
    auto callingConvention = getActualCC(rawCallingConvention);
    if (!callingConvention.hasValue()) {
      error();
      return nullptr;
    }

    typeOrOffset = FunctionType::get(getType(inputID), getType(resultID),
                                     autoClosure, blockCompatible, thin,
                                     callingConvention.getValue(), ctx);
    break;
  }

  case decls_block::METATYPE_TYPE: {
    TypeID instanceID;
    decls_block::MetaTypeTypeLayout::readRecord(scratch, instanceID);
    typeOrOffset = MetaTypeType::get(getType(instanceID), ctx);
    break;
  }

  case decls_block::LVALUE_TYPE: {
    TypeID objectTypeID;
    bool isImplicit, isNonSettable;
    decls_block::LValueTypeLayout::readRecord(scratch, objectTypeID,
                                              isImplicit, isNonSettable);
    LValueType::Qual quals;
    if (isImplicit)
      quals |= LValueType::Qual::Implicit;
    if (isNonSettable)
      quals |= LValueType::Qual::NonSettable;

    typeOrOffset = LValueType::get(getType(objectTypeID), quals, ctx);
    break;
  }

  case decls_block::PROTOCOL_TYPE: {
    DeclID declID;
    decls_block::ProtocolTypeLayout::readRecord(scratch, declID);
    auto proto = cast<ProtocolDecl>(getDecl(declID));
    typeOrOffset = proto->getDeclaredType();
    break;
  }

  case decls_block::ARCHETYPE_TYPE: {
    IdentifierID nameID;
    bool isPrimary;
    TypeID parentOrIndex;
    TypeID superclassID;
    ArrayRef<uint64_t> rawConformanceIDs;

    decls_block::ArchetypeTypeLayout::readRecord(scratch, nameID, isPrimary,
                                                 parentOrIndex, superclassID,
                                                 rawConformanceIDs);

    ArchetypeType *parent = nullptr;
    Optional<unsigned> index;
    if (isPrimary)
      index = parentOrIndex;
    else
      parent = getType(parentOrIndex)->castTo<ArchetypeType>();

    SmallVector<ProtocolDecl *, 4> conformances;
    for (DeclID protoID : rawConformanceIDs)
      conformances.push_back(cast<ProtocolDecl>(getDecl(protoID)));

    auto archetype = ArchetypeType::getNew(ctx, parent, getIdentifier(nameID),
                                           conformances, getType(superclassID),
                                           index);
    typeOrOffset = archetype;
    break;
  }

  case decls_block::PROTOCOL_COMPOSITION_TYPE: {
    ArrayRef<uint64_t> rawProtocolIDs;

    decls_block::ProtocolCompositionTypeLayout::readRecord(scratch,
                                                           rawProtocolIDs);
    SmallVector<Type, 4> protocols;
    for (TypeID protoID : rawProtocolIDs)
      protocols.push_back(getType(protoID));

    auto composition = ProtocolCompositionType::get(ctx, protocols);
    typeOrOffset = composition;
    break;
  }

  default:
    // We don't know how to deserialize this kind of type.
    error();
    return nullptr;
  }
  
  return typeOrOffset.get<Type>();
}

ModuleFile::ModuleFile(llvm::OwningPtr<llvm::MemoryBuffer> &&input)
  : ModuleContext(nullptr),
    InputFile(std::move(input)),
    InputReader(reinterpret_cast<const uint8_t *>(InputFile->getBufferStart()),
                reinterpret_cast<const uint8_t *>(InputFile->getBufferEnd())),
    Status(ModuleStatus::Valid) {
  llvm::BitstreamCursor cursor{InputReader};

  for (unsigned char byte : SIGNATURE) {
    if (cursor.AtEndOfStream() || cursor.Read(8) != byte)
      return error();
  }

  // Future-proofing: make sure we validate the control block before we try to
  // read any other blocks.
  bool hasValidControlBlock = false;
  SmallVector<uint64_t, 64> scratch;

  auto topLevelEntry = cursor.advance();
  while (topLevelEntry.Kind == llvm::BitstreamEntry::SubBlock) {
    switch (topLevelEntry.ID) {
    case llvm::bitc::BLOCKINFO_BLOCK_ID:
      if (cursor.ReadBlockInfoBlock())
        return error();
      break;

    case CONTROL_BLOCK_ID: {
      cursor.EnterSubBlock(CONTROL_BLOCK_ID);

      ModuleStatus err = validateControlBlock(cursor, scratch);
      if (err != ModuleStatus::Valid)
        return error(err);

      hasValidControlBlock = true;
      break;
    }

    case INPUT_BLOCK_ID: {
      if (!hasValidControlBlock)
        return error();

      cursor.EnterSubBlock(INPUT_BLOCK_ID);

      auto next = cursor.advance();
      while (next.Kind == llvm::BitstreamEntry::Record) {
        scratch.clear();
        StringRef blobData;
        unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
        switch (kind) {
        case input_block::SOURCE_FILE:
          assert(scratch.empty());
          SourcePaths.push_back(blobData);
          break;
        case input_block::IMPORTED_MODULE:
          assert(scratch.empty());
          Dependencies.push_back(blobData);
          break;
        default:
          // Unknown input kind, possibly for use by a future version of the
          // module format.
          // FIXME: Should we warn about this?
          break;
        }

        next = cursor.advance();
      }

      if (next.Kind != llvm::BitstreamEntry::EndBlock)
        return error();

      break;
    }

    case DECLS_AND_TYPES_BLOCK_ID: {
      if (!hasValidControlBlock)
        return error();

      // The decls-and-types block is lazily loaded. Save the cursor and load
      // any abbrev records at the start of the block.
      DeclTypeCursor = cursor;
      DeclTypeCursor.EnterSubBlock(DECLS_AND_TYPES_BLOCK_ID);
      if (DeclTypeCursor.advance().Kind == llvm::BitstreamEntry::Error)
        return error();

      // With the main cursor, skip over the block and continue.
      if (cursor.SkipBlock())
        return error();
      break;
    }

    case IDENTIFIER_DATA_BLOCK_ID: {
      if (!hasValidControlBlock)
        return error();

      cursor.EnterSubBlock(IDENTIFIER_DATA_BLOCK_ID);

      auto next = cursor.advanceSkippingSubblocks();
      while (next.Kind == llvm::BitstreamEntry::Record) {
        scratch.clear();
        StringRef blobData;
        unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);

        switch (kind) {
        case identifier_block::IDENTIFIER_DATA:
          assert(scratch.empty());
          IdentifierData = blobData;
          break;
        default:
          // Unknown identifier data, which this version of the compiler won't
          // use.
          break;
        }

        next = cursor.advanceSkippingSubblocks();
      }

      if (next.Kind != llvm::BitstreamEntry::EndBlock)
        return error();

      break;
    }

    case INDEX_BLOCK_ID: {
      if (!hasValidControlBlock)
        return error();

      cursor.EnterSubBlock(INDEX_BLOCK_ID);

      auto next = cursor.advanceSkippingSubblocks();
      while (next.Kind == llvm::BitstreamEntry::Record) {
        scratch.clear();
        StringRef blobData;
        unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);

        switch (kind) {
        case index_block::DECL_OFFSETS:
          assert(blobData.empty());
          Decls.assign(scratch.begin(), scratch.end());
          break;
        case index_block::TYPE_OFFSETS:
          assert(blobData.empty());
          Types.assign(scratch.begin(), scratch.end());
          break;
        case index_block::IDENTIFIER_OFFSETS:
          assert(blobData.empty());
          Identifiers.assign(scratch.begin(), scratch.end());
          break;
        case index_block::TOP_LEVEL_DECLS:
          assert(blobData.empty());
          RawTopLevelIDs.assign(scratch.begin(), scratch.end());
          break;
        default:
          // Unknown index kind, which this version of the compiler won't use.
          break;
        }

        next = cursor.advanceSkippingSubblocks();
      }

      if (next.Kind != llvm::BitstreamEntry::EndBlock)
        return error();

      break;
    }

    case FALL_BACK_TO_TRANSLATION_UNIT_ID:
      // This is a bring-up hack and will eventually go away.
      Status = ModuleStatus::FallBackToTranslationUnit;
      break;

    default:
      // Unknown top-level block, possibly for use by a future version of the
      // module format.
      if (cursor.SkipBlock())
        return error();
      break;
    }
    
    topLevelEntry = cursor.advance(llvm::BitstreamCursor::AF_DontPopBlockAtEnd);
  }
  
  if (topLevelEntry.Kind != llvm::BitstreamEntry::EndBlock)
    return error();
}

bool ModuleFile::associateWithModule(Module *module) {
  assert(!ModuleContext && "already associated with an AST module");
  assert(Status == ModuleStatus::Valid && "invalid module file");

  ASTContext &ctx = module->Ctx;
  bool missingDependency = false;
  for (auto &dependency : Dependencies) {
    assert(!dependency.Mod && "already loaded?");
    Identifier ID = ctx.getIdentifier(dependency.Name);
    // FIXME: Provide a proper source location.
    dependency.Mod = ctx.getModule(std::make_pair(ID, SourceLoc()), false);
    if (!dependency.Mod)
      missingDependency = true;
  }

  if (missingDependency) {
    error(ModuleStatus::MissingDependency);
    return false;
  }

  ModuleContext = module;
  return true;
}

void ModuleFile::buildTopLevelDeclMap() {
  // FIXME: be more lazy about deserialization by encoding this some other way.
  for (DeclID ID : RawTopLevelIDs) {
    auto value = cast<ValueDecl>(getDecl(ID));
    TopLevelIDs[value->getName()] = ID;
  }

  RawTopLevelIDs.clear();
}

void ModuleFile::lookupValue(Identifier name,
                             SmallVectorImpl<ValueDecl*> &results) {
  if (!RawTopLevelIDs.empty())
    buildTopLevelDeclMap();

  if (DeclID ID = TopLevelIDs.lookup(name))
    results.push_back(cast<ValueDecl>(getDecl(ID)));
}
