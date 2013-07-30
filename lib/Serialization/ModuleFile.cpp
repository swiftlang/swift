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
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Serialization/BCReadingExtras.h"
#include "llvm/Support/MemoryBuffer.h"
#include <functional>

using namespace swift;
using namespace swift::serialization;

static constexpr const auto AF_DontPopBlockAtEnd =
  llvm::BitstreamCursor::AF_DontPopBlockAtEnd;

using ConformancePair = std::pair<ProtocolDecl *, ProtocolConformance *>;

/// Declares a member \c type that is a std::function compatible with the given
/// callable type.
///
/// \tparam T A callable type, such as a lambda.
template <typename T>
struct as_function : public as_function<decltype(&T::operator())> {};

template <typename L, typename R, typename... Args>
struct as_function<R(L::*)(Args...) const> {
  using type = std::function<R(Args...)>;
};

/// Returns a std::function that can call a void-returning, single-argument
/// lambda without copying it.
template <typename L>
static typename as_function<L>::type makeStackLambda(const L &theLambda) {
  return std::cref(theLambda);
}

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

/// Translate from the serialization DefaultArgumentKind enumerators, which are
/// guaranteed to be stable, to the AST ones.
static Optional<swift::DefaultArgumentKind>
getActualDefaultArgKind(uint8_t raw) {
  switch (static_cast<serialization::DefaultArgumentKind>(raw)) {
  case serialization::DefaultArgumentKind::None:
    return swift::DefaultArgumentKind::None;
  case serialization::DefaultArgumentKind::Normal:
    return swift::DefaultArgumentKind::Normal;
  case serialization::DefaultArgumentKind::Column:
    return swift::DefaultArgumentKind::Column;
  case serialization::DefaultArgumentKind::File:
    return swift::DefaultArgumentKind::File;
  case serialization::DefaultArgumentKind::Line:
    return swift::DefaultArgumentKind::Line;
  }
  return Nothing;
}

Pattern *ModuleFile::maybeReadPattern() {
  using namespace decls_block;
  
  SmallVector<uint64_t, 8> scratch;

  auto next = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
  if (next.Kind != llvm::BitstreamEntry::Record)
    return nullptr;

  unsigned kind = DeclTypeCursor.readRecord(next.ID, scratch);
  switch (kind) {
  case decls_block::PAREN_PATTERN: {
    bool isImplicit;
    ParenPatternLayout::readRecord(scratch, isImplicit);

    Pattern *subPattern = maybeReadPattern();
    assert(subPattern);

    auto result = new (ModuleContext->Ctx) ParenPattern(SourceLoc(),
                                                        subPattern,
                                                        SourceLoc(),
                                                        isImplicit);
    result->setType(subPattern->getType());
    return result;
  }
  case decls_block::TUPLE_PATTERN: {
    TypeID tupleTypeID;
    unsigned count;
    bool hasVararg;
    bool isImplicit;

    TuplePatternLayout::readRecord(scratch, tupleTypeID, count, hasVararg,
                                   isImplicit);

    SmallVector<TuplePatternElt, 8> elements;
    for ( ; count > 0; --count) {
      scratch.clear();
      next = DeclTypeCursor.advance();
      assert(next.Kind == llvm::BitstreamEntry::Record);

      kind = DeclTypeCursor.readRecord(next.ID, scratch);
      assert(kind == decls_block::TUPLE_PATTERN_ELT);

      // FIXME: Add something for this record or remove it.
      uint8_t rawDefaultArg;
      TuplePatternEltLayout::readRecord(scratch, rawDefaultArg);

      Pattern *subPattern = maybeReadPattern();
      assert(subPattern);

      // Decode the default argument kind.
      // FIXME: Default argument expression, if available.
      swift::DefaultArgumentKind defaultArgKind
        = swift::DefaultArgumentKind::None;
      if (auto defaultArg = getActualDefaultArgKind(rawDefaultArg))
        defaultArgKind = *defaultArg;

      elements.push_back(TuplePatternElt(subPattern, nullptr,
                                         defaultArgKind));
    }

    auto result = TuplePattern::create(ModuleContext->Ctx, SourceLoc(),
                                       elements, SourceLoc(), hasVararg,
                                       SourceLoc(), isImplicit);
    result->setType(getType(tupleTypeID));
    return result;
  }
  case decls_block::NAMED_PATTERN: {
    DeclID varID;
    bool isImplicit;
    NamedPatternLayout::readRecord(scratch, varID, isImplicit);

    auto var = cast<VarDecl>(getDecl(varID));
    auto result = new (ModuleContext->Ctx) NamedPattern(var, isImplicit);
    if (var->hasType())
      result->setType(var->getType());
    return result;
  }
  case decls_block::ANY_PATTERN: {
    TypeID typeID;
    bool isImplicit;

    AnyPatternLayout::readRecord(scratch, typeID, isImplicit);
    auto result = new (ModuleContext->Ctx) AnyPattern(SourceLoc(), isImplicit);
    result->setType(getType(typeID));
    return result;
  }
  case decls_block::TYPED_PATTERN: {
    TypeID typeID;
    bool isImplicit;

    TypedPatternLayout::readRecord(scratch, typeID, isImplicit);
    Pattern *subPattern = maybeReadPattern();
    assert(subPattern);

    TypeLoc typeInfo = TypeLoc::withoutLoc(getType(typeID));
    auto result = new (ModuleContext->Ctx) TypedPattern(subPattern, typeInfo,
                                                        isImplicit);
    result->setType(typeInfo.getType());
    return result;
  }
  default:
    return nullptr;
  }
}

Optional<ConformancePair> ModuleFile::maybeReadConformance(Type conformingType){
  using namespace decls_block;

  BCOffsetRAII lastRecordOffset(DeclTypeCursor);
  SmallVector<uint64_t, 16> scratch;

  auto next = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
  if (next.Kind != llvm::BitstreamEntry::Record)
    return Nothing;

  unsigned kind = DeclTypeCursor.readRecord(next.ID, scratch);
  if (kind != PROTOCOL_CONFORMANCE && kind != NO_CONFORMANCE)
    return Nothing;

  lastRecordOffset.reset();
  if (kind == NO_CONFORMANCE) {
    DeclID protoID;
    NoConformanceLayout::readRecord(scratch, protoID);
    return std::make_pair(cast<ProtocolDecl>(getDecl(protoID)), nullptr);
  }

  DeclID protoID, conformingDeclID;
  unsigned valueCount, typeCount, inheritedCount, defaultedCount;
  ArrayRef<uint64_t> rawIDs;

  ProtocolConformanceLayout::readRecord(scratch, protoID, conformingDeclID,
                                        valueCount, typeCount,
                                        inheritedCount, defaultedCount,
                                        rawIDs);

  InheritedConformanceMap inheritedConformances;

  while (inheritedCount--) {
    auto inherited = maybeReadConformance(conformingType);
    assert(inherited.hasValue());

    inheritedConformances.insert(inherited.getValue());
  }

  ASTContext &ctx = ModuleContext->Ctx;

  auto proto = cast<ProtocolDecl>(getDecl(protoID));
  // FIXME: Deserialize the conforming decl. This currently asserts out if the
  // conforming decl is an ExtensionDecl, with "cannot cross-reference this
  // kind of decl".
  // auto conformingDecl = getDecl(conformingDeclID);
  Decl *conformingDecl = nullptr;

  WitnessMap witnesses;
  ArrayRef<uint64_t>::iterator rawIDIter = rawIDs.begin();
  while (valueCount--) {
    auto first = cast<ValueDecl>(getDecl(*rawIDIter++));
    auto second = cast<ValueDecl>(getDecl(*rawIDIter++));
    unsigned substitutionCount = *rawIDIter++;

    SmallVector<Substitution, 8> substitutions;
    while (substitutionCount--) {
      auto sub = maybeReadSubstitution();
      assert(sub.hasValue());
      substitutions.push_back(sub.getValue());
    }

    ProtocolConformanceWitness witness{second, ctx.AllocateCopy(substitutions)};
    witnesses.insert(std::make_pair(first, witness));
  }
  assert(rawIDIter <= rawIDs.end() && "read too much");

  TypeWitnessMap typeWitnesses;
  while (typeCount--) {
    // FIXME: We don't actually want to allocate an archetype here; we just
    // want to get an access path within the protocol.
    auto first = cast<TypeAliasDecl>(getDecl(*rawIDIter++));
    auto second = maybeReadSubstitution();
    assert(second.hasValue());
    typeWitnesses[first] = *second;
  }
  assert(rawIDIter <= rawIDs.end() && "read too much");

  llvm::SmallVector<ValueDecl *, 4> defaultedDefinitions;
  while (defaultedCount--) {
    auto decl = cast<ValueDecl>(getDecl(*rawIDIter++));
    defaultedDefinitions.push_back(decl);
  }
  assert(rawIDIter <= rawIDs.end() && "read too much");

  // Reset the offset RAII to the end of the trailing records.
  lastRecordOffset.reset();

  std::unique_ptr<ProtocolConformance> conformance(
    new ProtocolConformance(conformingType,
                            proto,
                            conformingDecl,
                            std::move(witnesses),
                            std::move(typeWitnesses),
                            std::move(inheritedConformances),
                            defaultedDefinitions));
  return std::make_pair(proto, conformance.release());
}

/// Uniques a single protocol conformance and any inherited conformances, and
/// records the conformances in the ASTContext.
///
/// If a conformance has already been recorded, the given record \p conformance
/// will be deleted.
///
/// \returns The canonical conformance for this (\p decl, \p canTy) pair.
///
/// \sa processConformances
template <typename T>
ProtocolConformance *processConformance(ASTContext &ctx, T decl, CanType canTy,
                                        ProtocolDecl *proto,
                                        ProtocolConformance *conformance) {
  if (!conformance)
    return nullptr;

  for (auto &inherited : conformance->getInheritedConformances()) {
    inherited.second = processConformance(ctx, decl, canTy, inherited.first,
                                          inherited.second);
  }

  auto &conformanceRecord = ctx.ConformsTo[{canTy, proto}];

  if (conformanceRecord && conformanceRecord != conformance) {
    delete conformance;
    conformance = conformanceRecord;
  } else {
    conformanceRecord = conformance;
  }

  if (decl)
    ctx.recordConformance(proto, decl);

  return conformance;
}

/// Uniques and applies protocol conformances to a decl, as well as recording
/// the conformances in the ASTContext.
template <typename T>
void processConformances(ASTContext &ctx, T *decl, CanType canTy,
                         ArrayRef<ConformancePair> conformances) {
  SmallVector<ProtocolDecl *, 16> protoBuf;
  SmallVector<ProtocolConformance *, 16> conformanceBuf;
  for (auto conformancePair : conformances) {
    auto proto = conformancePair.first;
    auto conformance = conformancePair.second;
    conformance = processConformance(ctx, decl, canTy, proto, conformance);

    protoBuf.push_back(proto);
    conformanceBuf.push_back(conformance);
  }

  decl->setProtocols(ctx.AllocateCopy(protoBuf));
  decl->setConformances(ctx.AllocateCopy(conformanceBuf));
}


Optional<Substitution> ModuleFile::maybeReadSubstitution() {
  BCOffsetRAII lastRecordOffset(DeclTypeCursor);

  auto entry = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind != llvm::BitstreamEntry::Record)
    return Nothing;

  StringRef blobData;
  SmallVector<uint64_t, 2> scratch;
  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch,
                                                &blobData);
  if (recordID != decls_block::BOUND_GENERIC_SUBSTITUTION)
    return Nothing;


  TypeID archetypeID, replacementID;
  unsigned numConformances;
  decls_block::BoundGenericSubstitutionLayout::readRecord(scratch,
                                                          archetypeID,
                                                          replacementID,
                                                          numConformances);

  auto archetypeTy = getType(archetypeID)->castTo<ArchetypeType>();
  auto replacementTy = getType(replacementID);

  CanType canReplTy = replacementTy->getCanonicalType();
  ASTContext &ctx = ModuleContext->Ctx;

  SmallVector<ProtocolConformance *, 16> conformanceBuf;
  while (numConformances--) {
    auto conformancePair = maybeReadConformance(replacementTy);
    assert(conformancePair.hasValue() && "Missing conformance");
    auto conformance = processConformance(ctx, nullptr, canReplTy,
                                          conformancePair->first,
                                          conformancePair->second);
    conformanceBuf.push_back(conformance);
  }

  lastRecordOffset.reset();
  return Substitution{archetypeTy, replacementTy,
                      ctx.AllocateCopy(conformanceBuf)};
}

GenericParamList *ModuleFile::maybeReadGenericParams(DeclContext *DC) {
  using namespace decls_block;

  assert(DC && "need a context for the decls in the list");

  BCOffsetRAII lastRecordOffset(DeclTypeCursor);
  SmallVector<uint64_t, 8> scratch;
  StringRef blobData;

  auto next = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
  if (next.Kind != llvm::BitstreamEntry::Record)
    return nullptr;

  unsigned kind = DeclTypeCursor.readRecord(next.ID, scratch, &blobData);

  if (kind != GENERIC_PARAM_LIST)
    return nullptr;

  ArrayRef<uint64_t> rawArchetypeIDs;
  GenericParamListLayout::readRecord(scratch, rawArchetypeIDs);

  SmallVector<ArchetypeType *, 8> archetypes;
  for (TypeID next : rawArchetypeIDs)
    archetypes.push_back(getType(next)->castTo<ArchetypeType>());

  SmallVector<GenericParam, 8> params;
  SmallVector<Requirement, 8> requirements;
  while (true) {
    lastRecordOffset.reset();
    bool shouldContinue = true;

    auto entry = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
    if (entry.Kind != llvm::BitstreamEntry::Record)
      break;

    scratch.clear();
    unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch,
                                                  &blobData);
    switch (recordID) {
    case GENERIC_PARAM: {
      DeclID paramDeclID;
      GenericParamLayout::readRecord(scratch, paramDeclID);
      auto typeAlias = cast<TypeAliasDecl>(getDecl(paramDeclID, DC));
      params.push_back(GenericParam(typeAlias));
      break;
    }
    case GENERIC_REQUIREMENT: {
      uint8_t rawKind;
      ArrayRef<uint64_t> rawTypeIDs;
      GenericRequirementLayout::readRecord(scratch, rawKind, rawTypeIDs);

      switch (rawKind) {
      case GenericRequirementKind::Conformance: {
        assert(rawTypeIDs.size() == 2);
        auto subject = TypeLoc::withoutLoc(getType(rawTypeIDs[0]));
        auto constraint = TypeLoc::withoutLoc(getType(rawTypeIDs[1]));

        requirements.push_back(Requirement::getConformance(subject,
                                                           SourceLoc(),
                                                           constraint));
        break;
      }
      case GenericRequirementKind::SameType: {
        assert(rawTypeIDs.size() == 2);
        auto first = TypeLoc::withoutLoc(getType(rawTypeIDs[0]));
        auto second = TypeLoc::withoutLoc(getType(rawTypeIDs[1]));

        requirements.push_back(Requirement::getSameType(first,
                                                        SourceLoc(),
                                                        second));
        break;
      }
      default:
        // Unknown requirement kind. Drop the requirement and continue, but log
        // an error so that we don't actually try to generate code.
        error();
      }

      break;
    }
    default:
      // This record is not part of the GenericParamList.
      shouldContinue = false;
      break;
    }

    if (!shouldContinue)
      break;
  }

  auto paramList = GenericParamList::create(ModuleContext->Ctx, SourceLoc(),
                                            params, SourceLoc(), requirements,
                                            SourceLoc());
  paramList->setAllArchetypes(ModuleContext->Ctx.AllocateCopy(archetypes));
  paramList->setOuterParameters(DC->getGenericParamsOfContext());

  return paramList;
}

Optional<MutableArrayRef<Decl *>> ModuleFile::readMembers() {
  using namespace decls_block;

  auto entry = DeclTypeCursor.advance();
  if (entry.Kind != llvm::BitstreamEntry::Record)
    return Nothing;

  SmallVector<uint64_t, 16> memberIDBuffer;

  unsigned kind = DeclTypeCursor.readRecord(entry.ID, memberIDBuffer);
  assert(kind == DECL_CONTEXT);
  (void)kind;

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
    assert(*nextMember && "unable to deserialize next member");
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

Module *ModuleFile::getModule(Identifier name) {
  if (name.empty())
    return ModuleContext->Ctx.TheBuiltinModule;

  // FIXME: duplicated from NameBinder::getModule
  // FIXME: provide a real source location.
  if (name == ModuleContext->Name) {
    if (!ShadowedModule) {
      auto importer = ModuleContext->Ctx.getClangModuleLoader();
      assert(importer && "no way to import shadowed module (recursive xref?)");
      ShadowedModule = importer->loadModule(SourceLoc(),
                                            std::make_pair(name, SourceLoc()),
                                            false);
      assert(ShadowedModule && "missing shadowed module");
    }

    return ShadowedModule;
  }

  // FIXME: provide a real source location.
  return ModuleContext->Ctx.getModule(std::make_pair(name, SourceLoc()),
                                      false);
}


/// Translate from the Serialization assocativity enum values to the AST
/// strongly-typed enum.
///
/// The former is guaranteed to be stable, but may not reflect this version of
/// the AST.
static Optional<swift::Associativity> getActualAssociativity(uint8_t assoc) {
  switch (assoc) {
  case serialization::Associativity::LeftAssociative:
    return swift::Associativity::Left;
  case serialization::Associativity::RightAssociative:
    return swift::Associativity::Right;
  case serialization::Associativity::NonAssociative:
    return swift::Associativity::None;
  default:
    return Nothing;
  }
}

Decl *ModuleFile::getDecl(DeclID DID, Optional<DeclContext *> ForcedContext,
                          Optional<std::function<void(Decl*)>> DidRecord) {
  if (DID == 0)
    return nullptr;

  assert(DID <= Decls.size() && "invalid decl ID");
  auto &declOrOffset = Decls[DID-1];

  if (declOrOffset.isComplete()) {
    if (DidRecord)
      (*DidRecord)(declOrOffset);
    return declOrOffset;
  }

  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(declOrOffset);
  auto entry = DeclTypeCursor.advance();

  if (entry.Kind != llvm::BitstreamEntry::Record) {
    // We don't know how to serialize decls represented by sub-blocks.
    error();
    return nullptr;
  }

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
    TypeID superclassTypeID;

    decls_block::TypeAliasLayout::readRecord(scratch, nameID, contextID,
                                             underlyingTypeID,
                                             isGeneric, isImplicit,
                                             superclassTypeID);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);

    if (declOrOffset.isComplete())
      break;

    auto underlyingType = TypeLoc::withoutLoc(getType(underlyingTypeID));

    auto alias = new (ctx) TypeAliasDecl(SourceLoc(), getIdentifier(nameID),
                                         SourceLoc(), underlyingType,
                                         DC, { });
    declOrOffset = alias;

    if (isImplicit)
      alias->setImplicit();
    if (isGeneric) {
      alias->setGenericParameter();
      alias->setSuperclass(getType(superclassTypeID));
    }

    CanType canBaseTy = underlyingType.getType()->getCanonicalType();

    SmallVector<ConformancePair, 16> conformances;
    while (auto conformance = maybeReadConformance(underlyingType.getType()))
      conformances.push_back(*conformance);
    processConformances(ctx, alias, canBaseTy, conformances);

    break;
  }

  case decls_block::STRUCT_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;

    decls_block::StructLayout::readRecord(scratch, nameID, contextID,
                                          isImplicit);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    auto genericParams = maybeReadGenericParams(DC);

    auto theStruct = new (ctx) StructDecl(SourceLoc(), getIdentifier(nameID),
                                          SourceLoc(), { }, genericParams, DC);
    declOrOffset = theStruct;
    if (DidRecord) {
      (*DidRecord)(theStruct);
      DidRecord.reset();
    }

    if (isImplicit)
      theStruct->setImplicit();
    if (genericParams)
      for (auto &genericParam : *theStruct->getGenericParams())
        genericParam.getAsTypeParam()->setDeclContext(theStruct);

    CanType canTy = theStruct->getDeclaredTypeInContext()->getCanonicalType();

    SmallVector<ConformancePair, 16> conformances;
    while (auto conformance = maybeReadConformance(canTy))
      conformances.push_back(*conformance);
    processConformances(ctx, theStruct, canTy, conformances);

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
    auto parent = getDeclContext(parentID);
    if (declOrOffset.isComplete())
      break;

    auto thisDecl = cast<VarDecl>(getDecl(implicitThisID, nullptr));
    auto genericParams = maybeReadGenericParams(parent);

    auto ctor = new (ctx) ConstructorDecl(ctx.getIdentifier("constructor"),
                                          SourceLoc(), /*args=*/nullptr,
                                          thisDecl, genericParams, parent);
    declOrOffset = ctor;
    thisDecl->setDeclContext(ctor);

    Pattern *args = maybeReadPattern();
    assert(args && "missing arguments for constructor");
    ctor->setArguments(args);

    // This must be set after recording the constructor in the map.
    // A polymorphic constructor type needs to refer to the constructor to get
    // its generic parameters.
    ctor->setType(getType(signatureID));

    if (isImplicit)
      ctor->setImplicit();

    if (genericParams)
      for (auto &genericParam : *ctor->getGenericParams())
        genericParam.getAsTypeParam()->setDeclContext(ctor);

    break;
  }

  case decls_block::VAR_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;
    TypeID typeID;
    DeclID getterID, setterID;
    DeclID overriddenID;

    decls_block::VarLayout::readRecord(scratch, nameID, contextID, isImplicit,
                                       typeID, getterID, setterID,
                                       overriddenID);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);
    auto var = new (ctx) VarDecl(SourceLoc(), getIdentifier(nameID),
                                 getType(typeID), DC);

    declOrOffset = var;

    if (getterID || setterID) {
      var->setProperty(ctx, SourceLoc(),
                       cast_or_null<FuncDecl>(getDecl(getterID)),
                       cast_or_null<FuncDecl>(getDecl(setterID)),
                       SourceLoc());
    }

    if (isImplicit)
      var->setImplicit();

    var->setOverriddenDecl(cast_or_null<VarDecl>(getDecl(overriddenID)));
    break;
  }

  case decls_block::FUNC_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;
    bool isClassMethod;
    bool isAssignmentOrConversion;
    TypeID signatureID;
    DeclID associatedDeclID;
    DeclID overriddenID;

    decls_block::FuncLayout::readRecord(scratch, nameID, contextID, isImplicit,
                                        isClassMethod, isAssignmentOrConversion,
                                        signatureID, associatedDeclID,
                                        overriddenID);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    // Read generic params before reading the type, because the type may
    // reference generic parameters, and we want them to have a dummy
    // DeclContext for now.
    GenericParamList *genericParams = maybeReadGenericParams(DC);

    auto fn = new (ctx) FuncDecl(SourceLoc(), SourceLoc(),
                                 getIdentifier(nameID), SourceLoc(),
                                 genericParams, /*type=*/nullptr,
                                 /*body=*/nullptr, DC);
    declOrOffset = fn;

    // This must be set after recording the constructor in the map.
    // A polymorphic constructor type needs to refer to the constructor to get
    // its generic parameters.
    auto signature = getType(signatureID)->castTo<AnyFunctionType>();
    fn->setType(signature);

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
                                 DC);
    body->setType(signature);
    fn->setBody(body);

    if (genericParams)
      for (auto &genericParam : *fn->getGenericParams())
        genericParam.getAsTypeParam()->setDeclContext(body);

    fn->setOverriddenDecl(cast_or_null<FuncDecl>(getDecl(overriddenID)));

    fn->setStatic(isClassMethod);
    if (isImplicit)
      fn->setImplicit();
    if (!blobData.empty())
      fn->getMutableAttrs().AsmName = ctx.AllocateCopy(blobData);
    if (isAssignmentOrConversion) {
      if (fn->isOperator())
        fn->getMutableAttrs().Assignment = true;
      else
        fn->getMutableAttrs().Conversion = true;
    }

    if (Decl *associated = getDecl(associatedDeclID)) {
      if (auto op = dyn_cast<OperatorDecl>(associated)) {
        fn->setOperatorDecl(op);

        DeclAttributes &attrs = fn->getMutableAttrs();
        if (isa<PrefixOperatorDecl>(op))
          attrs.ExplicitPrefix = true;
        else if (isa<PostfixOperatorDecl>(op))
          attrs.ExplicitPostfix = true;
        // Note that an explicit [infix] is not required.
      }
      // Otherwise, unknown associated decl kind.
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
    ArrayRef<uint64_t> protocolIDs;

    decls_block::ProtocolLayout::readRecord(scratch, nameID, contextID,
                                            isImplicit, protocolIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    auto proto = new (ctx) ProtocolDecl(DC, SourceLoc(), SourceLoc(),
                                        getIdentifier(nameID), { });
    declOrOffset = proto;
    if (DidRecord) {
      (*DidRecord)(proto);
      DidRecord.reset();
    }

    if (isImplicit)
      proto->setImplicit();

    // Deserialize the list of protocols.
    SmallVector<ProtocolDecl *, 4> protocols;
    for (auto protoID : protocolIDs) {
      protocols.push_back(cast<ProtocolDecl>(getDecl(protoID)));
    }
    proto->setProtocols(ctx.AllocateCopy(protocols));

    auto members = readMembers();
    assert(members.hasValue() && "could not read struct members");
    proto->setMembers(members.getValue(), SourceRange());

    break;
  }

  case decls_block::PREFIX_OPERATOR_DECL: {
    IdentifierID nameID;
    DeclID contextID;

    decls_block::PrefixOperatorLayout::readRecord(scratch, nameID, contextID);
    declOrOffset = new (ctx) PrefixOperatorDecl(getDeclContext(contextID),
                                                SourceLoc(), SourceLoc(),
                                                getIdentifier(nameID),
                                                SourceLoc(), SourceLoc(),
                                                SourceLoc());
    break;
  }

  case decls_block::POSTFIX_OPERATOR_DECL: {
    IdentifierID nameID;
    DeclID contextID;

    decls_block::PostfixOperatorLayout::readRecord(scratch, nameID, contextID);
    declOrOffset = new (ctx) PostfixOperatorDecl(getDeclContext(contextID),
                                                 SourceLoc(), SourceLoc(),
                                                 getIdentifier(nameID),
                                                 SourceLoc(), SourceLoc(),
                                                 SourceLoc());
    break;
  }

  case decls_block::INFIX_OPERATOR_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    uint8_t rawAssociativity;
    unsigned precedence;

    decls_block::InfixOperatorLayout::readRecord(scratch, nameID, contextID,
                                                 rawAssociativity, precedence);

    auto associativity = getActualAssociativity(rawAssociativity);
    if (!associativity.hasValue()) {
      error();
      return nullptr;
    }

    InfixData infixData(precedence, associativity.getValue());

    declOrOffset = new (ctx) InfixOperatorDecl(getDeclContext(contextID),
                                               SourceLoc(), SourceLoc(),
                                               getIdentifier(nameID),
                                               SourceLoc(), SourceLoc(),
                                               SourceLoc(), SourceLoc(),
                                               SourceLoc(), SourceLoc(),
                                               SourceLoc(), infixData);
    break;
  }
      
  case decls_block::CLASS_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;
    TypeID superclassID;
    decls_block::ClassLayout::readRecord(scratch, nameID, contextID,
                                         isImplicit, superclassID);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    auto genericParams = maybeReadGenericParams(DC);

    auto theClass = new (ctx) ClassDecl(SourceLoc(), getIdentifier(nameID),
                                        SourceLoc(), { }, genericParams, DC);
    declOrOffset = theClass;
    if (DidRecord) {
      (*DidRecord)(theClass);
      DidRecord.reset();
    }

    if (isImplicit)
      theClass->setImplicit();
    if (superclassID)
      theClass->setSuperclass(getType(superclassID));
    if (genericParams)
      for (auto &genericParam : *theClass->getGenericParams())
        genericParam.getAsTypeParam()->setDeclContext(theClass);

    CanType canTy = theClass->getDeclaredTypeInContext()->getCanonicalType();

    SmallVector<ConformancePair, 16> conformances;
    while (auto conformance = maybeReadConformance(canTy))
      conformances.push_back(*conformance);
    processConformances(ctx, theClass, canTy, conformances);

    auto members = readMembers();
    assert(members.hasValue() && "could not read class members");
    theClass->setMembers(members.getValue(), SourceRange());

    break;
  }

  case decls_block::ONEOF_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;

    decls_block::OneOfLayout::readRecord(scratch, nameID, contextID,
                                         isImplicit);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    auto genericParams = maybeReadGenericParams(DC);

    // FIXME Preserve the isEnum bit.
    auto oneOf = new (ctx) OneOfDecl(SourceLoc(),
                                     /*isEnum*/ false,
                                     getIdentifier(nameID),
                                     SourceLoc(), { },
                                     genericParams, DC);
    declOrOffset = oneOf;
    if (DidRecord) {
      (*DidRecord)(oneOf);
      DidRecord.reset();
    }

    if (isImplicit)
      oneOf->setImplicit();
    if (genericParams)
      for (auto &genericParam : *oneOf->getGenericParams())
        genericParam.getAsTypeParam()->setDeclContext(oneOf);

    CanType canTy = oneOf->getDeclaredTypeInContext()->getCanonicalType();

    SmallVector<ConformancePair, 16> conformances;
    while (auto conformance = maybeReadConformance(canTy))
      conformances.push_back(*conformance);
    processConformances(ctx, oneOf, canTy, conformances);

    auto members = readMembers();
    assert(members.hasValue() && "could not read oneof members");
    oneOf->setMembers(members.getValue(), SourceRange());

    break;
  }

  case decls_block::ONEOF_ELEMENT_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    TypeID argTypeID, resTypeID, ctorTypeID;
    bool isImplicit;

    decls_block::OneOfElementLayout::readRecord(scratch, nameID, contextID,
                                                argTypeID, resTypeID,
                                                ctorTypeID,
                                                isImplicit);

    DeclContext *DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    auto argTy = getType(argTypeID);
    auto resTy = getType(resTypeID);
    auto elem = new (ctx) OneOfElementDecl(SourceLoc(),
                                           SourceLoc(),
                                           getIdentifier(nameID),
                                           TypeLoc::withoutLoc(argTy),
                                           SourceLoc(),
                                           TypeLoc::withoutLoc(resTy),
                                           DC);
    declOrOffset = elem;

    elem->setType(getType(ctorTypeID));
    if (isImplicit)
      elem->setImplicit();

    break;
  }

  case decls_block::SUBSCRIPT_DECL: {
    DeclID contextID;
    bool isImplicit;
    TypeID declTypeID, elemTypeID;
    DeclID getterID, setterID;
    DeclID overriddenID;

    decls_block::SubscriptLayout::readRecord(scratch, contextID, isImplicit,
                                             declTypeID, elemTypeID,
                                             getterID, setterID,
                                             overriddenID);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    Pattern *indices = maybeReadPattern();
    assert(indices);

    auto elemTy = TypeLoc::withoutLoc(getType(elemTypeID));
    auto getter = cast_or_null<FuncDecl>(getDecl(getterID));
    auto setter = cast_or_null<FuncDecl>(getDecl(setterID));

    auto subscript = new (ctx) SubscriptDecl(ctx.getIdentifier("__subscript"),
                                             SourceLoc(), indices, SourceLoc(),
                                             elemTy, SourceRange(),
                                             getter, setter, DC);
    declOrOffset = subscript;

    subscript->setType(getType(declTypeID));
    if (isImplicit)
      subscript->setImplicit();

    auto overriddenDecl = cast_or_null<SubscriptDecl>(getDecl(overriddenID));
    subscript->setOverriddenDecl(overriddenDecl);

    if (getter)
      getter->makeGetter(subscript);
    if (setter)
      setter->makeSetter(subscript);
    break;
  }

  case decls_block::EXTENSION_DECL: {
    TypeID baseID;
    DeclID contextID;
    bool isImplicit;

    decls_block::ExtensionLayout::readRecord(scratch, baseID, contextID,
                                             isImplicit);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    auto baseTy = TypeLoc::withoutLoc(getType(baseID));

    auto extension = new (ctx) ExtensionDecl(SourceLoc(), baseTy, { }, DC);
    declOrOffset = extension;

    if (isImplicit)
      extension->setImplicit();

    CanType canBaseTy = baseTy.getType()->getCanonicalType();

    SmallVector<ConformancePair, 16> conformances;
    while (auto conformance = maybeReadConformance(canBaseTy))
      conformances.push_back(*conformance);
    processConformances(ctx, extension, canBaseTy, conformances);

    auto members = readMembers();
    assert(members.hasValue() && "could not read extension members");
    extension->setMembers(members.getValue(), SourceRange());

    baseTy.getType()->getAnyNominal()->addExtension(extension);
    break;
  }

  case decls_block::DESTRUCTOR_DECL: {
    DeclID parentID;
    bool isImplicit;
    TypeID signatureID;
    DeclID implicitThisID;

    decls_block::DestructorLayout::readRecord(scratch, parentID, isImplicit,
                                              signatureID, implicitThisID);

    DeclContext *parent = getDeclContext(parentID);
    if (declOrOffset.isComplete())
      break;

    auto thisDecl = cast<VarDecl>(getDecl(implicitThisID, nullptr));

    auto dtor = new (ctx) DestructorDecl(ctx.getIdentifier("destructor"),
                                         SourceLoc(), thisDecl, parent);
    declOrOffset = dtor;
    thisDecl->setDeclContext(dtor);

    dtor->setType(getType(signatureID));
    if (isImplicit)
      dtor->setImplicit();

    break;
  }

  case decls_block::XREF: {
    uint8_t kind;
    TypeID expectedTypeID;
    bool isWithinExtension;
    ArrayRef<uint64_t> rawAccessPath;

    decls_block::XRefLayout::readRecord(scratch, kind, expectedTypeID,
                                        isWithinExtension, rawAccessPath);

    // First, find the module this reference is referring to.
    Module *M = getModule(getIdentifier(rawAccessPath.front()));
    assert(M && "missing dependency");
    rawAccessPath = rawAccessPath.slice(1);

    switch (kind) {
    case XRefKind::SwiftValue:
    case XRefKind::SwiftGenericParameter: {
      // Start by looking up the top-level decl in the module.
      Module *baseModule = M;
      if (isWithinExtension) {
        baseModule = getModule(getIdentifier(rawAccessPath.front()));
        rawAccessPath = rawAccessPath.slice(1);
      }

      SmallVector<ValueDecl *, 8> values;
      baseModule->lookupValue(Module::AccessPathTy(),
                              getIdentifier(rawAccessPath.front()),
                              NLKind::QualifiedLookup,
                              values);
      // FIXME: Yuck. The concept of a shadowed module needs to be moved up
      // higher, and it needs to be clear whether they are always reexported.
      if (auto loadedModule = dyn_cast<SerializedModule>(baseModule)) {
        if (loadedModule->File && loadedModule->File->ShadowedModule) {
          Module *shadowed = loadedModule->File->ShadowedModule;
          shadowed->lookupValue(Module::AccessPathTy(),
                                getIdentifier(rawAccessPath.front()),
                                NLKind::QualifiedLookup, values);
        }
      }
      rawAccessPath = rawAccessPath.slice(1);

      // Then, follow the chain of nested ValueDecls until we run out of
      // identifiers in the access path.
      SmallVector<ValueDecl *, 8> baseValues;
      for (IdentifierID nextID : rawAccessPath) {
        baseValues.swap(values);
        values.clear();
        for (auto base : baseValues) {
          if (auto nominal = dyn_cast<NominalTypeDecl>(base)) {
            Identifier memberName = getIdentifier(nextID);
            auto members = nominal->lookupDirect(memberName);
            values.append(members.begin(), members.end());
          }
        }
      }

      // If we have a type to validate against, filter out any ValueDecls that
      // don't match that type.
      CanType expectedTy;
      if (kind == XRefKind::SwiftValue)
        if (Type maybeExpectedTy = getType(expectedTypeID))
          expectedTy = maybeExpectedTy->getCanonicalType();

      ValueDecl *result = nullptr;
      for (auto value : values) {
        if (!value->hasClangNode() && value->getModuleContext() != M)
          continue;
        if (expectedTy && value->getType()->getCanonicalType() != expectedTy)
          continue;

        if (!result || result == value) {
          result = value;
          continue;
        }

        // It's an error if more than one value has the same type.
        // FIXME: Functions and constructors can overload based on parameter
        // names.
        error();
        return nullptr;
      }

      // It's an error if lookup doesn't actually find anything -- that means
      // the module's out of date.
      if (!result) {
        error();
        return nullptr;
      }

      if (kind == XRefKind::SwiftGenericParameter) {
        GenericParamList *paramList = nullptr;

        if (auto nominal = dyn_cast<NominalTypeDecl>(result))
          paramList = nominal->getGenericParams();
        else if (auto fn = dyn_cast<FuncDecl>(result))
          paramList = fn->getGenericParams();
        else if (auto ctor = dyn_cast<ConstructorDecl>(result))
          paramList = ctor->getGenericParams();

        if (!paramList) {
          error();
          return nullptr;
        }

        if (expectedTypeID >= paramList->size()) {
          error();
          return nullptr;
        }

        result = paramList->getParams()[expectedTypeID].getDecl();
        assert(result);
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

  if (DidRecord)
    (*DidRecord)(declOrOffset);
  return declOrOffset;
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

/// Translate from the serialization Ownership enumerators, which are
/// guaranteed to be stable, to the AST ones.
static
Optional<swift::Ownership> getActualOwnership(serialization::Ownership raw) {
  switch (raw) {
  case serialization::Ownership::Strong:  return swift::Ownership::Strong;
  case serialization::Ownership::Unowned: return swift::Ownership::Unowned;
  case serialization::Ownership::Weak:    return swift::Ownership::Weak;
  }
  return Nothing;
}

Type ModuleFile::getType(TypeID TID) {
  if (TID == 0)
    return Type();

  assert(TID <= Types.size() && "invalid decl ID");
  auto &typeOrOffset = Types[TID-1];

  if (typeOrOffset.isComplete())
    return typeOrOffset;

  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(typeOrOffset);
  auto entry = DeclTypeCursor.advance();

  if (entry.Kind != llvm::BitstreamEntry::Record) {
    // We don't know how to serialize types represented by sub-blocks.
    error();
    return nullptr;
  }

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

  case decls_block::NOMINAL_TYPE: {
    DeclID declID;
    TypeID parentID;
    decls_block::NominalTypeLayout::readRecord(scratch, declID, parentID);

    Type parentTy = getType(parentID);

    // Record the type as soon as possible. Members of a nominal type often
    // try to refer back to the type.
    getDecl(declID, Nothing, makeStackLambda([&](Decl *D) {
      // FIXME: Hack for "typedef struct CGRect CGRect". In the long run we need
      // something less brittle that would also handle pointer typedefs and
      // typedefs that just /happen/ to match a tagged name but don't actually
      // point to the tagged type.
      if (auto alias = dyn_cast<TypeAliasDecl>(D))
        D = alias->getUnderlyingType()->getAnyNominal();
      auto nominal = cast<NominalTypeDecl>(D);
      typeOrOffset = NominalType::get(nominal, parentTy, ctx);
    }));

    assert(typeOrOffset.isComplete());
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
      auto entry = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
      if (entry.Kind != llvm::BitstreamEntry::Record)
        break;

      scratch.clear();
      unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch,
                                                    &blobData);
      if (recordID != decls_block::TUPLE_TYPE_ELT)
        break;

      IdentifierID nameID;
      TypeID typeID;
      uint8_t rawDefArg;
      bool isVararg;
      decls_block::TupleTypeEltLayout::readRecord(scratch, nameID, typeID,
                                                  rawDefArg, isVararg);

      DefaultArgumentKind defArg = DefaultArgumentKind::None;
      if (auto actualDefArg = getActualDefaultArgKind(rawDefArg))
        defArg = *actualDefArg;
      elements.push_back({getType(typeID), getIdentifier(nameID), defArg,
                          isVararg});
    }

    typeOrOffset = TupleType::get(elements, ctx);
    break;
  }

  case decls_block::FUNCTION_TYPE: {
    TypeID inputID;
    TypeID resultID;
    uint8_t rawCallingConvention;
    bool autoClosure;
    bool thin;
    bool noreturn;
    bool blockCompatible;

    decls_block::FunctionTypeLayout::readRecord(scratch, inputID, resultID,
                                                rawCallingConvention,
                                                autoClosure, thin,
                                                noreturn,
                                                blockCompatible);
    auto callingConvention = getActualCC(rawCallingConvention);
    if (!callingConvention.hasValue()) {
      error();
      return nullptr;
    }

    auto Info = FunctionType::ExtInfo(callingConvention.getValue(),
                                      thin,
                                      noreturn,
                                      autoClosure,
                                      blockCompatible);
    
    typeOrOffset = FunctionType::get(getType(inputID), getType(resultID),
                                     Info, ctx);
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

  case decls_block::REFERENCE_STORAGE_TYPE: {
    uint8_t rawOwnership;
    TypeID referentTypeID;
    decls_block::ReferenceStorageTypeLayout::readRecord(scratch, rawOwnership,
                                                        referentTypeID);

    auto ownership =
      getActualOwnership((serialization::Ownership) rawOwnership);
    if (!ownership.hasValue()) {
      error();
      break;
    }

    typeOrOffset = ReferenceStorageType::get(getType(referentTypeID),
                                             ownership.getValue(), ctx);
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
    Type superclass;
    Optional<unsigned> index;
    SmallVector<ProtocolDecl *, 4> conformances;

    if (isPrimary)
      index = parentOrIndex;
    else
      parent = getType(parentOrIndex)->castTo<ArchetypeType>();

    superclass = getType(superclassID);

    for (DeclID protoID : rawConformanceIDs)
      conformances.push_back(cast<ProtocolDecl>(getDecl(protoID)));

    // See if we triggered deserialization through our conformances.
    if (typeOrOffset.isComplete())
      break;

    auto archetype = ArchetypeType::getNew(ctx, parent, getIdentifier(nameID),
                                           conformances, superclass, index);
    typeOrOffset = archetype;

    auto entry = DeclTypeCursor.advance();
    if (entry.Kind != llvm::BitstreamEntry::Record) {
      error();
      break;
    }

    scratch.clear();
    unsigned kind = DeclTypeCursor.readRecord(entry.ID, scratch);
    if (kind != decls_block::ARCHETYPE_NESTED_TYPE_NAMES) {
      error();
      break;
    }

    ArrayRef<uint64_t> rawNameIDs;
    decls_block::ArchetypeNestedTypeNamesLayout::readRecord(scratch,
                                                            rawNameIDs);

    entry = DeclTypeCursor.advance();
    if (entry.Kind != llvm::BitstreamEntry::Record) {
      error();
      break;
    }

    SmallVector<uint64_t, 16> scratch2;
    kind = DeclTypeCursor.readRecord(entry.ID, scratch2);
    if (kind != decls_block::ARCHETYPE_NESTED_TYPES) {
      error();
      break;
    }

    ArrayRef<uint64_t> rawTypeIDs;
    decls_block::ArchetypeNestedTypesLayout::readRecord(scratch2, rawTypeIDs);

    SmallVector<std::pair<Identifier, ArchetypeType *>, 4> nestedTypes;
    for_each(rawNameIDs, rawTypeIDs, [&](IdentifierID nameID, TypeID nestedID) {
      auto nestedTy = getType(nestedID)->castTo<ArchetypeType>();
      nestedTypes.push_back(std::make_pair(getIdentifier(nameID), nestedTy));
    });
    archetype->setNestedTypes(ctx, nestedTypes);

    break;
  }

  case decls_block::PROTOCOL_COMPOSITION_TYPE: {
    ArrayRef<uint64_t> rawProtocolIDs;

    decls_block::ProtocolCompositionTypeLayout::readRecord(scratch,
                                                           rawProtocolIDs);
    SmallVector<Type, 4> protocols;
    for (TypeID protoID : rawProtocolIDs)
      protocols.push_back(getType(protoID));

    typeOrOffset = ProtocolCompositionType::get(ctx, protocols);
    break;
  }

  case decls_block::SUBSTITUTED_TYPE: {
    TypeID originalID, replacementID;

    decls_block::SubstitutedTypeLayout::readRecord(scratch, originalID,
                                                   replacementID);
    typeOrOffset = SubstitutedType::get(getType(originalID),
                                        getType(replacementID),
                                        ctx);
    break;
  }

  case decls_block::BOUND_GENERIC_TYPE: {
    DeclID declID;
    TypeID parentID;
    ArrayRef<uint64_t> rawArgumentIDs;

    decls_block::BoundGenericTypeLayout::readRecord(scratch, declID, parentID,
                                                    rawArgumentIDs);
    SmallVector<Type, 8> genericArgs;
    for (TypeID type : rawArgumentIDs)
      genericArgs.push_back(getType(type));

    auto nominal = cast<NominalTypeDecl>(getDecl(declID));
    auto parentTy = getType(parentID);

    auto boundTy = BoundGenericType::get(nominal, parentTy, genericArgs);
    typeOrOffset = boundTy;

    // BoundGenericTypes get uniqued in the ASTContext, so it's possible this
    // type already has its substitutions. In that case, ignore the module's.
    if (boundTy->hasSubstitutions())
      break;

    SmallVector<Substitution, 8> substitutions;
    while (auto sub = maybeReadSubstitution())
      substitutions.push_back(sub.getValue());

    boundTy->setSubstitutions(ctx.AllocateCopy(substitutions));
    break;
  }

  case decls_block::POLYMORPHIC_FUNCTION_TYPE: {
    TypeID inputID;
    TypeID resultID;
    DeclID genericContextID;
    uint8_t rawCallingConvention;
    bool thin;
    bool noreturn = false;

    //todo add noreturn serialization.
    decls_block::PolymorphicFunctionTypeLayout::readRecord(scratch,
                                                           inputID,
                                                           resultID,
                                                           genericContextID,
                                                           rawCallingConvention,
                                                           thin,
                                                           noreturn);
    auto callingConvention = getActualCC(rawCallingConvention);
    if (!callingConvention.hasValue()) {
      error();
      return nullptr;
    }

    Decl *genericContext = getDecl(genericContextID);
    assert(genericContext && "loading PolymorphicFunctionType before its decl");

    GenericParamList *paramList = nullptr;
    switch (genericContext->getKind()) {
    case DeclKind::Constructor:
      paramList = cast<ConstructorDecl>(genericContext)->getGenericParams();
      break;
    case DeclKind::Func:
      paramList = cast<FuncDecl>(genericContext)->getGenericParams();
      break;
    case DeclKind::Class:
    case DeclKind::Struct:
    case DeclKind::OneOf:
      paramList = cast<NominalTypeDecl>(genericContext)->getGenericParams();
      break;
    default:
      break;
    }
    assert(paramList && "missing generic params for polymorphic function");

    auto Info = PolymorphicFunctionType::ExtInfo(callingConvention.getValue(),
                                                 thin,
                                                 noreturn);

    typeOrOffset = PolymorphicFunctionType::get(getType(inputID),
                                                getType(resultID),
                                                paramList,
                                                Info,
                                                ctx);
    break;
  }

  case decls_block::ARRAY_SLICE_TYPE: {
    TypeID baseID, implID;
    decls_block::ArraySliceTypeLayout::readRecord(scratch, baseID, implID);

    auto sliceTy = ArraySliceType::get(getType(baseID), ctx);
    typeOrOffset = sliceTy;

    // Slice types are uniqued by the ASTContext, so they may already have
    // type information. If so, ignore the information in the module.
    if (!sliceTy->hasImplementationType())
      sliceTy->setImplementationType(getType(implID));
    break;
  }

  case decls_block::ARRAY_TYPE: {
    TypeID baseID;
    uint64_t size;
    decls_block::ArrayTypeLayout::readRecord(scratch, baseID, size);

    typeOrOffset = ArrayType::get(getType(baseID), size, ctx);
    break;
  }

  case decls_block::UNBOUND_GENERIC_TYPE: {
    DeclID genericID;
    TypeID parentID;
    decls_block::UnboundGenericTypeLayout::readRecord(scratch,
                                                      genericID, parentID);

    auto genericDecl = cast<NominalTypeDecl>(getDecl(genericID));
    typeOrOffset = UnboundGenericType::get(genericDecl, getType(parentID), ctx);
    break;
  }

  default:
    // We don't know how to deserialize this kind of type.
    error();
    return nullptr;
  }
  
  return typeOrOffset;
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
        case index_block::OPERATORS:
          assert(blobData.empty());
          RawOperatorIDs.assign(scratch.begin(), scratch.end());
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
    
    topLevelEntry = cursor.advance(AF_DontPopBlockAtEnd);
  }
  
  if (topLevelEntry.Kind != llvm::BitstreamEntry::EndBlock)
    return error();
}

bool ModuleFile::associateWithModule(Module *module) {
  assert(Status == ModuleStatus::Valid && "invalid module file");
  assert(!ModuleContext && "already associated with an AST module");
  ModuleContext = module;

  ASTContext &ctx = module->Ctx;
  bool missingDependency = false;
  for (auto &dependency : Dependencies) {
    assert(!dependency.Mod && "already loaded?");
    dependency.Mod = getModule(ctx.getIdentifier(dependency.Name));
    if (!dependency.Mod)
      missingDependency = true;
  }

  if (missingDependency) {
    error(ModuleStatus::MissingDependency);
    return false;
  }


  // FIXME: The only reason we're deserializing eagerly is to force extensions
  // to load, which they shouldn't yet anyway.
  buildTopLevelDeclMap();
  return Status == ModuleStatus::Valid;
}

void ModuleFile::buildTopLevelDeclMap() {
  // FIXME: be more lazy about deserialization by encoding this some other way.
  for (DeclID ID : RawTopLevelIDs) {
    // FIXME: Right now ExtensionDecls are in here as well.
    if (auto value = dyn_cast<ValueDecl>(getDecl(ID)))
      TopLevelDecls[value->getName()].push_back(value);
  }

  RawTopLevelIDs.clear();
}

void ModuleFile::lookupValue(Identifier name,
                             SmallVectorImpl<ValueDecl*> &results) {
  auto iter = TopLevelDecls.find(name);
  if (iter == TopLevelDecls.end())
    return;

  results.append(iter->second.begin(), iter->second.end());
}

OperatorKind getOperatorKind(DeclKind kind) {
  switch (kind) {
  case DeclKind::PrefixOperator:
    return Prefix;
  case DeclKind::PostfixOperator:
    return Postfix;
  case DeclKind::InfixOperator:
    return Infix;
  default:
    llvm_unreachable("unknown operator fixity");
  }
}

OperatorDecl *ModuleFile::lookupOperator(Identifier name, DeclKind fixity) {
  if (!RawOperatorIDs.empty()) {
    for (DeclID ID : RawOperatorIDs) {
      auto op = cast<OperatorDecl>(getDecl(ID));
      OperatorKey key(op->getName(), getOperatorKind(op->getKind()));
      Operators[key] = op;
    }

    RawOperatorIDs = {};
  }

  return Operators.lookup(OperatorKey(name, getOperatorKind(fixity)));
}

void ModuleFile::getReexportedModules(
    SmallVectorImpl<Module::ImportedModule> &results) {
  // FIXME: Lock down on re-exports.
  // FIXME: Handle imports with access paths.
  for (auto &dep : Dependencies)
    results.push_back({Module::AccessPathTy(), dep.Mod});
}

void ModuleFile::lookupVisibleDecls(Module::AccessPathTy accessPath,
                                    VisibleDeclConsumer &consumer,
                                    NLKind lookupKind) {
  // FIXME: Validate against access path.
  for (auto &topLevelEntry : TopLevelDecls) {
    for (auto &value : topLevelEntry.second)
      consumer.foundDecl(value);
  }
}

