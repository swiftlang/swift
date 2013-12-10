//===--- Deserialization.cpp - Loading a serialized AST ---------*- c++ -*-===//
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
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Serialization/BCReadingExtras.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::serialization;

using ConformancePair = std::pair<ProtocolDecl *, ProtocolConformance *>;


namespace {
  class PrettyDeclDeserialization : public llvm::PrettyStackTraceEntry {
    const ModuleFile::Serialized<Decl*> &DeclOrOffset;
    DeclID ID;
    decls_block::RecordKind Kind;
  public:
    PrettyDeclDeserialization(const ModuleFile::Serialized<Decl*> &declOrOffset,
                              DeclID DID, decls_block::RecordKind kind)
      : DeclOrOffset(declOrOffset), ID(DID), Kind(kind) {
    }

    static const char *getRecordKindString(decls_block::RecordKind Kind) {
      switch (Kind) {
#define RECORD(Id) case decls_block::Id: return #Id;
#include "DeclTypeRecordNodes.def"
      }
    }

    virtual void print(raw_ostream &os) const override {
      if (!DeclOrOffset.isComplete()) {
        os << "While deserializing decl #" << ID << " ("
           << getRecordKindString(Kind) << ")\n";
        return;
      }

      os << "While deserializing ";
      if (auto VD = dyn_cast<ValueDecl>(DeclOrOffset.get())) {
        os << "'" << VD->getName() << "' ("
           << Decl::getKindName(VD->getKind()) << "Decl) \n";
      } else {
        os << Decl::getKindName(DeclOrOffset.get()->getKind())
           << "Decl #" << ID << "\n";
      }
    }
  };

  class PrettyXRefDeserialization : public llvm::PrettyStackTraceEntry {
    ModuleFile &File;
    XRefKind Kind;
    Module &ContainingModule;
    ArrayRef<uint64_t> RawPath;

  public:
    PrettyXRefDeserialization(ModuleFile &file, XRefKind kind, Module &M,
                              ArrayRef<uint64_t> rawPath)
      : File(file), Kind(kind), ContainingModule(M), RawPath(rawPath) {}

    virtual void print(raw_ostream &os) const override {
      auto NamePath = RawPath;

      os << "Cross-reference to ";
      switch (Kind) {
      case XRefKind::SwiftValue:
      case XRefKind::SwiftOperator:
        break;
      case XRefKind::SwiftGenericParameter:
        os << "generic param " << RawPath.back() << " of ";
        NamePath = RawPath.slice(0, RawPath.size()-1);
        break;
      }

      os << "'";
      interleave(NamePath,
                 [&](IdentifierID IID) { os << File.getIdentifier(IID); },
                 [&]() { os << "."; });
      os << "' in " << ContainingModule.Name << "\n";
    }
  };
} // end anonymous namespace


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

    auto result = new (getContext()) ParenPattern(SourceLoc(),
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

    auto result = TuplePattern::create(getContext(), SourceLoc(),
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
    auto result = new (getContext()) NamedPattern(var, isImplicit);
    if (var->hasType())
      result->setType(var->getType());
    return result;
  }
  case decls_block::ANY_PATTERN: {
    TypeID typeID;
    bool isImplicit;

    AnyPatternLayout::readRecord(scratch, typeID, isImplicit);
    auto result = new (getContext()) AnyPattern(SourceLoc(), isImplicit);
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
    auto result = new (getContext()) TypedPattern(subPattern, typeInfo,
                                                        isImplicit);
    result->setType(typeInfo.getType());
    return result;
  }
  default:
    return nullptr;
  }
}

ProtocolConformance *
ModuleFile::readUnderlyingConformance(ProtocolDecl *proto,
                                      DeclID typeID,
                                      IdentifierID moduleID,
                                      llvm::BitstreamCursor &Cursor) {
  if (moduleID == serialization::BUILTIN_MODULE_ID) {
    // The underlying conformance is in the following record.
    return maybeReadConformance(getType(typeID), Cursor)->second;
  }

  // Dig out the protocol conformance within the nominal declaration.
  auto nominal = cast<NominalTypeDecl>(getDecl(typeID));
  Module *owningModule = getModule(moduleID);
  (void)owningModule; // FIXME: Currently only used for checking.

  // Search protocols
  for (unsigned i = 0, n = nominal->getProtocols().size(); i != n; ++i) {
    if (nominal->getProtocols()[i] == proto) {
      // FIXME: Eventually, filter by owning module.
      assert(nominal->getModuleContext() == owningModule);
      return nominal->getConformances()[i];
    }
  }

  // Search extensions.
  for (auto ext : nominal->getExtensions()) {
    for (unsigned i = 0, n = ext->getProtocols().size(); i != n; ++i) {
      if (ext->getProtocols()[i] == proto) {
        // FIXME: Eventually, filter by owning module.
        assert(ext->getModuleContext() == owningModule);
        return ext->getConformances()[i];
      }
    }
  }

  llvm_unreachable("Unable to find underlying conformance");
}

Optional<ConformancePair> ModuleFile::maybeReadConformance(Type conformingType,
                              llvm::BitstreamCursor &Cursor){
  using namespace decls_block;

  BCOffsetRAII lastRecordOffset(Cursor);
  SmallVector<uint64_t, 16> scratch;

  auto next = Cursor.advance(AF_DontPopBlockAtEnd);
  if (next.Kind != llvm::BitstreamEntry::Record)
    return Nothing;

  unsigned kind = Cursor.readRecord(next.ID, scratch);
  switch (kind) {
  case NO_CONFORMANCE: {
    lastRecordOffset.reset();
    DeclID protoID;
    NoConformanceLayout::readRecord(scratch, protoID);
    return std::make_pair(cast<ProtocolDecl>(getDecl(protoID)), nullptr);
  }

  case NORMAL_PROTOCOL_CONFORMANCE:
    // Handled below.
    break;

  case SPECIALIZED_PROTOCOL_CONFORMANCE: {
    DeclID protoID;
    DeclID typeID;
    IdentifierID moduleID;
    unsigned numTypeWitnesses;
    unsigned numSubstitutions;
    ArrayRef<uint64_t> rawIDs;
    SpecializedProtocolConformanceLayout::readRecord(scratch, protoID,
                                                     typeID,
                                                     moduleID,
                                                     numTypeWitnesses,
                                                     numSubstitutions,
                                                     rawIDs);

    ASTContext &ctx = getContext();

    auto proto = cast<ProtocolDecl>(getDecl(protoID));

    // Read the substitutions.
    SmallVector<Substitution, 4> substitutions;
    while (numSubstitutions--) {
      auto sub = maybeReadSubstitution(Cursor);
      assert(sub.hasValue() && "Missing substitution?");
      substitutions.push_back(*sub);
    }

    // Read the type witnesses.
    ArrayRef<uint64_t>::iterator rawIDIter = rawIDs.begin();
    TypeWitnessMap typeWitnesses;
    while (numTypeWitnesses--) {
      // FIXME: We don't actually want to allocate an archetype here; we just
      // want to get an access path within the protocol.
      auto first = cast<AssociatedTypeDecl>(getDecl(*rawIDIter++));
      auto second = maybeReadSubstitution(Cursor);
      assert(second.hasValue());
      typeWitnesses[first] = *second;
    }
    assert(rawIDIter <= rawIDs.end() && "read too much");

    ProtocolConformance *genericConformance
      = readUnderlyingConformance(proto, typeID, moduleID, Cursor);

    // Reset the offset RAII to the end of the trailing records.
    lastRecordOffset.reset();

    assert(genericConformance && "Missing generic conformance?");
    return { proto,
             ctx.getSpecializedConformance(conformingType,
                                           genericConformance,
                                           substitutions,
                                           std::move(typeWitnesses)) };
  }

  case INHERITED_PROTOCOL_CONFORMANCE: {
    DeclID protoID;
    DeclID typeID;
    IdentifierID moduleID;
    InheritedProtocolConformanceLayout::readRecord(scratch, protoID,
                                                   typeID,
                                                   moduleID);

    ASTContext &ctx = getContext();

    auto proto = cast<ProtocolDecl>(getDecl(protoID));

    ProtocolConformance *inheritedConformance
      = readUnderlyingConformance(proto, typeID, moduleID, Cursor);

    // Reset the offset RAII to the end of the trailing records.
    lastRecordOffset.reset();
    assert(inheritedConformance && "Missing generic conformance?");
    return { proto,
             ctx.getInheritedConformance(conformingType,
                                         inheritedConformance) };
  }
      
  // Not a protocol conformance.
  default:
    return Nothing;
  }

  lastRecordOffset.reset();

  DeclID protoID;
  unsigned valueCount, typeCount, inheritedCount, defaultedCount;
  ArrayRef<uint64_t> rawIDs;

  NormalProtocolConformanceLayout::readRecord(scratch, protoID,
                                              valueCount, typeCount,
                                              inheritedCount, defaultedCount,
                                              rawIDs);

  InheritedConformanceMap inheritedConformances;

  while (inheritedCount--) {
    auto inherited = maybeReadConformance(conformingType, Cursor);
    assert(inherited.hasValue());

    inheritedConformances.insert(inherited.getValue());
  }

  ASTContext &ctx = getContext();

  auto proto = cast<ProtocolDecl>(getDecl(protoID));

  WitnessMap witnesses;
  ArrayRef<uint64_t>::iterator rawIDIter = rawIDs.begin();
  while (valueCount--) {
    auto first = cast<ValueDecl>(getDecl(*rawIDIter++));
    auto second = cast<ValueDecl>(getDecl(*rawIDIter++));
    unsigned substitutionCount = *rawIDIter++;

    SmallVector<Substitution, 8> substitutions;
    while (substitutionCount--) {
      auto sub = maybeReadSubstitution(Cursor);
      assert(sub.hasValue());
      substitutions.push_back(sub.getValue());
    }

    ConcreteDeclRef witness;
    if (substitutions.empty())
      witness = ConcreteDeclRef(second);
    else
      witness = ConcreteDeclRef(ctx, second, substitutions);

    witnesses.insert(std::make_pair(first, witness));
  }
  assert(rawIDIter <= rawIDs.end() && "read too much");

  TypeWitnessMap typeWitnesses;
  while (typeCount--) {
    // FIXME: We don't actually want to allocate an archetype here; we just
    // want to get an access path within the protocol.
    auto first = cast<AssociatedTypeDecl>(getDecl(*rawIDIter++));
    auto second = maybeReadSubstitution(Cursor);
    assert(second.hasValue());
    typeWitnesses[first] = *second;
  }
  assert(rawIDIter <= rawIDs.end() && "read too much");

  SmallVector<ValueDecl *, 4> defaultedDefinitions;
  while (defaultedCount--) {
    auto decl = cast<ValueDecl>(getDecl(*rawIDIter++));
    defaultedDefinitions.push_back(decl);
  }
  assert(rawIDIter <= rawIDs.end() && "read too much");

  // Reset the offset RAII to the end of the trailing records.
  lastRecordOffset.reset();

  return { proto,
           ctx.getConformance(conformingType, proto,
                              FileContext->getParentModule(),
                              std::move(witnesses),
                              std::move(typeWitnesses),
                              std::move(inheritedConformances),
                              defaultedDefinitions) };
}

/// Applies protocol conformances to a decl.
template <typename T>
void processConformances(ASTContext &ctx, T *decl,
                         ArrayRef<ConformancePair> conformances) {
  SmallVector<ProtocolDecl *, 16> protoBuf;
  SmallVector<ProtocolConformance *, 16> conformanceBuf;
  for (auto conformancePair : conformances) {
    auto proto = conformancePair.first;
    auto conformance = conformancePair.second;

    protoBuf.push_back(proto);
    conformanceBuf.push_back(conformance);
  }

  decl->setProtocols(ctx.AllocateCopy(protoBuf));
  decl->setConformances(ctx.AllocateCopy(conformanceBuf));
}


Optional<Substitution> ModuleFile::maybeReadSubstitution(
                           llvm::BitstreamCursor &Cursor) {
  BCOffsetRAII lastRecordOffset(Cursor);

  auto entry = Cursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind != llvm::BitstreamEntry::Record)
    return Nothing;

  StringRef blobData;
  SmallVector<uint64_t, 2> scratch;
  unsigned recordID = Cursor.readRecord(entry.ID, scratch,
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

  ASTContext &ctx = getContext();

  SmallVector<ProtocolConformance *, 16> conformanceBuf;
  while (numConformances--) {
    auto conformancePair = maybeReadConformance(replacementTy, Cursor);
    assert(conformancePair.hasValue() && "Missing conformance");
    conformanceBuf.push_back(conformancePair->second);
  }

  lastRecordOffset.reset();
  return Substitution{archetypeTy, replacementTy,
                      ctx.AllocateCopy(conformanceBuf)};
}

GenericParamList *
ModuleFile::maybeGetOrReadGenericParams(serialization::DeclID genericContextID,
                                        DeclContext *DC) {
  if (genericContextID) {
    Decl *genericContext = getDecl(genericContextID);
    assert(genericContext && "loading PolymorphicFunctionType before its decl");

    switch (genericContext->getKind()) {
    case DeclKind::Constructor:
      return cast<ConstructorDecl>(genericContext)->getGenericParams();
    case DeclKind::Func:
      return cast<FuncDecl>(genericContext)->getGenericParams();
    case DeclKind::Class:
    case DeclKind::Struct:
    case DeclKind::Enum:
    case DeclKind::Protocol:
      return cast<NominalTypeDecl>(genericContext)->getGenericParams();
    default:
      return nullptr;
    }
  } else {
    return maybeReadGenericParams(DC);
  }
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
  SmallVector<RequirementRepr, 8> requirements;
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
      auto genericParam = cast<GenericTypeParamDecl>(getDecl(paramDeclID, DC));
      params.push_back(GenericParam(genericParam));
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

        requirements.push_back(RequirementRepr::getConformance(subject,
                                                           SourceLoc(),
                                                           constraint));
        break;
      }
      case GenericRequirementKind::SameType: {
        assert(rawTypeIDs.size() == 2);
        auto first = TypeLoc::withoutLoc(getType(rawTypeIDs[0]));
        auto second = TypeLoc::withoutLoc(getType(rawTypeIDs[1]));

        requirements.push_back(RequirementRepr::getSameType(first,
                                                            SourceLoc(),
                                                            second));
        break;
      }

      case ValueWitnessMarker: {
        // Shouldn't happen where we have requirement representations.
        error();
        break;
      }

      default:
        // Unknown requirement kind. Drop the requirement and continue, but log
        // an error so that we don't actually try to generate code.
        error();
      }

      break;
    }
    case LAST_GENERIC_REQUIREMENT:
      // Read the end-of-requirements record.
      uint8_t dummy;
      LastGenericRequirementLayout::readRecord(scratch, dummy);
      lastRecordOffset.reset();
      shouldContinue = false;
      break;

    default:
      // This record is not part of the GenericParamList.
      shouldContinue = false;
      break;
    }

    if (!shouldContinue)
      break;
  }

  auto paramList = GenericParamList::create(getContext(), SourceLoc(),
                                            params, SourceLoc(), requirements,
                                            SourceLoc());
  paramList->setAllArchetypes(getContext().AllocateCopy(archetypes));
  paramList->setOuterParameters(DC->getGenericParamsOfContext());

  return paramList;
}

void ModuleFile::readGenericRequirements(
                   SmallVectorImpl<Requirement> &requirements) {
  using namespace decls_block;

  BCOffsetRAII lastRecordOffset(DeclTypeCursor);
  SmallVector<uint64_t, 8> scratch;
  StringRef blobData;

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
    case GENERIC_REQUIREMENT: {
      uint8_t rawKind;
      ArrayRef<uint64_t> rawTypeIDs;
      GenericRequirementLayout::readRecord(scratch, rawKind, rawTypeIDs);

      switch (rawKind) {
      case GenericRequirementKind::Conformance: {
        assert(rawTypeIDs.size() == 2);
        auto subject = getType(rawTypeIDs[0]);
        auto constraint = getType(rawTypeIDs[1]);

        requirements.push_back(Requirement(RequirementKind::Conformance,
                                           subject, constraint));
        break;
      }
      case GenericRequirementKind::SameType: {
        assert(rawTypeIDs.size() == 2);
        auto first = getType(rawTypeIDs[0]);
        auto second = getType(rawTypeIDs[1]);

        requirements.push_back(Requirement(RequirementKind::SameType,
                                           first, second));
        break;
      }
      case GenericRequirementKind::ValueWitnessMarker: {
        assert(rawTypeIDs.size() == 1);
        auto first = getType(rawTypeIDs[0]);

        requirements.push_back(Requirement(RequirementKind::ValueWitnessMarker,
                                           first, Type()));
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

  ASTContext &ctx = getContext();
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

  size_t rawID = IID - NUM_SPECIAL_MODULES;
  assert(rawID < Identifiers.size() && "invalid identifier ID");
  auto identRecord = Identifiers[rawID];

  if (identRecord.Offset == 0)
    return identRecord.Ident;

  assert(!IdentifierData.empty() && "no identifier data in module");

  StringRef rawStrPtr = IdentifierData.substr(identRecord.Offset);
  size_t terminatorOffset = rawStrPtr.find('\0');
  assert(terminatorOffset != StringRef::npos &&
         "unterminated identifier string data");

  return getContext().getIdentifier(rawStrPtr.slice(0, terminatorOffset));
}

DeclContext *ModuleFile::getDeclContext(DeclID DID) {
  if (DID == 0)
    return FileContext;

  Decl *D = getDecl(DID);

  if (auto ND = dyn_cast<NominalTypeDecl>(D))
    return ND;
  if (auto ED = dyn_cast<ExtensionDecl>(D))
    return ED;
  if (auto AFD = dyn_cast<AbstractFunctionDecl>(D))
    return AFD;

  llvm_unreachable("unknown DeclContext kind");
}

Module *ModuleFile::getModule(ModuleID MID) {
  if (MID == BUILTIN_MODULE_ID)
    return getContext().TheBuiltinModule;
  if (MID == CURRENT_MODULE_ID)
    return FileContext->getParentModule();
  return getModule(getIdentifier(MID));
}

Module *ModuleFile::getModule(Identifier name) {
  if (name.empty())
    return getContext().TheBuiltinModule;

  // FIXME: duplicated from NameBinder::getModule
  // FIXME: provide a real source location.
  if (name == FileContext->getParentModule()->Name) {
    if (!ShadowedModule) {
      auto importer = getContext().getClangModuleLoader();
      assert(importer && "no way to import shadowed module");
      ShadowedModule = importer->loadModule(SourceLoc(),
                                            std::make_pair(name, SourceLoc()));
      assert(ShadowedModule && "missing shadowed module");
    }

    return ShadowedModule;
  }

  // FIXME: provide a real source location.
  return getContext().getModule(std::make_pair(name, SourceLoc()));
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
                          std::function<void(Decl*)> DidRecord) {
  if (DID == 0)
    return nullptr;

  assert(DID <= Decls.size() && "invalid decl ID");
  auto &declOrOffset = Decls[DID-1];

  if (declOrOffset.isComplete()) {
    if (DidRecord)
      DidRecord(declOrOffset);
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

  ASTContext &ctx = getContext();

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch, &blobData);

  PrettyDeclDeserialization stackTraceEntry(
     declOrOffset, DID, static_cast<decls_block::RecordKind>(recordID));

  switch (recordID) {
  case decls_block::TYPE_ALIAS_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    TypeID underlyingTypeID, interfaceTypeID;
    bool isImplicit;

    decls_block::TypeAliasLayout::readRecord(scratch, nameID, contextID,
                                             underlyingTypeID, interfaceTypeID,
                                             isImplicit);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);
    auto underlyingType = TypeLoc::withoutLoc(getType(underlyingTypeID));
    
    if (declOrOffset.isComplete())
      break;

    auto alias = new (ctx) TypeAliasDecl(SourceLoc(), getIdentifier(nameID),
                                         SourceLoc(), underlyingType,
                                         DC, { });
    declOrOffset = alias;

    if (auto interfaceType = getType(interfaceTypeID))
      alias->setInterfaceType(interfaceType);

    if (isImplicit)
      alias->setImplicit();

    SmallVector<ConformancePair, 16> conformances;
    while (auto conformance = maybeReadConformance(underlyingType.getType(),
                                                   DeclTypeCursor))
      conformances.push_back(*conformance);
    processConformances(ctx, alias, conformances);
    alias->setCheckedInheritanceClause();
    break;
  }

  case decls_block::GENERIC_TYPE_PARAM_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;
    unsigned depth;
    unsigned index;
    TypeID superclassID;
    TypeID archetypeID;

    decls_block::GenericTypeParamDeclLayout::readRecord(scratch, nameID,
                                                        contextID,
                                                        isImplicit,
                                                        depth,
                                                        index,
                                                        superclassID,
                                                        archetypeID);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);

    if (declOrOffset.isComplete())
      break;

    auto genericParam = new (ctx) GenericTypeParamDecl(DC,
                                                       getIdentifier(nameID),
                                                       SourceLoc(),
                                                       depth,
                                                       index);
    declOrOffset = genericParam;

    if (isImplicit)
      genericParam->setImplicit();

    genericParam->setSuperclass(getType(superclassID));
    genericParam->setArchetype(getType(archetypeID)->castTo<ArchetypeType>());

    SmallVector<ConformancePair, 16> conformances;
    while (auto conformance
           = maybeReadConformance(genericParam->getDeclaredType(),
                                  DeclTypeCursor))
      conformances.push_back(*conformance);
    processConformances(ctx, genericParam, conformances);
    genericParam->setCheckedInheritanceClause();
    break;
  }

  case decls_block::ASSOCIATED_TYPE_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    TypeID superclassID;
    TypeID archetypeID;
    bool isImplicit;

    decls_block::AssociatedTypeDeclLayout::readRecord(scratch, nameID,
                                                      contextID,
                                                      superclassID,
                                                      archetypeID,
                                                      isImplicit);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);

    if (declOrOffset.isComplete())
      break;

    auto assocType = new (ctx) AssociatedTypeDecl(DC, SourceLoc(),
                                                  getIdentifier(nameID),
                                                  SourceLoc());
    declOrOffset = assocType;

    assocType->setSuperclass(getType(superclassID));
    assocType->setArchetype(getType(archetypeID)->castTo<ArchetypeType>());
    if (isImplicit)
      assocType->setImplicit();

    SmallVector<ConformancePair, 16> conformances;
    while (auto conformance
           = maybeReadConformance(assocType->getDeclaredType(), DeclTypeCursor))
      conformances.push_back(*conformance);
    processConformances(ctx, assocType, conformances);
    assocType->setCheckedInheritanceClause();
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
      DidRecord(theStruct);
      DidRecord = nullptr;
    }

    if (isImplicit)
      theStruct->setImplicit();
    if (genericParams) {
      SmallVector<GenericTypeParamType *, 4> paramTypes;
      for (auto &genericParam : *theStruct->getGenericParams()) {
        genericParam.getAsTypeParam()->setDeclContext(theStruct);
        paramTypes.push_back(genericParam.getAsTypeParam()->getDeclaredType()
                               ->castTo<GenericTypeParamType>());
      }

      // Read the generic requirements.
      SmallVector<Requirement, 4> requirements;
      readGenericRequirements(requirements);

      theStruct->setGenericSignature(paramTypes, requirements);
    }

    theStruct->computeType();

    CanType canTy = theStruct->getDeclaredTypeInContext()->getCanonicalType();

    SmallVector<ConformancePair, 16> conformances;
    while (auto conformance = maybeReadConformance(canTy, DeclTypeCursor))
      conformances.push_back(*conformance);
    processConformances(ctx, theStruct, conformances);

    auto members = readMembers();
    assert(members.hasValue() && "could not read struct members");
    theStruct->setMembers(members.getValue(), SourceRange());
    theStruct->setCheckedInheritanceClause();
    break;
  }

  case decls_block::CONSTRUCTOR_DECL: {
    DeclID parentID;
    bool isImplicit, hasSelectorStyleSignature, isObjC, isTransparent;
    TypeID signatureID;
    TypeID interfaceID;
    DeclID implicitSelfID;

    decls_block::ConstructorLayout::readRecord(scratch, parentID, isImplicit,
                                               hasSelectorStyleSignature,
                                               isObjC, isTransparent,
                                               signatureID, interfaceID,
                                               implicitSelfID);
    auto parent = getDeclContext(parentID);
    if (declOrOffset.isComplete())
      break;

    auto selfDecl = cast<VarDecl>(getDecl(implicitSelfID, nullptr));
    auto genericParams = maybeReadGenericParams(parent);

    auto ctor = new (ctx) ConstructorDecl(ctx.getIdentifier("init"),
                                          SourceLoc(), /*argParams=*/nullptr,
                                          /*bodyParams=*/nullptr, selfDecl,
                                          genericParams, parent);
    declOrOffset = ctor;
    selfDecl->setDeclContext(ctor);

    Pattern *argParams = maybeReadPattern();
    assert(argParams && "missing argument patterns for constructor");
    ctor->setArgParams(argParams);

    Pattern *bodyParams = maybeReadPattern();
    assert(bodyParams && "missing body patterns for constructor");
    ctor->setBodyParams(bodyParams);

    // This must be set after recording the constructor in the map.
    // A polymorphic constructor type needs to refer to the constructor to get
    // its generic parameters.
    ctor->setType(getType(signatureID));
    if (auto interfaceType = getType(interfaceID))
      ctor->setInterfaceType(interfaceType);

    // Set the initializer type of the constructor.
    auto allocType = ctor->getType();
    auto selfTy = allocType->castTo<AnyFunctionType>()->getInput()
                    ->castTo<MetaTypeType>()->getInstanceType();
    if (auto polyFn = allocType->getAs<PolymorphicFunctionType>()) {
      ctor->setInitializerType(
        PolymorphicFunctionType::get(selfTy, polyFn->getResult(),
                                     &polyFn->getGenericParams(),
                                     polyFn->getExtInfo(),
                                     ctx));
    } else {
      auto fn = allocType->castTo<FunctionType>();
      ctor->setInitializerType(FunctionType::get(selfTy,
                                                 fn->getResult(),
                                                 fn->getExtInfo(),
                                                 ctx));
    }

    // Set the initializer interface type of the constructor.
    allocType = ctor->getInterfaceType();
    selfTy = allocType->castTo<AnyFunctionType>()->getInput()
               ->castTo<MetaTypeType>()->getInstanceType();
    if (auto polyFn = allocType->getAs<GenericFunctionType>()) {
      ctor->setInitializerInterfaceType(
              GenericFunctionType::get(polyFn->getGenericParams(),
                                       polyFn->getRequirements(),
                                       selfTy, polyFn->getResult(),
                                       polyFn->getExtInfo(),
                                       ctx));
    } else {
      auto fn = allocType->castTo<FunctionType>();
      ctor->setInitializerInterfaceType(FunctionType::get(selfTy,
                                                          fn->getResult(),
                                                          fn->getExtInfo(),
                                                          ctx));
    }

    if (isImplicit)
      ctor->setImplicit();
    if (hasSelectorStyleSignature)
      ctor->setHasSelectorStyleSignature();
    ctor->setIsObjC(isObjC);
    if (isTransparent)
      ctor->getMutableAttrs().setAttr(AK_transparent, SourceLoc());

    if (genericParams)
      for (auto &genericParam : *ctor->getGenericParams())
        genericParam.getAsTypeParam()->setDeclContext(ctor);

    break;
  }

  case decls_block::VAR_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit, isObjC, isIBOutlet, isOptional, isStatic;
    TypeID typeID, interfaceTypeID;
    DeclID getterID, setterID;
    DeclID overriddenID;

    decls_block::VarLayout::readRecord(scratch, nameID, contextID, isImplicit,
                                       isObjC, isIBOutlet, isOptional, isStatic,
                                       typeID, interfaceTypeID,
                                       getterID, setterID, overriddenID);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    auto var = new (ctx) VarDecl(isStatic, SourceLoc(), getIdentifier(nameID),
                                 getType(typeID), DC);

    declOrOffset = var;

    if (auto interfaceType = getType(interfaceTypeID))
      var->setInterfaceType(interfaceType);

    if (getterID || setterID) {
      var->setComputedAccessors(ctx, SourceLoc(),
                                cast_or_null<FuncDecl>(getDecl(getterID)),
                                cast_or_null<FuncDecl>(getDecl(setterID)),
                                SourceLoc());
    }

    if (isImplicit)
      var->setImplicit();
    var->setIsObjC(isObjC);
    if (isIBOutlet)
      var->getMutableAttrs().setAttr(AK_IBOutlet, SourceLoc());
    if (isOptional)
      var->getMutableAttrs().setAttr(AK_optional, SourceLoc());
    
    var->setOverriddenDecl(cast_or_null<VarDecl>(getDecl(overriddenID)));
    break;
  }

  case decls_block::FUNC_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;
    bool hasSelectorStyleSignature;
    bool isClassMethod;
    bool isAssignmentOrConversion;
    bool isObjC, isIBAction, isTransparent, isOptional;
    unsigned numParamPatterns;
    TypeID signatureID;
    TypeID interfaceTypeID;
    DeclID associatedDeclID;
    DeclID overriddenID;

    decls_block::FuncLayout::readRecord(scratch, nameID, contextID, isImplicit,
                                        hasSelectorStyleSignature,
                                        isClassMethod, isAssignmentOrConversion,
                                        isObjC, isIBAction, isTransparent,
                                        isOptional,
                                        numParamPatterns, signatureID,
                                        interfaceTypeID, associatedDeclID,
                                        overriddenID);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    // Read generic params before reading the type, because the type may
    // reference generic parameters, and we want them to have a dummy
    // DeclContext for now.
    GenericParamList *genericParams = maybeReadGenericParams(DC);

    auto fn = FuncDecl::createDeserialized(
        ctx, SourceLoc(), SourceLoc(), getIdentifier(nameID), SourceLoc(),
        genericParams, /*type=*/nullptr, numParamPatterns, DC);
    declOrOffset = fn;

    // This must be set after recording the constructor in the map.
    // A polymorphic constructor type needs to refer to the constructor to get
    // its generic parameters.
    auto signature = getType(signatureID)->castTo<AnyFunctionType>();
    fn->setType(signature);

    // Set the interface type.
    if (auto interfaceType = getType(interfaceTypeID))
      fn->setInterfaceType(interfaceType);

    SmallVector<Pattern *, 16> patternBuf;
    while (Pattern *pattern = maybeReadPattern())
      patternBuf.push_back(pattern);

    assert(!patternBuf.empty());
    assert((patternBuf.size() == numParamPatterns ||
            patternBuf.size() == numParamPatterns * 2) &&
           "incorrect number of parameters");

    ArrayRef<Pattern *> patterns(patternBuf);
    ArrayRef<Pattern *> argPatterns = patterns.slice(0, numParamPatterns);
    ArrayRef<Pattern *> bodyPatterns = patterns.slice(numParamPatterns);
    if (bodyPatterns.empty())
      bodyPatterns = argPatterns;
    fn->setDeserializedSignature(argPatterns, bodyPatterns,
                                 TypeLoc::withoutLoc(signature->getResult()));

    if (genericParams)
      for (auto &genericParam : *fn->getGenericParams())
        genericParam.getAsTypeParam()->setDeclContext(fn);

    fn->setOverriddenDecl(cast_or_null<FuncDecl>(getDecl(overriddenID)));

    fn->setStatic(isClassMethod);
    if (isImplicit)
      fn->setImplicit();
    if (hasSelectorStyleSignature)
      fn->setHasSelectorStyleSignature();
    if (!blobData.empty())
      fn->getMutableAttrs().AsmName = ctx.AllocateCopy(blobData);
    if (isAssignmentOrConversion) {
      if (fn->isOperator())
        fn->getMutableAttrs().setAttr(AK_assignment, SourceLoc());
      else
        fn->getMutableAttrs().setAttr(AK_conversion, SourceLoc());
    }
    fn->setIsObjC(isObjC);
    if (isIBAction)
      fn->getMutableAttrs().setAttr(AK_IBAction, SourceLoc());
    if (isTransparent)
      fn->getMutableAttrs().setAttr(AK_transparent, SourceLoc());
    if (isOptional)
      fn->getMutableAttrs().setAttr(AK_optional, SourceLoc());

    if (Decl *associated = getDecl(associatedDeclID)) {
      if (auto op = dyn_cast<OperatorDecl>(associated)) {
        fn->setOperatorDecl(op);

        if (isa<PrefixOperatorDecl>(op))
          fn->getMutableAttrs().setAttr(AK_prefix, SourceLoc());
        else if (isa<PostfixOperatorDecl>(op))
          fn->getMutableAttrs().setAttr(AK_postfix, SourceLoc());
        // Note that an explicit [infix] is not required.
      }
      // Otherwise, unknown associated decl kind.
    }

    break;
  }

  case decls_block::PATTERN_BINDING_DECL: {
    DeclID contextID;
    bool isImplicit;
    bool isStatic;

    decls_block::PatternBindingLayout::readRecord(scratch, contextID,
                                                  isImplicit,
                                                  isStatic);
    Pattern *pattern = maybeReadPattern();
    assert(pattern);

    auto binding = new (ctx) PatternBindingDecl(SourceLoc(),
                                                SourceLoc(), pattern,
                                                /*init=*/nullptr,
                                                getDeclContext(contextID));
    binding->setStatic(isStatic);
    declOrOffset = binding;

    if (isImplicit)
      binding->setImplicit();

    break;
  }

  case decls_block::PROTOCOL_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit, isClassProtocol, isObjC;
    ArrayRef<uint64_t> protocolIDs;

    decls_block::ProtocolLayout::readRecord(scratch, nameID, contextID,
                                            isImplicit, isClassProtocol, isObjC,
                                            protocolIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    auto proto = new (ctx) ProtocolDecl(DC, SourceLoc(), SourceLoc(),
                                        getIdentifier(nameID), { });
    declOrOffset = proto;

    if (DidRecord) {
      DidRecord(proto);
      DidRecord = nullptr;
    }

    if (auto genericParams = maybeReadGenericParams(DC)) {
      proto->setGenericParams(genericParams);
      SmallVector<GenericTypeParamType *, 4> paramTypes;
      for (auto &genericParam : *proto->getGenericParams()) {
        genericParam.getAsTypeParam()->setDeclContext(proto);
        paramTypes.push_back(genericParam.getAsTypeParam()->getDeclaredType()
                             ->castTo<GenericTypeParamType>());
      }

      // Read the generic requirements.
      SmallVector<Requirement, 4> requirements;
      readGenericRequirements(requirements);

      proto->setGenericSignature(paramTypes, requirements);
    }


    if (isImplicit)
      proto->setImplicit();
    if (isClassProtocol)
      proto->getMutableAttrs().setAttr(AK_class_protocol, SourceLoc());
    proto->setIsObjC(isObjC);
    proto->computeType();

    // Deserialize the list of protocols.
    SmallVector<ProtocolDecl *, 4> protocols;
    for (auto protoID : protocolIDs) {
      protocols.push_back(cast<ProtocolDecl>(getDecl(protoID)));
    }
    proto->setProtocols(ctx.AllocateCopy(protocols));

    auto members = readMembers();
    assert(members.hasValue() && "could not read struct members");
    proto->setMembers(members.getValue(), SourceRange());
    proto->setCheckedInheritanceClause();
    proto->setCircularityCheck(CircularityCheck::Checked);
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
    bool isImplicit, isObjC, isIBLiveView;
    TypeID superclassID;
    unsigned resilienceKind;
    decls_block::ClassLayout::readRecord(scratch, nameID, contextID,
                                         isImplicit, isObjC, isIBLiveView,
                                         resilienceKind, superclassID);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    auto genericParams = maybeReadGenericParams(DC);

    auto theClass = new (ctx) ClassDecl(SourceLoc(), getIdentifier(nameID),
                                        SourceLoc(), { }, genericParams, DC);
    declOrOffset = theClass;
    if (DidRecord) {
      DidRecord(theClass);
      DidRecord = nullptr;
    }

    if (isImplicit)
      theClass->setImplicit();
    if (superclassID)
      theClass->setSuperclass(getType(superclassID));
    if ((Resilience)resilienceKind == Resilience::Fragile)
      theClass->getMutableAttrs().setAttr(AK_fragile, SourceLoc());
    else if ((Resilience)resilienceKind == Resilience::InherentlyFragile)
      theClass->getMutableAttrs().setAttr(AK_born_fragile, SourceLoc());
    else if ((Resilience)resilienceKind == Resilience::Resilient)
      theClass->getMutableAttrs().setAttr(AK_resilient, SourceLoc());
    if (genericParams) {
      SmallVector<GenericTypeParamType *, 4> paramTypes;
      for (auto &genericParam : *theClass->getGenericParams()) {
        genericParam.getAsTypeParam()->setDeclContext(theClass);
        paramTypes.push_back(genericParam.getAsTypeParam()->getDeclaredType()
                               ->castTo<GenericTypeParamType>());
      }

      // Read the generic requirements.
      SmallVector<Requirement, 4> requirements;
      readGenericRequirements(requirements);

      theClass->setGenericSignature(paramTypes, requirements);
    }
    theClass->setIsObjC(isObjC);
    if (isIBLiveView)
      theClass->getMutableAttrs().setAttr(AK_IBLiveView, SourceLoc());
    theClass->computeType();

    CanType canTy = theClass->getDeclaredTypeInContext()->getCanonicalType();

    SmallVector<ConformancePair, 16> conformances;
    while (auto conformance = maybeReadConformance(canTy, DeclTypeCursor))
      conformances.push_back(*conformance);
    processConformances(ctx, theClass, conformances);

    auto members = readMembers();
    assert(members.hasValue() && "could not read class members");
    theClass->setMembers(members.getValue(), SourceRange());
    theClass->setCheckedInheritanceClause();
    theClass->setCircularityCheck(CircularityCheck::Checked);
    break;
  }

  case decls_block::ENUM_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;
    TypeID rawTypeID;

    decls_block::EnumLayout::readRecord(scratch, nameID, contextID,
                                        isImplicit, rawTypeID);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    auto genericParams = maybeReadGenericParams(DC);

    auto theEnum = new (ctx) EnumDecl(SourceLoc(), getIdentifier(nameID),
                                      SourceLoc(), { }, genericParams, DC);

    declOrOffset = theEnum;
    if (DidRecord) {
      DidRecord(theEnum);
      DidRecord = nullptr;
    }

    if (isImplicit)
      theEnum->setImplicit();
    theEnum->setRawType(getType(rawTypeID));
    if (genericParams) {
      SmallVector<GenericTypeParamType *, 4> paramTypes;
      for (auto &genericParam : *theEnum->getGenericParams()) {
        genericParam.getAsTypeParam()->setDeclContext(theEnum);
        paramTypes.push_back(genericParam.getAsTypeParam()->getDeclaredType()
                               ->castTo<GenericTypeParamType>());
      }

      // Read the generic requirements.
      SmallVector<Requirement, 4> requirements;
      readGenericRequirements(requirements);

      theEnum->setGenericSignature(paramTypes, requirements);
    }

    theEnum->computeType();
    CanType canTy = theEnum->getDeclaredTypeInContext()->getCanonicalType();

    SmallVector<ConformancePair, 16> conformances;
    while (auto conformance = maybeReadConformance(canTy, DeclTypeCursor))
      conformances.push_back(*conformance);
    processConformances(ctx, theEnum, conformances);

    auto members = readMembers();
    assert(members.hasValue() && "could not read enum members");
    theEnum->setMembers(members.getValue(), SourceRange());
    theEnum->setCheckedInheritanceClause();
    break;
  }

  case decls_block::ENUM_ELEMENT_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    TypeID argTypeID, ctorTypeID, interfaceTypeID;
    bool isImplicit;

    decls_block::EnumElementLayout::readRecord(scratch, nameID, contextID,
                                                argTypeID, ctorTypeID,
                                                interfaceTypeID, isImplicit);

    DeclContext *DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      break;

    auto argTy = getType(argTypeID);
    // FIXME: Deserialize the literal raw value, if any.
    auto elem = new (ctx) EnumElementDecl(SourceLoc(),
                                          getIdentifier(nameID),
                                          TypeLoc::withoutLoc(argTy),
                                          SourceLoc(),
                                          nullptr,
                                          DC);
    declOrOffset = elem;

    elem->setType(getType(ctorTypeID));
    if (auto interfaceType = getType(interfaceTypeID))
      elem->setInterfaceType(interfaceType);
    if (isImplicit)
      elem->setImplicit();

    break;
  }

  case decls_block::SUBSCRIPT_DECL: {
    DeclID contextID;
    bool isImplicit, isObjC, isOptional;
    TypeID declTypeID, elemTypeID, interfaceTypeID;
    DeclID getterID, setterID;
    DeclID overriddenID;

    decls_block::SubscriptLayout::readRecord(scratch, contextID, isImplicit,
                                             isObjC, isOptional,
                                             declTypeID, elemTypeID,
                                             interfaceTypeID,
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

    auto subscript = new (ctx) SubscriptDecl(ctx.getIdentifier("subscript"),
                                             SourceLoc(), indices, SourceLoc(),
                                             elemTy, SourceRange(),
                                             getter, setter, DC);
    declOrOffset = subscript;

    subscript->setType(getType(declTypeID));
    if (auto interfaceType = getType(interfaceTypeID))
      subscript->setInterfaceType(interfaceType);
    if (isImplicit)
      subscript->setImplicit();
    subscript->setIsObjC(isObjC);
    if (isOptional)
      subscript->getMutableAttrs().setAttr(AK_optional, SourceLoc());
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
    while (auto conformance = maybeReadConformance(canBaseTy, DeclTypeCursor))
      conformances.push_back(*conformance);
    processConformances(ctx, extension, conformances);

    auto members = readMembers();
    assert(members.hasValue() && "could not read extension members");
    extension->setMembers(members.getValue(), SourceRange());

    baseTy.getType()->getAnyNominal()->addExtension(extension);
    extension->setCheckedInheritanceClause();
    break;
  }

  case decls_block::DESTRUCTOR_DECL: {
    DeclID parentID;
    bool isImplicit, isObjC;
    TypeID signatureID;
    DeclID implicitSelfID;

    decls_block::DestructorLayout::readRecord(scratch, parentID, isImplicit,
                                              isObjC, signatureID,
                                              implicitSelfID);

    DeclContext *parent = getDeclContext(parentID);
    if (declOrOffset.isComplete())
      break;

    auto selfDecl = cast<VarDecl>(getDecl(implicitSelfID, nullptr));

    auto dtor = new (ctx) DestructorDecl(ctx.getIdentifier("destructor"),
                                         SourceLoc(), selfDecl, parent);
    declOrOffset = dtor;
    selfDecl->setDeclContext(dtor);

    dtor->setType(getType(signatureID));
    if (isImplicit)
      dtor->setImplicit();
    dtor->setIsObjC(isObjC);

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
    Module *M = getModule(rawAccessPath.front());
    assert(M && "missing dependency");
    rawAccessPath = rawAccessPath.slice(1);

    PrettyXRefDeserialization moreStackInfo(*this, static_cast<XRefKind>(kind),
                                            *M, rawAccessPath);

    switch (kind) {
    case XRefKind::SwiftValue:
    case XRefKind::SwiftGenericParameter: {
      // Start by looking up the top-level decl in the module.
      Module *baseModule = M;
      if (isWithinExtension) {
        baseModule = getModule(rawAccessPath.front());
        assert(baseModule && "missing dependency");
        rawAccessPath = rawAccessPath.slice(1);
      }

      // We do a full qualified lookup from within the referenced module.
      // This avoids inefficient access control restrictions from elsewhere in
      // this module, but handles the case where a declaration is no longer
      // *declared* in this module, merely re-exported. This also handles
      // adapter modules for Clang frameworks.
      SmallVector<ValueDecl *, 8> values;
      baseModule->lookupQualified(ModuleType::get(baseModule),
                                  getIdentifier(rawAccessPath.front()),
                                  NL_QualifiedDefault,
                                  /*typeResolver=*/nullptr,
                                  values);
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
        if (expectedTy &&
            value->getInterfaceType()->getCanonicalType() != expectedTy)
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
        declOrOffset = M->lookupInfixOperator(opName);
        break;
      }
      case OperatorKind::Prefix: {
        declOrOffset = M->lookupPrefixOperator(opName);
        break;
      }
      case OperatorKind::Postfix: {
        declOrOffset = M->lookupPostfixOperator(opName);
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
    DidRecord(declOrOffset);
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
  CASE(WitnessMethod)
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

/// Translate from the serialization ParameterConvention enumerators,
/// which are guaranteed to be stable, to the AST ones.
static
Optional<swift::ParameterConvention> getActualParameterConvention(uint8_t raw) {
  switch (serialization::ParameterConvention(raw)) {
#define CASE(ID) \
  case serialization::ParameterConvention::ID: \
    return swift::ParameterConvention::ID;
  CASE(Indirect_In)
  CASE(Indirect_Out)
  CASE(Indirect_Inout)
  CASE(Direct_Owned)
  CASE(Direct_Unowned)
  CASE(Direct_Guaranteed)
#undef CASE
  }
  return Nothing;
}

/// Translate from the serialization ResultConvention enumerators,
/// which are guaranteed to be stable, to the AST ones.
static
Optional<swift::ResultConvention> getActualResultConvention(uint8_t raw) {
  switch (serialization::ResultConvention(raw)) {
#define CASE(ID) \
  case serialization::ResultConvention::ID: return swift::ResultConvention::ID;
  CASE(Owned)
  CASE(Unowned)
  CASE(Autoreleased)
#undef CASE
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

  ASTContext &ctx = getContext();

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
    DeclID assocTypeOrProtoID;
    TypeID superclassID;
    ArrayRef<uint64_t> rawConformanceIDs;

    decls_block::ArchetypeTypeLayout::readRecord(scratch, nameID, isPrimary,
                                                 parentOrIndex,
                                                 assocTypeOrProtoID,
                                                 superclassID,
                                                 rawConformanceIDs);

    ArchetypeType *parent = nullptr;
    Type superclass;
    Optional<unsigned> index;
    SmallVector<ProtocolDecl *, 4> conformances;

    if (isPrimary)
      index = parentOrIndex;
    else
      parent = getType(parentOrIndex)->castTo<ArchetypeType>();

    ArchetypeType::AssocTypeOrProtocolType assocTypeOrProto;
    auto assocTypeOrProtoDecl = getDecl(assocTypeOrProtoID);
    if (auto assocType
          = dyn_cast_or_null<AssociatedTypeDecl>(assocTypeOrProtoDecl))
      assocTypeOrProto = assocType;
    else
      assocTypeOrProto = cast_or_null<ProtocolDecl>(assocTypeOrProtoDecl);

    superclass = getType(superclassID);

    for (DeclID protoID : rawConformanceIDs)
      conformances.push_back(cast<ProtocolDecl>(getDecl(protoID)));

    // See if we triggered deserialization through our conformances.
    if (typeOrOffset.isComplete())
      break;

    auto archetype = ArchetypeType::getNew(ctx, parent, assocTypeOrProto,
                                           getIdentifier(nameID), conformances,
                                           superclass, index);
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

  case decls_block::GENERIC_TYPE_PARAM_TYPE: {
    DeclID declIDOrDepth;
    unsigned indexPlusOne;

    decls_block::GenericTypeParamTypeLayout::readRecord(scratch, declIDOrDepth,
                                                        indexPlusOne);

    if (indexPlusOne == 0) {
      auto genericParam
        = dyn_cast_or_null<GenericTypeParamDecl>(getDecl(declIDOrDepth));

      if (!genericParam) {
        error();
        return nullptr;
      }

      // See if we triggered deserialization through our conformances.
      if (typeOrOffset.isComplete())
        break;

      typeOrOffset = genericParam->getDeclaredType();
      break;
    }

    typeOrOffset = GenericTypeParamType::get(declIDOrDepth,indexPlusOne-1,ctx);
    break;
  }

  case decls_block::ASSOCIATED_TYPE_TYPE: {
    DeclID declID;

    decls_block::AssociatedTypeTypeLayout::readRecord(scratch, declID);

    auto assocType = dyn_cast_or_null<AssociatedTypeDecl>(getDecl(declID));
    if (!assocType) {
      error();
      return nullptr;
    }

    // See if we triggered deserialization through our conformances.
    if (typeOrOffset.isComplete())
      break;
    
    typeOrOffset = assocType->getDeclaredType();
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

  case decls_block::DEPENDENT_MEMBER_TYPE: {
    TypeID baseID;
    DeclID assocTypeID;

    decls_block::DependentMemberTypeLayout::readRecord(scratch, baseID,
                                                       assocTypeID);
    typeOrOffset = DependentMemberType::get(
                     getType(baseID),
                     cast<AssociatedTypeDecl>(getDecl(assocTypeID)),
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

    GenericParamList *paramList =
      maybeGetOrReadGenericParams(genericContextID, FileContext);
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

  case decls_block::GENERIC_FUNCTION_TYPE: {
    TypeID inputID;
    TypeID resultID;
    uint8_t rawCallingConvention;
    bool thin;
    bool noreturn = false;
    ArrayRef<uint64_t> genericParamIDs;

    //todo add noreturn serialization.
    decls_block::GenericFunctionTypeLayout::readRecord(scratch,
                                                       inputID,
                                                       resultID,
                                                       rawCallingConvention,
                                                       thin,
                                                       noreturn,
                                                       genericParamIDs);
    auto callingConvention = getActualCC(rawCallingConvention);
    if (!callingConvention.hasValue()) {
      error();
      return nullptr;
    }

    // Read the generic parameters.
    SmallVector<GenericTypeParamType *, 4> genericParams;
    for (auto paramID : genericParamIDs) {
      auto param = dyn_cast_or_null<GenericTypeParamType>(
                     getType(paramID).getPointer());
      if (!param) {
        error();
        break;
      }

      genericParams.push_back(param);
    }

    // Read the generic requirements.
    SmallVector<Requirement, 4> requirements;
    readGenericRequirements(requirements);
    auto info = GenericFunctionType::ExtInfo(callingConvention.getValue(),
                                             thin,
                                             noreturn);

    typeOrOffset = GenericFunctionType::get(genericParams,
                                            requirements,
                                            getType(inputID),
                                            getType(resultID),
                                            info,
                                            ctx);
    break;
  }

  case decls_block::SIL_FUNCTION_TYPE: {
    TypeID resultID;
    uint8_t rawResultConvention;
    uint8_t rawCallingConvention;
    uint8_t rawCalleeConvention;
    bool thin;
    bool noreturn = false;
    DeclID genericContextID;
    ArrayRef<uint64_t> paramIDs;

    decls_block::SILFunctionTypeLayout::readRecord(scratch,
                                                   resultID,
                                                   rawResultConvention,
                                                   genericContextID,
                                                   rawCalleeConvention,
                                                   rawCallingConvention,
                                                   thin,
                                                   noreturn,
                                                   paramIDs);

    // Process the ExtInfo.
    auto callingConvention = getActualCC(rawCallingConvention);
    if (!callingConvention.hasValue()) {
      error();
      return nullptr;
    }
    SILFunctionType::ExtInfo extInfo(callingConvention.getValue(),
                                     thin, noreturn);

    // Process the result.
    auto resultConvention = getActualResultConvention(rawResultConvention);
    if (!resultConvention.hasValue()) {
      error();
      return nullptr;
    }
    SILResultInfo result(getType(resultID)->getCanonicalType(),
                         resultConvention.getValue());

    // Process the parameters.
    if (paramIDs.size() & 1) {
      error();
      return nullptr;
    }
    SmallVector<SILParameterInfo, 8> params;
    params.reserve(paramIDs.size() / 2);
    for (size_t i = 0, e = paramIDs.size(); i != e; i += 2) {
      auto type = getType(paramIDs[i])->getCanonicalType();
      auto convention = getActualParameterConvention(paramIDs[i+1]);
      if (!convention.hasValue()) {
        error();
        return nullptr;
      }
      SILParameterInfo param(type, convention.getValue());
      params.push_back(param);
    }

    // Process the callee convention.
    auto calleeConvention = getActualParameterConvention(rawCalleeConvention);
    if (!calleeConvention.hasValue()) {
      error();
      return nullptr;
    }

    // Read the generic parameters.
    auto genericParams =
      maybeGetOrReadGenericParams(genericContextID, FileContext);

    typeOrOffset = SILFunctionType::get(genericParams, extInfo,
                                        calleeConvention.getValue(),
                                        params, result, ctx);
    break;
  }

  case decls_block::ARRAY_SLICE_TYPE: {
    TypeID baseID;
    decls_block::ArraySliceTypeLayout::readRecord(scratch, baseID);

    auto sliceTy = ArraySliceType::get(getType(baseID), ctx);
    typeOrOffset = sliceTy;
    break;
  }

  case decls_block::OPTIONAL_TYPE: {
    TypeID baseID;
    decls_block::OptionalTypeLayout::readRecord(scratch, baseID);

    auto optionalTy = OptionalType::get(getType(baseID), ctx);
    typeOrOffset = optionalTy;
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
