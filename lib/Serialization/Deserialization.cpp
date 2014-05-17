//===--- Deserialization.cpp - Loading a serialized AST -------------------===//
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

#include "swift/Serialization/ModuleFile.h"
#include "swift/Serialization/ModuleFormat.h"
#include "swift/AST/AST.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Serialization/BCReadingExtras.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::serialization;

using ConformancePair = std::pair<ProtocolDecl *, ProtocolConformance *>;


namespace {
  struct IDAndKind {
    const Decl *D;
    DeclID ID;
  };

  static raw_ostream &operator<<(raw_ostream &os, IDAndKind &&pair) {
    return os << Decl::getKindName(pair.D->getKind())
              << "Decl #" << pair.ID;
  }

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
#include "swift/Serialization/DeclTypeRecordNodes.def"
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
        os << "'" << VD->getName() << "' (" << IDAndKind{VD, ID} << ") \n";
      } else if (auto ED = dyn_cast<ExtensionDecl>(DeclOrOffset.get())) {
        os << "extension of '" << ED->getExtendedType() << "' ("
           << IDAndKind{ED, ID} << ") \n";
      } else {
        os << IDAndKind{DeclOrOffset.get(), ID} << "\n";
      }
    }
  };

  class PrettyXRefTrace : public llvm::PrettyStackTraceEntry {
    class PathPiece {
    public:
      enum class Kind {
        Value,
        Operator,
        OperatorFilter,
        Accessor,
        Extension,
        GenericParam,
        Unknown
      };

    private:
      Kind kind;
      void *data;

      template <typename T>
      T getDataAs() const {
        return llvm::PointerLikeTypeTraits<T>::getFromVoidPointer(data);
      }

    public:
      template <typename T>
      PathPiece(Kind K, T value)
        : kind(K),
          data(llvm::PointerLikeTypeTraits<T>::getAsVoidPointer(value)) {}

      void print(raw_ostream &os) const {
        switch (kind) {
        case Kind::Value:
          os << getDataAs<Identifier>();
          break;
        case Kind::Extension:
          if (getDataAs<Module *>())
            os << "in an extension in module '" << getDataAs<Module *>()->Name
               << "'";
          else
            os << "in an extension in any module";
          break;
        case Kind::Operator:
          os << "operator " << getDataAs<Identifier>();
          break;
        case Kind::OperatorFilter:
          switch (getDataAs<uintptr_t>()) {
          case Infix:
            os << "(infix)";
            break;
          case Prefix:
            os << "(prefix)";
            break;
          case Postfix:
            os << "(postfix)";
            break;
          default:
            os << "(unknown operator filter)";
            break;
          }
          break;
        case Kind::Accessor:
          switch (getDataAs<uintptr_t>()) {
          case Getter:
            os << "(getter)";
            break;
          case Setter:
            os << "(setter)";
            break;
          case WillSet:
            os << "(willSet)";
            break;
          case DidSet:
            os << "(didSet)";
            break;
          default:
            os << "(unknown accessor kind)";
            break;
          }
          break;
        case Kind::GenericParam:
          os << "generic param #" << getDataAs<uintptr_t>();
          break;
        case Kind::Unknown:
          os << "unknown xref kind " << getDataAs<uintptr_t>();
          break;
        }
      }
    };

  private:
    Module &baseM;
    SmallVector<PathPiece, 8> path;

  public:
    PrettyXRefTrace(Module &M) : baseM(M) {}

    void addValue(Identifier name) {
      path.push_back({ PathPiece::Kind::Value, name });
    }

    void addOperator(Identifier name) {
      path.push_back({ PathPiece::Kind::Operator, name });
    }

    void addOperatorFilter(uint8_t fixity) {
      path.push_back({ PathPiece::Kind::OperatorFilter,
                       static_cast<uintptr_t>(fixity) });
    }

    void addAccessor(uint8_t kind) {
      path.push_back({ PathPiece::Kind::Accessor,
                       static_cast<uintptr_t>(kind) });
    }

    void addExtension(Module *M) {
      path.push_back({ PathPiece::Kind::Extension, M });
    }

    void addGenericParam(uintptr_t index) {
      path.push_back({ PathPiece::Kind::GenericParam, index });
    }

    void addUnknown(uintptr_t kind) {
      path.push_back({ PathPiece::Kind::Unknown, kind });
    }

    virtual void print(raw_ostream &os) const override {
      os << "Cross-reference to module '" << baseM.Name << "'\n";
      for (auto &piece : path) {
        os << "\t... ";
        piece.print(os);
        os << "\n";
      }
    }
  };
} // end anonymous namespace


/// Skips a single record in the bitstream.
///
/// Returns true if the next entry is a record of type \p recordKind.
/// Destroys the stream position if the next entry is not a record.
static bool skipRecord(llvm::BitstreamCursor &cursor, unsigned recordKind) {
  auto next = cursor.advance(AF_DontPopBlockAtEnd);
  if (next.Kind != llvm::BitstreamEntry::Record)
    return false;

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;

#if NDEBUG
  cursor.skipRecord(next.ID);
  return true;
#else
  unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
  return kind == recordKind;
#endif
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
  case serialization::DefaultArgumentKind::Inherited:
    return swift::DefaultArgumentKind::Inherited;
  case serialization::DefaultArgumentKind::Column:
    return swift::DefaultArgumentKind::Column;
  case serialization::DefaultArgumentKind::File:
    return swift::DefaultArgumentKind::File;
  case serialization::DefaultArgumentKind::Line:
    return swift::DefaultArgumentKind::Line;
  case serialization::DefaultArgumentKind::Function:
    return swift::DefaultArgumentKind::Function;
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
  case decls_block::VAR_PATTERN: {
    bool isImplicit;
    VarPatternLayout::readRecord(scratch, isImplicit);
    Pattern *subPattern = maybeReadPattern();
    assert(subPattern);

    auto result = new (getContext()) VarPattern(SourceLoc(), subPattern,
                                                isImplicit);
    result->setType(subPattern->getType());
    return result;
  }

  default:
    return nullptr;
  }
}

/// Find a (possibly-inherited) conformance for a particular protocol.
// FIXME: Checking the module is not very resilient. What if the conformance is
// moved into a re-exported module instead?
static ProtocolConformance *findConformance(ProtocolDecl *proto,
                                            const Module *module,
                                            ProtocolConformance *conformance) {
  if (!conformance)
    return nullptr;

  if (conformance->getProtocol() == proto) {
    if (conformance->getDeclContext()->getParentModule() == module)
      return conformance;
    return nullptr;
  }

  auto &inheritedMap = conformance->getInheritedConformances();
  auto directIter = inheritedMap.find(proto);
  if (directIter != inheritedMap.end()) {
    if (directIter->second->getDeclContext()->getParentModule() == module)
      return directIter->second;
    return nullptr;
  }

  for (auto inheritedEntry : inheritedMap)
    if (auto result = findConformance(proto, module, inheritedEntry.second))
      return result;

  return nullptr;
}


ProtocolConformance *
ModuleFile::readReferencedConformance(ProtocolDecl *proto,
                                      DeclID typeID,
                                      IdentifierID moduleID,
                                      llvm::BitstreamCursor &Cursor) {
  if (moduleID == serialization::BUILTIN_MODULE_ID) {
    // The underlying conformance is in the following record.
    return *maybeReadConformance(getType(typeID), Cursor);
  }

  // Dig out the protocol conformance within the nominal declaration.
  auto nominal = cast<NominalTypeDecl>(getDecl(typeID));
  Module *owningModule = getModule(moduleID);

  // Search protocols
  for (auto conformance : nominal->getConformances())
    if (auto result = findConformance(proto, owningModule, conformance))
      return result;

  // Search extensions.
  for (auto ext : nominal->getExtensions())
    for (auto conformance : ext->getConformances())
      if (auto result = findConformance(proto, owningModule, conformance))
        return result;

  llvm_unreachable("Unable to find underlying conformance");
}

Optional<ProtocolConformance *>
ModuleFile::maybeReadConformance(Type conformingType,
                                 llvm::BitstreamCursor &Cursor) {
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
    return nullptr;
  }

  case NORMAL_PROTOCOL_CONFORMANCE:
    // Handled below.
    break;

  case SPECIALIZED_PROTOCOL_CONFORMANCE: {
    DeclID protoID;
    DeclID typeID;
    ModuleID moduleID;
    unsigned numSubstitutions;
    SpecializedProtocolConformanceLayout::readRecord(scratch, protoID,
                                                     typeID,
                                                     moduleID,
                                                     numSubstitutions);

    ASTContext &ctx = getContext();

    auto proto = cast<ProtocolDecl>(getDecl(protoID));

    // Read the substitutions.
    SmallVector<Substitution, 4> substitutions;
    while (numSubstitutions--) {
      auto sub = maybeReadSubstitution(Cursor);
      assert(sub.hasValue() && "Missing substitution?");
      substitutions.push_back(*sub);
    }

    ProtocolConformance *genericConformance
      = readReferencedConformance(proto, typeID, moduleID, Cursor);

    // Reset the offset RAII to the end of the trailing records.
    lastRecordOffset.reset();

    assert(genericConformance && "Missing generic conformance?");
    return ctx.getSpecializedConformance(conformingType, genericConformance,
                                         ctx.AllocateCopy(substitutions));
  }

  case INHERITED_PROTOCOL_CONFORMANCE: {
    DeclID protoID;
    DeclID typeID;
    ModuleID moduleID;
    InheritedProtocolConformanceLayout::readRecord(scratch, protoID,
                                                   typeID,
                                                   moduleID);

    ASTContext &ctx = getContext();

    auto proto = cast<ProtocolDecl>(getDecl(protoID));

    ProtocolConformance *inheritedConformance
      = readReferencedConformance(proto, typeID, moduleID, Cursor);

    // Reset the offset RAII to the end of the trailing records.
    lastRecordOffset.reset();
    assert(inheritedConformance && "Missing generic conformance?");
    return ctx.getInheritedConformance(conformingType, inheritedConformance);
  }

  // Not a protocol conformance.
  default:
    return Nothing;
  }

  lastRecordOffset.reset();

  DeclID protoID;
  ModuleID ownerID;
  unsigned valueCount, typeCount, inheritedCount, defaultedCount;
  bool isIncomplete;
  ArrayRef<uint64_t> rawIDs;

  NormalProtocolConformanceLayout::readRecord(scratch, protoID, ownerID,
                                              valueCount, typeCount,
                                              inheritedCount, defaultedCount,
                                              isIncomplete, rawIDs);

  auto proto = cast<ProtocolDecl>(getDecl(protoID));
  ASTContext &ctx = getContext();
  auto conformance = ctx.getConformance(conformingType, proto, SourceLoc(),
                                        getModule(ownerID),
                                        ProtocolConformanceState::Incomplete);

  InheritedConformanceMap inheritedConformances;

  while (inheritedCount--) {
    auto inherited = maybeReadConformance(conformingType, Cursor);
    assert(inherited.hasValue());

    inheritedConformances[(*inherited)->getProtocol()] = *inherited;
  }

  // Reset the offset RAII to the end of the trailing records.
  lastRecordOffset.reset();

  if (conformance->getState() == ProtocolConformanceState::Incomplete)
    if (conformance->getInheritedConformances().empty())
      for (auto inherited : inheritedConformances)
        conformance->setInheritedConformance(inherited.first, inherited.second);

  if (isIncomplete)
    return conformance;

  WitnessMap witnesses;
  ArrayRef<uint64_t>::iterator rawIDIter = rawIDs.begin();
  while (valueCount--) {
    auto first = cast<ValueDecl>(getDecl(*rawIDIter++));
    auto second = cast_or_null<ValueDecl>(getDecl(*rawIDIter++));
    assert(second || first->getAttrs().isOptional());

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
    if (second)
      ctx.recordConformingDecl(second, first);
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

  // If we have a complete protocol conformance do not attempt to initialize
  // it. Just return the conformance.
  if (conformance->getState() == ProtocolConformanceState::Complete)
    return conformance;

  // Set type witnesses.
  for (auto typeWitness : typeWitnesses) {
    conformance->setTypeWitness(typeWitness.first, typeWitness.second);
  }

  // Set witnesses.
  for (auto witness : witnesses) {
    conformance->setWitness(witness.first, witness.second);
  }

  // Note any defaulted definitions.
  for (auto defaulted : defaultedDefinitions) {
    conformance->addDefaultDefinition(defaulted);
  }

  conformance->setState(ProtocolConformanceState::Complete);
  return conformance;
}

Optional<Substitution>
ModuleFile::maybeReadSubstitution(llvm::BitstreamCursor &cursor) {
  BCOffsetRAII lastRecordOffset(cursor);

  auto entry = cursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind != llvm::BitstreamEntry::Record)
    return Nothing;

  StringRef blobData;
  SmallVector<uint64_t, 2> scratch;
  unsigned recordID = cursor.readRecord(entry.ID, scratch, &blobData);
  if (recordID != decls_block::BOUND_GENERIC_SUBSTITUTION)
    return Nothing;


  TypeID archetypeID, replacementID;
  ArrayRef<uint64_t> rawConformanceData;
  decls_block::BoundGenericSubstitutionLayout::readRecord(scratch,
                                                          archetypeID,
                                                          replacementID,
                                                          rawConformanceData);

  auto archetypeTy = getType(archetypeID)->castTo<ArchetypeType>();
  auto replacementTy = getType(replacementID);

  SmallVector<ProtocolConformance *, 16> conformanceBuf;
  ArrayRef<ProtocolDecl *> protos = archetypeTy->getConformsTo();
  assert(rawConformanceData.size() % 2 == 0);
  while (!rawConformanceData.empty()) {
    ProtocolConformance *conformance = nullptr;
    // A null associated type means there is no conformance.
    if (DeclID(rawConformanceData[0])) {
      conformance = readReferencedConformance(protos.front(),
                                              rawConformanceData[0],
                                              rawConformanceData[1],
                                              cursor);
      assert(conformance && "Missing conformance");
    }
    conformanceBuf.push_back(conformance);

    protos = protos.slice(1);
    rawConformanceData = rawConformanceData.slice(2);
  }

  lastRecordOffset.reset();
  return Substitution{archetypeTy, replacementTy,
                      getContext().AllocateCopy(conformanceBuf)};
}

GenericParamList *
ModuleFile::maybeGetOrReadGenericParams(serialization::DeclID genericContextID,
                                        DeclContext *DC,
                                        llvm::BitstreamCursor &Cursor) {
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
    return maybeReadGenericParams(DC, Cursor);
  }
}

GenericParamList *ModuleFile::maybeReadGenericParams(DeclContext *DC,
                                               llvm::BitstreamCursor &Cursor,
                                               GenericParamList *outerParams) {
  using namespace decls_block;

  assert(DC && "need a context for the decls in the list");

  BCOffsetRAII lastRecordOffset(Cursor);
  SmallVector<uint64_t, 8> scratch;
  StringRef blobData;

  auto next = Cursor.advance(AF_DontPopBlockAtEnd);
  if (next.Kind != llvm::BitstreamEntry::Record)
    return nullptr;

  // Read the raw archetype IDs into a different scratch buffer
  // because we need to keep this alive for longer.
  SmallVector<uint64_t, 4> rawArchetypeIDsBuffer;
  unsigned kind = Cursor.readRecord(next.ID, rawArchetypeIDsBuffer, &blobData);
  if (kind != GENERIC_PARAM_LIST)
    return nullptr;

  // Read in the raw-archetypes buffer, but don't try to consume it yet.
  ArrayRef<uint64_t> rawArchetypeIDs;
  GenericParamListLayout::readRecord(rawArchetypeIDsBuffer, rawArchetypeIDs);

  SmallVector<GenericParam, 8> params;
  SmallVector<RequirementRepr, 8> requirements;
  SmallVector<ArchetypeType *, 8> archetypes;

  // The GenericTypeParamDecls might be from a different module file.
  // If so, we need to map the archetype IDs from the serialized
  // all-archetypes list in this module file over to the corresponding
  // archetypes from the original generic parameter decls, or else
  // we'll end up constructing fresh archetypes that don't match the
  // ones from the generic parameters.

  // We have to do this mapping before we might call getType on one of
  // those archetypes, but after we've read all the generic parameters.
  // Therefore we do it lazily.
  bool haveMappedArchetypes = false;
  auto mapArchetypes = [&] {
    if (haveMappedArchetypes) return;

    GenericParamList::deriveAllArchetypes(params, archetypes);
    assert(rawArchetypeIDs.size() == archetypes.size());
    for (unsigned index : indices(rawArchetypeIDs)) {
      TypeID TID = rawArchetypeIDs[index];
      auto &typeOrOffset = Types[TID-1];
      if (typeOrOffset.isComplete()) {
        // FIXME: this assertion is absolutely correct, but it's
        // currently fouled up by the presence of archetypes in
        // substitutions.  Those *should* be irrelevant for all the
        // cases where this is wrong, but...

        //assert(typeOrOffset.get().getPointer() == archetypes[index] &&
        //       "already deserialized this archetype to a different type!");

        // TODO: remove unsafeOverwrite when this hack goes away
        typeOrOffset.unsafeOverwrite(archetypes[index]);
      } else {
        typeOrOffset = archetypes[index];
      }
    }

    haveMappedArchetypes = true;
  };

  while (true) {
    lastRecordOffset.reset();
    bool shouldContinue = true;

    auto entry = Cursor.advance(AF_DontPopBlockAtEnd);
    if (entry.Kind != llvm::BitstreamEntry::Record)
      break;

    scratch.clear();
    unsigned recordID = Cursor.readRecord(entry.ID, scratch,
                                                  &blobData);
    switch (recordID) {
    case GENERIC_PARAM: {
      assert(!haveMappedArchetypes &&
             "generic parameters interleaved with requirements?");
      DeclID paramDeclID;
      GenericParamLayout::readRecord(scratch, paramDeclID);
      auto genericParam = cast<GenericTypeParamDecl>(getDecl(paramDeclID, DC));
      // FIXME: There are unfortunate inconsistencies in the treatment of
      // generic param decls. Currently the first request for context wins
      // because we don't want to change context on-the-fly.
      // Here are typical scenarios:
      // (1) AST reads decl, get's scope.
      //     Later, readSILFunction tries to force module scope.
      // (2) readSILFunction forces module scope.
      //     Later, readVTable requests an enclosing scope.
      // ...other combinations are possible, but as long as AST lookups
      // precede SIL linkage, we should be ok.
      assert((genericParam->getDeclContext()->isModuleScopeContext() ||
              DC->isModuleScopeContext() ||
              genericParam->getDeclContext() == DC) &&
             "Mismatched decl context for generic types.");
      params.push_back(GenericParam(genericParam));
      break;
    }
    case GENERIC_REQUIREMENT: {
      uint8_t rawKind;
      ArrayRef<uint64_t> rawTypeIDs;
      GenericRequirementLayout::readRecord(scratch, rawKind, rawTypeIDs);

      mapArchetypes();

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

      case WitnessMarker: {
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

  // Make sure we map the archetypes if we haven't yet.
  mapArchetypes();

  auto paramList = GenericParamList::create(getContext(), SourceLoc(),
                                            params, SourceLoc(), requirements,
                                            SourceLoc());
  paramList->setAllArchetypes(getContext().AllocateCopy(archetypes));
  paramList->setOuterParameters(outerParams ? outerParams :
                                DC->getGenericParamsOfContext());

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
      case GenericRequirementKind::WitnessMarker: {
        assert(rawTypeIDs.size() == 1);
        auto first = getType(rawTypeIDs[0]);

        requirements.push_back(Requirement(RequirementKind::WitnessMarker,
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
  MutableArrayRef<Decl *> members = ctx.Allocate<Decl *>(rawMemberIDs.size());

  auto nextMember = members.begin();
  for (DeclID rawID : rawMemberIDs) {
    *nextMember = getDecl(rawID);
    assert(*nextMember && "unable to deserialize next member");
    ++nextMember;
  }

  return members;
}

/// Remove values from \p values that don't match the expected type or module.
///
/// Both \p expectedTy and \p expectedModule can be omitted, in which case any
/// type or module is accepted. Values imported from Clang can also appear in
/// any module.
static void filterValues(Type expectedTy, Module *expectedModule,
                         SmallVectorImpl<ValueDecl *> &values) {
  CanType canTy;
  if (expectedTy)
    canTy = expectedTy->getCanonicalType();

  auto newEnd = std::remove_if(values.begin(), values.end(),
                               [=](ValueDecl *value) {
    if (canTy && value->getInterfaceType()->getCanonicalType() != canTy)
      return true;
    // FIXME: Should be able to move a value from an extension in a derived
    // module to the original definition in a base module.
    if (expectedModule && !value->hasClangNode() &&
        value->getModuleContext() != expectedModule)
      return true;
    return false;
  });
  values.erase(newEnd, values.end());
}

Decl *ModuleFile::resolveCrossReference(Module *M, uint32_t pathLen) {
  using namespace decls_block;
  assert(M && "missing dependency");
  PrettyXRefTrace pathTrace(*M);

  auto entry = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind != llvm::BitstreamEntry::Record) {
    error();
    return nullptr;
  }

  SmallVector<ValueDecl *, 8> values;
  SmallVector<uint64_t, 8> scratch;
  StringRef blobData;

  // Read the first path piece. This one is special because lookup is performed
  // against the base module, rather than against the previous link in the path.
  // In particular, operator path pieces represent actual operators here, but
  // filters on operator functions when they appear later on.
  scratch.clear();
  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch,
                                                &blobData);
  switch (recordID) {
  case XREF_TYPE_PATH_PIECE:
  case XREF_VALUE_PATH_PIECE: {
    IdentifierID IID;
    TypeID TID = 0;
    if (recordID == XREF_TYPE_PATH_PIECE)
      XRefTypePathPieceLayout::readRecord(scratch, IID);
    else
      XRefValuePathPieceLayout::readRecord(scratch, TID, IID);

    Identifier name = getIdentifier(IID);
    pathTrace.addValue(name);

    M->lookupQualified(ModuleType::get(M), name, NL_QualifiedDefault,
                       /*typeResolver=*/nullptr, values);
    filterValues(getType(TID), nullptr, values);
    break;
  }

  case XREF_EXTENSION_PATH_PIECE:
    llvm_unreachable("can only extend a nominal");

  case XREF_OPERATOR_OR_ACCESSOR_PATH_PIECE: {
    IdentifierID IID;
    uint8_t rawOpKind;
    XRefOperatorOrAccessorPathPieceLayout::readRecord(scratch, IID, rawOpKind);

    Identifier opName = getIdentifier(IID);
    pathTrace.addOperator(opName);

    switch (rawOpKind) {
    case OperatorKind::Infix:
      return M->lookupInfixOperator(opName);
    case OperatorKind::Prefix:
      return M->lookupPrefixOperator(opName);
    case OperatorKind::Postfix:
      return M->lookupPostfixOperator(opName);
    default:
      // Unknown operator kind.
      error();
      return nullptr;
    }
  }

  case XREF_GENERIC_PARAM_PATH_PIECE:
    llvm_unreachable("only in a nominal or function");

  default:
    // Unknown xref kind.
    pathTrace.addUnknown(recordID);
    error();
    return nullptr;
  }

  if (values.empty()) {
    error();
    return nullptr;
  }

  // Reset module filter.
  M = nullptr;

  // For remaining path pieces, filter or drill down into the results we have.
  while (--pathLen) {
    auto entry = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
    if (entry.Kind != llvm::BitstreamEntry::Record) {
      error();
      return nullptr;
    }

    scratch.clear();
    unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch,
                                                  &blobData);
    switch (recordID) {
    case XREF_TYPE_PATH_PIECE:
    case XREF_VALUE_PATH_PIECE: {
      IdentifierID IID;
      TypeID TID = 0;
      if (recordID == XREF_TYPE_PATH_PIECE)
        XRefTypePathPieceLayout::readRecord(scratch, IID);
      else
        XRefValuePathPieceLayout::readRecord(scratch, TID, IID);

      Identifier memberName = getIdentifier(IID);
      pathTrace.addValue(memberName);

      if (values.size() != 1) {
        error();
        return nullptr;
      }

      auto nominal = dyn_cast<NominalTypeDecl>(values.front());
      values.clear();

      if (!nominal) {
        error();
        return nullptr;
      }

      auto members = nominal->lookupDirect(memberName);
      values.append(members.begin(), members.end());
      filterValues(getType(TID), M, values);
      break;
    }

    case XREF_EXTENSION_PATH_PIECE: {
      ModuleID ownerID;
      XRefExtensionPathPieceLayout::readRecord(scratch, ownerID);
      M = getModule(ownerID);
      pathTrace.addExtension(M);
      continue;
    }

    case XREF_OPERATOR_OR_ACCESSOR_PATH_PIECE: {
      uint8_t rawKind;
      XRefOperatorOrAccessorPathPieceLayout::readRecord(scratch, Nothing,
                                                        rawKind);

      if (values.size() == 1) {
        if (auto storage = dyn_cast<AbstractStorageDecl>(values.front())) {
          pathTrace.addAccessor(rawKind);
          switch (rawKind) {
          case Getter:
            values.front() = storage->getGetter();
            break;
          case Setter:
            values.front() = storage->getSetter();
            break;
          case WillSet:
          case DidSet:
            llvm_unreachable("invalid XREF accessor kind");
          default:
            // Unknown accessor kind.
            error();
            return nullptr;
          }

          break;
        }
      }

      pathTrace.addOperatorFilter(rawKind);

      auto newEnd = std::remove_if(values.begin(), values.end(),
                                   [=](ValueDecl *value) {
        auto fn = dyn_cast<FuncDecl>(value);
        if (!fn)
          return true;
        if (!fn->getOperatorDecl())
          return true;
        if (getStableFixity(fn->getOperatorDecl()->getKind()) != rawKind)
          return true;
        return false;
      });
      values.erase(newEnd, values.end());
      break;
    }

    case XREF_GENERIC_PARAM_PATH_PIECE: {
      if (values.size() != 1) {
        error();
        return nullptr;
      }

      uint32_t paramIndex;
      XRefGenericParamPathPieceLayout::readRecord(scratch, paramIndex);

      pathTrace.addGenericParam(paramIndex);

      ValueDecl *base = values.front();
      GenericParamList *paramList = nullptr;

      if (auto nominal = dyn_cast<NominalTypeDecl>(base))
        paramList = nominal->getGenericParams();
      else if (auto fn = dyn_cast<FuncDecl>(base))
        paramList = fn->getGenericParams();
      else if (auto ctor = dyn_cast<ConstructorDecl>(base))
        paramList = ctor->getGenericParams();

      if (!paramList || paramIndex >= paramList->size()) {
        error();
        return nullptr;
      }

      values.clear();
      values.push_back(paramList->getParams()[paramIndex].getDecl());
      assert(values.back());
      break;
    }

    default:
      // Unknown xref path piece.
      pathTrace.addUnknown(recordID);
      error();
      return nullptr;
    }

    if (values.empty()) {
      error();
      return nullptr;
    }

    // Reset the module filter.
    M = nullptr;
  }

  // Make sure we /used/ the last module filter we got.
  // This catches the case where the last path piece we saw was an Extension
  // path piece, which is not a valid way to end a path. (Cross-references to
  // extensions are not allowed because they cannot be uniquely named.)
  if (M) {
    error();
    return nullptr;
  }

  // When all is said and done, we should have a single value here to return.
  if (values.size() != 1) {
    error();
    return nullptr;
  }

  return values.front();
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
  if (MID < NUM_SPECIAL_MODULES) {
    switch (static_cast<SpecialModuleID>(static_cast<uint8_t>(MID))) {
    case BUILTIN_MODULE_ID:
      return getContext().TheBuiltinModule;
    case CURRENT_MODULE_ID:
      return FileContext->getParentModule();
    case OBJC_HEADER_MODULE_ID: {
      auto clangImporter =
        static_cast<ClangImporter *>(getContext().getClangModuleLoader());
      return clangImporter->getImportedHeaderModule();
    }
    case NUM_SPECIAL_MODULES:
      llvm_unreachable("implementation detail only");
    }
  }
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

static Optional<swift::StaticSpellingKind>
getActualStaticSpellingKind(uint8_t raw) {
  switch (serialization::StaticSpellingKind(raw)) {
  case serialization::StaticSpellingKind::None:
    return swift::StaticSpellingKind::None;
  case serialization::StaticSpellingKind::KeywordStatic:
    return swift::StaticSpellingKind::KeywordStatic;
  case serialization::StaticSpellingKind::KeywordClass:
    return swift::StaticSpellingKind::KeywordClass;
  }
  return Nothing;
}

static bool isDeclAttrRecord(unsigned ID) {
  using namespace decls_block;
  switch (ID) {
#define DECL_ATTR(NAME, CLASS, ...) \
  case CLASS##_DECL_ATTR: return true;
#define VIRTUAL_DECL_ATTR(NAME, CLASS, ...)
#include "swift/Serialization/DeclTypeRecordNodes.def"
  default: return false;
  }
}

static Optional<swift::CtorInitializerKind>
getActualCtorInitializerKind(uint8_t raw) {
  switch (serialization::CtorInitializerKind(raw)) {
  case serialization::Designated:
    return swift::CtorInitializerKind::Designated;

  case serialization::Convenience:
    return swift::CtorInitializerKind::Convenience;
  }
  return Nothing;
}

Decl *ModuleFile::getDecl(DeclID DID, Optional<DeclContext *> ForcedContext) {
  if (DID == 0)
    return nullptr;

  assert(DID <= Decls.size() && "invalid decl ID");
  auto &declOrOffset = Decls[DID-1];

  if (declOrOffset.isComplete())
    return declOrOffset;

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

  // Read the attributes (if any).
  DeclAttribute *DAttrs = nullptr;
  DeclAttribute **AttrsNext = &DAttrs;
  auto AddAttribute = [&](DeclAttribute *Attr) {
    // Advance the linked list.
    *AttrsNext = Attr;
    AttrsNext = Attr->getMutableNext();
  };
  unsigned recordID;

  while (true) {
    if (entry.Kind != llvm::BitstreamEntry::Record) {
      // We don't know how to serialize decls represented by sub-blocks.
      error();
      return nullptr;
    }

    recordID = DeclTypeCursor.readRecord(entry.ID, scratch, &blobData);

    if (isDeclAttrRecord(recordID)) {
      DeclAttribute *Attr = nullptr;
      switch (recordID) {
      case decls_block::Asmname_DECL_ATTR:
        bool isImplicit;
        serialization::decls_block::AsmnameDeclAttrLayout::readRecord(
            scratch, isImplicit);
        Attr = new (ctx) AsmnameAttr(blobData, isImplicit);
        break;

      case decls_block::Availability_DECL_ATTR: {
        bool isImplicit;
        bool isUnavailable;
        unsigned platform;
        unsigned messageSize;
        serialization::decls_block::AvailabilityDeclAttrLayout::readRecord(
            scratch, isImplicit, isUnavailable, platform, messageSize);
        StringRef message = blobData;
        Attr = new (ctx) AvailabilityAttr(SourceLoc(), SourceRange(),
                                       (AvailabilityAttr::PlatformKind)platform,
                                       message, isUnavailable, isImplicit);
        break;
      }

      case decls_block::ObjC_DECL_ATTR: {
        bool isImplicit;
        uint64_t numArgs;
        ArrayRef<uint64_t> rawPieceIDs;
        serialization::decls_block::ObjCDeclAttrLayout::readRecord(
            scratch, isImplicit, numArgs, rawPieceIDs);

        SmallVector<Identifier, 4> pieces;
        for (auto pieceID : rawPieceIDs)
          pieces.push_back(getIdentifier(pieceID));

        if (numArgs == 0)
          Attr = ObjCAttr::create(ctx, Nothing);
        else
          Attr = ObjCAttr::create(ctx, ObjCSelector(ctx, numArgs-1, pieces));
        break;
      }

#define SIMPLE_DECL_ATTR(NAME, CLASS, ...) \
      case decls_block::CLASS##_DECL_ATTR: { \
        bool isImplicit; \
        serialization::decls_block::CLASS##DeclAttrLayout::readRecord( \
            scratch, isImplicit); \
        Attr = new (ctx) CLASS##Attr(isImplicit); \
        break; \
      }
#include "swift/AST/Attr.def"

      default:
        // We don't know how to deserialize this kind of attribute.
        error();
        return nullptr;
      }

      if (!Attr)
        return nullptr;

      AddAttribute(Attr);

      // Advance bitstream cursor to the next record.
      entry = DeclTypeCursor.advance();

      // Prepare to read the next record.
      scratch.clear();
      continue;
    }

    break;
  }

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
      return declOrOffset;

    auto alias = new (ctx) TypeAliasDecl(SourceLoc(), getIdentifier(nameID),
                                         SourceLoc(), underlyingType, DC);
    declOrOffset = alias;

    if (auto interfaceType = getType(interfaceTypeID))
      alias->setInterfaceType(interfaceType);

    if (isImplicit)
      alias->setImplicit();

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
    ArrayRef<uint64_t> rawProtocolIDs;

    decls_block::GenericTypeParamDeclLayout::readRecord(scratch, nameID,
                                                        contextID,
                                                        isImplicit,
                                                        depth,
                                                        index,
                                                        superclassID,
                                                        archetypeID,
                                                        rawProtocolIDs);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);

    if (declOrOffset.isComplete())
      return declOrOffset;

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

    auto protos = ctx.Allocate<ProtocolDecl *>(rawProtocolIDs.size());
    for_each(protos, rawProtocolIDs, [this](ProtocolDecl *&p, uint64_t rawID) {
      p = cast<ProtocolDecl>(getDecl(rawID));
    });
    genericParam->setProtocols(protos);

    genericParam->setCheckedInheritanceClause();
    break;
  }

  case decls_block::ASSOCIATED_TYPE_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    TypeID superclassID;
    TypeID archetypeID;
    TypeID defaultDefinitionID;
    bool isImplicit;
    ArrayRef<uint64_t> rawProtocolIDs;

    decls_block::AssociatedTypeDeclLayout::readRecord(scratch, nameID,
                                                      contextID,
                                                      superclassID,
                                                      archetypeID,
                                                      defaultDefinitionID,
                                                      isImplicit,
                                                      rawProtocolIDs);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto assocType = new (ctx) AssociatedTypeDecl(DC, SourceLoc(),
                                                  getIdentifier(nameID),
                                                  SourceLoc(), this,
                                                  defaultDefinitionID);
    declOrOffset = assocType;

    assocType->setSuperclass(getType(superclassID));
    assocType->setArchetype(getType(archetypeID)->castTo<ArchetypeType>());
    if (isImplicit)
      assocType->setImplicit();

    auto protos = ctx.Allocate<ProtocolDecl *>(rawProtocolIDs.size());
    for_each(protos, rawProtocolIDs, [this](ProtocolDecl *&p, uint64_t rawID) {
      p = cast<ProtocolDecl>(getDecl(rawID));
    });
    assocType->setProtocols(protos);

    assocType->setCheckedInheritanceClause();
    break;
  }

  case decls_block::STRUCT_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;
    ArrayRef<uint64_t> rawProtocolIDs;

    decls_block::StructLayout::readRecord(scratch, nameID, contextID,
                                          isImplicit, rawProtocolIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto genericParams = maybeReadGenericParams(DC, DeclTypeCursor);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto theStruct = new (ctx) StructDecl(SourceLoc(), getIdentifier(nameID),
                                          SourceLoc(), { }, genericParams, DC);
    declOrOffset = theStruct;

    if (isImplicit)
      theStruct->setImplicit();
    if (genericParams) {
      SmallVector<GenericTypeParamType *, 4> paramTypes;
      for (auto &genericParam : *theStruct->getGenericParams()) {
        paramTypes.push_back(genericParam.getAsTypeParam()->getDeclaredType()
                               ->castTo<GenericTypeParamType>());
      }

      // Read the generic requirements.
      SmallVector<Requirement, 4> requirements;
      readGenericRequirements(requirements);

      auto sig = GenericSignature::get(paramTypes, requirements);
      theStruct->setGenericSignature(sig);
    }

    theStruct->computeType();

    auto protocols = ctx.Allocate<ProtocolDecl *>(rawProtocolIDs.size());
    for_each(protocols, rawProtocolIDs, [this](ProtocolDecl *&p,
                                               uint64_t rawID) {
      p = cast<ProtocolDecl>(getDecl(rawID));
    });
    theStruct->setProtocols(protocols);

    theStruct->setMemberLoader(this, DeclTypeCursor.GetCurrentBitNo());
    skipRecord(DeclTypeCursor, decls_block::DECL_CONTEXT);
    theStruct->setConformanceLoader(this, DeclTypeCursor.GetCurrentBitNo());

    theStruct->setCheckedInheritanceClause();
    break;
  }

  case decls_block::CONSTRUCTOR_DECL: {
    DeclID parentID;
    bool isImplicit, isObjC, isTransparent;
    uint8_t storedInitKind;
    TypeID signatureID;
    TypeID interfaceID;
    DeclID overriddenID;
    ArrayRef<uint64_t> argNameIDs;

    decls_block::ConstructorLayout::readRecord(scratch, parentID, isImplicit,
                                               isObjC, isTransparent,
                                               storedInitKind,
                                               signatureID, interfaceID,
                                               overriddenID, argNameIDs);
    auto parent = getDeclContext(parentID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto genericParams = maybeReadGenericParams(parent, DeclTypeCursor);
    if (declOrOffset.isComplete())
      return declOrOffset;

    // Resolve the name ids.
    SmallVector<Identifier, 2> argNames;
    for (auto argNameID : argNameIDs)
      argNames.push_back(getIdentifier(argNameID));

    DeclName name(ctx, ctx.Id_init, argNames);
    auto ctor = new (ctx) ConstructorDecl(name, SourceLoc(),
                                          /*bodyParams=*/nullptr, nullptr,
                                          genericParams, parent);
    declOrOffset = ctor;

    Pattern *bodyParams0 = maybeReadPattern();
    Pattern *bodyParams1 = maybeReadPattern();
    assert(bodyParams0&&bodyParams1 && "missing body patterns for constructor");
    ctor->setBodyParams(bodyParams0, bodyParams1);

    // This must be set after recording the constructor in the map.
    // A polymorphic constructor type needs to refer to the constructor to get
    // its generic parameters.
    ctor->setType(getType(signatureID));
    if (auto interfaceType = getType(interfaceID))
      ctor->setInterfaceType(interfaceType);

    // Set the initializer type of the constructor.
    auto allocType = ctor->getType();
    auto selfTy = allocType->castTo<AnyFunctionType>()->getInput()
                    ->castTo<MetatypeType>()->getInstanceType();
    if (auto polyFn = allocType->getAs<PolymorphicFunctionType>()) {
      ctor->setInitializerType(
        PolymorphicFunctionType::get(selfTy, polyFn->getResult(),
                                     &polyFn->getGenericParams(),
                                     polyFn->getExtInfo()));
    } else {
      auto fn = allocType->castTo<FunctionType>();
      ctor->setInitializerType(FunctionType::get(selfTy,
                                                 fn->getResult(),
                                                 fn->getExtInfo()));
    }

    // Set the initializer interface type of the constructor.
    allocType = ctor->getInterfaceType();
    selfTy = allocType->castTo<AnyFunctionType>()->getInput()
               ->castTo<MetatypeType>()->getInstanceType();
    if (auto polyFn = allocType->getAs<GenericFunctionType>()) {
      ctor->setInitializerInterfaceType(
              GenericFunctionType::get(polyFn->getGenericSignature(),
                                       selfTy, polyFn->getResult(),
                                       polyFn->getExtInfo()));
    } else {
      auto fn = allocType->castTo<FunctionType>();
      ctor->setInitializerInterfaceType(FunctionType::get(selfTy,
                                                          fn->getResult(),
                                                          fn->getExtInfo()));
    }

    if (isImplicit)
      ctor->setImplicit();
    if (isTransparent)
      ctor->getMutableAttrs().setAttr(AK_transparent, SourceLoc());
    if (auto initKind = getActualCtorInitializerKind(storedInitKind))
      ctor->setInitKind(*initKind);
    if (auto overridden
          = dyn_cast_or_null<ConstructorDecl>(getDecl(overriddenID)))
      ctor->setOverriddenDecl(overridden);
    break;
  }

  case decls_block::VAR_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit, isObjC, isOptional, isStatic, isLet;
    uint8_t StorageKind;
    TypeID typeID, interfaceTypeID;
    DeclID getterID, setterID, willSetID, didSetID;
    DeclID overriddenID;

    decls_block::VarLayout::readRecord(scratch, nameID, contextID, isImplicit,
                                       isObjC, isOptional, isStatic,
                                       isLet, StorageKind, typeID,
                                       interfaceTypeID, getterID, setterID,
                                       willSetID, didSetID, overriddenID);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto type = getType(typeID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto var = new (ctx) VarDecl(isStatic, isLet, SourceLoc(),
                                 getIdentifier(nameID), type, DC);

    declOrOffset = var;

    if (auto interfaceType = getType(interfaceTypeID))
      var->setInterfaceType(interfaceType);

    switch ((VarDeclStorageKind)StorageKind) {
    case VarDeclStorageKind::Stored: break;
    case VarDeclStorageKind::StoredWithTrivialAccessors:
      var->makeStoredWithTrivialAccessors(
                                    cast_or_null<FuncDecl>(getDecl(getterID)),
                                    cast_or_null<FuncDecl>(getDecl(setterID)));
      break;
    case VarDeclStorageKind::Computed:
      var->makeComputed(SourceLoc(),
                        cast_or_null<FuncDecl>(getDecl(getterID)),
                        cast_or_null<FuncDecl>(getDecl(setterID)),
                        SourceLoc());
      break;
    case VarDeclStorageKind::Observing:
      var->makeObserving(SourceLoc(),
                             cast_or_null<FuncDecl>(getDecl(willSetID)),
                             cast_or_null<FuncDecl>(getDecl(didSetID)),
                             SourceLoc());
      var->setObservingAccessors(cast_or_null<FuncDecl>(getDecl(getterID)),
                                     cast_or_null<FuncDecl>(getDecl(setterID)));
      break;
    }

    if (isImplicit)
      var->setImplicit();
    if (isOptional)
      var->getMutableAttrs().setAttr(AK_optional, SourceLoc());

    if (auto overridden = cast_or_null<VarDecl>(getDecl(overriddenID))) {
      var->setOverriddenDecl(overridden);
      AddAttribute(new (ctx) OverrideAttr(SourceLoc()));
    }

    break;
  }

  case decls_block::PARAM_DECL: {
    IdentifierID argNameID, paramNameID;
    DeclID contextID;
    bool isLet;
    TypeID typeID, interfaceTypeID;

    decls_block::ParamLayout::readRecord(scratch, argNameID, paramNameID,
                                         contextID, isLet, typeID,
                                         interfaceTypeID);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto type = getType(typeID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto param = new (ctx) ParamDecl(isLet, SourceLoc(), 
                                     getIdentifier(argNameID), SourceLoc(),
                                     getIdentifier(paramNameID), type, DC);

    declOrOffset = param;

    if (auto interfaceType = getType(interfaceTypeID))
      param->setInterfaceType(interfaceType);

    break;
  }

  case decls_block::FUNC_DECL: {
    DeclID contextID;
    bool isImplicit;
    bool isStatic;
    uint8_t RawStaticSpelling;
    bool isAssignmentOrConversion;
    bool isObjC, isTransparent, isMutating, hasDynamicSelf;
    bool isOptional;
    unsigned numParamPatterns;
    TypeID signatureID;
    TypeID interfaceTypeID;
    DeclID associatedDeclID;
    DeclID overriddenID;
    DeclID accessorStorageDeclID;
    bool hasCompoundName;
    ArrayRef<uint64_t> nameIDs;

    decls_block::FuncLayout::readRecord(scratch, contextID, isImplicit,
                                        isStatic, RawStaticSpelling,
                                        isAssignmentOrConversion,
                                        isObjC, isTransparent,
                                        isMutating, hasDynamicSelf, isOptional,
                                        numParamPatterns, signatureID,
                                        interfaceTypeID, associatedDeclID,
                                        overriddenID, accessorStorageDeclID,
                                        hasCompoundName, nameIDs);
    
    // Resolve the name ids.
    SmallVector<Identifier, 2> names;
    for (auto nameID : nameIDs)
      names.push_back(getIdentifier(nameID));

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    // Read generic params before reading the type, because the type may
    // reference generic parameters, and we want them to have a dummy
    // DeclContext for now.
    GenericParamList *genericParams = maybeReadGenericParams(DC, DeclTypeCursor);

    auto StaticSpelling = getActualStaticSpellingKind(RawStaticSpelling);
    if (!StaticSpelling.hasValue()) {
      error();
      return nullptr;
    }

    if (declOrOffset.isComplete())
      return declOrOffset;

    DeclName name;
    if (!names.empty()) {
      if (hasCompoundName)
        name = DeclName(ctx, names[0],
                        llvm::makeArrayRef(names.begin() + 1, names.end()));
      else
        name = DeclName(names[0]);
    }
    auto fn = FuncDecl::createDeserialized(
        ctx, SourceLoc(), StaticSpelling.getValue(), SourceLoc(), name,
        SourceLoc(), genericParams, /*type=*/nullptr, numParamPatterns, DC);
    declOrOffset = fn;

    if (Decl *associated = getDecl(associatedDeclID)) {
      if (auto op = dyn_cast<OperatorDecl>(associated)) {
        fn->setOperatorDecl(op);

        if (isa<PrefixOperatorDecl>(op))
          fn->getMutableAttrs().setAttr(AK_prefix, SourceLoc());
        else if (isa<PostfixOperatorDecl>(op))
          fn->getMutableAttrs().setAttr(AK_postfix, SourceLoc());
        // Note that an explicit @infix is not required.
      }
      // Otherwise, unknown associated decl kind.
    }

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
    assert((patternBuf.size() == numParamPatterns) &&
           "incorrect number of parameters");

    ArrayRef<Pattern *> patterns(patternBuf);
    fn->setDeserializedSignature(patterns,
                                 TypeLoc::withoutLoc(signature->getResult()));

    if (auto overridden = cast_or_null<FuncDecl>(getDecl(overriddenID))) {
      fn->setOverriddenDecl(overridden);
      AddAttribute(new (ctx) OverrideAttr(SourceLoc()));
    }

    fn->setStatic(isStatic);
    if (isImplicit)
      fn->setImplicit();
    if (isAssignmentOrConversion) {
      assert(!fn->isOperator());
      fn->getMutableAttrs().setAttr(AK_conversion, SourceLoc());
    }
    if (isTransparent)
      fn->getMutableAttrs().setAttr(AK_transparent, SourceLoc());
    fn->setMutating(isMutating);
    fn->setDynamicSelf(hasDynamicSelf);
    if (isOptional)
      fn->getMutableAttrs().setAttr(AK_optional, SourceLoc());
    // If we are an accessor on a var or subscript, make sure it is deserialized
    // too.
    getDecl(accessorStorageDeclID);
    break;
  }

  case decls_block::PATTERN_BINDING_DECL: {
    DeclID contextID;
    bool isImplicit;
    bool isStatic;
    uint8_t RawStaticSpelling;

    decls_block::PatternBindingLayout::readRecord(scratch, contextID,
                                                  isImplicit,
                                                  isStatic,
                                                  RawStaticSpelling);
    Pattern *pattern = maybeReadPattern();
    assert(pattern);

    auto StaticSpelling = getActualStaticSpellingKind(RawStaticSpelling);
    if (!StaticSpelling.hasValue()) {
      error();
      return nullptr;
    }

    auto binding = new (ctx) PatternBindingDecl(
        SourceLoc(), StaticSpelling.getValue(), SourceLoc(), pattern,
        /*init=*/nullptr,
        /*conditional=*/false, getDeclContext(contextID));
    declOrOffset = binding;

    binding->setStatic(isStatic);

    if (isImplicit)
      binding->setImplicit();

    break;
  }

  case decls_block::PROTOCOL_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit, isObjC;
    ArrayRef<uint64_t> protocolIDs;

    decls_block::ProtocolLayout::readRecord(scratch, nameID, contextID,
                                            isImplicit, isObjC,
                                            protocolIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto proto = new (ctx) ProtocolDecl(DC, SourceLoc(), SourceLoc(),
                                        getIdentifier(nameID), { });
    declOrOffset = proto;

    if (auto genericParams = maybeReadGenericParams(DC, DeclTypeCursor)) {
      proto->setGenericParams(genericParams);
      SmallVector<GenericTypeParamType *, 4> paramTypes;
      for (auto &genericParam : *proto->getGenericParams()) {
        paramTypes.push_back(genericParam.getAsTypeParam()->getDeclaredType()
                             ->castTo<GenericTypeParamType>());
      }

      // Read the generic requirements.
      SmallVector<Requirement, 4> requirements;
      readGenericRequirements(requirements);

      auto sig = GenericSignature::get(paramTypes, requirements);
      proto->setGenericSignature(sig);
    }


    if (isImplicit)
      proto->setImplicit();
    proto->computeType();

    // Deserialize the list of protocols.
    auto inherited = ctx.Allocate<ProtocolDecl *>(protocolIDs.size());
    for_each(inherited, protocolIDs, [this](ProtocolDecl *&p, uint64_t rawID) {
      p = cast<ProtocolDecl>(getDecl(rawID));
    });
    proto->setProtocols(inherited);

    proto->setMemberLoader(this, DeclTypeCursor.GetCurrentBitNo());
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
    bool isImplicit, isObjC, attrRequiresStoredPropertyInits;
    bool requiresStoredPropertyInits;
    bool foreign;
    TypeID superclassID;
    ArrayRef<uint64_t> rawProtocolIDs;
    decls_block::ClassLayout::readRecord(scratch, nameID, contextID,
                                         isImplicit, isObjC,
                                         attrRequiresStoredPropertyInits,
                                         requiresStoredPropertyInits,
                                         foreign,
                                         superclassID, rawProtocolIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto genericParams = maybeReadGenericParams(DC, DeclTypeCursor);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto theClass = new (ctx) ClassDecl(SourceLoc(), getIdentifier(nameID),
                                        SourceLoc(), { }, genericParams, DC);
    declOrOffset = theClass;

    theClass->setAddedImplicitInitializers();
    if (isImplicit)
      theClass->setImplicit();
    if (superclassID)
      theClass->setSuperclass(getType(superclassID));
    if (attrRequiresStoredPropertyInits)
      theClass->getMutableAttrs().setAttr(AK_requires_stored_property_inits,
                                          SourceLoc());
    if (requiresStoredPropertyInits)
      theClass->setRequiresStoredPropertyInits(true);
    if (foreign)
      theClass->setForeign();
    if (genericParams) {
      SmallVector<GenericTypeParamType *, 4> paramTypes;
      for (auto &genericParam : *theClass->getGenericParams()) {
        paramTypes.push_back(genericParam.getAsTypeParam()->getDeclaredType()
                               ->castTo<GenericTypeParamType>());
      }

      // Read the generic requirements.
      SmallVector<Requirement, 4> requirements;
      readGenericRequirements(requirements);

      GenericSignature *sig = GenericSignature::get(paramTypes, requirements);
      theClass->setGenericSignature(sig);
    }
    theClass->computeType();

    auto protocols = ctx.Allocate<ProtocolDecl *>(rawProtocolIDs.size());
    for_each(protocols, rawProtocolIDs, [this](ProtocolDecl *&p,
                                               uint64_t rawID) {
      p = cast<ProtocolDecl>(getDecl(rawID));
    });
    theClass->setProtocols(protocols);

    theClass->setMemberLoader(this, DeclTypeCursor.GetCurrentBitNo());
    theClass->setHasDestructor();
    skipRecord(DeclTypeCursor, decls_block::DECL_CONTEXT);
    theClass->setConformanceLoader(this, DeclTypeCursor.GetCurrentBitNo());

    theClass->setCheckedInheritanceClause();
    theClass->setCircularityCheck(CircularityCheck::Checked);
    break;
  }

  case decls_block::ENUM_DECL: {
    IdentifierID nameID;
    DeclID contextID;
    bool isImplicit;
    TypeID rawTypeID;
    ArrayRef<uint64_t> rawProtocolIDs;

    decls_block::EnumLayout::readRecord(scratch, nameID, contextID,
                                        isImplicit, rawTypeID,
                                        rawProtocolIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto genericParams = maybeReadGenericParams(DC, DeclTypeCursor);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto theEnum = new (ctx) EnumDecl(SourceLoc(), getIdentifier(nameID),
                                      SourceLoc(), { }, genericParams, DC);

    declOrOffset = theEnum;

    if (isImplicit)
      theEnum->setImplicit();
    theEnum->setRawType(getType(rawTypeID));
    if (genericParams) {
      SmallVector<GenericTypeParamType *, 4> paramTypes;
      for (auto &genericParam : *theEnum->getGenericParams()) {
        paramTypes.push_back(genericParam.getAsTypeParam()->getDeclaredType()
                               ->castTo<GenericTypeParamType>());
      }

      // Read the generic requirements.
      SmallVector<Requirement, 4> requirements;
      readGenericRequirements(requirements);

      GenericSignature *sig = GenericSignature::get(paramTypes, requirements);
      theEnum->setGenericSignature(sig);
    }

    theEnum->computeType();

    auto protocols = ctx.Allocate<ProtocolDecl *>(rawProtocolIDs.size());
    for_each(protocols, rawProtocolIDs, [this](ProtocolDecl *&p,
                                               uint64_t rawID) {
      p = cast<ProtocolDecl>(getDecl(rawID));
    });
    theEnum->setProtocols(protocols);

    theEnum->setMemberLoader(this, DeclTypeCursor.GetCurrentBitNo());
    skipRecord(DeclTypeCursor, decls_block::DECL_CONTEXT);
    theEnum->setConformanceLoader(this, DeclTypeCursor.GetCurrentBitNo());

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
      return declOrOffset;

    auto argTy = getType(argTypeID);
    if (declOrOffset.isComplete())
      return declOrOffset;

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
    ArrayRef<uint64_t> argNameIDs;

    decls_block::SubscriptLayout::readRecord(scratch, contextID, isImplicit,
                                             isObjC, isOptional,
                                             declTypeID, elemTypeID,
                                             interfaceTypeID,
                                             getterID, setterID,
                                             overriddenID,
                                             argNameIDs);
    auto Getter = cast_or_null<FuncDecl>(getDecl(getterID));
    auto Setter = cast_or_null<FuncDecl>(getDecl(setterID));
    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    Pattern *indices = maybeReadPattern();
    assert(indices);

    auto elemTy = TypeLoc::withoutLoc(getType(elemTypeID));
    if (declOrOffset.isComplete())
      return declOrOffset;

    // Resolve the name ids.
    SmallVector<Identifier, 2> argNames;
    for (auto argNameID : argNameIDs)
      argNames.push_back(getIdentifier(argNameID));

    DeclName name(ctx, ctx.Id_subscript, argNames);
    auto subscript = new (ctx) SubscriptDecl(name, SourceLoc(), indices,
                                             SourceLoc(), elemTy, DC);
    declOrOffset = subscript;

    subscript->setAccessors(SourceRange(), Getter, Setter);
    subscript->setType(getType(declTypeID));
    if (auto interfaceType = getType(interfaceTypeID))
      subscript->setInterfaceType(interfaceType);
    if (isImplicit)
      subscript->setImplicit();
    if (isOptional)
      subscript->getMutableAttrs().setAttr(AK_optional, SourceLoc());
    if (auto overridden = cast_or_null<SubscriptDecl>(getDecl(overriddenID))) {
      subscript->setOverriddenDecl(overridden);
      AddAttribute(new (ctx) OverrideAttr(SourceLoc()));
    }
    break;
  }

  case decls_block::EXTENSION_DECL: {
    TypeID baseID;
    DeclID contextID;
    bool isImplicit;
    ArrayRef<uint64_t> rawProtocolIDs;

    decls_block::ExtensionLayout::readRecord(scratch, baseID, contextID,
                                             isImplicit, rawProtocolIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto baseTy = TypeLoc::withoutLoc(getType(baseID));
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto extension = new (ctx) ExtensionDecl(SourceLoc(), baseTy, { }, DC);
    declOrOffset = extension;

    if (isImplicit)
      extension->setImplicit();

    auto protocols = ctx.Allocate<ProtocolDecl *>(rawProtocolIDs.size());
    for_each(protocols, rawProtocolIDs, [this](ProtocolDecl *&p,
                                               uint64_t rawID) {
      p = cast<ProtocolDecl>(getDecl(rawID));
    });
    extension->setProtocols(protocols);

    extension->setMemberLoader(this, DeclTypeCursor.GetCurrentBitNo());
    skipRecord(DeclTypeCursor, decls_block::DECL_CONTEXT);
    extension->setConformanceLoader(this, DeclTypeCursor.GetCurrentBitNo());

    baseTy.getType()->getAnyNominal()->addExtension(extension);
    extension->setCheckedInheritanceClause();
    break;
  }

  case decls_block::DESTRUCTOR_DECL: {
    DeclID parentID;
    bool isImplicit, isObjC;
    TypeID signatureID;

    decls_block::DestructorLayout::readRecord(scratch, parentID, isImplicit,
                                              isObjC, signatureID);

    DeclContext *parent = getDeclContext(parentID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto dtor = new (ctx) DestructorDecl(ctx.Id_deinit, SourceLoc(),
                                         /*selfpat*/nullptr, parent);
    declOrOffset = dtor;


    Pattern *selfParams = maybeReadPattern();
    assert(selfParams && "Didn't get self pattern?");
    dtor->setSelfPattern(selfParams);

    dtor->setType(getType(signatureID));
    if (isImplicit)
      dtor->setImplicit();

    break;
  }

  case decls_block::XREF: {
    assert(DAttrs == nullptr);
    ModuleID baseModuleID;
    uint32_t pathLen;
    decls_block::XRefLayout::readRecord(scratch, baseModuleID, pathLen);
    declOrOffset = resolveCrossReference(getModule(baseModuleID), pathLen);
    break;
  }

  default:
    // We don't know how to deserialize this kind of decl.
    error();
    return nullptr;
  }

  // Record the attributes.
  if (DAttrs)
    declOrOffset.get()->getMutableAttrs().setRawAttributeChain(DAttrs);

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
  case serialization::Ownership::Strong:   return swift::Ownership::Strong;
  case serialization::Ownership::Unmanaged:return swift::Ownership::Unmanaged;
  case serialization::Ownership::Unowned:  return swift::Ownership::Unowned;
  case serialization::Ownership::Weak:     return swift::Ownership::Weak;
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
  CASE(UnownedInnerPointer)
  CASE(Autoreleased)
#undef CASE
  }
  return Nothing;
}

static AnyFunctionType::Representation
getFunctionRepresentation(bool thin, bool blockCompatible) {
  // A type cannot be both thin and block-compatible.
  assert(!thin || !blockCompatible);
  if (thin)
    return AnyFunctionType::Representation::Thin;
  else if (blockCompatible)
    return AnyFunctionType::Representation::Block;
  else
    return AnyFunctionType::Representation::Thick;
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
    auto nominal = cast<NominalTypeDecl>(getDecl(declID));
    typeOrOffset = NominalType::get(nominal, parentTy, ctx);

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
                                getFunctionRepresentation(thin, blockCompatible),
                                noreturn,
                                autoClosure);
    
    typeOrOffset = FunctionType::get(getType(inputID), getType(resultID),
                                     Info);
    break;
  }

  case decls_block::EXISTENTIAL_METATYPE_TYPE: {
    TypeID instanceID;
    uint8_t repr;
    decls_block::ExistentialMetatypeTypeLayout::readRecord(scratch,
                                                           instanceID, repr);

    switch (repr) {
    case serialization::MetatypeRepresentation::MR_None:
      typeOrOffset = ExistentialMetatypeType::get(getType(instanceID));
      break;

    case serialization::MetatypeRepresentation::MR_Thin:
      error();
      break;

    case serialization::MetatypeRepresentation::MR_Thick:
      typeOrOffset = ExistentialMetatypeType::get(getType(instanceID),
                                       MetatypeRepresentation::Thick);
      break;

    case serialization::MetatypeRepresentation::MR_ObjC:
      typeOrOffset = ExistentialMetatypeType::get(getType(instanceID),
                                       MetatypeRepresentation::ObjC);
      break;

    default:
      error();
      break;
    }
    break;
  }

  case decls_block::METATYPE_TYPE: {
    TypeID instanceID;
    uint8_t repr;
    decls_block::MetatypeTypeLayout::readRecord(scratch, instanceID, repr);

    switch (repr) {
    case serialization::MetatypeRepresentation::MR_None:
      typeOrOffset = MetatypeType::get(getType(instanceID));
      break;

    case serialization::MetatypeRepresentation::MR_Thin:
      typeOrOffset = MetatypeType::get(getType(instanceID),
                                       MetatypeRepresentation::Thin);
      break;

    case serialization::MetatypeRepresentation::MR_Thick:
      typeOrOffset = MetatypeType::get(getType(instanceID),
                                       MetatypeRepresentation::Thick);
      break;

    case serialization::MetatypeRepresentation::MR_ObjC:
      typeOrOffset = MetatypeType::get(getType(instanceID),
                                       MetatypeRepresentation::ObjC);
      break;

    default:
      error();
      break;
    }
    break;
  }

  case decls_block::DYNAMIC_SELF_TYPE: {
    TypeID selfID;
    decls_block::DynamicSelfTypeLayout::readRecord(scratch, selfID);
    typeOrOffset = DynamicSelfType::get(getType(selfID), ctx);
    break;
  }

  case decls_block::LVALUE_TYPE: {
    TypeID objectTypeID;
    decls_block::LValueTypeLayout::readRecord(scratch, objectTypeID);
    typeOrOffset = LValueType::get(getType(objectTypeID));
    break;
  }
  case decls_block::INOUT_TYPE: {
    TypeID objectTypeID;
    decls_block::LValueTypeLayout::readRecord(scratch, objectTypeID);
    typeOrOffset = InOutType::get(getType(objectTypeID));
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
    
    // Read the associated type names.
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
    
    // Read whether the associated types are dependent archetypes.
    entry = DeclTypeCursor.advance();
    if (entry.Kind != llvm::BitstreamEntry::Record) {
      error();
      break;
    }
    
    SmallVector<uint64_t, 16> scratch2;
    kind = DeclTypeCursor.readRecord(entry.ID, scratch2);
    if (kind != decls_block::ARCHETYPE_NESTED_TYPES_ARE_ARCHETYPES) {
      error();
      break;
    }
    
    ArrayRef<uint64_t> areArchetypes;
    decls_block::ArchetypeNestedTypesAreArchetypesLayout
      ::readRecord(scratch2, areArchetypes);
    
    // Read the associated type ids.
    entry = DeclTypeCursor.advance();
    if (entry.Kind != llvm::BitstreamEntry::Record) {
      error();
      break;
    }

    SmallVector<uint64_t, 16> scratch3;
    kind = DeclTypeCursor.readRecord(entry.ID, scratch3);
    if (kind != decls_block::ARCHETYPE_NESTED_TYPES) {
      error();
      break;
    }

    ArrayRef<uint64_t> rawTypeIDs;
    decls_block::ArchetypeNestedTypesLayout::readRecord(scratch3, rawTypeIDs);
    
    // Build the nested types array.
    SmallVector<std::pair<Identifier, ArchetypeType::NestedType>, 4>
      nestedTypes;
    for_each3(rawNameIDs, areArchetypes, rawTypeIDs,
              [&](IdentifierID nameID, bool isArchetype, TypeID nestedID) {
      ArchetypeType::NestedType nestedTy = getType(nestedID);
      if (isArchetype) {
        nestedTy = nestedTy.get<Type>()->castTo<ArchetypeType>();
      }
      nestedTypes.push_back(std::make_pair(getIdentifier(nameID), nestedTy));
    });
    archetype->setNestedTypes(ctx, nestedTypes);

    break;
  }

  case decls_block::OPENED_EXISTENTIAL_TYPE: {
    TypeID existentialID;

    decls_block::OpenedExistentialTypeLayout::readRecord(scratch,
                                                         existentialID);

    typeOrOffset = ArchetypeType::getOpened(getType(existentialID));
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

    auto nominal = cast<NominalTypeDecl>(getDecl(declID));
    auto parentTy = getType(parentID);

    // Check the first ID to decide if we are using indices to the Decl's
    // Archetypes. 
    SmallVector<Type, 8> genericArgs;
    if (rawArgumentIDs.size() > 1 && rawArgumentIDs[0] == INT32_MAX) {
      for (unsigned i = 1; i < rawArgumentIDs.size(); i++) {
        auto index = rawArgumentIDs[i];
        genericArgs.push_back(nominal->getGenericParams()
                                     ->getAllArchetypes()[index]);
      }
    } else {
      for (TypeID type : rawArgumentIDs)
        genericArgs.push_back(getType(type));
    }

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
      maybeGetOrReadGenericParams(genericContextID, FileContext, DeclTypeCursor);
    assert(paramList && "missing generic params for polymorphic function");
    
    auto rep = thin
      ? AnyFunctionType::Representation::Thin
      : AnyFunctionType::Representation::Thick;
    
    auto Info = PolymorphicFunctionType::ExtInfo(callingConvention.getValue(),
                                                 rep,
                                                 noreturn);

    typeOrOffset = PolymorphicFunctionType::get(getType(inputID),
                                                getType(resultID),
                                                paramList, Info);
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
    
    auto rep = thin
      ? AnyFunctionType::Representation::Thin
      : AnyFunctionType::Representation::Thick;
    
    // Read the generic requirements.
    SmallVector<Requirement, 4> requirements;
    readGenericRequirements(requirements);
    auto info = GenericFunctionType::ExtInfo(callingConvention.getValue(),
                                             rep, noreturn);

    auto sig = GenericSignature::get(genericParams, requirements);
    typeOrOffset = GenericFunctionType::get(sig,
                                            getType(inputID),
                                            getType(resultID),
                                            info);
    break;
  }
      
  case decls_block::SIL_BLOCK_STORAGE_TYPE: {
    TypeID captureID;
    
    decls_block::SILBlockStorageTypeLayout::readRecord(scratch, captureID);
    typeOrOffset = SILBlockStorageType::get(getType(captureID)
                                              ->getCanonicalType());
    break;
  }
      
  case decls_block::SIL_FUNCTION_TYPE: {
    TypeID interfaceResultID;
    uint8_t rawInterfaceResultConvention;
    uint8_t rawCallingConvention;
    uint8_t rawCalleeConvention;
    bool thin, block;
    bool noreturn = false;
    unsigned numGenericParams;
    ArrayRef<uint64_t> paramIDs;

    decls_block::SILFunctionTypeLayout::readRecord(scratch,
                                                   interfaceResultID,
                                                   rawInterfaceResultConvention,
                                                   rawCalleeConvention,
                                                   rawCallingConvention,
                                                   thin,
                                                   block,
                                                   noreturn,
                                                   numGenericParams,
                                                   paramIDs);

    // Process the ExtInfo.
    auto callingConvention = getActualCC(rawCallingConvention);
    if (!callingConvention.hasValue()) {
      error();
      return nullptr;
    }
    SILFunctionType::ExtInfo extInfo(callingConvention.getValue(),
                                     getFunctionRepresentation(thin, block),
                                     noreturn);

    // Process the result.
    auto interfaceResultConvention
      = getActualResultConvention(rawInterfaceResultConvention);
    if (!interfaceResultConvention.hasValue()) {
      error();
      return nullptr;
    }
    SILResultInfo interfaceResult(getType(interfaceResultID)->getCanonicalType(),
                                  interfaceResultConvention.getValue());

    // Process the parameters.
    unsigned numParamIDs = paramIDs.size() - numGenericParams;
    SmallVector<SILParameterInfo, 8> allParams;
    allParams.reserve(numParamIDs);
    for (size_t i = 0, e = numParamIDs; i != e; i += 2) {
      auto type = getType(paramIDs[i])->getCanonicalType();
      auto convention = getActualParameterConvention(paramIDs[i+1]);
      if (!convention.hasValue()) {
        error();
        return nullptr;
      }
      SILParameterInfo param(type, convention.getValue());
      allParams.push_back(param);
    }

    // Process the callee convention.
    auto calleeConvention = getActualParameterConvention(rawCalleeConvention);
    if (!calleeConvention.hasValue()) {
      error();
      return nullptr;
    }

    // Process the generic signature parameters.
    SmallVector<GenericTypeParamType *, 8> genericParamTypes;
    for (auto id : paramIDs.slice(numParamIDs)) {
      genericParamTypes.push_back(
                  cast<GenericTypeParamType>(getType(id)->getCanonicalType()));
    }

    // Read the generic requirements, if any.
    SmallVector<Requirement, 4> requirements;
    readGenericRequirements(requirements);

    GenericSignature *genericSig = nullptr;
    if (!genericParamTypes.empty() || !requirements.empty())
      genericSig = GenericSignature::get(genericParamTypes, requirements);

    typeOrOffset = SILFunctionType::get(genericSig, extInfo,
                                        calleeConvention.getValue(),
                                        allParams, interfaceResult,
                                        ctx);
    break;
  }

  case decls_block::ARRAY_SLICE_TYPE: {
    TypeID baseID;
    decls_block::ArraySliceTypeLayout::readRecord(scratch, baseID);

    auto sliceTy = ArraySliceType::get(getType(baseID));
    typeOrOffset = sliceTy;
    break;
  }

  case decls_block::OPTIONAL_TYPE: {
    TypeID baseID;
    decls_block::OptionalTypeLayout::readRecord(scratch, baseID);

    auto optionalTy = OptionalType::get(getType(baseID));
    typeOrOffset = optionalTy;
    break;
  }

  case decls_block::UNCHECKED_OPTIONAL_TYPE: {
    TypeID baseID;
    decls_block::ImplicitlyUnwrappedOptionalTypeLayout::readRecord(scratch, baseID);

    auto optionalTy = ImplicitlyUnwrappedOptionalType::get(getType(baseID));
    typeOrOffset = optionalTy;
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

ArrayRef<Decl *> ModuleFile::loadAllMembers(const Decl *D,
                                            uint64_t contextData) {
  // FIXME: Add PrettyStackTrace.
  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(contextData);
  auto result = readMembers();
  assert(result && "unable to read members");
  return result.getValue();
}

ArrayRef<ProtocolConformance *>
ModuleFile::loadAllConformances(const Decl *D, uint64_t contextData) {
  // FIXME: Add PrettyStackTrace.
  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(contextData);

  Type conformingTy;
  if (auto nominal = dyn_cast<NominalTypeDecl>(D))
    conformingTy = nominal->getDeclaredTypeInContext();
  else
    conformingTy = cast<ExtensionDecl>(D)->getDeclaredTypeInContext();
  CanType canTy = conformingTy->getCanonicalType();

  SmallVector<ProtocolConformance *, 16> conformances;
  while (auto conformance = maybeReadConformance(canTy, DeclTypeCursor))
    conformances.push_back(conformance.getValue());
  return getContext().AllocateCopy(conformances);
}

TypeLoc
ModuleFile::loadAssociatedTypeDefault(const swift::AssociatedTypeDecl *ATD,
                                      uint64_t contextData) {
  return TypeLoc::withoutLoc(getType(contextData));
}
