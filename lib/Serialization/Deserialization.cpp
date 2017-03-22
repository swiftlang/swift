//===--- Deserialization.cpp - Loading a serialized AST -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Serialization/ModuleFile.h"
#include "swift/Serialization/ModuleFormat.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Parse/Parser.h"
#include "swift/Serialization/BCReadingExtras.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Basic/Defer.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "Serialization"

STATISTIC(NumDeclsLoaded, "# of decls deserialized");
STATISTIC(NumMemberListsLoaded,
          "# of nominals/extensions whose members were loaded");
STATISTIC(NumNestedTypeShortcuts,
          "# of same-module nested types resolved without lookup");

using namespace swift;
using namespace swift::serialization;

StringRef swift::getNameOfModule(const ModuleFile *MF) {
  return MF->Name;
}

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
    const ModuleFile *MF;
    const ModuleFile::Serialized<Decl*> &DeclOrOffset;
    DeclID ID;
    decls_block::RecordKind Kind;
  public:
    PrettyDeclDeserialization(ModuleFile *module,
                              const ModuleFile::Serialized<Decl*> &declOrOffset,
                              DeclID DID, decls_block::RecordKind kind)
      : MF(module), DeclOrOffset(declOrOffset), ID(DID), Kind(kind) {
    }

    static const char *getRecordKindString(decls_block::RecordKind Kind) {
      switch (Kind) {
#define RECORD(Id) case decls_block::Id: return #Id;
#include "swift/Serialization/DeclTypeRecordNodes.def"
      }

      llvm_unreachable("Unhandled RecordKind in switch.");
    }

    void print(raw_ostream &os) const override {
      if (!DeclOrOffset.isComplete()) {
        os << "While deserializing decl #" << ID << " ("
           << getRecordKindString(Kind) << ")";
      } else {
        os << "While deserializing ";

        if (auto VD = dyn_cast<ValueDecl>(DeclOrOffset.get())) {
          os << "'" << VD->getName() << "' (" << IDAndKind{VD, ID} << ")";
        } else if (auto ED = dyn_cast<ExtensionDecl>(DeclOrOffset.get())) {
          os << "extension of '" << ED->getExtendedType() << "' ("
             << IDAndKind{ED, ID} << ")";
        } else {
          os << IDAndKind{DeclOrOffset.get(), ID};
        }
      }
      os << " in '" << getNameOfModule(MF) << "'\n";
    }
  };

  class PrettyXRefTrace : public llvm::PrettyStackTraceEntry {
    class PathPiece {
    public:
      enum class Kind {
        Value,
        Type,
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
        case Kind::Type:
          os << "with type " << getDataAs<Type>();
          break;
        case Kind::Extension:
          if (getDataAs<ModuleDecl *>())
            os << "in an extension in module '" << getDataAs<ModuleDecl *>()->getName()
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
          case MaterializeForSet:
            os << "(materializeForSet)";
            break;
          case Addressor:
            os << "(addressor)";
            break;
          case MutableAddressor:
            os << "(mutableAddressor)";
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
    ModuleDecl &baseM;
    SmallVector<PathPiece, 8> path;

  public:
    PrettyXRefTrace(ModuleDecl &M) : baseM(M) {}

    void addValue(Identifier name) {
      path.push_back({ PathPiece::Kind::Value, name });
    }

    void addType(Type ty) {
      path.push_back({ PathPiece::Kind::Type, ty });
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

    void addExtension(ModuleDecl *M) {
      path.push_back({ PathPiece::Kind::Extension, M });
    }

    void addGenericParam(uintptr_t index) {
      path.push_back({ PathPiece::Kind::GenericParam, index });
    }

    void addUnknown(uintptr_t kind) {
      path.push_back({ PathPiece::Kind::Unknown, kind });
    }

    void removeLast() {
      path.pop_back();
    }

    void print(raw_ostream &os) const override {
      os << "Cross-reference to module '" << baseM.getName() << "'\n";
      for (auto &piece : path) {
        os << "\t... ";
        piece.print(os);
        os << "\n";
      }
    }
  };

  class PrettyStackTraceModuleFile : public llvm::PrettyStackTraceEntry {
    const char *Action;
    const ModuleFile *MF;
  public:
    explicit PrettyStackTraceModuleFile(const char *action, ModuleFile *module)
        : Action(action), MF(module) {}

    void print(raw_ostream &os) const override {
      os << Action << " \'" << getNameOfModule(MF) << "'\n";
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

ModuleFile &ModuleFile::getModuleFileForDelayedActions() {
  assert(FileContext && "cannot delay actions before associating with a file");
  ModuleDecl *associatedModule = getAssociatedModule();

  // Check for the common case.
  if (associatedModule->getFiles().size() == 1)
    return *this;

  for (FileUnit *file : associatedModule->getFiles())
    if (auto *serialized = dyn_cast<SerializedASTFile>(file))
      return serialized->File;

  llvm_unreachable("should always have FileContext in the list of files");
}

void ModuleFile::finishPendingActions() {
  assert(&getModuleFileForDelayedActions() == this &&
         "wrong module used for delayed actions");
  while (!DelayedGenericEnvironments.empty()) {
    // Force completion of the last generic environment.
    auto genericEnvDC = DelayedGenericEnvironments.back();
    DelayedGenericEnvironments.pop_back();
    (void)genericEnvDC->getGenericEnvironmentOfContext();
  }
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
  case serialization::DefaultArgumentKind::DSOHandle:
    return swift::DefaultArgumentKind::DSOHandle;
  case serialization::DefaultArgumentKind::Nil:
    return swift::DefaultArgumentKind::Nil;
  case serialization::DefaultArgumentKind::EmptyArray:
    return swift::DefaultArgumentKind::EmptyArray;
  case serialization::DefaultArgumentKind::EmptyDictionary:
    return swift::DefaultArgumentKind::EmptyDictionary;
  }
  return None;
}

ParameterList *ModuleFile::readParameterList() {
  using namespace decls_block;

  SmallVector<uint64_t, 8> scratch;
  auto entry = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch);
  assert(recordID == PARAMETERLIST);
  (void) recordID;
  unsigned numParams;
  decls_block::ParameterListLayout::readRecord(scratch, numParams);

  SmallVector<ParamDecl*, 8> params;
  for (unsigned i = 0; i != numParams; ++i) {
    scratch.clear();
    auto entry = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
    unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch);
    assert(recordID == PARAMETERLIST_ELT);
    (void) recordID;
    
    DeclID paramID;
    bool isVariadic;
    uint8_t rawDefaultArg;
    decls_block::ParameterListEltLayout::readRecord(scratch, paramID,
                                                    isVariadic, rawDefaultArg);
    

    auto decl = cast<ParamDecl>(getDecl(paramID));
    decl->setVariadic(isVariadic);

    // Decode the default argument kind.
    // FIXME: Default argument expression, if available.
    if (auto defaultArg = getActualDefaultArgKind(rawDefaultArg))
      decl->setDefaultArgumentKind(*defaultArg);
    params.push_back(decl);
  }
  
  return ParameterList::create(getContext(), params);
}

Pattern *ModuleFile::maybeReadPattern(DeclContext *owningDC) {
  using namespace decls_block;

  SmallVector<uint64_t, 8> scratch;

  BCOffsetRAII restoreOffset(DeclTypeCursor);
  auto next = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
  if (next.Kind != llvm::BitstreamEntry::Record)
    return nullptr;

  /// Local function to record the type of this pattern.
  auto recordPatternType = [&](Pattern *pattern, Type type) {
    if (type->hasTypeParameter())
      pattern->setDelayedInterfaceType(type, owningDC);
    else
      pattern->setType(type);
  };

  unsigned kind = DeclTypeCursor.readRecord(next.ID, scratch);
  switch (kind) {
  case decls_block::PAREN_PATTERN: {
    bool isImplicit;
    ParenPatternLayout::readRecord(scratch, isImplicit);

    Pattern *subPattern = maybeReadPattern(owningDC);
    assert(subPattern);

    auto result = new (getContext()) ParenPattern(SourceLoc(),
                                                  subPattern,
                                                  SourceLoc(),
                                                  isImplicit);

    if (Type interfaceType = subPattern->getDelayedInterfaceType())
      result->setDelayedInterfaceType(ParenType::get(getContext(),
                                                     interfaceType), owningDC);
    else
      result->setType(ParenType::get(getContext(), subPattern->getType()));
    restoreOffset.reset();
    return result;
  }
  case decls_block::TUPLE_PATTERN: {
    TypeID tupleTypeID;
    unsigned count;
    bool isImplicit;

    TuplePatternLayout::readRecord(scratch, tupleTypeID, count, isImplicit);

    SmallVector<TuplePatternElt, 8> elements;
    for ( ; count > 0; --count) {
      scratch.clear();
      next = DeclTypeCursor.advance();
      assert(next.Kind == llvm::BitstreamEntry::Record);

      kind = DeclTypeCursor.readRecord(next.ID, scratch);
      assert(kind == decls_block::TUPLE_PATTERN_ELT);

      // FIXME: Add something for this record or remove it.
      IdentifierID labelID;
      TuplePatternEltLayout::readRecord(scratch, labelID);
      Identifier label = getIdentifier(labelID);

      Pattern *subPattern = maybeReadPattern(owningDC);
      assert(subPattern);
      elements.push_back(TuplePatternElt(label, SourceLoc(), subPattern));
    }

    auto result = TuplePattern::create(getContext(), SourceLoc(),
                                       elements, SourceLoc(), isImplicit);
    recordPatternType(result, getType(tupleTypeID));
    restoreOffset.reset();
    return result;
  }
  case decls_block::NAMED_PATTERN: {
    DeclID varID;
    TypeID typeID;
    bool isImplicit;
    NamedPatternLayout::readRecord(scratch, varID, typeID, isImplicit);

    auto var = cast<VarDecl>(getDecl(varID));
    auto result = new (getContext()) NamedPattern(var, isImplicit);
    recordPatternType(result, getType(typeID));
    restoreOffset.reset();
    return result;
  }
  case decls_block::ANY_PATTERN: {
    TypeID typeID;
    bool isImplicit;

    AnyPatternLayout::readRecord(scratch, typeID, isImplicit);
    auto result = new (getContext()) AnyPattern(SourceLoc(), isImplicit);
    recordPatternType(result, getType(typeID));
    restoreOffset.reset();
    return result;
  }
  case decls_block::TYPED_PATTERN: {
    TypeID typeID;
    bool isImplicit;

    TypedPatternLayout::readRecord(scratch, typeID, isImplicit);
    Pattern *subPattern = maybeReadPattern(owningDC);
    assert(subPattern);

    auto result = new (getContext()) TypedPattern(subPattern, TypeLoc(),
                                                  isImplicit);
    recordPatternType(result, getType(typeID));
    restoreOffset.reset();
    return result;
  }
  case decls_block::VAR_PATTERN: {
    bool isImplicit, isLet;
    VarPatternLayout::readRecord(scratch, isLet, isImplicit);
    Pattern *subPattern = maybeReadPattern(owningDC);
    assert(subPattern);

    auto result = new (getContext()) VarPattern(SourceLoc(), isLet, subPattern,
                                                isImplicit);
    if (Type interfaceType = subPattern->getDelayedInterfaceType())
      result->setDelayedInterfaceType(interfaceType, owningDC);
    else
      result->setType(subPattern->getType());
    restoreOffset.reset();
    return result;
  }

  default:
    return nullptr;
  }
}

SILLayout *ModuleFile::readSILLayout(llvm::BitstreamCursor &Cursor) {
  using namespace decls_block;

  SmallVector<uint64_t, 16> scratch;

  auto next = Cursor.advance(AF_DontPopBlockAtEnd);
  assert(next.Kind == llvm::BitstreamEntry::Record);

  unsigned kind = Cursor.readRecord(next.ID, scratch);
  switch (kind) {
  case decls_block::SIL_LAYOUT: {
    unsigned numFields;
    ArrayRef<uint64_t> types;
    decls_block::SILLayoutLayout::readRecord(scratch, numFields, types);
    
    SmallVector<SILField, 4> fields;
    for (auto fieldInfo : types.slice(0, numFields)) {
      bool isMutable = fieldInfo & 0x80000000U;
      auto typeId = fieldInfo & 0x7FFFFFFFU;
      fields.push_back(
        SILField(getType(typeId)->getCanonicalType(),
                 isMutable));
    }
    
    SmallVector<GenericTypeParamType*, 4> genericParams;
    for (auto typeId : types.slice(numFields)) {
      auto type = getType(typeId)->castTo<GenericTypeParamType>();
      genericParams.push_back(type);
    }
    
    SmallVector<Requirement, 4> requirements;
    readGenericRequirements(requirements, DeclTypeCursor);
    CanGenericSignature sig;
    if (!genericParams.empty() || !requirements.empty()) {
      sig = GenericSignature::get(genericParams, requirements)
        ->getCanonicalSignature();
    }
    return SILLayout::get(getContext(), sig, fields);
  }
  default:
    error();
    return nullptr;
  }
}

ProtocolConformanceRef ModuleFile::readConformance(
                                             llvm::BitstreamCursor &Cursor,
                                             GenericEnvironment *genericEnv) {
  using namespace decls_block;

  SmallVector<uint64_t, 16> scratch;

  auto next = Cursor.advance(AF_DontPopBlockAtEnd);
  assert(next.Kind == llvm::BitstreamEntry::Record);

  unsigned kind = Cursor.readRecord(next.ID, scratch);
  switch (kind) {
  case ABSTRACT_PROTOCOL_CONFORMANCE: {
    DeclID protoID;
    AbstractProtocolConformanceLayout::readRecord(scratch, protoID);
    auto proto = cast<ProtocolDecl>(getDecl(protoID));
    return ProtocolConformanceRef(proto);
  }

  case SPECIALIZED_PROTOCOL_CONFORMANCE: {
    TypeID conformingTypeID;
    unsigned numSubstitutions;
    SpecializedProtocolConformanceLayout::readRecord(scratch, conformingTypeID,
                                                     numSubstitutions);

    ASTContext &ctx = getContext();
    Type conformingType = getType(conformingTypeID);
    if (genericEnv) {
      conformingType = genericEnv->mapTypeIntoContext(conformingType);
    }

    PrettyStackTraceType trace(getAssociatedModule()->getASTContext(),
                               "reading specialized conformance for",
                               conformingType);

    // Read the substitutions.
    SmallVector<Substitution, 4> substitutions;
    while (numSubstitutions--) {
      auto sub = maybeReadSubstitution(Cursor, genericEnv);
      assert(sub.hasValue() && "Missing substitution?");
      substitutions.push_back(*sub);
    }

    ProtocolConformanceRef genericConformance =
      readConformance(Cursor, genericEnv);
    PrettyStackTraceDecl traceTo("... to", genericConformance.getRequirement());

    assert(genericConformance.isConcrete() && "Abstract generic conformance?");
    auto conformance =
           ctx.getSpecializedConformance(conformingType,
                                         genericConformance.getConcrete(),
                                         substitutions);
    return ProtocolConformanceRef(conformance);
  }

  case INHERITED_PROTOCOL_CONFORMANCE: {
    TypeID conformingTypeID;
    InheritedProtocolConformanceLayout::readRecord(scratch, conformingTypeID);

    ASTContext &ctx = getContext();
    Type conformingType = getType(conformingTypeID);
    if (genericEnv) {
      conformingType = genericEnv->mapTypeIntoContext(conformingType);
    }

    PrettyStackTraceType trace(getAssociatedModule()->getASTContext(),
                               "reading inherited conformance for",
                               conformingType);

    ProtocolConformanceRef inheritedConformance =
      readConformance(Cursor, genericEnv);
    PrettyStackTraceDecl traceTo("... to",
                                 inheritedConformance.getRequirement());

    assert(inheritedConformance.isConcrete() &&
           "Abstract inherited conformance?");
    auto conformance =
      ctx.getInheritedConformance(conformingType,
                                  inheritedConformance.getConcrete());
    return ProtocolConformanceRef(conformance);
  }

  case NORMAL_PROTOCOL_CONFORMANCE_ID: {
    NormalConformanceID conformanceID;
    NormalProtocolConformanceIdLayout::readRecord(scratch, conformanceID);
    return ProtocolConformanceRef(readNormalConformance(conformanceID));
  }

  case PROTOCOL_CONFORMANCE_XREF: {
    DeclID protoID;
    DeclID nominalID;
    ModuleID moduleID;
    ProtocolConformanceXrefLayout::readRecord(scratch, protoID, nominalID,
                                              moduleID);

    auto nominal = cast<NominalTypeDecl>(getDecl(nominalID));
    PrettyStackTraceDecl trace("cross-referencing conformance for", nominal);
    auto proto = cast<ProtocolDecl>(getDecl(protoID));
    PrettyStackTraceDecl traceTo("... to", proto);
    auto module = getModule(moduleID);

    SmallVector<ProtocolConformance *, 2> conformances;
    nominal->lookupConformance(module, proto, conformances);
    PrettyStackTraceModuleFile traceMsg(
        "If you're seeing a crash here, check that your SDK and dependencies "
        "are at least as new as the versions used to build", this);
    // This would normally be an assertion but it's more useful to print the
    // PrettyStackTrace here even in no-asserts builds.
    if (conformances.empty())
      abort();
    return ProtocolConformanceRef(conformances.front());
  }

  // Not a protocol conformance.
  default:
    error();
    ProtocolConformance *conformance = nullptr;
    return ProtocolConformanceRef(conformance); // FIXME: this will assert
  }
}

NormalProtocolConformance *ModuleFile::readNormalConformance(
                             NormalConformanceID conformanceID) {
  auto &conformanceEntry = NormalConformances[conformanceID-1];
  if (conformanceEntry.isComplete()) {
    return conformanceEntry.get();
  }

  using namespace decls_block;

  // Find the conformance record.
  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(conformanceEntry);
  auto entry = DeclTypeCursor.advance();
  if (entry.Kind != llvm::BitstreamEntry::Record) {
    error();
    return nullptr;
  }

  DeclID protoID;
  DeclContextID contextID;
  unsigned valueCount, typeCount, inheritedCount;
  ArrayRef<uint64_t> rawIDs;
  SmallVector<uint64_t, 16> scratch;

  unsigned kind = DeclTypeCursor.readRecord(entry.ID, scratch);
  if (kind != NORMAL_PROTOCOL_CONFORMANCE) {
    error();
    return nullptr;
  }
  NormalProtocolConformanceLayout::readRecord(scratch, protoID,
                                              contextID, valueCount,
                                              typeCount, inheritedCount,
                                              rawIDs);

  ASTContext &ctx = getContext();
  DeclContext *dc = getDeclContext(contextID);
  Type conformingType = dc->getDeclaredTypeInContext();
  PrettyStackTraceType trace(ctx, "reading conformance for", conformingType);

  auto proto = cast<ProtocolDecl>(getDecl(protoID));
  PrettyStackTraceDecl traceTo("... to", proto);

  auto conformance = ctx.getConformance(conformingType, proto, SourceLoc(), dc,
                                        ProtocolConformanceState::Incomplete);

  // Record this conformance.
  if (conformanceEntry.isComplete())
    return conformance;

  uint64_t offset = conformanceEntry;
  conformanceEntry = conformance;

  dc->getAsNominalTypeOrNominalTypeExtensionContext()
    ->registerProtocolConformance(conformance);

  // Read requirement signature conformances.
  SmallVector<ProtocolConformanceRef, 4> reqConformances;
  for (auto req : proto->getRequirementSignature()->getRequirements()) {
    if (req.getKind() == RequirementKind::Conformance) {
      auto reqConformance = readConformance(DeclTypeCursor);
      reqConformances.push_back(reqConformance);
    }
  }
  conformance->setSignatureConformances(reqConformances);

  // Read inherited conformances.
  InheritedConformanceMap inheritedConformances;
  while (inheritedCount--) {
    auto inheritedRef = readConformance(DeclTypeCursor);
    assert(inheritedRef.isConcrete());
    auto inherited = inheritedRef.getConcrete();
    inheritedConformances[inherited->getProtocol()] = inherited;
  }

  // If the conformance is complete, we're done.
  if (conformance->isComplete())
    return conformance;

  // Record the inherited conformance.
  if (conformance->getInheritedConformances().empty())
    for (auto inherited : inheritedConformances)
      conformance->setInheritedConformance(inherited.first, inherited.second);

  conformance->setState(ProtocolConformanceState::Complete);
  conformance->setLazyLoader(this, offset);
  return conformance;
}

Optional<Substitution>
ModuleFile::maybeReadSubstitution(llvm::BitstreamCursor &cursor,
                                  GenericEnvironment *genericEnv) {
  BCOffsetRAII lastRecordOffset(cursor);

  auto entry = cursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind != llvm::BitstreamEntry::Record)
    return None;

  StringRef blobData;
  SmallVector<uint64_t, 2> scratch;
  unsigned recordID = cursor.readRecord(entry.ID, scratch, &blobData);
  if (recordID != decls_block::BOUND_GENERIC_SUBSTITUTION)
    return None;

  TypeID replacementID;
  unsigned numConformances;
  decls_block::BoundGenericSubstitutionLayout::readRecord(scratch,
                                                          replacementID,
                                                          numConformances);

  auto replacementTy = getType(replacementID);
  if (genericEnv) {
    replacementTy = genericEnv->mapTypeIntoContext(replacementTy);
  }

  SmallVector<ProtocolConformanceRef, 4> conformanceBuf;
  while (numConformances--) {
    conformanceBuf.push_back(readConformance(cursor));
  }

  lastRecordOffset.reset();
  return Substitution{replacementTy,
                      getContext().AllocateCopy(conformanceBuf)};
}

GenericParamList *ModuleFile::maybeReadGenericParams(DeclContext *DC,
                                               GenericParamList *outerParams) {
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

  SmallVector<GenericTypeParamDecl *, 8> params;

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
      params.push_back(genericParam);
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

  auto paramList = GenericParamList::create(getContext(), SourceLoc(),
                                            params, SourceLoc(), { },
                                            SourceLoc());
  paramList->setOuterParameters(outerParams ? outerParams :
                                DC->getGenericParamsOfContext());

  return paramList;
}

void ModuleFile::readGenericRequirements(
                   SmallVectorImpl<Requirement> &requirements,
                   llvm::BitstreamCursor &Cursor) {
  using namespace decls_block;

  BCOffsetRAII lastRecordOffset(Cursor);
  SmallVector<uint64_t, 8> scratch;
  StringRef blobData;

  while (true) {
    lastRecordOffset.reset();
    bool shouldContinue = true;

    auto entry = Cursor.advance(AF_DontPopBlockAtEnd);
    if (entry.Kind != llvm::BitstreamEntry::Record)
      break;

    scratch.clear();
    unsigned recordID = Cursor.readRecord(entry.ID, scratch, &blobData);
    switch (recordID) {
    case GENERIC_REQUIREMENT: {
      uint8_t rawKind;
      uint64_t rawTypeIDs[2];
      GenericRequirementLayout::readRecord(scratch, rawKind,
                                           rawTypeIDs[0], rawTypeIDs[1]);

      switch (rawKind) {
      case GenericRequirementKind::Conformance: {
        auto subject = getType(rawTypeIDs[0]);
        auto constraint = getType(rawTypeIDs[1]);

        requirements.push_back(Requirement(RequirementKind::Conformance,
                                           subject, constraint));
        break;
      }
      case GenericRequirementKind::Superclass: {
        auto subject = getType(rawTypeIDs[0]);
        auto constraint = getType(rawTypeIDs[1]);

        requirements.push_back(Requirement(RequirementKind::Superclass,
                                           subject, constraint));
        break;
      }
      case GenericRequirementKind::SameType: {
        auto first = getType(rawTypeIDs[0]);
        auto second = getType(rawTypeIDs[1]);

        requirements.push_back(Requirement(RequirementKind::SameType,
                                           first, second));
        break;
      }
      default:
        // Unknown requirement kind. Drop the requirement and continue, but log
        // an error so that we don't actually try to generate code.
        error();
      }
      break;
      }
    case LAYOUT_REQUIREMENT: {
      uint8_t rawKind;
      uint64_t rawTypeID;
      uint32_t size;
      uint32_t alignment;
      LayoutRequirementLayout::readRecord(scratch, rawKind, rawTypeID,
                                          size, alignment);

      auto first = getType(rawTypeID);
      LayoutConstraint layout;
      LayoutConstraintKind kind = LayoutConstraintKind::UnknownLayout;
      switch (rawKind) {
      default: {
        // Unknown layout requirement kind.
        error();
        break;
      }
      case LayoutRequirementKind::NativeRefCountedObject: {
        kind = LayoutConstraintKind::NativeRefCountedObject;
        break;
      }
      case LayoutRequirementKind::RefCountedObject: {
        kind = LayoutConstraintKind::RefCountedObject;
        break;
      }
      case LayoutRequirementKind::Trivial: {
        kind = LayoutConstraintKind::Trivial;
        break;
      }
      case LayoutRequirementKind::TrivialOfExactSize: {
        kind = LayoutConstraintKind::TrivialOfExactSize;
        break;
      }
      case LayoutRequirementKind::TrivialOfAtMostSize: {
        kind = LayoutConstraintKind::TrivialOfAtMostSize;
        break;
      }
      case LayoutRequirementKind::UnknownLayout: {
        kind = LayoutConstraintKind::UnknownLayout;
        break;
      }
      }

      ASTContext &ctx = getContext();
      if (kind != LayoutConstraintKind::TrivialOfAtMostSize &&
          kind != LayoutConstraintKind::TrivialOfExactSize)
        layout = LayoutConstraint::getLayoutConstraint(kind, ctx);
      else
        layout =
            LayoutConstraint::getLayoutConstraint(kind, size, alignment, ctx);

      requirements.push_back(
          Requirement(RequirementKind::Layout, first, layout));
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

void ModuleFile::configureGenericEnvironment(
             GenericContext *genericDecl,
             serialization::GenericEnvironmentID envID) {
  if (envID == 0) return;

  auto sigOrEnv = getGenericSignatureOrEnvironment(envID);

  // If we just have a generic signature, set up lazy generic environment
  // creation.
  if (auto genericSig = sigOrEnv.dyn_cast<GenericSignature *>()) {
    genericDecl->setLazyGenericEnvironment(this, genericSig, envID);
    ModuleFile &delayedActionFile = getModuleFileForDelayedActions();
    delayedActionFile.DelayedGenericEnvironments.push_back(genericDecl);
    return;
  }

  // If we have a full generic environment, it's because it happened to be
  // deserialized already. Record it directly.
  if (auto genericEnv = sigOrEnv.dyn_cast<GenericEnvironment *>()) {
    genericDecl->setGenericEnvironment(genericEnv);
    return;
  }
}

llvm::PointerUnion<GenericSignature *, GenericEnvironment *>
ModuleFile::getGenericSignatureOrEnvironment(
                                         serialization::GenericEnvironmentID ID,
                                         bool wantEnvironment) {
  // The empty result with the type the caller expects.
  llvm::PointerUnion<GenericSignature *, GenericEnvironment *> result;
  if (wantEnvironment)
    result = static_cast<GenericEnvironment *>(nullptr);

  // Zero is a sentinel for having no generic environment.
  if (ID == 0) return result;

  assert(ID <= GenericEnvironments.size() && "invalid GenericEnvironment ID");
  auto &envOrOffset = GenericEnvironments[ID-1];

  // If we've already deserialized this generic environment, return it.
  if (envOrOffset.isComplete()) {
    return envOrOffset.get();
  }

  // Read the generic environment.
  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(envOrOffset);
  DeserializingEntityRAII deserializingEntity(*this);

  SmallVector<GenericTypeParamType *, 4> paramTypes;
  {
    using namespace decls_block;

    StringRef blobData;
    SmallVector<uint64_t, 8> scratch;

    // we only want to be tracking the offset for this part of the function,
    // since loading the generic signature (a) may read the record we reject,
    // and (b) shouldn't have its progress erased. (That function also does its
    // own internal tracking.)
    BCOffsetRAII lastRecordOffset(DeclTypeCursor);

    auto entry = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
    if (entry.Kind != llvm::BitstreamEntry::Record)
      return result;

    unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch, &blobData);
    switch (recordID) {
    case GENERIC_ENVIRONMENT: {
      lastRecordOffset.reset();

      ArrayRef<uint64_t> rawParamIDs;
      GenericEnvironmentLayout::readRecord(scratch, rawParamIDs);

      for (unsigned i = 0, n = rawParamIDs.size(); i != n; ++i) {
        auto paramTy = getType(rawParamIDs[i])->castTo<GenericTypeParamType>();
        paramTypes.push_back(paramTy);
      }
      break;
    }

    case SIL_GENERIC_ENVIRONMENT: {
      ArrayRef<uint64_t> rawParamIDs;
      SILGenericEnvironmentLayout::readRecord(scratch, rawParamIDs);
      lastRecordOffset.reset();

      if (rawParamIDs.size() % 2 != 0) {
        error();
        return result;
      }

      for (unsigned i = 0, n = rawParamIDs.size(); i != n; i += 2) {
        Identifier name = getIdentifier(rawParamIDs[i]);
        auto paramTy = getType(rawParamIDs[i+1])->castTo<GenericTypeParamType>();

        if (!name.empty()) {
          auto paramDecl =
            createDecl<GenericTypeParamDecl>(getAssociatedModule(),
                                             name,
                                             SourceLoc(),
                                             paramTy->getDepth(),
                                             paramTy->getIndex());
          paramTy = paramDecl->getDeclaredInterfaceType()
                     ->castTo<GenericTypeParamType>();
        }

        paramTypes.push_back(paramTy);
      }
      break;
    }

    default:
      error();
      return result;
    }
  }

  // If there are no parameters, the environment is empty.
  if (paramTypes.empty()) {
    if (wantEnvironment)
      envOrOffset = nullptr;

    return result;
  }

  // Read the generic requirements.
  SmallVector<Requirement, 4> requirements;
  readGenericRequirements(requirements, DeclTypeCursor);

  // Construct the generic signature from the loaded parameters and
  // requirements.
  auto signature = GenericSignature::get(paramTypes, requirements);

  // If we only want the signature, return it now.
  if (!wantEnvironment) return signature;

  // If we've already deserialized this generic environment, return it.
  if (envOrOffset.isComplete()) {
    return envOrOffset.get();
  }

  // Form the generic environment. Record it now so that deserialization of
  // the archetypes in the environment can refer to this environment.
  auto genericEnv = signature->createGenericEnvironment(*getAssociatedModule());
  envOrOffset = genericEnv;

  return genericEnv;
}

GenericEnvironment *ModuleFile::getGenericEnvironment(
                                       serialization::GenericEnvironmentID ID) {
  return getGenericSignatureOrEnvironment(ID, /*wantEnvironment=*/true)
           .get<GenericEnvironment *>();
;
}

bool ModuleFile::readMembers(SmallVectorImpl<Decl *> &Members) {
  using namespace decls_block;

  auto entry = DeclTypeCursor.advance();
  if (entry.Kind != llvm::BitstreamEntry::Record)
    return true;

  SmallVector<uint64_t, 16> memberIDBuffer;

  unsigned kind = DeclTypeCursor.readRecord(entry.ID, memberIDBuffer);
  assert(kind == MEMBERS);
  (void)kind;

  ArrayRef<uint64_t> rawMemberIDs;
  decls_block::MembersLayout::readRecord(memberIDBuffer, rawMemberIDs);

  if (rawMemberIDs.empty())
    return false;

  Members.reserve(rawMemberIDs.size());
  for (DeclID rawID : rawMemberIDs) {
    Decl *D = getDecl(rawID);
    assert(D && "unable to deserialize next member");
    Members.push_back(D);
  }

  return false;
}

bool ModuleFile::readDefaultWitnessTable(ProtocolDecl *proto) {
  using namespace decls_block;

  auto entry = DeclTypeCursor.advance();
  if (entry.Kind != llvm::BitstreamEntry::Record)
    return true;

  SmallVector<uint64_t, 16> witnessIDBuffer;

  unsigned kind = DeclTypeCursor.readRecord(entry.ID, witnessIDBuffer);
  assert(kind == DEFAULT_WITNESS_TABLE);
  (void)kind;

  ArrayRef<uint64_t> rawWitnessIDs;
  decls_block::DefaultWitnessTableLayout::readRecord(
      witnessIDBuffer, rawWitnessIDs);

  if (rawWitnessIDs.empty())
    return false;

  unsigned e = rawWitnessIDs.size();
  assert(e % 2 == 0 && "malformed default witness table");
  (void) e;

  for (unsigned i = 0, e = rawWitnessIDs.size(); i < e; i += 2) {
    ValueDecl *requirement = cast<ValueDecl>(getDecl(rawWitnessIDs[i]));
    assert(requirement && "unable to deserialize next requirement");
    ValueDecl *witness = cast<ValueDecl>(getDecl(rawWitnessIDs[i + 1]));
    assert(witness && "unable to deserialize next witness");
    assert(requirement->getDeclContext() == proto);

    proto->setDefaultWitness(requirement, witness);
  }

  return false;
}

static Optional<swift::CtorInitializerKind>
getActualCtorInitializerKind(uint8_t raw) {
  switch (serialization::CtorInitializerKind(raw)) {
#define CASE(NAME) \
  case serialization::CtorInitializerKind::NAME: \
    return swift::CtorInitializerKind::NAME;
  CASE(Designated)
  CASE(Convenience)
  CASE(Factory)
  CASE(ConvenienceFactory)
#undef CASE
  }
  return None;
}

/// Remove values from \p values that don't match the expected type or module.
///
/// Any of \p expectedTy, \p expectedModule, or \p expectedGenericSig can be
/// omitted, in which case any type or module is accepted. Values imported
/// from Clang can also appear in any module.
static void filterValues(Type expectedTy, ModuleDecl *expectedModule,
                         CanGenericSignature expectedGenericSig, bool isType,
                         bool inProtocolExt, bool isStatic,
                         Optional<swift::CtorInitializerKind> ctorInit,
                         SmallVectorImpl<ValueDecl *> &values) {
  CanType canTy;
  if (expectedTy)
    canTy = expectedTy->getCanonicalType();

  auto newEnd = std::remove_if(values.begin(), values.end(),
                               [=](ValueDecl *value) {
    if (isType != isa<TypeDecl>(value))
      return true;
    if (!value->hasInterfaceType())
      return true;
    if (canTy && value->getInterfaceType()->getCanonicalType() != canTy)
      return true;
    if (value->isStatic() != isStatic)
      return true;
    // FIXME: Should be able to move a value from an extension in a derived
    // module to the original definition in a base module.
    if (expectedModule && !value->hasClangNode() &&
        value->getModuleContext() != expectedModule)
      return true;

    // If we're expecting a member within a constrained extension with a
    // particular generic signature, match that signature.
    if (expectedGenericSig &&
        value->getDeclContext()->getGenericSignatureOfContext()
          ->getCanonicalSignature() != expectedGenericSig)
      return true;

    // If we don't expect a specific generic signature, ignore anything from a
    // constrained extension.
    if (!expectedGenericSig &&
        isa<ExtensionDecl>(value->getDeclContext()) &&
        cast<ExtensionDecl>(value->getDeclContext())->isConstrainedExtension())
      return true;

    // If we're looking at members of a protocol or protocol extension,
    // filter by whether we expect to find something in a protocol extension or
    // not. This lets us distinguish between a protocol member and a protocol
    // extension member that have the same type.
    if (value->getDeclContext()->getAsProtocolOrProtocolExtensionContext() &&
        (bool)value->getDeclContext()->getAsProtocolExtensionContext()
          != inProtocolExt)
      return true;

    // If we're expecting an initializer with a specific kind, and this is not
    // an initializer with that kind, remove it.
    if (ctorInit) {
      if (!isa<ConstructorDecl>(value) ||
          cast<ConstructorDecl>(value)->getInitKind() != *ctorInit)
        return true;
    }
    return false;
  });
  values.erase(newEnd, values.end());
}

Decl *ModuleFile::resolveCrossReference(ModuleDecl *baseModule,
                                        uint32_t pathLen) {
  using namespace decls_block;
  assert(baseModule && "missing dependency");
  PrettyXRefTrace pathTrace(*baseModule);

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
    bool isType = (recordID == XREF_TYPE_PATH_PIECE);
    bool inProtocolExt = false;
    bool isStatic = false;
    if (isType)
      XRefTypePathPieceLayout::readRecord(scratch, IID, inProtocolExt);
    else
      XRefValuePathPieceLayout::readRecord(scratch, TID, IID, inProtocolExt,
                                           isStatic);

    Identifier name = getIdentifier(IID);
    pathTrace.addValue(name);

    Type filterTy = getType(TID);
    if (!isType)
      pathTrace.addType(filterTy);

    bool retrying = false;
    retry:

    baseModule->lookupQualified(ModuleType::get(baseModule), name,
                                NL_QualifiedDefault | NL_KnownNoDependency,
                                /*typeResolver=*/nullptr, values);
    filterValues(filterTy, nullptr, nullptr, isType, inProtocolExt, isStatic,
                 None, values);

    // HACK HACK HACK: Omit-needless-words hack to try to cope with
    // the "NS" prefix being added/removed. No "real" compiler mode
    // has to go through this path, but it's an option we toggle for
    // testing.
    if (values.empty() && !retrying &&
        (baseModule->getName().str() == "ObjectiveC" ||
         baseModule->getName().str() == "Foundation")) {
      if (name.str().startswith("NS")) {
        if (name.str().size() > 2 && name.str() != "NSCocoaError") {
          auto known = getKnownFoundationEntity(name.str());
          if (!known) {
            name = getContext().getIdentifier(name.str().substr(2));
            retrying = true;
            goto retry;
          }
        }
      } else {
        SmallString<16> buffer;
        buffer += "NS";
        buffer += name.str();
        // FIXME: Try uppercasing for non-types.
        name = getContext().getIdentifier(buffer);
        retrying = true;
        goto retry;
      }
    }

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
      return baseModule->lookupInfixOperator(opName);
    case OperatorKind::Prefix:
      return baseModule->lookupPrefixOperator(opName);
    case OperatorKind::Postfix:
      return baseModule->lookupPostfixOperator(opName);
    case OperatorKind::PrecedenceGroup:
      return baseModule->lookupPrecedenceGroup(opName);
    default:
      // Unknown operator kind.
      error();
      return nullptr;
    }
  }

  case XREF_GENERIC_PARAM_PATH_PIECE:
  case XREF_INITIALIZER_PATH_PIECE:
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

  // Filters for values discovered in the remaining path pieces.
  ModuleDecl *M = nullptr;
  CanGenericSignature genericSig = nullptr;

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
    case XREF_TYPE_PATH_PIECE: {
      if (values.size() == 1) {
        ModuleDecl *module = values.front()->getModuleContext();
        if (module == this->getAssociatedModule()) {
          // Fast path for nested types in the same module.
          IdentifierID IID;
          bool onlyInNominal = false;
          XRefTypePathPieceLayout::readRecord(scratch, IID, onlyInNominal);
          Identifier memberName = getIdentifier(IID);
          pathTrace.addValue(memberName);

          llvm::PrettyStackTraceString message{
              "If you're seeing a crash here, try passing "
              "-Xfrontend -disable-serialization-nested-type-lookup-table"};

          TypeDecl *nestedType = nullptr;
          if (onlyInNominal) {
            // Only look in the file containing the type itself.
            const DeclContext *dc = values.front()->getDeclContext();
            auto *serializedFile =
                dyn_cast<SerializedASTFile>(dc->getModuleScopeContext());
            if (serializedFile) {
              nestedType =
                  serializedFile->File.lookupNestedType(memberName,
                                                        values.front());
            }
          } else {
            // Fault in extensions, then ask every serialized AST in the module.
            (void)cast<NominalTypeDecl>(values.front())->getExtensions();
            for (FileUnit *file : module->getFiles()) {
              if (file == getFile())
                continue;
              auto *serializedFile = dyn_cast<SerializedASTFile>(file);
              if (!serializedFile)
                continue;
              nestedType =
                  serializedFile->File.lookupNestedType(memberName,
                                                        values.front());
              if (nestedType)
                break;
            }
          }

          if (nestedType) {
            values.clear();
            values.push_back(nestedType);
            ++NumNestedTypeShortcuts;
            break;
          }

          pathTrace.removeLast();
        }
      }
      LLVM_FALLTHROUGH;
    }
    case XREF_VALUE_PATH_PIECE:
    case XREF_INITIALIZER_PATH_PIECE: {
      TypeID TID = 0;
      Identifier memberName;
      Optional<swift::CtorInitializerKind> ctorInit;
      bool isType = false;
      bool inProtocolExt = false;
      bool isStatic = false;
      switch (recordID) {
      case XREF_TYPE_PATH_PIECE: {
        IdentifierID IID;
        XRefTypePathPieceLayout::readRecord(scratch, IID, inProtocolExt);
        memberName = getIdentifier(IID);
        isType = true;
        break;
      }

      case XREF_VALUE_PATH_PIECE: {
        IdentifierID IID;
        XRefValuePathPieceLayout::readRecord(scratch, TID, IID, inProtocolExt,
                                             isStatic);
        memberName = getIdentifier(IID);
        break;
      }

      case XREF_INITIALIZER_PATH_PIECE: {
        uint8_t kind;
        XRefInitializerPathPieceLayout::readRecord(scratch, TID, inProtocolExt,
                                                   kind);
        memberName = getContext().Id_init;
        ctorInit = getActualCtorInitializerKind(kind);
        break;
      }
        
      default:
        llvm_unreachable("Unhandled path piece");
      }

      pathTrace.addValue(memberName);

      Type filterTy = getType(TID);
      if (!isType)
        pathTrace.addType(filterTy);

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
      filterValues(filterTy, M, genericSig, isType, inProtocolExt, isStatic,
                   ctorInit, values);
      break;
    }

    case XREF_EXTENSION_PATH_PIECE: {
      ModuleID ownerID;
      ArrayRef<uint64_t> genericParamIDs;
      XRefExtensionPathPieceLayout::readRecord(scratch, ownerID,
                                               genericParamIDs);
      M = getModule(ownerID);
      pathTrace.addExtension(M);

      // Read the generic signature, if we have one.
      if (!genericParamIDs.empty()) {
        SmallVector<GenericTypeParamType *, 4> params;
        SmallVector<Requirement, 5> requirements;
        for (TypeID paramID : genericParamIDs) {
          params.push_back(getType(paramID)->castTo<GenericTypeParamType>());
        }
        readGenericRequirements(requirements, DeclTypeCursor);

        genericSig = GenericSignature::getCanonical(params, requirements);
      }

      continue;
    }

    case XREF_OPERATOR_OR_ACCESSOR_PATH_PIECE: {
      uint8_t rawKind;
      XRefOperatorOrAccessorPathPieceLayout::readRecord(scratch, None,
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
          case MaterializeForSet:
            values.front() = storage->getMaterializeForSetFunc();
            break;
          case Addressor:
            values.front() = storage->getAddressor();
            break;
          case MutableAddressor:
            values.front() = storage->getMutableAddressor();
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

      if (auto nominal = dyn_cast<NominalTypeDecl>(base)) {
        if (genericSig) {
          // Find an extension in the requested module that has the
          // correct generic signature.
          for (auto ext : nominal->getExtensions()) {
            if (ext->getModuleContext() == M &&
                ext->getGenericSignature()->getCanonicalSignature()
                  == genericSig) {
              paramList = ext->getGenericParams();
              break;
            }
          }
          assert(paramList && "Couldn't find constrained extension");
        } else {
          // Simple case: use the nominal type's generic parameters.
          paramList = nominal->getGenericParams();
        }
      } else if (auto alias = dyn_cast<TypeAliasDecl>(base)) {
        paramList = alias->getGenericParams();
      } else if (auto fn = dyn_cast<AbstractFunctionDecl>(base))
        paramList = fn->getGenericParams();

      if (!paramList || paramIndex >= paramList->size()) {
        error();
        return nullptr;
      }

      values.clear();
      values.push_back(paramList->getParams()[paramIndex]);
      assert(values.back());
      break;
    }

    default:
      // Unknown xref path piece.
      pathTrace.addUnknown(recordID);
      error();
      return nullptr;
    }

    Optional<PrettyStackTraceModuleFile> traceMsg;
    if (M != getAssociatedModule()) {
      traceMsg.emplace("If you're seeing a crash here, check that your SDK "
                         "and dependencies match the versions used to build",
                       this);
    }

    if (values.empty()) {
      error();
      return nullptr;
    }

    // Reset the module filter.
    M = nullptr;
    genericSig = nullptr;
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

DeclContext *ModuleFile::getLocalDeclContext(DeclContextID DCID) {
  assert(DCID != 0 && "invalid local DeclContext ID 0");
  auto &declContextOrOffset = LocalDeclContexts[DCID-1];

  if (declContextOrOffset.isComplete())
    return declContextOrOffset;

  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(declContextOrOffset);
  auto entry = DeclTypeCursor.advance();

  if (entry.Kind != llvm::BitstreamEntry::Record) {
    error();
    return nullptr;
  }

  ASTContext &ctx = getContext();
  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;

  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch,
                                                &blobData);
  switch(recordID) {
  case decls_block::ABSTRACT_CLOSURE_EXPR_CONTEXT: {
    TypeID closureTypeID;
    unsigned discriminator = 0;
    bool implicit = false;
    DeclContextID parentID;

    decls_block::AbstractClosureExprLayout::readRecord(scratch,
                                                       closureTypeID,
                                                       implicit,
                                                       discriminator,
                                                       parentID);
    DeclContext *parent = getDeclContext(parentID);
    auto type = getType(closureTypeID);

    declContextOrOffset = new (ctx)
      SerializedAbstractClosureExpr(type, implicit, discriminator, parent);
    break;
  }

  case decls_block::TOP_LEVEL_CODE_DECL_CONTEXT: {
    DeclContextID parentID;
    decls_block::TopLevelCodeDeclContextLayout::readRecord(scratch,
                                                           parentID);
    DeclContext *parent = getDeclContext(parentID);

    declContextOrOffset = new (ctx) SerializedTopLevelCodeDeclContext(parent);
    break;
  }

  case decls_block::PATTERN_BINDING_INITIALIZER_CONTEXT: {
    DeclID bindingID;
    uint32_t bindingIndex;
    decls_block::PatternBindingInitializerLayout::readRecord(scratch,
                                                             bindingID,
                                                             bindingIndex);
    auto decl = getDecl(bindingID);
    PatternBindingDecl *binding = cast<PatternBindingDecl>(decl);

    if (!declContextOrOffset.isComplete())
      declContextOrOffset = new (ctx)
        SerializedPatternBindingInitializer(binding, bindingIndex);
    break;
  }

  case decls_block::DEFAULT_ARGUMENT_INITIALIZER_CONTEXT: {
    DeclContextID parentID;
    unsigned index = 0;
    decls_block::DefaultArgumentInitializerLayout::readRecord(scratch,
                                                              parentID,
                                                              index);
    DeclContext *parent = getDeclContext(parentID);

    declContextOrOffset = new (ctx)
      SerializedDefaultArgumentInitializer(index, parent);
    break;
  }

  default:
    llvm_unreachable("Unknown record ID found when reading local DeclContext.");
  }
  return declContextOrOffset;
}

DeclContext *ModuleFile::getDeclContext(DeclContextID DCID) {
  if (DCID == 0)
    return FileContext;

  assert(DCID <= DeclContexts.size() && "invalid DeclContext ID");
  auto &declContextOrOffset = DeclContexts[DCID-1];

  if (declContextOrOffset.isComplete())
    return declContextOrOffset;

  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(declContextOrOffset);
  auto entry = DeclTypeCursor.advance();

  if (entry.Kind != llvm::BitstreamEntry::Record) {
    error();
    return nullptr;
  }

  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;

  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch, &blobData);

  if (recordID != decls_block::DECL_CONTEXT)
    llvm_unreachable("Expected a DECL_CONTEXT record");

  DeclContextID declOrDeclContextId;
  bool isDecl;

  decls_block::DeclContextLayout::readRecord(scratch, declOrDeclContextId,
                                             isDecl);

  if (!isDecl)
    return getLocalDeclContext(declOrDeclContextId);

  auto D = getDecl(declOrDeclContextId);

  if (auto ND = dyn_cast<NominalTypeDecl>(D)) {
    declContextOrOffset = ND;
  } else if (auto ED = dyn_cast<ExtensionDecl>(D)) {
    declContextOrOffset = ED;
  } else if (auto AFD = dyn_cast<AbstractFunctionDecl>(D)) {
    declContextOrOffset = AFD;
  } else if (auto SD = dyn_cast<SubscriptDecl>(D)) {
    declContextOrOffset = SD;
  } else if (auto TAD = dyn_cast<TypeAliasDecl>(D)) {
    declContextOrOffset = TAD;
  } else {
    llvm_unreachable("Unknown Decl : DeclContext kind");
  }
  
  return declContextOrOffset;
}

ModuleDecl *ModuleFile::getModule(ModuleID MID) {
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

ModuleDecl *ModuleFile::getModule(ArrayRef<Identifier> name) {
  if (name.empty() || name.front().empty())
    return getContext().TheBuiltinModule;

  // FIXME: duplicated from NameBinder::getModule
  if (name.size() == 1 &&
      name.front() == FileContext->getParentModule()->getName()) {
    if (!ShadowedModule) {
      auto importer = getContext().getClangModuleLoader();
      assert(importer && "no way to import shadowed module");
      ShadowedModule = importer->loadModule(SourceLoc(),
                                            { { name.front(), SourceLoc() } });
    }

    return ShadowedModule;
  }

  SmallVector<ImportDecl::AccessPathElement, 4> importPath;
  for (auto pathElem : name)
    importPath.push_back({ pathElem, SourceLoc() });
  return getContext().getModule(importPath);
}


/// Translate from the Serialization associativity enum values to the AST
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
    return None;
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
  return None;
}

static bool isDeclAttrRecord(unsigned ID) {
  using namespace decls_block;
  switch (ID) {
#define DECL_ATTR(NAME, CLASS, ...) case CLASS##_DECL_ATTR: return true;
#include "swift/Serialization/DeclTypeRecordNodes.def"
  default: return false;
  }
}

static Optional<swift::Accessibility>
getActualAccessibility(uint8_t raw) {
  switch (serialization::AccessibilityKind(raw)) {
#define CASE(NAME) \
  case serialization::AccessibilityKind::NAME: \
    return Accessibility::NAME;
  CASE(Private)
  CASE(FilePrivate)
  CASE(Internal)
  CASE(Public)
  CASE(Open)
#undef CASE
  }
  return None;
}

static Optional<swift::OptionalTypeKind>
getActualOptionalTypeKind(uint8_t raw) {
  switch (serialization::OptionalTypeKind(raw)) {
  case serialization::OptionalTypeKind::None:
    return OTK_None;
  case serialization::OptionalTypeKind::Optional:
    return OTK_Optional;
  case serialization::OptionalTypeKind::ImplicitlyUnwrappedOptional:
    return OTK_ImplicitlyUnwrappedOptional;
  }

  return None;
}

static Optional<swift::AddressorKind>
getActualAddressorKind(uint8_t raw) {
  switch (serialization::AddressorKind(raw)) {
  case serialization::AddressorKind::NotAddressor:
    return swift::AddressorKind::NotAddressor;
  case serialization::AddressorKind::Unsafe:
    return swift::AddressorKind::Unsafe;
  case serialization::AddressorKind::Owning:
    return swift::AddressorKind::Owning;
  case serialization::AddressorKind::NativeOwning:
    return swift::AddressorKind::NativeOwning;
  case serialization::AddressorKind::NativePinning:
    return swift::AddressorKind::NativePinning;
  }

  return None;
}

void ModuleFile::configureStorage(AbstractStorageDecl *decl,
                                  unsigned rawStorageKind,
                                  serialization::DeclID getter,
                                  serialization::DeclID setter,
                                  serialization::DeclID materializeForSet,
                                  serialization::DeclID addressor,
                                  serialization::DeclID mutableAddressor,
                                  serialization::DeclID willSet,
                                  serialization::DeclID didSet) {
  // We currently don't serialize these locations.
  SourceLoc beginLoc, endLoc;

  auto makeAddressed = [&] {
    decl->makeAddressed(beginLoc,
                        cast_or_null<FuncDecl>(getDecl(addressor)),
                        cast_or_null<FuncDecl>(getDecl(mutableAddressor)),
                        endLoc);
  };

  auto addTrivialAccessors = [&] {
    decl->addTrivialAccessors(
                       cast_or_null<FuncDecl>(getDecl(getter)),
                       cast_or_null<FuncDecl>(getDecl(setter)),
                       cast_or_null<FuncDecl>(getDecl(materializeForSet)));
  };

  auto setObservingAccessors = [&] {
    decl->setObservingAccessors(
                       cast_or_null<FuncDecl>(getDecl(getter)),
                       cast_or_null<FuncDecl>(getDecl(setter)),
                       cast_or_null<FuncDecl>(getDecl(materializeForSet)));
  };

  switch ((StorageKind) rawStorageKind) {
  case StorageKind::Stored:
    return;

  case StorageKind::StoredWithTrivialAccessors:
    addTrivialAccessors();
    return;

  case StorageKind::StoredWithObservers:
    decl->makeStoredWithObservers(beginLoc,
                       cast_or_null<FuncDecl>(getDecl(willSet)),
                       cast_or_null<FuncDecl>(getDecl(didSet)),
                       endLoc);
    setObservingAccessors();
    return;

  case StorageKind::InheritedWithObservers:
    decl->makeInheritedWithObservers(beginLoc,
                       cast_or_null<FuncDecl>(getDecl(willSet)),
                       cast_or_null<FuncDecl>(getDecl(didSet)),
                       endLoc);
    setObservingAccessors();
    return;

  case StorageKind::Addressed:
    makeAddressed();
    return;

  case StorageKind::AddressedWithTrivialAccessors:
    makeAddressed();
    addTrivialAccessors();
    return;

  case StorageKind::AddressedWithObservers:
    decl->makeAddressedWithObservers(beginLoc,
                       cast_or_null<FuncDecl>(getDecl(addressor)),
                       cast_or_null<FuncDecl>(getDecl(mutableAddressor)),
                       cast_or_null<FuncDecl>(getDecl(willSet)),
                       cast_or_null<FuncDecl>(getDecl(didSet)),
                       endLoc);
    setObservingAccessors();
    return;

  case StorageKind::Computed:
    decl->makeComputed(beginLoc,
                       cast_or_null<FuncDecl>(getDecl(getter)),
                       cast_or_null<FuncDecl>(getDecl(setter)),
                       cast_or_null<FuncDecl>(getDecl(materializeForSet)),
                       endLoc);
    return;

  case StorageKind::ComputedWithMutableAddress:
    decl->makeComputedWithMutableAddress(beginLoc,
                       cast_or_null<FuncDecl>(getDecl(getter)),
                       cast_or_null<FuncDecl>(getDecl(setter)),
                       cast_or_null<FuncDecl>(getDecl(materializeForSet)),
                       cast_or_null<FuncDecl>(getDecl(mutableAddressor)),
                       endLoc);
    return;
  }
  llvm_unreachable("bad storage kind");
}

template <typename T, typename ...Args>
T *ModuleFile::createDecl(Args &&... args) {
  // Note that this method is not used for all decl kinds.
  static_assert(std::is_base_of<Decl, T>::value, "not a Decl");
  T *result = new (getContext()) T(std::forward<Args>(args)...);
  result->setEarlyAttrValidation(true);
  return result;
}

static const uint64_t lazyConformanceContextDataPositionMask = 0xFFFFFFFFFFFF;

/// Decode the context data for lazily-loaded conformances.
static std::pair<uint64_t, uint64_t> decodeLazyConformanceContextData(
                                       uint64_t contextData) {
  return std::make_pair(contextData >> 48,
                        contextData & lazyConformanceContextDataPositionMask);
}

/// Encode the context data for lazily-loaded conformances.
static uint64_t encodeLazyConformanceContextData(uint64_t numProtocols,
                                                 uint64_t bitPosition) {
  assert(numProtocols < 0xFFFF);
  assert(bitPosition < lazyConformanceContextDataPositionMask);
  return (numProtocols << 48) | bitPosition;
}

Decl *ModuleFile::getDecl(DeclID DID, Optional<DeclContext *> ForcedContext) {
  if (DID == 0)
    return nullptr;

  assert(DID <= Decls.size() && "invalid decl ID");
  auto &declOrOffset = Decls[DID-1];

  if (declOrOffset.isComplete())
    return declOrOffset;

  ++NumDeclsLoaded;
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

  class PrivateDiscriminatorRAII {
    ModuleFile &moduleFile;
    Serialized<Decl *> &declOrOffset;

  public:
    Identifier discriminator;

    PrivateDiscriminatorRAII(ModuleFile &moduleFile,
                      Serialized<Decl *> &declOrOffset)
      : moduleFile(moduleFile), declOrOffset(declOrOffset) {}

    ~PrivateDiscriminatorRAII() {
      if (!discriminator.empty() && declOrOffset.isComplete())
        if (auto value = dyn_cast_or_null<ValueDecl>(declOrOffset.get()))
          moduleFile.PrivateDiscriminatorsByValue[value] = discriminator;
    }
  };

  class LocalDiscriminatorRAII {
    Serialized<Decl *> &declOrOffset;

  public:
    unsigned discriminator;

    LocalDiscriminatorRAII(Serialized<Decl *> &declOrOffset)
      : declOrOffset(declOrOffset), discriminator(0) {}

    ~LocalDiscriminatorRAII() {
      if (discriminator != 0 && declOrOffset.isComplete())
        if (auto value = dyn_cast<ValueDecl>(declOrOffset.get()))
          value->setLocalDiscriminator(discriminator);
    }
  };

  PrivateDiscriminatorRAII privateDiscriminatorRAII{*this, declOrOffset};
  LocalDiscriminatorRAII localDiscriminatorRAII(declOrOffset);
  DeserializingEntityRAII deserializingEntity(*this);

  // Local function that handles the "inherited" list for a type.
  auto handleInherited
    = [&](TypeDecl *nominal, ArrayRef<uint64_t> rawInheritedIDs) {
      auto inheritedTypes = ctx.Allocate<TypeLoc>(rawInheritedIDs.size());
      for_each(inheritedTypes, rawInheritedIDs,
               [this](TypeLoc &tl, uint64_t rawID) {
         tl = TypeLoc::withoutLoc(getType(rawID));
      });
      nominal->setInherited(inheritedTypes);
      nominal->setCheckedInheritanceClause();
  };

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
      case decls_block::SILGenName_DECL_ATTR: {
        bool isImplicit;
        serialization::decls_block::SILGenNameDeclAttrLayout::readRecord(
            scratch, isImplicit);
        Attr = new (ctx) SILGenNameAttr(blobData, isImplicit);
        break;
      }

      case decls_block::CDecl_DECL_ATTR: {
        bool isImplicit;
        serialization::decls_block::CDeclDeclAttrLayout::readRecord(
            scratch, isImplicit);
        Attr = new (ctx) CDeclAttr(blobData, isImplicit);
        break;
      }

      case decls_block::Alignment_DECL_ATTR: {
        bool isImplicit;
        unsigned alignment;
        serialization::decls_block::AlignmentDeclAttrLayout::readRecord(
            scratch, isImplicit, alignment);
        Attr = new (ctx) AlignmentAttr(alignment, SourceLoc(), SourceRange(),
                                       isImplicit);
        break;
      }
      
      case decls_block::SwiftNativeObjCRuntimeBase_DECL_ATTR: {
        bool isImplicit;
        IdentifierID nameID;
        serialization::decls_block::SwiftNativeObjCRuntimeBaseDeclAttrLayout
          ::readRecord(scratch, isImplicit, nameID);
        
        auto name = getIdentifier(nameID);
        Attr = new (ctx) SwiftNativeObjCRuntimeBaseAttr(name, SourceLoc(),
                                                        SourceRange(),
                                                        isImplicit);
        break;
      }

      case decls_block::Semantics_DECL_ATTR: {
        bool isImplicit;
        serialization::decls_block::SemanticsDeclAttrLayout::readRecord(
            scratch, isImplicit);
        Attr = new (ctx) SemanticsAttr(blobData, isImplicit);
        break;
      }

      case decls_block::Inline_DECL_ATTR: {
        unsigned kind;
        serialization::decls_block::InlineDeclAttrLayout::readRecord(
            scratch, kind);
        Attr = new (ctx) InlineAttr((InlineKind)kind);
        break;
      }

      case decls_block::Effects_DECL_ATTR: {
        unsigned kind;
        serialization::decls_block::EffectsDeclAttrLayout::readRecord(scratch,
                                                                      kind);
        Attr = new (ctx) EffectsAttr((EffectsKind)kind);
        break;
      }

      case decls_block::Available_DECL_ATTR: {
#define LIST_VER_TUPLE_PIECES(X)\
  X##_Major, X##_Minor, X##_Subminor, X##_HasMinor, X##_HasSubminor
#define DEF_VER_TUPLE_PIECES(X) unsigned LIST_VER_TUPLE_PIECES(X)
#define DECODE_VER_TUPLE(X)\
  if (X##_HasMinor) {\
    if (X##_HasSubminor)\
      X = clang::VersionTuple(X##_Major, X##_Minor, X##_Subminor);\
    else\
      X = clang::VersionTuple(X##_Major, X##_Minor);\
    }\
  else X = clang::VersionTuple(X##_Major);

        bool isImplicit;
        bool isUnavailable;
        bool isDeprecated;
        DEF_VER_TUPLE_PIECES(Introduced);
        DEF_VER_TUPLE_PIECES(Deprecated);
        DEF_VER_TUPLE_PIECES(Obsoleted);
        unsigned platform, messageSize, renameSize;
        // Decode the record, pulling the version tuple information.
        serialization::decls_block::AvailableDeclAttrLayout::readRecord(
            scratch, isImplicit, isUnavailable, isDeprecated,
            LIST_VER_TUPLE_PIECES(Introduced),
            LIST_VER_TUPLE_PIECES(Deprecated),
            LIST_VER_TUPLE_PIECES(Obsoleted),
            platform, messageSize, renameSize);

        StringRef message = blobData.substr(0, messageSize);
        blobData = blobData.substr(messageSize);
        StringRef rename = blobData.substr(0, renameSize);
        clang::VersionTuple Introduced, Deprecated, Obsoleted;
        DECODE_VER_TUPLE(Introduced)
        DECODE_VER_TUPLE(Deprecated)
        DECODE_VER_TUPLE(Obsoleted)

        PlatformAgnosticAvailabilityKind platformAgnostic;
        if (isUnavailable)
          platformAgnostic = PlatformAgnosticAvailabilityKind::Unavailable;
        else if (isDeprecated)
          platformAgnostic = PlatformAgnosticAvailabilityKind::Deprecated;
        else if (((PlatformKind)platform) == PlatformKind::none &&
                 (!Introduced.empty() ||
                  !Deprecated.empty() ||
                  !Obsoleted.empty()))
          platformAgnostic =
            PlatformAgnosticAvailabilityKind::SwiftVersionSpecific;
        else
          platformAgnostic = PlatformAgnosticAvailabilityKind::None;

        Attr = new (ctx) AvailableAttr(
          SourceLoc(), SourceRange(),
          (PlatformKind)platform, message, rename,
          Introduced, SourceRange(),
          Deprecated, SourceRange(),
          Obsoleted, SourceRange(),
          platformAgnostic, isImplicit);
        break;

#undef DEF_VER_TUPLE_PIECES
#undef LIST_VER_TUPLE_PIECES
#undef DECODE_VER_TUPLE
      }

      case decls_block::AutoClosure_DECL_ATTR: {
        bool isImplicit;
        bool isEscaping;
        serialization::decls_block::AutoClosureDeclAttrLayout::readRecord(
          scratch, isImplicit, isEscaping);
        Attr = new (ctx) AutoClosureAttr(SourceLoc(), SourceRange(),
                                         isEscaping, isImplicit);
        break;
      }

      case decls_block::ObjC_DECL_ATTR: {
        bool isImplicit;
        bool isImplicitName;
        uint64_t numArgs;
        ArrayRef<uint64_t> rawPieceIDs;
        serialization::decls_block::ObjCDeclAttrLayout::readRecord(
          scratch, isImplicit, isImplicitName, numArgs, rawPieceIDs);

        SmallVector<Identifier, 4> pieces;
        for (auto pieceID : rawPieceIDs)
          pieces.push_back(getIdentifier(pieceID));

        if (numArgs == 0)
          Attr = ObjCAttr::create(ctx, None, isImplicitName);
        else
          Attr = ObjCAttr::create(ctx, ObjCSelector(ctx, numArgs-1, pieces),
                                  isImplicitName);
        Attr->setImplicit(isImplicit);
        break;
      }

      case decls_block::Specialize_DECL_ATTR: {
        unsigned exported;
        SpecializeAttr::SpecializationKind specializationKind;
        unsigned specializationKindVal;
        SmallVector<Requirement, 8> requirements;

        serialization::decls_block::SpecializeDeclAttrLayout::readRecord(
          scratch, exported, specializationKindVal);

        specializationKind = specializationKindVal
                                 ? SpecializeAttr::SpecializationKind::Partial
                                 : SpecializeAttr::SpecializationKind::Full;

        readGenericRequirements(requirements, DeclTypeCursor);

        Attr = SpecializeAttr::create(ctx, SourceLoc(), SourceRange(),
                                      requirements, exported != 0,
                                      specializationKind);
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

    } else if (recordID == decls_block::PRIVATE_DISCRIMINATOR) {
      IdentifierID discriminatorID;
      decls_block::PrivateDiscriminatorLayout::readRecord(scratch,
                                                          discriminatorID);
      privateDiscriminatorRAII.discriminator = getIdentifier(discriminatorID);

    } else if (recordID == decls_block::LOCAL_DISCRIMINATOR) {
      unsigned discriminator;
      decls_block::LocalDiscriminatorLayout::readRecord(scratch, discriminator);
      localDiscriminatorRAII.discriminator = discriminator;
    } else {
      break;
    }

    // Advance bitstream cursor to the next record.
    entry = DeclTypeCursor.advance();

    // Prepare to read the next record.
    scratch.clear();
  }

  PrettyDeclDeserialization stackTraceEntry(
     this, declOrOffset, DID, static_cast<decls_block::RecordKind>(recordID));

  switch (recordID) {
  case decls_block::TYPE_ALIAS_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    TypeID underlyingTypeID, interfaceTypeID;
    bool isImplicit;
    GenericEnvironmentID genericEnvID;
    uint8_t rawAccessLevel;

    decls_block::TypeAliasLayout::readRecord(scratch, nameID, contextID,
                                             underlyingTypeID, interfaceTypeID,
                                             isImplicit, genericEnvID,
                                             rawAccessLevel);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);

    auto genericParams = maybeReadGenericParams(DC);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto alias = createDecl<TypeAliasDecl>(SourceLoc(), SourceLoc(),
                                           getIdentifier(nameID),
                                           SourceLoc(), genericParams, DC);
    declOrOffset = alias;

    configureGenericEnvironment(alias, genericEnvID);

    alias->setUnderlyingType(getType(underlyingTypeID));

    if (auto accessLevel = getActualAccessibility(rawAccessLevel)) {
      alias->setAccessibility(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    if (auto interfaceType = getType(interfaceTypeID))
      alias->setInterfaceType(interfaceType);

    if (isImplicit)
      alias->setImplicit();

    alias->setCheckedInheritanceClause();
    break;
  }

  case decls_block::GENERIC_TYPE_PARAM_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    bool isImplicit;
    unsigned depth;
    unsigned index;

    decls_block::GenericTypeParamDeclLayout::readRecord(scratch, nameID,
                                                        contextID,
                                                        isImplicit,
                                                        depth,
                                                        index);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);

    if (declOrOffset.isComplete())
      return declOrOffset;

    auto genericParam = createDecl<GenericTypeParamDecl>(DC,
                                                         getIdentifier(nameID),
                                                         SourceLoc(),
                                                         depth,
                                                         index);
    declOrOffset = genericParam;

    if (isImplicit)
      genericParam->setImplicit();

    break;
  }

  case decls_block::ASSOCIATED_TYPE_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    TypeID defaultDefinitionID;
    bool isImplicit;
    ArrayRef<uint64_t> rawInheritedIDs;

    decls_block::AssociatedTypeDeclLayout::readRecord(scratch, nameID,
                                                      contextID,
                                                      defaultDefinitionID,
                                                      isImplicit,
                                                      rawInheritedIDs);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    // The where-clause information is pushed up into the protocol
    // (specifically, into its requirement signature) and
    // serialized/deserialized there, so the actual Decl doesn't need to store
    // it.
    TrailingWhereClause *trailingWhere = nullptr;
    auto assocType = createDecl<AssociatedTypeDecl>(
        DC, SourceLoc(), getIdentifier(nameID), SourceLoc(), trailingWhere,
        this, defaultDefinitionID);
    declOrOffset = assocType;

    assocType->computeType();

    assert(!assocType->getDeclaredInterfaceType()->hasError() &&
           "erroneous associated type");

    Accessibility parentAccess = cast<ProtocolDecl>(DC)->getFormalAccess();
    assocType->setAccessibility(std::max(parentAccess,Accessibility::Internal));
    if (isImplicit)
      assocType->setImplicit();

    auto inherited = ctx.Allocate<TypeLoc>(rawInheritedIDs.size());
    for_each(inherited, rawInheritedIDs, [this](TypeLoc &loc, uint64_t rawID) {
      loc.setType(getType(rawID));
    });
    assocType->setInherited(inherited);

    assocType->setCheckedInheritanceClause();
    break;
  }

  case decls_block::STRUCT_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    bool isImplicit;
    GenericEnvironmentID genericEnvID;
    uint8_t rawAccessLevel;
    unsigned numConformances;
    ArrayRef<uint64_t> rawInheritedIDs;

    decls_block::StructLayout::readRecord(scratch, nameID, contextID,
                                          isImplicit, genericEnvID,
                                          rawAccessLevel,
                                          numConformances,
                                          rawInheritedIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto genericParams = maybeReadGenericParams(DC);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto theStruct = createDecl<StructDecl>(SourceLoc(), getIdentifier(nameID),
                                            SourceLoc(), None, genericParams,
                                            DC);
    declOrOffset = theStruct;

    // Read the generic environment.
    configureGenericEnvironment(theStruct, genericEnvID);

    if (auto accessLevel = getActualAccessibility(rawAccessLevel)) {
      theStruct->setAccessibility(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    if (isImplicit)
      theStruct->setImplicit();

    theStruct->computeType();

    handleInherited(theStruct, rawInheritedIDs);

    theStruct->setMemberLoader(this, DeclTypeCursor.GetCurrentBitNo());
    skipRecord(DeclTypeCursor, decls_block::MEMBERS);
    theStruct->setConformanceLoader(
      this,
      encodeLazyConformanceContextData(numConformances,
                                       DeclTypeCursor.GetCurrentBitNo()));

    break;
  }

  case decls_block::CONSTRUCTOR_DECL: {
    DeclContextID contextID;
    uint8_t rawFailability;
    bool isImplicit, isObjC, hasStubImplementation, throws;
    GenericEnvironmentID genericEnvID;
    uint8_t storedInitKind, rawAccessLevel;
    TypeID interfaceID;
    DeclID overriddenID;
    ArrayRef<uint64_t> argNameIDs;

    decls_block::ConstructorLayout::readRecord(scratch, contextID,
                                               rawFailability, isImplicit, 
                                               isObjC, hasStubImplementation,
                                               throws, storedInitKind,
                                               genericEnvID, interfaceID,
                                               overriddenID, rawAccessLevel,
                                               argNameIDs);
    auto parent = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto *genericParams = maybeReadGenericParams(parent);
    if (declOrOffset.isComplete())
      return declOrOffset;

    // Resolve the name ids.
    SmallVector<Identifier, 2> argNames;
    for (auto argNameID : argNameIDs)
      argNames.push_back(getIdentifier(argNameID));

    OptionalTypeKind failability = OTK_None;
    if (auto actualFailability = getActualOptionalTypeKind(rawFailability))
      failability = *actualFailability;

    DeclName name(ctx, ctx.Id_init, argNames);
    auto ctor =
      createDecl<ConstructorDecl>(name, SourceLoc(),
                                  failability, /*FailabilityLoc=*/SourceLoc(),
                                  /*Throws=*/throws, /*ThrowsLoc=*/SourceLoc(),
                                  /*BodyParams=*/nullptr, nullptr,
                                  genericParams, parent);
    declOrOffset = ctor;

    configureGenericEnvironment(ctor, genericEnvID);

    if (auto accessLevel = getActualAccessibility(rawAccessLevel)) {
      ctor->setAccessibility(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    auto *bodyParams0 = readParameterList();
    bodyParams0->get(0)->setImplicit();  // self is implicit.
    
    auto *bodyParams1 = readParameterList();
    assert(bodyParams0 && bodyParams1 && "missing parameters for constructor");
    ctor->setParameterLists(bodyParams0->get(0), bodyParams1);

    auto interfaceType = getType(interfaceID);
    ctor->setInterfaceType(interfaceType);

    // Set the initializer interface type of the constructor.
    auto allocType = ctor->getInterfaceType();
    auto selfTy = ctor->computeInterfaceSelfType(/*isInitializingCtor=*/true);
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

    if (auto errorConvention = maybeReadForeignErrorConvention())
      ctor->setForeignErrorConvention(*errorConvention);

    if (isImplicit)
      ctor->setImplicit();
    if (hasStubImplementation)
      ctor->setStubImplementation(true);
    if (auto initKind = getActualCtorInitializerKind(storedInitKind))
      ctor->setInitKind(*initKind);
    if (auto overridden
          = dyn_cast_or_null<ConstructorDecl>(getDecl(overriddenID)))
      ctor->setOverriddenDecl(overridden);
    break;
  }

  case decls_block::VAR_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    bool isImplicit, isObjC, isStatic, isLet, hasNonPatternBindingInit;
    uint8_t storageKind, rawAccessLevel, rawSetterAccessLevel;
    TypeID interfaceTypeID;
    DeclID getterID, setterID, materializeForSetID, willSetID, didSetID;
    DeclID addressorID, mutableAddressorID, overriddenID;

    decls_block::VarLayout::readRecord(scratch, nameID, contextID,
                                       isImplicit, isObjC, isStatic, isLet,
                                       hasNonPatternBindingInit, storageKind,
                                       interfaceTypeID, getterID,
                                       setterID, materializeForSetID,
                                       addressorID, mutableAddressorID,
                                       willSetID, didSetID, overriddenID,
                                       rawAccessLevel, rawSetterAccessLevel);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto var = createDecl<VarDecl>(/*IsStatic*/isStatic, /*IsLet*/isLet,
                                   /*IsCaptureList*/false, SourceLoc(),
                                   getIdentifier(nameID), Type(), DC);
    var->setHasNonPatternBindingInit(hasNonPatternBindingInit);
    declOrOffset = var;

    Type interfaceType = getType(interfaceTypeID);
    var->setInterfaceType(interfaceType);

    if (auto referenceStorage = interfaceType->getAs<ReferenceStorageType>())
      AddAttribute(new (ctx) OwnershipAttr(referenceStorage->getOwnership()));

    configureStorage(var, storageKind, getterID, setterID, materializeForSetID,
                     addressorID, mutableAddressorID, willSetID, didSetID);

    if (auto accessLevel = getActualAccessibility(rawAccessLevel)) {
      var->setAccessibility(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    if (var->isSettable(nullptr)) {
      if (auto setterAccess = getActualAccessibility(rawSetterAccessLevel)) {
        var->setSetterAccessibility(*setterAccess);
      } else {
        error();
        return nullptr;
      }
    }

    if (isImplicit)
      var->setImplicit();

    if (auto overridden = cast_or_null<VarDecl>(getDecl(overriddenID))) {
      var->setOverriddenDecl(overridden);
      AddAttribute(new (ctx) OverrideAttr(SourceLoc()));
    }

    break;
  }

  case decls_block::PARAM_DECL: {
    IdentifierID argNameID, paramNameID;
    DeclContextID contextID;
    bool isLet;
    TypeID interfaceTypeID;

    decls_block::ParamLayout::readRecord(scratch, argNameID, paramNameID,
                                         contextID, isLet, interfaceTypeID);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto param = createDecl<ParamDecl>(isLet, SourceLoc(), SourceLoc(),
                                       getIdentifier(argNameID), SourceLoc(),
                                       getIdentifier(paramNameID), Type(), DC);

    declOrOffset = param;

    auto paramTy = getType(interfaceTypeID);
    if (paramTy->hasError()) {
      // FIXME: This should never happen, because we don't serialize
      // error types.
      DC->dumpContext();
      paramTy->dump();
      error();
      return nullptr;
    }

    param->setInterfaceType(paramTy);
    break;
  }

  case decls_block::FUNC_DECL: {
    DeclContextID contextID;
    bool isImplicit;
    bool isStatic;
    uint8_t rawStaticSpelling, rawAccessLevel, rawAddressorKind;
    bool isObjC, isMutating, hasDynamicSelf, throws;
    unsigned numParamPatterns;
    GenericEnvironmentID genericEnvID;
    TypeID interfaceTypeID;
    DeclID associatedDeclID;
    DeclID overriddenID;
    DeclID accessorStorageDeclID;
    bool hasCompoundName;
    ArrayRef<uint64_t> nameIDs;

    decls_block::FuncLayout::readRecord(scratch, contextID, isImplicit,
                                        isStatic, rawStaticSpelling, isObjC,
                                        isMutating, hasDynamicSelf, throws,
                                        numParamPatterns, genericEnvID,
                                        interfaceTypeID, associatedDeclID,
                                        overriddenID, accessorStorageDeclID,
                                        hasCompoundName, rawAddressorKind,
                                        rawAccessLevel, nameIDs);
    
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
    GenericParamList *genericParams = maybeReadGenericParams(DC);

    auto staticSpelling = getActualStaticSpellingKind(rawStaticSpelling);
    if (!staticSpelling.hasValue()) {
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
        ctx, /*StaticLoc=*/SourceLoc(), staticSpelling.getValue(),
        /*FuncLoc=*/SourceLoc(), name, /*NameLoc=*/SourceLoc(),
        /*Throws=*/throws, /*ThrowsLoc=*/SourceLoc(),
        /*AccessorKeywordLoc=*/SourceLoc(), genericParams,
        numParamPatterns, DC);
    fn->setEarlyAttrValidation();
    declOrOffset = fn;

    configureGenericEnvironment(fn, genericEnvID);

    if (auto accessLevel = getActualAccessibility(rawAccessLevel)) {
      fn->setAccessibility(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    if (auto addressorKind = getActualAddressorKind(rawAddressorKind)) {
      if (*addressorKind != AddressorKind::NotAddressor)
        fn->setAddressorKind(*addressorKind);
    } else {
      error();
      return nullptr;
    }

    if (Decl *associated = getDecl(associatedDeclID)) {
      if (auto op = dyn_cast<OperatorDecl>(associated)) {
        fn->setOperatorDecl(op);

        if (isa<PrefixOperatorDecl>(op))
          fn->getAttrs().add(new (ctx) PrefixAttr(/*implicit*/false));
        else if (isa<PostfixOperatorDecl>(op))
          fn->getAttrs().add(new (ctx) PostfixAttr(/*implicit*/false));
        // Note that an explicit 'infix' is not required.
      }
      // Otherwise, unknown associated decl kind.
    }

    // Set the interface type.
    auto interfaceType = getType(interfaceTypeID);
    fn->setInterfaceType(interfaceType);

    SmallVector<ParameterList*, 2> paramLists;
    for (unsigned i = 0, e = numParamPatterns; i != e; ++i)
      paramLists.push_back(readParameterList());

    // If the first parameter list is (self), mark it implicit.
    if (numParamPatterns && DC->isTypeContext())
      paramLists[0]->get(0)->setImplicit();
    
    fn->setDeserializedSignature(paramLists, TypeLoc());

    if (auto errorConvention = maybeReadForeignErrorConvention())
      fn->setForeignErrorConvention(*errorConvention);

    if (auto overridden = cast_or_null<FuncDecl>(getDecl(overriddenID))) {
      fn->setOverriddenDecl(overridden);
      AddAttribute(new (ctx) OverrideAttr(SourceLoc()));
    }

    fn->setStatic(isStatic);
    if (isImplicit)
      fn->setImplicit();
    fn->setMutating(isMutating);
    fn->setDynamicSelf(hasDynamicSelf);

    // If we are an accessor on a var or subscript, make sure it is deserialized
    // too.
    getDecl(accessorStorageDeclID);
    break;
  }

  case decls_block::PATTERN_BINDING_DECL: {
    DeclContextID contextID;
    bool isImplicit;
    bool isStatic;
    uint8_t RawStaticSpelling;
    unsigned numPatterns;
    ArrayRef<uint64_t> initContextIDs;

    decls_block::PatternBindingLayout::readRecord(scratch, contextID,
                                                  isImplicit,
                                                  isStatic,
                                                  RawStaticSpelling,
                                                  numPatterns,
                                                  initContextIDs);
    auto StaticSpelling = getActualStaticSpellingKind(RawStaticSpelling);
    if (!StaticSpelling.hasValue()) {
      error();
      return nullptr;
    }

    auto dc = getDeclContext(contextID);
    auto binding =
      PatternBindingDecl::createDeserialized(ctx, SourceLoc(),
                                             StaticSpelling.getValue(),
                                             SourceLoc(), numPatterns, dc);
    binding->setEarlyAttrValidation(true);
    declOrOffset = binding;

    binding->setStatic(isStatic);

    if (isImplicit)
      binding->setImplicit();

    for (unsigned i = 0; i != numPatterns; ++i) {
      auto pattern = maybeReadPattern(dc);
      assert(pattern);
      DeclContext *initContext = nullptr;
      if (!initContextIDs.empty())
        initContext = getDeclContext(initContextIDs[i]);
      binding->setPattern(i, pattern, initContext);
    }

    break;
  }

  case decls_block::PROTOCOL_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    bool isImplicit, isClassBounded, isObjC;
    GenericEnvironmentID genericEnvID;
    uint8_t rawAccessLevel;
    ArrayRef<uint64_t> rawInheritedIDs;

    decls_block::ProtocolLayout::readRecord(scratch, nameID, contextID,
                                            isImplicit, isClassBounded, isObjC,
                                            genericEnvID, rawAccessLevel,
                                            rawInheritedIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto proto = createDecl<ProtocolDecl>(DC, SourceLoc(), SourceLoc(),
                                          getIdentifier(nameID), None,
                                          /*TrailingWhere=*/nullptr);
    declOrOffset = proto;

    proto->setRequiresClass(isClassBounded);
    
    if (auto accessLevel = getActualAccessibility(rawAccessLevel)) {
      proto->setAccessibility(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    auto genericParams = maybeReadGenericParams(DC);
    assert(genericParams && "protocol with no generic parameters?");
    proto->setGenericParams(genericParams);

    handleInherited(proto, rawInheritedIDs);

    configureGenericEnvironment(proto, genericEnvID);

    SmallVector<Requirement, 4> requirements;
    readGenericRequirements(requirements, DeclTypeCursor);

    if (isImplicit)
      proto->setImplicit();
    proto->computeType();

    auto signature = GenericSignature::get(
                               { proto->getProtocolSelfType() }, requirements);
    proto->setRequirementSignature(signature);

    proto->setMemberLoader(this, DeclTypeCursor.GetCurrentBitNo());
    proto->setCircularityCheck(CircularityCheck::Checked);
    break;
  }

  case decls_block::PREFIX_OPERATOR_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;

    decls_block::PrefixOperatorLayout::readRecord(scratch, nameID,
                                                  contextID);
    auto DC = getDeclContext(contextID);
    declOrOffset = createDecl<PrefixOperatorDecl>(DC, SourceLoc(),
                                                  getIdentifier(nameID),
                                                  SourceLoc());
    break;
  }

  case decls_block::POSTFIX_OPERATOR_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;

    decls_block::PostfixOperatorLayout::readRecord(scratch, nameID,
                                                   contextID);

    auto DC = getDeclContext(contextID);
    declOrOffset = createDecl<PostfixOperatorDecl>(DC, SourceLoc(),
                                                   getIdentifier(nameID),
                                                   SourceLoc());
    break;
  }

  case decls_block::INFIX_OPERATOR_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    DeclID precedenceGroupID;

    decls_block::InfixOperatorLayout::readRecord(scratch, nameID, contextID,
                                                 precedenceGroupID);

    PrecedenceGroupDecl *precedenceGroup = nullptr;
    Identifier precedenceGroupName;
    if (precedenceGroupID) {
      precedenceGroup =
        dyn_cast_or_null<PrecedenceGroupDecl>(getDecl(precedenceGroupID));
      if (precedenceGroup) {
        precedenceGroupName = precedenceGroup->getName();
      }
    }

    auto DC = getDeclContext(contextID);

    auto result = createDecl<InfixOperatorDecl>(DC, SourceLoc(),
                                                 getIdentifier(nameID),
                                                 SourceLoc(), SourceLoc(),
                                                 precedenceGroupName,
                                                 SourceLoc());
    result->setPrecedenceGroup(precedenceGroup);

    declOrOffset = result;
    break;
  }

  case decls_block::PRECEDENCE_GROUP_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    uint8_t rawAssociativity;
    bool assignment;
    unsigned numHigherThan;
    ArrayRef<uint64_t> rawRelations;

    decls_block::PrecedenceGroupLayout::readRecord(scratch, nameID, contextID,
                                                   rawAssociativity,
                                                   assignment, numHigherThan,
                                                   rawRelations);

    auto DC = getDeclContext(contextID);

    auto associativity = getActualAssociativity(rawAssociativity);
    if (!associativity.hasValue()) {
      error();
      return nullptr;
    }

    if (numHigherThan > rawRelations.size()) {
      error();
      return nullptr;
    }

    SmallVector<PrecedenceGroupDecl::Relation, 4> higherThan;
    for (auto relID : rawRelations.slice(0, numHigherThan)) {
      PrecedenceGroupDecl *rel = nullptr;
      if (relID)
        rel = dyn_cast_or_null<PrecedenceGroupDecl>(getDecl(relID));
      if (!rel) {
        error();
        return nullptr;
      }

      higherThan.push_back({SourceLoc(), rel->getName(), rel});
    }

    SmallVector<PrecedenceGroupDecl::Relation, 4> lowerThan;
    for (auto relID : rawRelations.slice(numHigherThan)) {
      PrecedenceGroupDecl *rel = nullptr;
      if (relID)
        rel = dyn_cast_or_null<PrecedenceGroupDecl>(getDecl(relID));
      if (!rel) {
        error();
        return nullptr;
      }

      lowerThan.push_back({SourceLoc(), rel->getName(), rel});
    }

    declOrOffset = PrecedenceGroupDecl::create(DC, SourceLoc(), SourceLoc(),
                                               getIdentifier(nameID),
                                               SourceLoc(),
                                               SourceLoc(), SourceLoc(),
                                               *associativity,
                                               SourceLoc(), SourceLoc(),
                                               assignment,
                                               SourceLoc(), higherThan,
                                               SourceLoc(), lowerThan,
                                               SourceLoc());
    break;
  }

  case decls_block::CLASS_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    bool isImplicit, isObjC, requiresStoredPropertyInits;
    GenericEnvironmentID genericEnvID;
    TypeID superclassID;
    uint8_t rawAccessLevel;
    unsigned numConformances;
    ArrayRef<uint64_t> rawInheritedIDs;
    decls_block::ClassLayout::readRecord(scratch, nameID, contextID,
                                         isImplicit, isObjC,
                                         requiresStoredPropertyInits,
                                         genericEnvID, superclassID,
                                         rawAccessLevel, numConformances,
                                         rawInheritedIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto genericParams = maybeReadGenericParams(DC);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto theClass = createDecl<ClassDecl>(SourceLoc(), getIdentifier(nameID),
                                          SourceLoc(), None, genericParams, DC);
    declOrOffset = theClass;

    configureGenericEnvironment(theClass, genericEnvID);

    if (auto accessLevel = getActualAccessibility(rawAccessLevel)) {
      theClass->setAccessibility(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    theClass->setAddedImplicitInitializers();
    if (isImplicit)
      theClass->setImplicit();
    theClass->setSuperclass(getType(superclassID));
    if (requiresStoredPropertyInits)
      theClass->setRequiresStoredPropertyInits(true);

    theClass->computeType();

    handleInherited(theClass, rawInheritedIDs);

    theClass->setMemberLoader(this, DeclTypeCursor.GetCurrentBitNo());
    theClass->setHasDestructor();
    skipRecord(DeclTypeCursor, decls_block::MEMBERS);
    theClass->setConformanceLoader(
      this,
      encodeLazyConformanceContextData(numConformances,
                                       DeclTypeCursor.GetCurrentBitNo()));

    theClass->setCircularityCheck(CircularityCheck::Checked);
    break;
  }

  case decls_block::ENUM_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    bool isImplicit;
    GenericEnvironmentID genericEnvID;
    TypeID rawTypeID;
    uint8_t rawAccessLevel;
    unsigned numConformances;
    ArrayRef<uint64_t> rawInheritedIDs;

    decls_block::EnumLayout::readRecord(scratch, nameID, contextID,
                                        isImplicit, genericEnvID, rawTypeID,
                                        rawAccessLevel,
                                        numConformances, rawInheritedIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto genericParams = maybeReadGenericParams(DC);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto theEnum = createDecl<EnumDecl>(SourceLoc(), getIdentifier(nameID),
                                        SourceLoc(), None, genericParams, DC);

    declOrOffset = theEnum;

    configureGenericEnvironment(theEnum, genericEnvID);

    if (auto accessLevel = getActualAccessibility(rawAccessLevel)) {
      theEnum->setAccessibility(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    if (isImplicit)
      theEnum->setImplicit();
    theEnum->setRawType(getType(rawTypeID));

    theEnum->computeType();

    handleInherited(theEnum, rawInheritedIDs);

    theEnum->setMemberLoader(this, DeclTypeCursor.GetCurrentBitNo());
    skipRecord(DeclTypeCursor, decls_block::MEMBERS);
    theEnum->setConformanceLoader(
      this,
      encodeLazyConformanceContextData(numConformances,
                                       DeclTypeCursor.GetCurrentBitNo()));
    break;
  }

  case decls_block::ENUM_ELEMENT_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    TypeID interfaceTypeID;
    bool hasArgumentType;
    bool isImplicit; bool isNegative;
    unsigned rawValueKindID;

    decls_block::EnumElementLayout::readRecord(scratch, nameID,
                                               contextID,
                                               interfaceTypeID,
                                               hasArgumentType,
                                               isImplicit, rawValueKindID,
                                               isNegative);

    DeclContext *DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto elem = createDecl<EnumElementDecl>(SourceLoc(),
                                            getIdentifier(nameID),
                                            TypeLoc(),
                                            hasArgumentType,
                                            SourceLoc(),
                                            nullptr,
                                            DC);
    declOrOffset = elem;
    
    // Deserialize the literal raw value, if any.
    switch ((EnumElementRawValueKind)rawValueKindID) {
    case EnumElementRawValueKind::None:
      break;
    case EnumElementRawValueKind::IntegerLiteral: {
      auto literalText = getContext().AllocateCopy(blobData);
      auto literal = new (getContext()) IntegerLiteralExpr(literalText,
                                                           SourceLoc(),
                                                           /*implicit*/ true);
      if (isNegative)
        literal->setNegative(SourceLoc());
      elem->setRawValueExpr(literal);
    }
    }

    auto interfaceType = getType(interfaceTypeID);
    elem->setInterfaceType(interfaceType);

    if (isImplicit)
      elem->setImplicit();
    elem->setAccessibility(std::max(cast<EnumDecl>(DC)->getFormalAccess(),
                                    Accessibility::Internal));

    break;
  }

  case decls_block::SUBSCRIPT_DECL: {
    DeclContextID contextID;
    bool isImplicit, isObjC;
    GenericEnvironmentID genericEnvID;
    TypeID interfaceTypeID;
    DeclID getterID, setterID, materializeForSetID;
    DeclID addressorID, mutableAddressorID, willSetID, didSetID;
    DeclID overriddenID;
    uint8_t rawAccessLevel, rawSetterAccessLevel;
    uint8_t rawStorageKind;
    ArrayRef<uint64_t> argNameIDs;

    decls_block::SubscriptLayout::readRecord(scratch, contextID,
                                             isImplicit, isObjC, rawStorageKind,
                                             genericEnvID,
                                             interfaceTypeID,
                                             getterID, setterID,
                                             materializeForSetID,
                                             addressorID, mutableAddressorID,
                                             willSetID, didSetID,
                                             overriddenID, rawAccessLevel,
                                             rawSetterAccessLevel,
                                             argNameIDs);
    auto parent = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto *genericParams = maybeReadGenericParams(parent);
    if (declOrOffset.isComplete())
      return declOrOffset;

    // Resolve the name ids.
    SmallVector<Identifier, 2> argNames;
    for (auto argNameID : argNameIDs)
      argNames.push_back(getIdentifier(argNameID));

    DeclName name(ctx, ctx.Id_subscript, argNames);
    auto subscript = createDecl<SubscriptDecl>(name, SourceLoc(), nullptr,
                                               SourceLoc(), TypeLoc(),
                                               parent, genericParams);
    declOrOffset = subscript;

    configureGenericEnvironment(subscript, genericEnvID);

    subscript->setIndices(readParameterList());

    configureStorage(subscript, rawStorageKind,
                     getterID, setterID, materializeForSetID,
                     addressorID, mutableAddressorID, willSetID, didSetID);

    if (auto accessLevel = getActualAccessibility(rawAccessLevel)) {
      subscript->setAccessibility(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    if (subscript->isSettable()) {
      if (auto setterAccess = getActualAccessibility(rawSetterAccessLevel)) {
        subscript->setSetterAccessibility(*setterAccess);
      } else {
        error();
        return nullptr;
      }
    }

    auto interfaceType = getType(interfaceTypeID);
    subscript->setInterfaceType(interfaceType);

    if (isImplicit)
      subscript->setImplicit();
    if (auto overridden = cast_or_null<SubscriptDecl>(getDecl(overriddenID))) {
      subscript->setOverriddenDecl(overridden);
      AddAttribute(new (ctx) OverrideAttr(SourceLoc()));
    }
    break;
  }

  case decls_block::EXTENSION_DECL: {
    TypeID baseID;
    DeclContextID contextID;
    bool isImplicit;
    GenericEnvironmentID genericEnvID;
    unsigned numConformances;
    ArrayRef<uint64_t> rawInheritedIDs;

    decls_block::ExtensionLayout::readRecord(scratch, baseID, contextID,
                                             isImplicit, genericEnvID,
                                             numConformances, rawInheritedIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto extension = ExtensionDecl::create(ctx, SourceLoc(), TypeLoc(), { },
                                           DC, nullptr);
    extension->setEarlyAttrValidation();
    declOrOffset = extension;

    // Generic parameter lists are written from outermost to innermost.
    // Keep reading until we run out of generic parameter lists.
    GenericParamList *outerParams = nullptr;
    while (auto *genericParams = maybeReadGenericParams(DC, outerParams))
      outerParams = genericParams;
    extension->setGenericParams(outerParams);

    configureGenericEnvironment(extension, genericEnvID);

    auto baseTy = getType(baseID);
    auto nominal = baseTy->getAnyNominal();
    extension->getExtendedTypeLoc().setType(baseTy);

    if (isImplicit)
      extension->setImplicit();

    auto inheritedTypes = ctx.Allocate<TypeLoc>(rawInheritedIDs.size());
    for_each(inheritedTypes, rawInheritedIDs,
             [this](TypeLoc &tl, uint64_t rawID) {
      tl = TypeLoc::withoutLoc(getType(rawID));
    });
    extension->setInherited(inheritedTypes);
    extension->setCheckedInheritanceClause();

    extension->setMemberLoader(this, DeclTypeCursor.GetCurrentBitNo());
    skipRecord(DeclTypeCursor, decls_block::MEMBERS);
    extension->setConformanceLoader(
      this,
      encodeLazyConformanceContextData(numConformances,
                                       DeclTypeCursor.GetCurrentBitNo()));

    nominal->addExtension(extension);

#ifndef NDEBUG
    if (outerParams) {
      unsigned paramCount = 0;
      for (auto *paramList = outerParams;
           paramList != nullptr;
           paramList = paramList->getOuterParameters()) {
        paramCount += paramList->size();
      }
      assert(paramCount ==
             extension->getGenericSignature()->getGenericParams().size());
    }
#endif

    break;
  }

  case decls_block::DESTRUCTOR_DECL: {
    DeclContextID contextID;
    bool isImplicit, isObjC;
    GenericEnvironmentID genericEnvID;
    TypeID interfaceID;

    decls_block::DestructorLayout::readRecord(scratch, contextID,
                                              isImplicit, isObjC,
                                              genericEnvID,
                                              interfaceID);

    DeclContext *DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto dtor = createDecl<DestructorDecl>(ctx.Id_deinit, SourceLoc(),
                                           /*selfpat*/nullptr, DC);
    declOrOffset = dtor;

    configureGenericEnvironment(dtor, genericEnvID);

    dtor->setAccessibility(std::max(cast<ClassDecl>(DC)->getFormalAccess(),
                                    Accessibility::Internal));
    auto *selfParams = readParameterList();
    selfParams->get(0)->setImplicit();  // self is implicit.

    assert(selfParams && "Didn't get self pattern?");
    dtor->setSelfDecl(selfParams->get(0));

    auto interfaceType = getType(interfaceID);
    dtor->setInterfaceType(interfaceType);

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
    declOrOffset.get()->getAttrs().setRawAttributeChain(DAttrs);

  auto decl = declOrOffset.get();
  decl->setValidationStarted();
  return decl;
}

/// Translate from the Serialization function type repr enum values to the AST
/// strongly-typed enum.
///
/// The former is guaranteed to be stable, but may not reflect this version of
/// the AST.
static Optional<swift::FunctionType::Representation>
getActualFunctionTypeRepresentation(uint8_t rep) {
  switch (rep) {
#define CASE(THE_CC) \
  case (uint8_t)serialization::FunctionTypeRepresentation::THE_CC: \
    return swift::FunctionType::Representation::THE_CC;
  CASE(Swift)
  CASE(Block)
  CASE(Thin)
  CASE(CFunctionPointer)
#undef CASE
  default:
    return None;
  }
}

/// Translate from the Serialization function type repr enum values to the AST
/// strongly-typed enum.
///
/// The former is guaranteed to be stable, but may not reflect this version of
/// the AST.
static Optional<swift::SILFunctionType::Representation>
getActualSILFunctionTypeRepresentation(uint8_t rep) {
  switch (rep) {
#define CASE(THE_CC) \
  case (uint8_t)serialization::SILFunctionTypeRepresentation::THE_CC: \
    return swift::SILFunctionType::Representation::THE_CC;
  CASE(Thick)
  CASE(Block)
  CASE(Thin)
  CASE(CFunctionPointer)
  CASE(Method)
  CASE(ObjCMethod)
  CASE(WitnessMethod)
#undef CASE
  default:
    return None;
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
  return None;
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
  CASE(Indirect_Inout)
  CASE(Indirect_InoutAliasable)
  CASE(Indirect_In_Guaranteed)
  CASE(Direct_Owned)
  CASE(Direct_Unowned)
  CASE(Direct_Guaranteed)
#undef CASE
  }
  return None;
}

/// Translate from the serialization ResultConvention enumerators,
/// which are guaranteed to be stable, to the AST ones.
static
Optional<swift::ResultConvention> getActualResultConvention(uint8_t raw) {
  switch (serialization::ResultConvention(raw)) {
#define CASE(ID) \
  case serialization::ResultConvention::ID: return swift::ResultConvention::ID;
  CASE(Indirect)
  CASE(Owned)
  CASE(Unowned)
  CASE(UnownedInnerPointer)
  CASE(Autoreleased)
#undef CASE
  }
  return None;
}

Type ModuleFile::getType(TypeID TID) {
  if (TID == 0)
    return Type();

  assert(TID <= Types.size() && "invalid type ID");
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

    typeOrOffset = alias->getDeclaredInterfaceType();
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
    bool isVariadic, isAutoClosure, isEscaping;
    decls_block::ParenTypeLayout::readRecord(scratch, underlyingID, isVariadic,
                                             isAutoClosure, isEscaping);
    typeOrOffset = ParenType::get(
        ctx, getType(underlyingID),
        ParameterTypeFlags(isVariadic, isAutoClosure, isEscaping));
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
      bool isVariadic, isAutoClosure, isEscaping;
      decls_block::TupleTypeEltLayout::readRecord(
          scratch, nameID, typeID, isVariadic, isAutoClosure, isEscaping);

      elements.emplace_back(
          getType(typeID), getIdentifier(nameID),
          ParameterTypeFlags(isVariadic, isAutoClosure, isEscaping));
    }

    typeOrOffset = TupleType::get(elements, ctx);
    break;
  }

  case decls_block::FUNCTION_TYPE: {
    TypeID inputID;
    TypeID resultID;
    uint8_t rawRepresentation;
    bool autoClosure, noescape, throws;

    decls_block::FunctionTypeLayout::readRecord(scratch, inputID, resultID,
                                                rawRepresentation,
                                                autoClosure,
                                                noescape,
                                                throws);
    auto representation = getActualFunctionTypeRepresentation(rawRepresentation);
    if (!representation.hasValue()) {
      error();
      return nullptr;
    }
    
    auto Info = FunctionType::ExtInfo(*representation,
                               autoClosure, noescape,
                               throws);
    
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
    GenericEnvironmentID envID;
    TypeID interfaceTypeID;

    decls_block::ArchetypeTypeLayout::readRecord(scratch, envID,
                                                 interfaceTypeID);

    auto env = getGenericEnvironment(envID);
    if (!env) {
      error();
      break;
    }

    Type interfaceType = getType(interfaceTypeID);
    Type contextType = env->mapTypeIntoContext(interfaceType);
    typeOrOffset = contextType;

    if (contextType->hasError()) {
      error();
      break;
    }

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

      typeOrOffset = genericParam->getDeclaredInterfaceType();
      break;
    }

    typeOrOffset = GenericTypeParamType::get(declIDOrDepth,indexPlusOne-1,ctx);
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

  case decls_block::DEPENDENT_MEMBER_TYPE: {
    TypeID baseID;
    DeclID assocTypeID;

    decls_block::DependentMemberTypeLayout::readRecord(scratch, baseID,
                                                       assocTypeID);
    typeOrOffset = DependentMemberType::get(
                     getType(baseID),
                     cast<AssociatedTypeDecl>(getDecl(assocTypeID)));
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

    SmallVector<Type, 8> genericArgs;
    for (TypeID type : rawArgumentIDs)
      genericArgs.push_back(getType(type));

    auto boundTy = BoundGenericType::get(nominal, parentTy, genericArgs);
    typeOrOffset = boundTy;
    break;
  }

  case decls_block::GENERIC_FUNCTION_TYPE: {
    TypeID inputID;
    TypeID resultID;
    uint8_t rawRep;
    bool throws = false;
    ArrayRef<uint64_t> genericParamIDs;

    decls_block::GenericFunctionTypeLayout::readRecord(scratch,
                                                       inputID,
                                                       resultID,
                                                       rawRep,
                                                       throws,
                                                       genericParamIDs);
    auto rep = getActualFunctionTypeRepresentation(rawRep);
    if (!rep.hasValue()) {
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
    readGenericRequirements(requirements, DeclTypeCursor);
    auto info = GenericFunctionType::ExtInfo(*rep, throws);

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

  case decls_block::SIL_BOX_TYPE: {
    SILLayoutID layoutID;

    decls_block::SILBoxTypeLayout::readRecord(scratch, layoutID);
    
    // Get the layout.
    auto getLayout = [&]() -> SILLayout * {
      assert(layoutID > 0 && layoutID <= SILLayouts.size()
             && "invalid layout ID");

      auto &layoutOrOffset = SILLayouts[layoutID - 1];
      if (layoutOrOffset.isComplete()) {
        return layoutOrOffset;
      }
      
      BCOffsetRAII saveOffset(DeclTypeCursor);
      DeclTypeCursor.JumpToBit(layoutOrOffset);
      auto layout = readSILLayout(DeclTypeCursor);
      if (!layout) {
        error();
        return nullptr;
      }
      layoutOrOffset = layout;
      return layout;
    };
    
    auto layout = getLayout();
    if (!layout)
      return nullptr;
    
    SmallVector<Substitution, 4> genericArgs;
    if (auto sig = layout->getGenericSignature()) {
      for (unsigned i : range(sig->getSubstitutionListSize())) {
        (void)i;
        auto sub = maybeReadSubstitution(DeclTypeCursor);
        if (!sub) {
          error();
          return nullptr;
        }

        genericArgs.push_back(
          Substitution(sub->getReplacement()->getCanonicalType(),
                       sub->getConformances()));
      }
    }

    typeOrOffset = SILBoxType::get(getContext(), layout, genericArgs);
    break;
  }
      
  case decls_block::SIL_FUNCTION_TYPE: {
    uint8_t rawCalleeConvention;
    uint8_t rawRepresentation;
    bool pseudogeneric = false;
    bool hasErrorResult;
    unsigned numParams;
    unsigned numResults;
    ArrayRef<uint64_t> variableData;

    decls_block::SILFunctionTypeLayout::readRecord(scratch,
                                             rawCalleeConvention,
                                             rawRepresentation,
                                             pseudogeneric,
                                             hasErrorResult,
                                             numParams,
                                             numResults,
                                             variableData);

    // Process the ExtInfo.
    auto representation
      = getActualSILFunctionTypeRepresentation(rawRepresentation);
    if (!representation.hasValue()) {
      error();
      return nullptr;
    }
    SILFunctionType::ExtInfo extInfo(*representation, pseudogeneric);

    // Process the callee convention.
    auto calleeConvention = getActualParameterConvention(rawCalleeConvention);
    if (!calleeConvention.hasValue()) {
      error();
      return nullptr;
    }

    auto processParameter = [&](TypeID typeID, uint64_t rawConvention)
                                  -> Optional<SILParameterInfo> {
      auto convention = getActualParameterConvention(rawConvention);
      auto type = getType(typeID);
      if (!convention || !type) return None;
      return SILParameterInfo(type->getCanonicalType(), *convention);
    };

    auto processResult = [&](TypeID typeID, uint64_t rawConvention)
                               -> Optional<SILResultInfo> {
      auto convention = getActualResultConvention(rawConvention);
      auto type = getType(typeID);
      if (!convention || !type) return None;
      return SILResultInfo(type->getCanonicalType(), *convention);
    };

    // Bounds check.  FIXME: overflow
    if (2 * numParams + 2 * numResults + 2 * unsigned(hasErrorResult)
          > variableData.size()) {
      error();
      return nullptr;
    }

    unsigned nextVariableDataIndex = 0;

    // Process the parameters.
    SmallVector<SILParameterInfo, 8> allParams;
    allParams.reserve(numParams);
    for (unsigned i = 0; i != numParams; ++i) {
      auto typeID = variableData[nextVariableDataIndex++];
      auto rawConvention = variableData[nextVariableDataIndex++];
      auto param = processParameter(typeID, rawConvention);
      if (!param) {
        error();
        return nullptr;
      }
      allParams.push_back(*param);
    }

    // Process the results.
    SmallVector<SILResultInfo, 8> allResults;
    allParams.reserve(numResults);
    for (unsigned i = 0; i != numResults; ++i) {
      auto typeID = variableData[nextVariableDataIndex++];
      auto rawConvention = variableData[nextVariableDataIndex++];
      auto result = processResult(typeID, rawConvention);
      if (!result) {
        error();
        return nullptr;
      }
      allResults.push_back(*result);
    }

    // Process the error result.
    Optional<SILResultInfo> errorResult;
    if (hasErrorResult) {
      auto typeID = variableData[nextVariableDataIndex++];
      auto rawConvention = variableData[nextVariableDataIndex++];
      errorResult = processResult(typeID, rawConvention);
      if (!errorResult) {
        error();
        return nullptr;
      }
    }

    // Process the generic signature parameters.
    SmallVector<GenericTypeParamType *, 8> genericParamTypes;
    for (auto id : variableData.slice(nextVariableDataIndex)) {
      genericParamTypes.push_back(
                  cast<GenericTypeParamType>(getType(id)->getCanonicalType()));
    }

    // Read the generic requirements, if any.
    SmallVector<Requirement, 4> requirements;
    readGenericRequirements(requirements, DeclTypeCursor);

    GenericSignature *genericSig = nullptr;
    if (!genericParamTypes.empty() || !requirements.empty())
      genericSig = GenericSignature::get(genericParamTypes, requirements,
                                         /*isKnownCanonical=*/true);

    typeOrOffset = SILFunctionType::get(genericSig, extInfo,
                                        calleeConvention.getValue(),
                                        allParams, allResults, errorResult,
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

  case decls_block::DICTIONARY_TYPE: {
    TypeID keyID, valueID;
    decls_block::DictionaryTypeLayout::readRecord(scratch, keyID, valueID);

    auto dictTy = DictionaryType::get(getType(keyID), getType(valueID));
    typeOrOffset = dictTy;
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

#ifndef NDEBUG
  PrettyStackTraceType trace(ctx, "deserializing", typeOrOffset.get());
  if (typeOrOffset.get()->hasError()) {
    typeOrOffset.get()->dump();
    llvm_unreachable("deserialization produced an invalid type "
                     "(rdar://problem/30382791)");
  }
#endif

  // Invoke the callback on the deserialized type.
  DeserializedTypeCallback(typeOrOffset);

  return typeOrOffset;
}

void ModuleFile::loadAllMembers(Decl *D, uint64_t contextData) {
  PrettyStackTraceDecl trace("loading members for", D);
  ++NumMemberListsLoaded;

  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(contextData);
  SmallVector<Decl *, 16> members;
  bool Err = readMembers(members);
  assert(!Err && "unable to read members");
  (void)Err;

  IterableDeclContext *IDC;
  if (auto *nominal = dyn_cast<NominalTypeDecl>(D))
    IDC = nominal;
  else
    IDC = cast<ExtensionDecl>(D);

  for (auto member : members)
    IDC->addMember(member);

  if (auto *proto = dyn_cast<ProtocolDecl>(D)) {
    PrettyStackTraceDecl trace("reading default witness table for", D);
    bool Err = readDefaultWitnessTable(proto);
    assert(!Err && "unable to read default witness table");
    (void)Err;
  }
}

void
ModuleFile::loadAllConformances(const Decl *D, uint64_t contextData,
                          SmallVectorImpl<ProtocolConformance*> &conformances) {
  PrettyStackTraceDecl trace("loading conformances for", D);

  uint64_t numConformances;
  uint64_t bitPosition;
  std::tie(numConformances, bitPosition)
    = decodeLazyConformanceContextData(contextData);

  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(bitPosition);

  while (numConformances--) {
    auto conf = readConformance(DeclTypeCursor);
    if (conf.isConcrete())
      conformances.push_back(conf.getConcrete());
  }
}

TypeLoc
ModuleFile::loadAssociatedTypeDefault(const swift::AssociatedTypeDecl *ATD,
                                      uint64_t contextData) {
  return TypeLoc::withoutLoc(getType(contextData));
}

void ModuleFile::finishNormalConformance(NormalProtocolConformance *conformance,
                                         uint64_t contextData) {
  using namespace decls_block;

  // Find the conformance record.
  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(contextData);
  auto entry = DeclTypeCursor.advance();
  assert(entry.Kind == llvm::BitstreamEntry::Record &&
         "registered lazy loader incorrectly");

  DeclID protoID;
  DeclContextID contextID;
  unsigned valueCount, typeCount, inheritedCount;
  ArrayRef<uint64_t> rawIDs;
  SmallVector<uint64_t, 16> scratch;

  unsigned kind = DeclTypeCursor.readRecord(entry.ID, scratch);
  (void) kind;
  assert(kind == NORMAL_PROTOCOL_CONFORMANCE &&
         "registered lazy loader incorrectly");
  NormalProtocolConformanceLayout::readRecord(scratch, protoID,
                                              contextID, valueCount,
                                              typeCount, inheritedCount,
                                              rawIDs);

  // Skip requirement signature conformances.
  auto proto = conformance->getProtocol();
  for (auto req : proto->getRequirementSignature()->getRequirements()) {
    if (req.getKind() == RequirementKind::Conformance) {
      (void)readConformance(DeclTypeCursor);
    }
  }

  // Skip trailing inherited conformances.
  while (inheritedCount--)
    (void)readConformance(DeclTypeCursor);

  ArrayRef<uint64_t>::iterator rawIDIter = rawIDs.begin();

  while (valueCount--) {
    auto req = cast<ValueDecl>(getDecl(*rawIDIter++));
    auto witness = cast_or_null<ValueDecl>(getDecl(*rawIDIter++));
    assert(witness ||
           req->getAttrs().hasAttribute<OptionalAttr>() ||
           req->getAttrs().isUnavailable(getContext()));
    if (!witness) {
      conformance->setWitness(req, Witness());
      continue;
    }

    // Generic signature and environment.
    GenericSignature *syntheticSig = nullptr;
    GenericEnvironment *syntheticEnv = nullptr;

    // Requirement -> synthetic map.
    SmallVector<Substitution, 4> reqToSyntheticSubs;
    if (unsigned numGenericParams = *rawIDIter++) {
      // Generic parameters of the synthetic environment.
      SmallVector<GenericTypeParamType *, 2> genericParams;
      while (numGenericParams--) {
        genericParams.push_back(
          getType(*rawIDIter++)->castTo<GenericTypeParamType>());
      }

      // Generic requirements of the synthetic environment.
      SmallVector<Requirement, 4> requirements;
      readGenericRequirements(requirements, DeclTypeCursor);

      // Form the generic signature for the synthetic environment.
      syntheticSig = GenericSignature::get(genericParams, requirements);

      // Create the synthetic environment.
      syntheticEnv =
        syntheticSig->createGenericEnvironment(*getAssociatedModule());

      // Requirement -> synthetic substitutions.
      if (unsigned numReqSubstitutions = *rawIDIter++) {
        while (numReqSubstitutions--) {
          auto sub = maybeReadSubstitution(DeclTypeCursor, nullptr);
          reqToSyntheticSubs.push_back(*sub);
        }
      }
    }

    // Witness substitutions.
    SmallVector<Substitution, 4> witnessSubstitutions;
    if (unsigned numWitnessSubstitutions = *rawIDIter++) {
      while (numWitnessSubstitutions--) {
        auto sub = maybeReadSubstitution(DeclTypeCursor, syntheticEnv);
        witnessSubstitutions.push_back(*sub);
      }
    }

    // Handle simple witnesses.
    if (witnessSubstitutions.empty() && !syntheticSig && !syntheticEnv &&
        reqToSyntheticSubs.empty()) {
      conformance->setWitness(req, Witness(witness));
      continue;
    }

    // Set the witness.
    conformance->setWitness(req, Witness(witness, witnessSubstitutions,
                                         syntheticEnv, reqToSyntheticSubs));
  }
  assert(rawIDIter <= rawIDs.end() && "read too much");

  TypeWitnessMap typeWitnesses;
  while (typeCount--) {
    // FIXME: We don't actually want to allocate an archetype here; we just
    // want to get an access path within the protocol.
    auto first = cast<AssociatedTypeDecl>(getDecl(*rawIDIter++));
    auto second = cast_or_null<TypeDecl>(getDecl(*rawIDIter++));
    auto third = maybeReadSubstitution(DeclTypeCursor);
    assert(third.hasValue());
    typeWitnesses[first] = std::make_pair(*third, second);
  }
  assert(rawIDIter <= rawIDs.end() && "read too much");


  // Set type witnesses.
  for (auto typeWitness : typeWitnesses) {
    conformance->setTypeWitness(typeWitness.first, typeWitness.second.first,
                                typeWitness.second.second);
  }
}

GenericEnvironment *ModuleFile::loadGenericEnvironment(const DeclContext *decl,
                                                       uint64_t contextData) {
  return getGenericEnvironment(contextData);
}

static Optional<ForeignErrorConvention::Kind>
decodeRawStableForeignErrorConventionKind(uint8_t kind) {
  switch (kind) {
  case static_cast<uint8_t>(ForeignErrorConventionKind::ZeroResult):
    return ForeignErrorConvention::ZeroResult;
  case static_cast<uint8_t>(ForeignErrorConventionKind::NonZeroResult):
    return ForeignErrorConvention::NonZeroResult;
  case static_cast<uint8_t>(ForeignErrorConventionKind::ZeroPreservedResult):
    return ForeignErrorConvention::ZeroPreservedResult;
  case static_cast<uint8_t>(ForeignErrorConventionKind::NilResult):
    return ForeignErrorConvention::NilResult;
  case static_cast<uint8_t>(ForeignErrorConventionKind::NonNilError):
    return ForeignErrorConvention::NonNilError;
  default:
    return None;
  }
}

Optional<ForeignErrorConvention> ModuleFile::maybeReadForeignErrorConvention() {
  using namespace decls_block;

  SmallVector<uint64_t, 8> scratch;

  BCOffsetRAII restoreOffset(DeclTypeCursor);

  auto next = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
  if (next.Kind != llvm::BitstreamEntry::Record)
    return None;

  unsigned recKind = DeclTypeCursor.readRecord(next.ID, scratch);
  switch (recKind) {
  case FOREIGN_ERROR_CONVENTION:
    restoreOffset.reset();
    break;

  default:
    return None;
  }

  uint8_t rawKind;
  bool isOwned;
  bool isReplaced;
  unsigned errorParameterIndex;
  TypeID errorParameterTypeID;
  TypeID resultTypeID;
  ForeignErrorConventionLayout::readRecord(scratch, rawKind,
                                           isOwned, isReplaced,
                                           errorParameterIndex,
                                           errorParameterTypeID,
                                           resultTypeID);

  ForeignErrorConvention::Kind kind;
  if (auto optKind = decodeRawStableForeignErrorConventionKind(rawKind))
    kind = *optKind;
  else {
    error();
    return None;
  }

  Type errorParameterType = getType(errorParameterTypeID);
  CanType canErrorParameterType;
  if (errorParameterType)
    canErrorParameterType = errorParameterType->getCanonicalType();

  Type resultType = getType(resultTypeID);
  CanType canResultType;
  if (resultType)
    canResultType = resultType->getCanonicalType();

  auto owned = isOwned ? ForeignErrorConvention::IsOwned
                       : ForeignErrorConvention::IsNotOwned;
  auto replaced = ForeignErrorConvention::IsReplaced_t(isOwned);
  switch (kind) {
  case ForeignErrorConvention::ZeroResult:
    return ForeignErrorConvention::getZeroResult(errorParameterIndex,
                                                 owned, replaced,
                                                 canErrorParameterType,
                                                 canResultType);

  case ForeignErrorConvention::NonZeroResult:
    return ForeignErrorConvention::getNonZeroResult(errorParameterIndex,
                                                    owned, replaced,
                                                    canErrorParameterType,
                                                    canResultType);

  case ForeignErrorConvention::ZeroPreservedResult:
    return ForeignErrorConvention::getZeroPreservedResult(errorParameterIndex,
                                                          owned, replaced,
                                                       canErrorParameterType);

  case ForeignErrorConvention::NilResult:
    return ForeignErrorConvention::getNilResult(errorParameterIndex,
                                                owned, replaced,
                                                canErrorParameterType);

  case ForeignErrorConvention::NonNilError:
    return ForeignErrorConvention::getNonNilError(errorParameterIndex,
                                                  owned, replaced,
                                                  canErrorParameterType);
  }

  llvm_unreachable("Unhandled ForeignErrorConvention in switch.");
}
