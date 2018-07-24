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

#include "DeserializationErrors.h"
#include "swift/Serialization/ModuleFile.h"
#include "swift/Serialization/ModuleFormat.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Serialization/BCReadingExtras.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "Serialization"

STATISTIC(NumDeclsLoaded, "# of decls deserialized");
STATISTIC(NumMemberListsLoaded,
          "# of nominals/extensions whose members were loaded");
STATISTIC(NumNormalProtocolConformancesLoaded,
          "# of normal protocol conformances deserialized");
STATISTIC(NumNormalProtocolConformancesCompleted,
          "# of normal protocol conformances completed");
STATISTIC(NumNestedTypeShortcuts,
          "# of same-module nested types resolved without lookup");

using namespace swift;
using namespace swift::serialization;
using llvm::Expected;

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
          os << "'" << VD->getBaseName() << "' (" << IDAndKind{VD, ID} << ")";
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

  class PrettyXRefTrace :
      public llvm::PrettyStackTraceEntry,
      public XRefTracePath {
  public:
    explicit PrettyXRefTrace(ModuleDecl &M) : XRefTracePath(M) {}

    void print(raw_ostream &os) const override {
      XRefTracePath::print(os, "\t");
    }
  };
} // end anonymous namespace

const char DeclDeserializationError::ID = '\0';
void DeclDeserializationError::anchor() {}
const char XRefError::ID = '\0';
void XRefError::anchor() {}
const char OverrideError::ID = '\0';
void OverrideError::anchor() {}
const char TypeError::ID = '\0';
void TypeError::anchor() {}
const char ExtensionError::ID = '\0';
void ExtensionError::anchor() {}

/// Skips a single record in the bitstream.
///
/// Returns true if the next entry is a record of type \p recordKind.
/// Destroys the stream position if the next entry is not a record.
static void skipRecord(llvm::BitstreamCursor &cursor, unsigned recordKind) {
  auto next = cursor.advance(AF_DontPopBlockAtEnd);
  assert(next.Kind == llvm::BitstreamEntry::Record);

#if NDEBUG
  cursor.skipRecord(next.ID);
#else
  SmallVector<uint64_t, 64> scratch;
  StringRef blobData;
  unsigned kind = cursor.readRecord(next.ID, scratch, &blobData);
  assert(kind == recordKind);
#endif
}

void ModuleFile::fatal(llvm::Error error) {
  if (FileContext) {
    getContext().Diags.diagnose(SourceLoc(), diag::serialization_fatal, Name);

    if (!CompatibilityVersion.empty()) {
      if (getContext().LangOpts.EffectiveLanguageVersion
            != CompatibilityVersion) {
        SmallString<16> effectiveVersionBuffer, compatVersionBuffer;
        {
          llvm::raw_svector_ostream out(effectiveVersionBuffer);
          out << getContext().LangOpts.EffectiveLanguageVersion;
        }
        {
          llvm::raw_svector_ostream out(compatVersionBuffer);
          out << CompatibilityVersion;
        }
        getContext().Diags.diagnose(
            SourceLoc(), diag::serialization_compatibility_version_mismatch,
            effectiveVersionBuffer, Name, compatVersionBuffer);
      }
    }
  }

  logAllUnhandledErrors(std::move(error), llvm::errs(),
                        "\n*** DESERIALIZATION FAILURE (please include this "
                        "section in any bug report) ***\n");
  abort();
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
  case serialization::DefaultArgumentKind::NilLiteral:
    return swift::DefaultArgumentKind::NilLiteral;
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

Expected<Pattern *> ModuleFile::readPattern(DeclContext *owningDC) {
  // Currently, the only case in which this function can fail (return an error)
  // is when reading a pattern for a single variable declaration.

  using namespace decls_block;

  auto readPatternUnchecked = [this](DeclContext *owningDC) -> Pattern * {
    Expected<Pattern *> deserialized = readPattern(owningDC);
    if (!deserialized) {
      fatal(deserialized.takeError());
    }
    assert(deserialized.get());
    return deserialized.get();
  };

  SmallVector<uint64_t, 8> scratch;

  BCOffsetRAII restoreOffset(DeclTypeCursor);
  auto next = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
  if (next.Kind != llvm::BitstreamEntry::Record) {
    error();
    return nullptr;
  }

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

    Pattern *subPattern = readPatternUnchecked(owningDC);

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

      Pattern *subPattern = readPatternUnchecked(owningDC);
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

    auto deserialized = getDeclChecked(varID);
    if (!deserialized) {
      // Pass through the error. It's too bad that it affects the whole pattern,
      // but that's what we get.
      return deserialized.takeError();
    }

    auto var = cast<VarDecl>(deserialized.get());
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

    Expected<Pattern *> subPattern = readPattern(owningDC);
    if (!subPattern) {
      // Pass through any errors.
      return subPattern;
    }

    auto result = new (getContext()) TypedPattern(subPattern.get(), TypeLoc(),
                                                  isImplicit);
    recordPatternType(result, getType(typeID));
    restoreOffset.reset();
    return result;
  }
  case decls_block::VAR_PATTERN: {
    bool isImplicit, isLet;
    VarPatternLayout::readRecord(scratch, isLet, isImplicit);

    Pattern *subPattern = readPatternUnchecked(owningDC);

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
    GenericSignatureID rawGenericSig;
    unsigned numFields;
    ArrayRef<uint64_t> types;
    decls_block::SILLayoutLayout::readRecord(scratch, rawGenericSig,
                                             numFields, types);
    
    SmallVector<SILField, 4> fields;
    for (auto fieldInfo : types.slice(0, numFields)) {
      bool isMutable = fieldInfo & 0x80000000U;
      auto typeId = fieldInfo & 0x7FFFFFFFU;
      fields.push_back(
        SILField(getType(typeId)->getCanonicalType(),
                 isMutable));
    }
    
    CanGenericSignature canSig;
    if (auto sig = getGenericSignature(rawGenericSig))
      canSig = sig->getCanonicalSignature();
    return SILLayout::get(getContext(), canSig, fields);
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

  if (getContext().Stats)
    getContext().Stats->getFrontendCounters().NumConformancesDeserialized++;

  unsigned kind = Cursor.readRecord(next.ID, scratch);
  switch (kind) {
  case INVALID_PROTOCOL_CONFORMANCE: {
    return ProtocolConformanceRef::forInvalid();
  }

  case ABSTRACT_PROTOCOL_CONFORMANCE: {
    DeclID protoID;
    AbstractProtocolConformanceLayout::readRecord(scratch, protoID);
    auto proto = cast<ProtocolDecl>(getDecl(protoID));
    return ProtocolConformanceRef(proto);
  }

  case SPECIALIZED_PROTOCOL_CONFORMANCE: {
    TypeID conformingTypeID;
    SubstitutionMapID substitutionMapID;
    SpecializedProtocolConformanceLayout::readRecord(scratch, conformingTypeID,
                                                     substitutionMapID);

    ASTContext &ctx = getContext();
    Type conformingType = getType(conformingTypeID);
    if (genericEnv) {
      conformingType = genericEnv->mapTypeIntoContext(conformingType);
    }

    PrettyStackTraceType trace(getAssociatedModule()->getASTContext(),
                               "reading specialized conformance for",
                               conformingType);

    auto subMap = getSubstitutionMap(substitutionMapID);

    ProtocolConformanceRef genericConformance =
      readConformance(Cursor, genericEnv);
    PrettyStackTraceDecl traceTo("... to", genericConformance.getRequirement());

    assert(genericConformance.isConcrete() && "Abstract generic conformance?");
    auto conformance =
           ctx.getSpecializedConformance(conformingType,
                                         genericConformance.getConcrete(),
                                         subMap);
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
        "are at least as new as the versions used to build", *this);
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
  unsigned valueCount, typeCount, conformanceCount;
  ArrayRef<uint64_t> rawIDs;
  SmallVector<uint64_t, 16> scratch;

  unsigned kind = DeclTypeCursor.readRecord(entry.ID, scratch);
  if (kind != NORMAL_PROTOCOL_CONFORMANCE) {
    error();
    return nullptr;
  }
  NormalProtocolConformanceLayout::readRecord(scratch, protoID,
                                              contextID, typeCount,
                                              valueCount, conformanceCount,
                                              rawIDs);

  ASTContext &ctx = getContext();
  DeclContext *dc = getDeclContext(contextID);
  assert(!isa<ClangModuleUnit>(dc->getModuleScopeContext())
         && "should not have serialized a conformance from a clang module");
  Type conformingType = dc->getDeclaredInterfaceType();
  PrettyStackTraceType trace(ctx, "reading conformance for", conformingType);

  auto proto = cast<ProtocolDecl>(getDecl(protoID));
  PrettyStackTraceDecl traceTo("... to", proto);
  ++NumNormalProtocolConformancesLoaded;

  auto conformance = ctx.getConformance(conformingType, proto, SourceLoc(), dc,
                                        ProtocolConformanceState::Incomplete);

  // Record this conformance.
  if (conformanceEntry.isComplete())
    return conformance;

  uint64_t offset = conformanceEntry;
  conformanceEntry = conformance;

  dc->getAsNominalTypeOrNominalTypeExtensionContext()
    ->registerProtocolConformance(conformance);

  // If the conformance is complete, we're done.
  if (conformance->isComplete())
    return conformance;

  conformance->setState(ProtocolConformanceState::Complete);
  conformance->setLazyLoader(this, offset);
  return conformance;
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
              genericParam->getDeclContext() == DC ||
              genericParam->getDeclContext()->isChildContextOf(DC)) &&
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

  // Don't create empty generic parameter lists.
  if (params.empty())
    return nullptr;

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
      case LayoutRequirementKind::NativeRefCountedObject:
        kind = LayoutConstraintKind::NativeRefCountedObject;
        break;
      case LayoutRequirementKind::RefCountedObject:
        kind = LayoutConstraintKind::RefCountedObject;
        break;
      case LayoutRequirementKind::Trivial:
        kind = LayoutConstraintKind::Trivial;
        break;
      case LayoutRequirementKind::TrivialOfExactSize:
        kind = LayoutConstraintKind::TrivialOfExactSize;
        break;
      case LayoutRequirementKind::TrivialOfAtMostSize:
        kind = LayoutConstraintKind::TrivialOfAtMostSize;
        break;
      case LayoutRequirementKind::Class:
        kind = LayoutConstraintKind::Class;
        break;
      case LayoutRequirementKind::NativeClass:
        kind = LayoutConstraintKind::NativeClass;
        break;
      case LayoutRequirementKind::UnknownLayout:
        kind = LayoutConstraintKind::UnknownLayout;
        break;
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
    return;
  }

  // If we have a full generic environment, it's because it happened to be
  // deserialized already. Record it directly.
  if (auto genericEnv = sigOrEnv.dyn_cast<GenericEnvironment *>()) {
    genericDecl->setGenericEnvironment(genericEnv);
    return;
  }
}

GenericSignature *ModuleFile::getGenericSignature(
                                      serialization::GenericSignatureID ID) {
  using namespace decls_block;

  // Zero is a sentinel for having no generic signature.
  if (ID == 0) return nullptr;

  assert(ID <= GenericSignatures.size() && "invalid GenericSignature ID");
  auto &sigOrOffset = GenericSignatures[ID-1];

  // If we've already deserialized this generic signature, return it.
  if (sigOrOffset.isComplete()) {
    return sigOrOffset.get();
  }

  // Read the generic signature.
  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(sigOrOffset);
  DeserializingEntityRAII deserializingEntity(*this);

  // Read the parameter types.
  SmallVector<GenericTypeParamType *, 4> paramTypes;
  StringRef blobData;
  SmallVector<uint64_t, 8> scratch;

  auto entry = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind != llvm::BitstreamEntry::Record) {
    error();
    return nullptr;
  }

  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch, &blobData);
  if (recordID != GENERIC_SIGNATURE) {
    error();
    return nullptr;
  }

  ArrayRef<uint64_t> rawParamIDs;
  GenericSignatureLayout::readRecord(scratch, rawParamIDs);

  for (unsigned i = 0, n = rawParamIDs.size(); i != n; ++i) {
    auto paramTy = getType(rawParamIDs[i])->castTo<GenericTypeParamType>();
    paramTypes.push_back(paramTy);
  }

  // Read the generic requirements.
  SmallVector<Requirement, 4> requirements;
  readGenericRequirements(requirements, DeclTypeCursor);

  // Construct the generic signature from the loaded parameters and
  // requirements.
  auto signature = GenericSignature::get(paramTypes, requirements);

  // If we've already deserialized this generic signature, return it.
  if (sigOrOffset.isComplete()) {
    return sigOrOffset.get();
  }

  sigOrOffset = signature;
  return signature;
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

  // Extract the bit offset or generic signature ID.
  uint64_t bitOffset = envOrOffset;
  GenericSignature *signature = nullptr;
  if (bitOffset & 0x01) {
    // We have a generic signature ID.
    signature = getGenericSignature(bitOffset >> 1);
  } else {
    bitOffset = bitOffset >> 1;

    // Read the generic environment.
    BCOffsetRAII restoreOffset(DeclTypeCursor);
    DeclTypeCursor.JumpToBit(bitOffset);
    DeserializingEntityRAII deserializingEntity(*this);

    SmallVector<GenericTypeParamType *, 4> paramTypes;
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
    if (recordID != SIL_GENERIC_ENVIRONMENT) {
      error();
      return result;
    }

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
    signature = GenericSignature::get(paramTypes, requirements);
  }

  // If we only want the signature, return it now.
  if (!wantEnvironment) return signature;

  // If we've already deserialized this generic environment, return it.
  if (envOrOffset.isComplete()) {
    return envOrOffset.get();
  }

  // Form the generic environment. Record it now so that deserialization of
  // the archetypes in the environment can refer to this environment.
  auto genericEnv = signature->createGenericEnvironment();
  envOrOffset = genericEnv;

  return genericEnv;
}

GenericEnvironment *ModuleFile::getGenericEnvironment(
                                       serialization::GenericEnvironmentID ID) {
  return getGenericSignatureOrEnvironment(ID, /*wantEnvironment=*/true)
           .get<GenericEnvironment *>();
}

SubstitutionMap ModuleFile::getSubstitutionMap(
                                        serialization::SubstitutionMapID id) {
  using namespace decls_block;

  // Zero is a sentinel for having an empty substitution map.
  if (id == 0) return SubstitutionMap();

  assert(id <= SubstitutionMaps.size() && "invalid SubstitutionMap ID");
  auto &substitutionsOrOffset = SubstitutionMaps[id-1];

  // If we've already deserialized this substitution map, return it.
  if (substitutionsOrOffset.isComplete()) {
    return substitutionsOrOffset.get();
  }

  // Read the substitution map.
  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(substitutionsOrOffset);
  DeserializingEntityRAII deserializingEntity(*this);

  // Read the substitution map.
  auto entry = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
  if (entry.Kind != llvm::BitstreamEntry::Record) {
    error();
    return SubstitutionMap();
  }

  StringRef blobData;
  SmallVector<uint64_t, 8> scratch;
  unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch, &blobData);
  if (recordID != SUBSTITUTION_MAP) {
    error();
    return SubstitutionMap();
  }

  GenericSignatureID genericSigID;
  uint64_t numConformances;
  ArrayRef<uint64_t> replacementTypeIDs;
  SubstitutionMapLayout::readRecord(scratch, genericSigID, numConformances,
                                    replacementTypeIDs);

  // Generic signature.
  auto genericSig = getGenericSignature(genericSigID);
  if (!genericSig) {
    error();
    return SubstitutionMap();
  }

  // Load the replacement types.
  SmallVector<Type, 4> replacementTypes;
  replacementTypes.reserve(replacementTypeIDs.size());
  for (auto typeID : replacementTypeIDs) {
    replacementTypes.push_back(getType(typeID));
  }

  // Read the conformances.
  SmallVector<ProtocolConformanceRef, 4> conformances;
  conformances.reserve(numConformances);
  for (unsigned i : range(numConformances)) {
    (void)i;
    conformances.push_back(readConformance(DeclTypeCursor));
  }

  // Form the substitution map and record it.
  auto substitutions =
    SubstitutionMap::get(genericSig, ArrayRef<Type>(replacementTypes),
                         ArrayRef<ProtocolConformanceRef>(conformances));
  substitutionsOrOffset = substitutions;
  return substitutions;
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

/// Determine whether the two modules are re-exported to the same module.
static bool reExportedToSameModule(const ModuleDecl *fromModule,
                                   const ModuleDecl *toModule) {
  auto fromClangModule
    = dyn_cast<ClangModuleUnit>(fromModule->getFiles().front());
  if (!fromClangModule)
    return false;

  auto toClangModule
  = dyn_cast<ClangModuleUnit>(toModule->getFiles().front());
  if (!toClangModule)
    return false;

  return fromClangModule->getExportedModuleName() ==
    toClangModule->getExportedModuleName();
}

/// Remove values from \p values that don't match the expected type or module.
///
/// Any of \p expectedTy, \p expectedModule, or \p expectedGenericSig can be
/// omitted, in which case any type or module is accepted. Values imported
/// from Clang can also appear in any module.
static void filterValues(Type expectedTy, ModuleDecl *expectedModule,
                         CanGenericSignature expectedGenericSig, bool isType,
                         bool inProtocolExt, bool importedFromClang,
                         bool isStatic,
                         Optional<swift::CtorInitializerKind> ctorInit,
                         SmallVectorImpl<ValueDecl *> &values) {
  CanType canTy;
  if (expectedTy)
    canTy = expectedTy->getCanonicalType();

  auto newEnd = std::remove_if(values.begin(), values.end(),
                               [=](ValueDecl *value) {
    // Ignore anything that was parsed (vs. deserialized), because a serialized
    // module cannot refer to it.
    if (value->getDeclContext()->getParentSourceFile())
      return true;

    if (isType != isa<TypeDecl>(value))
      return true;
    if (!value->hasInterfaceType())
      return true;
    if (canTy && value->getInterfaceType()->getCanonicalType() != canTy)
      return true;
    if (value->isStatic() != isStatic)
      return true;
    if (value->hasClangNode() != importedFromClang)
      return true;

    if (value->getAttrs().hasAttribute<ForbidSerializingReferenceAttr>())
      return true;

    // FIXME: Should be able to move a value from an extension in a derived
    // module to the original definition in a base module.
    if (expectedModule && !value->hasClangNode() &&
        value->getModuleContext() != expectedModule &&
        !reExportedToSameModule(value->getModuleContext(), expectedModule))
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

Expected<Decl *>
ModuleFile::resolveCrossReference(ModuleDecl *baseModule, uint32_t pathLen) {
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
    IdentifierID privateDiscriminator = 0;
    TypeID TID = 0;
    bool isType = (recordID == XREF_TYPE_PATH_PIECE);
    bool inProtocolExt = false;
    bool importedFromClang = false;
    bool isStatic = false;
    if (isType)
      XRefTypePathPieceLayout::readRecord(scratch, IID, privateDiscriminator,
                                          inProtocolExt, importedFromClang);
    else
      XRefValuePathPieceLayout::readRecord(scratch, TID, IID, inProtocolExt,
                                           importedFromClang, isStatic);

    DeclBaseName name = getDeclBaseName(IID);
    pathTrace.addValue(name);
    if (privateDiscriminator)
      pathTrace.addValue(getIdentifier(privateDiscriminator));

    Type filterTy;
    if (!isType) {
      auto maybeType = getTypeChecked(TID);
      if (!maybeType) {
        // FIXME: Don't throw away the inner error's information.
        llvm::consumeError(maybeType.takeError());
        return llvm::make_error<XRefError>("couldn't decode type",
                                           pathTrace, name);
      }
      filterTy = maybeType.get();
      pathTrace.addType(filterTy);
    }

    if (privateDiscriminator) {
      baseModule->lookupMember(values, baseModule, name,
                               getIdentifier(privateDiscriminator));
    } else {
      baseModule->lookupQualified(ModuleType::get(baseModule), name,
                                  NL_QualifiedDefault | NL_KnownNoDependency,
                                  /*typeResolver=*/nullptr, values);
    }
    filterValues(filterTy, nullptr, nullptr, isType, inProtocolExt,
                 importedFromClang, isStatic, None, values);
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

  auto getXRefDeclNameForError = [&]() -> DeclName {
    DeclName result = pathTrace.getLastName();
    while (--pathLen) {
      auto entry = DeclTypeCursor.advance(AF_DontPopBlockAtEnd);
      if (entry.Kind != llvm::BitstreamEntry::Record)
        return Identifier();

      unsigned recordID = DeclTypeCursor.readRecord(entry.ID, scratch,
                                                    &blobData);
      switch (recordID) {
      case XREF_TYPE_PATH_PIECE: {
        IdentifierID IID;
        XRefTypePathPieceLayout::readRecord(scratch, IID, None, None, None);
        result = getIdentifier(IID);
        break;
      }
      case XREF_VALUE_PATH_PIECE: {
        IdentifierID IID;
        XRefValuePathPieceLayout::readRecord(scratch, None, IID, None, None,
                                             None);
        result = getIdentifier(IID);
        break;
      }
      case XREF_INITIALIZER_PATH_PIECE:
        result = DeclBaseName::createConstructor();
        break;

      case XREF_EXTENSION_PATH_PIECE:
      case XREF_OPERATOR_OR_ACCESSOR_PATH_PIECE:
        break;

      case XREF_GENERIC_PARAM_PATH_PIECE:
        // Can't get the name without deserializing.
        result = Identifier();
        break;

      default:
        // Unknown encoding.
        return Identifier();
      }
    }
    return result;
  };

  if (values.empty()) {
    return llvm::make_error<XRefError>("top-level value not found", pathTrace,
                                       getXRefDeclNameForError());
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
      if (values.size() == 1 && isa<NominalTypeDecl>(values.front())) {
        // Fast path for nested types that avoids deserializing all
        // members of the parent type.
        IdentifierID IID;
        IdentifierID privateDiscriminator;
        bool importedFromClang = false;
        XRefTypePathPieceLayout::readRecord(scratch, IID, privateDiscriminator,
                                            /*inProtocolExt*/None,
                                            importedFromClang);
        if (privateDiscriminator)
          goto giveUpFastPath;

        Identifier memberName = getIdentifier(IID);
        pathTrace.addValue(memberName);

        llvm::PrettyStackTraceString message{
          "If you're seeing a crash here, try passing "
            "-Xfrontend -disable-serialization-nested-type-lookup-table"};

        auto *baseType = cast<NominalTypeDecl>(values.front());
        ModuleDecl *extensionModule = M;
        if (!extensionModule)
          extensionModule = baseType->getModuleContext();

        // FIXME: If 'importedFromClang' is true but 'extensionModule' is an
        // overlay module, the search below will fail and we'll fall back to
        // the slow path.

        // Fault in extensions, then ask every file in the module.
        (void)baseType->getExtensions();
        TypeDecl *nestedType = nullptr;
        for (FileUnit *file : extensionModule->getFiles()) {
          if (file == getFile())
            continue;
          nestedType = file->lookupNestedType(memberName, baseType);
          if (nestedType)
            break;
        }

        if (nestedType) {
          values.clear();
          values.push_back(nestedType);
          ++NumNestedTypeShortcuts;
          break;
        }

        pathTrace.removeLast();
      }
giveUpFastPath:
      LLVM_FALLTHROUGH;
    }
    case XREF_VALUE_PATH_PIECE:
    case XREF_INITIALIZER_PATH_PIECE: {
      TypeID TID = 0;
      DeclBaseName memberName;
      Identifier privateDiscriminator;
      Optional<swift::CtorInitializerKind> ctorInit;
      bool isType = false;
      bool inProtocolExt = false;
      bool importedFromClang = false;
      bool isStatic = false;
      switch (recordID) {
      case XREF_TYPE_PATH_PIECE: {
        IdentifierID IID, discriminatorID;
        XRefTypePathPieceLayout::readRecord(scratch, IID, discriminatorID,
                                            inProtocolExt, importedFromClang);
        memberName = getDeclBaseName(IID);
        privateDiscriminator = getIdentifier(discriminatorID);
        isType = true;
        break;
      }

      case XREF_VALUE_PATH_PIECE: {
        IdentifierID IID;
        XRefValuePathPieceLayout::readRecord(scratch, TID, IID, inProtocolExt,
                                             importedFromClang, isStatic);
        memberName = getDeclBaseName(IID);
        break;
      }

      case XREF_INITIALIZER_PATH_PIECE: {
        uint8_t kind;
        XRefInitializerPathPieceLayout::readRecord(scratch, TID, inProtocolExt,
                                                   importedFromClang, kind);
        memberName = DeclBaseName::createConstructor();
        ctorInit = getActualCtorInitializerKind(kind);
        break;
      }
        
      default:
        llvm_unreachable("Unhandled path piece");
      }

      pathTrace.addValue(memberName);
      if (!privateDiscriminator.empty())
        pathTrace.addPrivateDiscriminator(privateDiscriminator);

      Type filterTy;
      if (!isType) {
        auto maybeType = getTypeChecked(TID);
        if (!maybeType) {
          // FIXME: Don't throw away the inner error's information.
          llvm::consumeError(maybeType.takeError());
          return llvm::make_error<XRefError>("couldn't decode type",
                                             pathTrace, memberName);
        }
        filterTy = maybeType.get();
        pathTrace.addType(filterTy);
      }

      if (values.size() != 1) {
        return llvm::make_error<XRefError>("multiple matching base values",
                                           pathTrace,
                                           getXRefDeclNameForError());
      }

      auto nominal = dyn_cast<NominalTypeDecl>(values.front());
      values.clear();

      if (!nominal) {
        return llvm::make_error<XRefError>("base is not a nominal type",
                                           pathTrace,
                                           getXRefDeclNameForError());
      }

      if (!privateDiscriminator.empty()) {
        ModuleDecl *searchModule = M;
        if (!searchModule)
          searchModule = nominal->getModuleContext();
        searchModule->lookupMember(values, nominal, memberName,
                                   privateDiscriminator);

      } else {
        auto members = nominal->lookupDirect(memberName);
        values.append(members.begin(), members.end());
      }
      filterValues(filterTy, M, genericSig, isType, inProtocolExt,
                   importedFromClang, isStatic, ctorInit, values);
      break;
    }

    case XREF_EXTENSION_PATH_PIECE: {
      ModuleID ownerID;
      GenericSignatureID rawGenericSig;
      XRefExtensionPathPieceLayout::readRecord(scratch, ownerID, rawGenericSig);
      M = getModule(ownerID);
      pathTrace.addExtension(M);

      // Read the generic signature, if we have one.
      genericSig = CanGenericSignature(getGenericSignature(rawGenericSig));

      continue;
    }

    case XREF_OPERATOR_OR_ACCESSOR_PATH_PIECE: {
      uint8_t rawKind;
      XRefOperatorOrAccessorPathPieceLayout::readRecord(scratch, None,
                                                        rawKind);
      if (values.empty())
        break;

      if (!values.front()->getBaseName().isOperator()) {
        pathTrace.addAccessor(rawKind);
        if (auto storage = dyn_cast<AbstractStorageDecl>(values.front())) {
          switch (rawKind) {
          case Get:
            values.front() = storage->getGetter();
            break;
          case Set:
            values.front() = storage->getSetter();
            break;
          case MaterializeForSet:
            values.front() = storage->getMaterializeForSetFunc();
            break;
          case Address:
            values.front() = storage->getAddressor();
            break;
          case MutableAddress:
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
        }
        break;
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
        return llvm::make_error<XRefError>("multiple matching base values",
                                           pathTrace,
                                           getXRefDeclNameForError());
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
      } else if (auto fn = dyn_cast<AbstractFunctionDecl>(base)) {
        paramList = fn->getGenericParams();
      } else if (auto subscript = dyn_cast<SubscriptDecl>(base)) {
        paramList = subscript->getGenericParams();
      }

      if (!paramList) {
        return llvm::make_error<XRefError>(
            "cross-reference to generic param for non-generic type",
            pathTrace, getXRefDeclNameForError());
      }
      if (paramIndex >= paramList->size()) {
        return llvm::make_error<XRefError>(
            "generic argument index out of bounds",
            pathTrace, getXRefDeclNameForError());
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
                       *this);
    }

    if (values.empty()) {
      return llvm::make_error<XRefError>("result not found", pathTrace,
                                         getXRefDeclNameForError());
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
    return llvm::make_error<XRefError>("result is ambiguous", pathTrace,
                                       getXRefDeclNameForError());
  }

  return values.front();
}

DeclBaseName ModuleFile::getDeclBaseName(IdentifierID IID) {
  if (IID == 0)
    return Identifier();

  if (IID < NUM_SPECIAL_IDS) {
    switch (static_cast<SpecialIdentifierID>(static_cast<uint8_t>(IID))) {
    case BUILTIN_MODULE_ID:
    case CURRENT_MODULE_ID:
    case OBJC_HEADER_MODULE_ID:
        llvm_unreachable("Cannot get DeclBaseName of special module id");
    case SUBSCRIPT_ID:
      return DeclBaseName::createSubscript();
    case serialization::CONSTRUCTOR_ID:
      return DeclBaseName::createConstructor();
    case serialization::DESTRUCTOR_ID:
      return DeclBaseName::createDestructor();
    case NUM_SPECIAL_IDS:
      llvm_unreachable("implementation detail only");
    }
  }

  size_t rawID = IID - NUM_SPECIAL_IDS;
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

Identifier ModuleFile::getIdentifier(IdentifierID IID) {
  auto name = getDeclBaseName(IID);
  assert(!name.isSpecial());
  return name.getIdentifier();
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
  if (MID < NUM_SPECIAL_IDS) {
    switch (static_cast<SpecialIdentifierID>(static_cast<uint8_t>(MID))) {
    case BUILTIN_MODULE_ID:
      return getContext().TheBuiltinModule;
    case CURRENT_MODULE_ID:
      return FileContext->getParentModule();
    case OBJC_HEADER_MODULE_ID: {
      auto clangImporter =
        static_cast<ClangImporter *>(getContext().getClangModuleLoader());
      return clangImporter->getImportedHeaderModule();
    }
    case SUBSCRIPT_ID:
    case CONSTRUCTOR_ID:
    case DESTRUCTOR_ID:
      llvm_unreachable("Modules cannot be named with special names");
    case NUM_SPECIAL_IDS:
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

static Optional<swift::AccessLevel> getActualAccessLevel(uint8_t raw) {
  switch (serialization::AccessLevel(raw)) {
#define CASE(NAME) \
  case serialization::AccessLevel::NAME: \
    return swift::AccessLevel::NAME;
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

static Optional<swift::AccessorKind>
getActualAccessorKind(uint8_t raw) {
  switch (serialization::AccessorKind(raw)) {
#define ACCESSOR(ID) \
  case serialization::AccessorKind::ID: return swift::AccessorKind::ID;
#include "swift/AST/AccessorKinds.def"
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

static Optional<swift::SelfAccessKind>
getActualSelfAccessKind(uint8_t raw) {
  switch (serialization::SelfAccessKind(raw)) {
  case serialization::SelfAccessKind::NonMutating:
    return swift::SelfAccessKind::NonMutating;
  case serialization::SelfAccessKind::Mutating:
    return swift::SelfAccessKind::Mutating;
  case serialization::SelfAccessKind::__Consuming:
    return swift::SelfAccessKind::__Consuming;
  }
  return None;
}

static
Optional<swift::ResilienceExpansion> getActualResilienceExpansion(uint8_t raw) {
  switch (serialization::ResilienceExpansion(raw)) {
  case serialization::ResilienceExpansion::Minimal:
    return swift::ResilienceExpansion::Minimal;
  case serialization::ResilienceExpansion::Maximal:
    return swift::ResilienceExpansion::Maximal;
  }
  return None;
}

/// Translate from the serialization VarDeclSpecifier enumerators, which are
/// guaranteed to be stable, to the AST ones.
static Optional<swift::VarDecl::Specifier>
getActualVarDeclSpecifier(serialization::VarDeclSpecifier raw) {
  switch (raw) {
#define CASE(ID) \
  case serialization::VarDeclSpecifier::ID: \
    return swift::VarDecl::Specifier::ID;
  CASE(Let)
  CASE(Var)
  CASE(InOut)
  CASE(Shared)
  CASE(Owned)
  }
#undef CASE
  return None;
}

static Optional<swift::ReadImplKind>
getActualReadImplKind(unsigned rawKind) {
  switch (serialization::ReadImplKind(rawKind)) {
#define CASE(KIND)                        \
  case serialization::ReadImplKind::KIND: \
    return swift::ReadImplKind::KIND;
  CASE(Stored)
  CASE(Get)
  CASE(Inherited)
  CASE(Address)
#undef CASE
  }
  return None;
}

static Optional<swift::WriteImplKind>
getActualWriteImplKind(unsigned rawKind) {
  switch (serialization::WriteImplKind(rawKind)) {
#define CASE(KIND)                         \
  case serialization::WriteImplKind::KIND: \
    return swift::WriteImplKind::KIND;
  CASE(Immutable)
  CASE(Stored)
  CASE(Set)
  CASE(StoredWithObservers)
  CASE(InheritedWithObservers)
  CASE(MutableAddress)
#undef CASE
  }
  return None;
}

static Optional<swift::ReadWriteImplKind>
getActualReadWriteImplKind(unsigned rawKind) {
  switch (serialization::ReadWriteImplKind(rawKind)) {
#define CASE(KIND)                             \
  case serialization::ReadWriteImplKind::KIND: \
    return swift::ReadWriteImplKind::KIND;
  CASE(Immutable)
  CASE(Stored)
  CASE(MaterializeForSet)
  CASE(MutableAddress)
  CASE(MaterializeToTemporary)
#undef CASE
  }
  return None;
}

void ModuleFile::configureStorage(AbstractStorageDecl *decl,
                                  uint8_t rawReadImplKind,
                                  uint8_t rawWriteImplKind,
                                  uint8_t rawReadWriteImplKind,
                                  AccessorRecord &rawIDs) {
  auto readImpl = getActualReadImplKind(rawReadImplKind);
  if (!readImpl) return;

  auto writeImpl = getActualWriteImplKind(rawWriteImplKind);
  if (!writeImpl) return;

  auto readWriteImpl = getActualReadWriteImplKind(rawReadWriteImplKind);
  if (!readWriteImpl) return;

  SmallVector<AccessorDecl*, 8> accessors;
  for (DeclID id : rawIDs.IDs) {
    auto accessor = dyn_cast_or_null<AccessorDecl>(getDecl(id));
    if (!accessor) return;
    accessors.push_back(accessor);
  }

  auto implInfo = StorageImplInfo(*readImpl, *writeImpl, *readWriteImpl);
  if (implInfo.isSimpleStored() && accessors.empty())
    return;

  // We currently don't serialize these locations.
  SourceLoc beginLoc, endLoc;

  decl->setAccessors(implInfo, beginLoc, accessors, endLoc);
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
  Expected<Decl *> deserialized = getDeclChecked(DID, ForcedContext);
  if (!deserialized) {
    fatal(deserialized.takeError());
  }
  return deserialized.get();
}

Expected<Decl *>
ModuleFile::getDeclChecked(DeclID DID, Optional<DeclContext *> ForcedContext) {
  // Tag every deserialized ValueDecl coming out of getDeclChecked with its ID.
  Expected<Decl *> deserialized = getDeclCheckedImpl(DID, ForcedContext);
  if (deserialized && deserialized.get()) {
    if (auto *IDC = dyn_cast<IterableDeclContext>(deserialized.get())) {
      // Only set the DeclID on the returned Decl if it's one that was loaded
      // and _wasn't_ one that had its DeclID set elsewhere (a followed XREF).
      if (IDC->wasDeserialized() &&
          static_cast<uint32_t>(IDC->getDeclID()) == 0) {
        IDC->setDeclID(DID);
      }
    }
  }
  return deserialized;
}

Expected<Decl *>
ModuleFile::getDeclCheckedImpl(DeclID DID, Optional<DeclContext *> ForcedContext) {
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

  if (auto s = ctx.Stats)
    s->getFrontendCounters().NumDeclsDeserialized++;

  // Read the attributes (if any).
  // This isn't just using DeclAttributes because that would result in the
  // attributes getting reversed.
  // FIXME: If we reverse them at serialization time we could get rid of this.
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

      case decls_block::Optimize_DECL_ATTR: {
        unsigned kind;
        serialization::decls_block::OptimizeDeclAttrLayout::readRecord(
            scratch, kind);
        Attr = new (ctx) OptimizeAttr((OptimizationMode)kind);
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

      case decls_block::ObjC_DECL_ATTR: {
        bool isImplicit;
        bool isImplicitName;
        bool isSwift3Inferred;
        uint64_t numArgs;
        ArrayRef<uint64_t> rawPieceIDs;
        serialization::decls_block::ObjCDeclAttrLayout::readRecord(
          scratch, isImplicit, isSwift3Inferred, isImplicitName, numArgs,
          rawPieceIDs);

        SmallVector<Identifier, 4> pieces;
        for (auto pieceID : rawPieceIDs)
          pieces.push_back(getIdentifier(pieceID));

        if (numArgs == 0)
          Attr = ObjCAttr::create(ctx, None, isImplicitName);
        else
          Attr = ObjCAttr::create(ctx, ObjCSelector(ctx, numArgs-1, pieces),
                                  isImplicitName);
        Attr->setImplicit(isImplicit);
        cast<ObjCAttr>(Attr)->setSwift3Inferred(isSwift3Inferred);
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
    ArrayRef<uint64_t> dependencyIDs;

    decls_block::TypeAliasLayout::readRecord(scratch, nameID, contextID,
                                             underlyingTypeID, interfaceTypeID,
                                             isImplicit, genericEnvID,
                                             rawAccessLevel, dependencyIDs);

    Identifier name = getIdentifier(nameID);

    for (TypeID dependencyID : dependencyIDs) {
      auto dependency = getTypeChecked(dependencyID);
      if (!dependency) {
        return llvm::make_error<TypeError>(
            name, takeErrorInfo(dependency.takeError()));
      }
    }

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);

    auto genericParams = maybeReadGenericParams(DC);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto alias = createDecl<TypeAliasDecl>(SourceLoc(), SourceLoc(), name,
                                           SourceLoc(), genericParams, DC);
    declOrOffset = alias;

    configureGenericEnvironment(alias, genericEnvID);

    alias->setUnderlyingType(getType(underlyingTypeID));

    if (auto accessLevel = getActualAccessLevel(rawAccessLevel)) {
      alias->setAccess(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    if (isImplicit)
      alias->setImplicit();

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
    ArrayRef<uint64_t> rawOverriddenIDs;

    decls_block::AssociatedTypeDeclLayout::readRecord(scratch, nameID,
                                                      contextID,
                                                      defaultDefinitionID,
                                                      isImplicit,
                                                      rawOverriddenIDs);

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

    AccessLevel parentAccess = cast<ProtocolDecl>(DC)->getFormalAccess();
    assocType->setAccess(std::max(parentAccess, AccessLevel::Internal));
    if (isImplicit)
      assocType->setImplicit();

    // Overridden associated types.
    SmallVector<ValueDecl *, 2> overriddenAssocTypes;
    for (auto overriddenID : rawOverriddenIDs) {
      if (auto overriddenAssocType =
              dyn_cast_or_null<AssociatedTypeDecl>(getDecl(overriddenID))) {
        overriddenAssocTypes.push_back(overriddenAssocType);
      }
    }
    assocType->setOverriddenDecls(overriddenAssocTypes);

    break;
  }

  case decls_block::STRUCT_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    bool isImplicit;
    bool isObjC;
    GenericEnvironmentID genericEnvID;
    uint8_t rawAccessLevel;
    unsigned numConformances;
    ArrayRef<uint64_t> rawInheritedIDs;

    decls_block::StructLayout::readRecord(scratch, nameID, contextID,
                                          isImplicit, isObjC, genericEnvID,
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

    if (auto accessLevel = getActualAccessLevel(rawAccessLevel)) {
      theStruct->setAccess(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    if (isImplicit)
      theStruct->setImplicit();
    theStruct->setIsObjC(isObjC);

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
    bool needsNewVTableEntry, firstTimeRequired;
    uint8_t rawDefaultArgumentResilienceExpansion;
    unsigned numArgNames;
    ArrayRef<uint64_t> argNameAndDependencyIDs;

    decls_block::ConstructorLayout::readRecord(scratch, contextID,
                                               rawFailability, isImplicit, 
                                               isObjC, hasStubImplementation,
                                               throws, storedInitKind,
                                               genericEnvID, interfaceID,
                                               overriddenID,
                                               rawAccessLevel,
                                               needsNewVTableEntry,
                                               rawDefaultArgumentResilienceExpansion,
                                               firstTimeRequired,
                                               numArgNames,
                                               argNameAndDependencyIDs);

    // Resolve the name ids.
    SmallVector<Identifier, 2> argNames;
    for (auto argNameID : argNameAndDependencyIDs.slice(0, numArgNames))
      argNames.push_back(getIdentifier(argNameID));
    DeclName name(ctx, DeclBaseName::createConstructor(), argNames);

    Optional<swift::CtorInitializerKind> initKind =
        getActualCtorInitializerKind(storedInitKind);

    DeclDeserializationError::Flags errorFlags;
    if (initKind == CtorInitializerKind::Designated)
      errorFlags |= DeclDeserializationError::DesignatedInitializer;
    if (needsNewVTableEntry) {
      errorFlags |= DeclDeserializationError::NeedsVTableEntry;
      DeclAttributes attrs;
      attrs.setRawAttributeChain(DAttrs);
      if (attrs.hasAttribute<RequiredAttr>())
        errorFlags |= DeclDeserializationError::NeedsAllocatingVTableEntry;
    }
    if (firstTimeRequired)
      errorFlags |= DeclDeserializationError::NeedsAllocatingVTableEntry;

    auto overridden = getDeclChecked(overriddenID);
    if (!overridden) {
      llvm::consumeError(overridden.takeError());
      return llvm::make_error<OverrideError>(name, errorFlags);
    }

    for (auto dependencyID : argNameAndDependencyIDs.slice(numArgNames)) {
      auto dependency = getTypeChecked(dependencyID);
      if (!dependency) {
        return llvm::make_error<TypeError>(
            name, takeErrorInfo(dependency.takeError()), errorFlags);
      }
    }

    auto parent = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto *genericParams = maybeReadGenericParams(parent);
    if (declOrOffset.isComplete())
      return declOrOffset;

    OptionalTypeKind failability = OTK_None;
    if (auto actualFailability = getActualOptionalTypeKind(rawFailability))
      failability = *actualFailability;

    auto ctor =
      createDecl<ConstructorDecl>(name, SourceLoc(),
                                  failability, /*FailabilityLoc=*/SourceLoc(),
                                  /*Throws=*/throws, /*ThrowsLoc=*/SourceLoc(),
                                  /*BodyParams=*/nullptr, nullptr,
                                  genericParams, parent);
    declOrOffset = ctor;

    configureGenericEnvironment(ctor, genericEnvID);

    if (auto accessLevel = getActualAccessLevel(rawAccessLevel)) {
      ctor->setAccess(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    bool mutating = parent->getDeclaredInterfaceType()->hasReferenceSemantics();
    auto *selfDecl = ParamDecl::createSelf(SourceLoc(), parent,
                                           /*static*/ false,
                                           /*mutating*/ mutating);
    selfDecl->setImplicit();

    auto *bodyParams = readParameterList();
    assert(bodyParams && "missing parameters for constructor");
    ctor->setParameters(selfDecl, bodyParams);

    auto interfaceType = getType(interfaceID);
    ctor->setInterfaceType(interfaceType);

    // Set the initializer interface type of the constructor.
    auto allocType = ctor->getInterfaceType();
    auto selfParam = computeSelfParam(ctor, /*isInitializingCtor=*/true);
    if (auto polyFn = allocType->getAs<GenericFunctionType>()) {
      ctor->setInitializerInterfaceType(
              GenericFunctionType::get(polyFn->getGenericSignature(),
                                       {selfParam}, polyFn->getResult(),
                                       polyFn->getExtInfo()));
    } else {
      auto fn = allocType->castTo<FunctionType>();
      ctor->setInitializerInterfaceType(FunctionType::get({selfParam},
                                                          fn->getResult(),
                                                          fn->getExtInfo()));
    }

    if (auto errorConvention = maybeReadForeignErrorConvention())
      ctor->setForeignErrorConvention(*errorConvention);

    if (isImplicit)
      ctor->setImplicit();
    ctor->setIsObjC(isObjC);
    if (hasStubImplementation)
      ctor->setStubImplementation(true);
    if (initKind.hasValue())
      ctor->setInitKind(initKind.getValue());
    if (auto overriddenCtor = cast_or_null<ConstructorDecl>(overridden.get()))
      ctor->setOverriddenDecl(overriddenCtor);
    ctor->setNeedsNewVTableEntry(needsNewVTableEntry);

    if (auto defaultArgumentResilienceExpansion = getActualResilienceExpansion(
            rawDefaultArgumentResilienceExpansion)) {
      ctor->setDefaultArgumentResilienceExpansion(
          *defaultArgumentResilienceExpansion);
    } else {
      error();
      return nullptr;
    }

    break;
  }

  case decls_block::VAR_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    bool isImplicit, isObjC, isStatic, hasNonPatternBindingInit;
    bool isGetterMutating, isSetterMutating;
    unsigned rawSpecifier, numAccessors;
    uint8_t readImpl, writeImpl, readWriteImpl;
    uint8_t rawAccessLevel, rawSetterAccessLevel;
    TypeID interfaceTypeID;
    AccessorRecord accessors;
    DeclID overriddenID;
    ArrayRef<uint64_t> accessorAndDependencyIDs;

    decls_block::VarLayout::readRecord(scratch, nameID, contextID,
                                       isImplicit, isObjC, isStatic, rawSpecifier,
                                       hasNonPatternBindingInit,
                                       isGetterMutating, isSetterMutating,
                                       readImpl, writeImpl, readWriteImpl,
                                       numAccessors,
                                       interfaceTypeID,
                                       overriddenID,
                                       rawAccessLevel, rawSetterAccessLevel,
                                       accessorAndDependencyIDs);

    Identifier name = getIdentifier(nameID);

    Expected<Decl *> overridden = getDeclChecked(overriddenID);
    if (!overridden) {
      llvm::consumeError(overridden.takeError());
      return llvm::make_error<OverrideError>(name);
    }

    // Exctract the accessor IDs.
    for (DeclID accessorID : accessorAndDependencyIDs.slice(0, numAccessors)) {
      accessors.IDs.push_back(accessorID);
    }
    accessorAndDependencyIDs = accessorAndDependencyIDs.slice(numAccessors);

    for (TypeID dependencyID : accessorAndDependencyIDs) {
      auto dependency = getTypeChecked(dependencyID);
      if (!dependency) {
        // Stored properties in classes still impact class object layout because
        // their offset is computed and stored in the field offset vector.
        DeclDeserializationError::Flags flags;
        
        if (!isStatic) {
          auto actualReadImpl = getActualReadImplKind(readImpl);
          if (actualReadImpl && *actualReadImpl == ReadImplKind::Stored) {
            flags |= DeclDeserializationError::Flag::NeedsFieldOffsetVectorEntry;
          }
        }
        
        return llvm::make_error<TypeError>(
            name, takeErrorInfo(dependency.takeError()), flags);
      }
    }

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto specifier = getActualVarDeclSpecifier(
        (serialization::VarDeclSpecifier)rawSpecifier);
    if (!specifier) {
      error();
      return nullptr;
    }

    auto var = createDecl<VarDecl>(/*IsStatic*/ isStatic, *specifier,
                                   /*IsCaptureList*/ false, SourceLoc(), name,
                                   Type(), DC);
    var->setHasNonPatternBindingInit(hasNonPatternBindingInit);
    var->setIsGetterMutating(isGetterMutating);
    var->setIsSetterMutating(isSetterMutating);
    declOrOffset = var;

    Type interfaceType = getType(interfaceTypeID);
    var->setInterfaceType(interfaceType);

    if (auto referenceStorage = interfaceType->getAs<ReferenceStorageType>())
      AddAttribute(
          new (ctx) ReferenceOwnershipAttr(referenceStorage->getOwnership()));

    configureStorage(var, readImpl, writeImpl, readWriteImpl, accessors);

    if (auto accessLevel = getActualAccessLevel(rawAccessLevel)) {
      var->setAccess(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    if (var->isSettable(nullptr)) {
      if (auto setterAccess = getActualAccessLevel(rawSetterAccessLevel)) {
        var->setSetterAccess(*setterAccess);
      } else {
        error();
        return nullptr;
      }
    }

    if (isImplicit)
      var->setImplicit();
    var->setIsObjC(isObjC);

    if (auto overriddenVar = cast_or_null<VarDecl>(overridden.get())) {
      var->setOverriddenDecl(overriddenVar);
      AddAttribute(new (ctx) OverrideAttr(SourceLoc()));
    }

    break;
  }

  case decls_block::PARAM_DECL: {
    IdentifierID argNameID, paramNameID;
    DeclContextID contextID;
    unsigned rawSpecifier;
    TypeID interfaceTypeID;

    decls_block::ParamLayout::readRecord(scratch, argNameID, paramNameID,
                                         contextID, rawSpecifier,
                                         interfaceTypeID);

    auto DC = ForcedContext ? *ForcedContext : getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto specifier = getActualVarDeclSpecifier(
        (serialization::VarDeclSpecifier)rawSpecifier);
    if (!specifier) {
      error();
      return nullptr;
    }

    auto param = createDecl<ParamDecl>(*specifier, SourceLoc(), SourceLoc(),
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

    param->setInterfaceType(paramTy->getInOutObjectType());
    break;
  }

  case decls_block::FUNC_DECL:
  case decls_block::ACCESSOR_DECL: {
    bool isAccessor = (recordID == decls_block::ACCESSOR_DECL);

    DeclContextID contextID;
    bool isImplicit;
    bool isStatic;
    uint8_t rawStaticSpelling, rawAccessLevel, rawMutModifier;
    uint8_t rawAccessorKind, rawAddressorKind;
    bool isObjC, hasDynamicSelf, hasForcedStaticDispatch, throws;
    unsigned numNameComponentsBiased;
    GenericEnvironmentID genericEnvID;
    TypeID interfaceTypeID;
    DeclID associatedDeclID;
    DeclID overriddenID;
    DeclID accessorStorageDeclID;
    bool needsNewVTableEntry;
    uint8_t rawDefaultArgumentResilienceExpansion;
    ArrayRef<uint64_t> nameAndDependencyIDs;

    if (!isAccessor) {
      decls_block::FuncLayout::readRecord(scratch, contextID, isImplicit,
                                          isStatic, rawStaticSpelling, isObjC,
                                          rawMutModifier, hasDynamicSelf,
                                          hasForcedStaticDispatch, throws,
                                          genericEnvID,
                                          interfaceTypeID,
                                          associatedDeclID, overriddenID,
                                          numNameComponentsBiased,
                                          rawAccessLevel,
                                          needsNewVTableEntry,
                                          rawDefaultArgumentResilienceExpansion,
                                          nameAndDependencyIDs);
    } else {
      decls_block::AccessorLayout::readRecord(scratch, contextID, isImplicit,
                                          isStatic, rawStaticSpelling, isObjC,
                                          rawMutModifier, hasDynamicSelf,
                                          hasForcedStaticDispatch, throws,
                                          genericEnvID,
                                          interfaceTypeID,
                                          overriddenID,
                                          accessorStorageDeclID,
                                          rawAccessorKind, rawAddressorKind,
                                          rawAccessLevel,
                                          needsNewVTableEntry,
                                          rawDefaultArgumentResilienceExpansion,
                                          nameAndDependencyIDs);
    }

    DeclDeserializationError::Flags errorFlags;
    if (needsNewVTableEntry)
      errorFlags |= DeclDeserializationError::NeedsVTableEntry;

    // Parse the accessor-specific fields.
    AbstractStorageDecl *storage = nullptr;
    AccessorKind accessorKind;
    AddressorKind addressorKind;
    if (isAccessor) {
      auto storageResult = getDeclChecked(accessorStorageDeclID);
      if (!storageResult ||
          !(storage =
              dyn_cast_or_null<AbstractStorageDecl>(storageResult.get()))) {
        // FIXME: "TypeError" isn't exactly correct for this.
        return llvm::make_error<TypeError>(
            DeclName(), takeErrorInfo(storageResult.takeError()), errorFlags);
      }

      if (auto accessorKindResult = getActualAccessorKind(rawAccessorKind)) {
        accessorKind = *accessorKindResult;
      } else {
        error();
        return nullptr;
      }

      if (auto addressorKindResult = getActualAddressorKind(rawAddressorKind)) {
        addressorKind = *addressorKindResult;
      } else {
        error();
        return nullptr;
      }

      // Deserializing the storage declaration will cause a recurrence
      // into this code.  When we come out, don't create the accessor twice.
      // TODO: find some better way of breaking this cycle, like lazily
      // deserializing the accessors.
      if (auto accessor = storage->getAccessor(accessorKind))
        return accessor;
    }

    // Resolve the name ids.
    DeclName name;
    ArrayRef<uint64_t> dependencyIDs;
    if (isAccessor) {
      dependencyIDs = nameAndDependencyIDs;
    } else {
      Identifier baseName = getIdentifier(nameAndDependencyIDs.front());
      if (numNameComponentsBiased != 0) {
        SmallVector<Identifier, 2> names;
        for (auto nameID : nameAndDependencyIDs.slice(1,
                                                      numNameComponentsBiased-1)){
          names.push_back(getIdentifier(nameID));
        }
        name = DeclName(ctx, baseName, names);
        dependencyIDs = nameAndDependencyIDs.slice(numNameComponentsBiased);
      } else {
        name = baseName;
        dependencyIDs = nameAndDependencyIDs.drop_front();
      }
    }

    Expected<Decl *> overridden = getDeclChecked(overriddenID);
    if (!overridden) {
      llvm::consumeError(overridden.takeError());
      return llvm::make_error<OverrideError>(name, errorFlags);
    }

    for (TypeID dependencyID : dependencyIDs) {
      auto dependency = getTypeChecked(dependencyID);
      if (!dependency) {
        return llvm::make_error<TypeError>(
            name, takeErrorInfo(dependency.takeError()), errorFlags);
      }
    }

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

    bool hasImplicitSelfDecl = DC->isTypeContext();
    FuncDecl *fn;
    if (!isAccessor) {
      fn = FuncDecl::createDeserialized(
        ctx, /*StaticLoc=*/SourceLoc(), staticSpelling.getValue(),
        /*FuncLoc=*/SourceLoc(), name, /*NameLoc=*/SourceLoc(),
        /*Throws=*/throws, /*ThrowsLoc=*/SourceLoc(),
        genericParams, hasImplicitSelfDecl, DC);
    } else {
      fn = AccessorDecl::createDeserialized(
        ctx, /*FuncLoc=*/SourceLoc(), /*AccessorKeywordLoc=*/SourceLoc(),
        accessorKind, addressorKind, storage,
        /*StaticLoc=*/SourceLoc(), staticSpelling.getValue(),
        /*Throws=*/throws, /*ThrowsLoc=*/SourceLoc(),
        genericParams, hasImplicitSelfDecl, DC);
    }
    fn->setEarlyAttrValidation();
    declOrOffset = fn;

    configureGenericEnvironment(fn, genericEnvID);

    if (auto accessLevel = getActualAccessLevel(rawAccessLevel)) {
      fn->setAccess(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    if (auto SelfAccessKind = getActualSelfAccessKind(rawMutModifier)) {
      fn->setSelfAccessKind(*SelfAccessKind);
    } else {
      error();
      return nullptr;
    }

    if (!isAccessor) {
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
    }

    // Set the interface type.
    auto interfaceType = getType(interfaceTypeID);
    fn->setInterfaceType(interfaceType);

    ParamDecl *selfDecl = nullptr;
    if (DC->isTypeContext()) {
      selfDecl = ParamDecl::createSelf(SourceLoc(), DC,
                                       fn->isStatic(),
                                       fn->isMutating());
      selfDecl->setImplicit();
    }

    ParameterList *paramList = readParameterList();

    fn->setParameters(selfDecl, paramList);

    if (auto errorConvention = maybeReadForeignErrorConvention())
      fn->setForeignErrorConvention(*errorConvention);

    if (auto overriddenFunc = cast_or_null<FuncDecl>(overridden.get())) {
      fn->setOverriddenDecl(overriddenFunc);
      AddAttribute(new (ctx) OverrideAttr(SourceLoc()));
    }

    fn->setStatic(isStatic);
    if (isImplicit)
      fn->setImplicit();
    fn->setIsObjC(isObjC);
    fn->setDynamicSelf(hasDynamicSelf);
    fn->setForcedStaticDispatch(hasForcedStaticDispatch);
    fn->setNeedsNewVTableEntry(needsNewVTableEntry);

    if (auto defaultArgumentResilienceExpansion = getActualResilienceExpansion(
            rawDefaultArgumentResilienceExpansion)) {
      fn->setDefaultArgumentResilienceExpansion(
          *defaultArgumentResilienceExpansion);
    } else {
      error();
      return nullptr;
    }

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

    SmallVector<std::pair<Pattern *, DeclContextID>, 4> patterns;
    for (unsigned i = 0; i != numPatterns; ++i) {
      auto pattern = readPattern(dc);
      if (!pattern) {
        // Silently drop the pattern...
        llvm::consumeError(pattern.takeError());
        // ...but continue to read any further patterns we're expecting.
        continue;
      }

      patterns.emplace_back(pattern.get(), DeclContextID());
      if (!initContextIDs.empty())
        patterns.back().second = initContextIDs[i];
    }

    auto binding =
      PatternBindingDecl::createDeserialized(ctx, SourceLoc(),
                                             StaticSpelling.getValue(),
                                             SourceLoc(), patterns.size(), dc);
    binding->setEarlyAttrValidation(true);
    declOrOffset = binding;

    binding->setStatic(isStatic);

    if (isImplicit)
      binding->setImplicit();

    for (unsigned i = 0; i != patterns.size(); ++i) {
      DeclContext *initContext = getDeclContext(patterns[i].second);
      binding->setPattern(i, patterns[i].first, initContext);
    }

    break;
  }

  case decls_block::PROTOCOL_DECL: {
    IdentifierID nameID;
    DeclContextID contextID;
    bool isImplicit, isClassBounded, isObjC, existentialTypeSupported;
    GenericEnvironmentID genericEnvID;
    TypeID superclassID;
    uint8_t rawAccessLevel;
    ArrayRef<uint64_t> rawInheritedIDs;

    decls_block::ProtocolLayout::readRecord(scratch, nameID, contextID,
                                            isImplicit, isClassBounded, isObjC,
                                            existentialTypeSupported,
                                            genericEnvID, superclassID,
                                            rawAccessLevel, rawInheritedIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto proto = createDecl<ProtocolDecl>(DC, SourceLoc(), SourceLoc(),
                                          getIdentifier(nameID), None,
                                          /*TrailingWhere=*/nullptr);
    declOrOffset = proto;

    proto->setSuperclass(getType(superclassID));
    proto->setRequiresClass(isClassBounded);
    proto->setExistentialTypeSupported(existentialTypeSupported);

    if (auto accessLevel = getActualAccessLevel(rawAccessLevel)) {
      proto->setAccess(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    auto genericParams = maybeReadGenericParams(DC);
    assert(genericParams && "protocol with no generic parameters?");
    proto->setGenericParams(genericParams);

    handleInherited(proto, rawInheritedIDs);

    configureGenericEnvironment(proto, genericEnvID);

    if (isImplicit)
      proto->setImplicit();
    proto->setIsObjC(isObjC);

    proto->computeType();

    proto->setCircularityCheck(CircularityCheck::Checked);

    // Establish the requirement signature.
    {
      SmallVector<Requirement, 4> requirements;
      readGenericRequirements(requirements, DeclTypeCursor);
      proto->setRequirementSignature(requirements);
    }

    proto->setMemberLoader(this, DeclTypeCursor.GetCurrentBitNo());

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
    bool inheritsSuperclassInitializers;
    GenericEnvironmentID genericEnvID;
    TypeID superclassID;
    uint8_t rawAccessLevel;
    unsigned numConformances;
    ArrayRef<uint64_t> rawInheritedIDs;
    decls_block::ClassLayout::readRecord(scratch, nameID, contextID,
                                         isImplicit, isObjC,
                                         requiresStoredPropertyInits,
                                         inheritsSuperclassInitializers,
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

    if (auto accessLevel = getActualAccessLevel(rawAccessLevel)) {
      theClass->setAccess(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    theClass->setAddedImplicitInitializers();
    if (isImplicit)
      theClass->setImplicit();
    theClass->setIsObjC(isObjC);
    theClass->setSuperclass(getType(superclassID));
    if (requiresStoredPropertyInits)
      theClass->setRequiresStoredPropertyInits(true);
    if (inheritsSuperclassInitializers)
      theClass->setInheritsSuperclassInitializers();

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
    bool isObjC;
    GenericEnvironmentID genericEnvID;
    TypeID rawTypeID;
    uint8_t rawAccessLevel;
    unsigned numConformances, numInheritedTypes;
    ArrayRef<uint64_t> rawInheritedAndDependencyIDs;

    decls_block::EnumLayout::readRecord(scratch, nameID, contextID,
                                        isImplicit, isObjC, genericEnvID,
                                        rawTypeID, rawAccessLevel,
                                        numConformances, numInheritedTypes,
                                        rawInheritedAndDependencyIDs);

    auto DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    Identifier name = getIdentifier(nameID);
    for (TypeID dependencyID :
           rawInheritedAndDependencyIDs.slice(numInheritedTypes)) {
      auto dependency = getTypeChecked(dependencyID);
      if (!dependency) {
        return llvm::make_error<TypeError>(
            name, takeErrorInfo(dependency.takeError()));
      }
    }

    auto genericParams = maybeReadGenericParams(DC);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto theEnum = createDecl<EnumDecl>(SourceLoc(), name, SourceLoc(), None,
                                        genericParams, DC);

    declOrOffset = theEnum;

    configureGenericEnvironment(theEnum, genericEnvID);

    if (auto accessLevel = getActualAccessLevel(rawAccessLevel)) {
      theEnum->setAccess(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    if (isImplicit)
      theEnum->setImplicit();
    theEnum->setIsObjC(isObjC);

    theEnum->setRawType(getType(rawTypeID));

    theEnum->computeType();

    handleInherited(theEnum,
                    rawInheritedAndDependencyIDs.slice(0, numInheritedTypes));

    theEnum->setMemberLoader(this, DeclTypeCursor.GetCurrentBitNo());
    skipRecord(DeclTypeCursor, decls_block::MEMBERS);
    theEnum->setConformanceLoader(
      this,
      encodeLazyConformanceContextData(numConformances,
                                       DeclTypeCursor.GetCurrentBitNo()));
    break;
  }

  case decls_block::ENUM_ELEMENT_DECL: {
    DeclContextID contextID;
    TypeID interfaceTypeID;
    bool isImplicit; bool hasPayload; bool isNegative;
    unsigned rawValueKindID;
    IdentifierID blobData;
    uint8_t rawResilienceExpansion;
    unsigned numArgNames;
    ArrayRef<uint64_t> argNameAndDependencyIDs;

    decls_block::EnumElementLayout::readRecord(scratch, contextID,
                                               interfaceTypeID,
                                               isImplicit, hasPayload,
                                               rawValueKindID, isNegative,
                                               blobData,
                                               rawResilienceExpansion,
                                               numArgNames,
                                               argNameAndDependencyIDs);

    // Resolve the name ids.
    Identifier baseName = getIdentifier(argNameAndDependencyIDs.front());
    SmallVector<Identifier, 2> argNames;
    for (auto argNameID : argNameAndDependencyIDs.slice(1, numArgNames-1))
      argNames.push_back(getIdentifier(argNameID));
    DeclName compoundName(ctx, baseName, argNames);
    DeclName name = argNames.empty() ? baseName : compoundName;
    
    for (TypeID dependencyID : argNameAndDependencyIDs.slice(numArgNames+1)) {
      auto dependency = getTypeChecked(dependencyID);
      if (!dependency) {
        return llvm::make_error<TypeError>(
          name, takeErrorInfo(dependency.takeError()));
      }
    }

    // Read payload parameter list, if it exists.
    ParameterList *paramList = nullptr;
    if (hasPayload) {
      paramList = readParameterList();
    }

    DeclContext *DC = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto elem = createDecl<EnumElementDecl>(SourceLoc(),
                                            name,
                                            paramList,
                                            SourceLoc(),
                                            nullptr,
                                            DC);
    declOrOffset = elem;
    
    // Deserialize the literal raw value, if any.
    switch ((EnumElementRawValueKind)rawValueKindID) {
    case EnumElementRawValueKind::None:
      break;
    case EnumElementRawValueKind::IntegerLiteral: {
      auto literalText = getIdentifier(blobData);
      auto literal = new (getContext()) IntegerLiteralExpr(literalText.get(),
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
    elem->setAccess(std::max(cast<EnumDecl>(DC)->getFormalAccess(),
                             AccessLevel::Internal));

    if (auto resilienceExpansion = getActualResilienceExpansion(
                                       rawResilienceExpansion)) {
      elem->setDefaultArgumentResilienceExpansion(*resilienceExpansion);
    } else {
      error();
      return nullptr;
    }
    break;
  }

  case decls_block::SUBSCRIPT_DECL: {
    DeclContextID contextID;
    bool isImplicit, isObjC, isGetterMutating, isSetterMutating;
    GenericEnvironmentID genericEnvID;
    TypeID interfaceTypeID;
    AccessorRecord accessors;
    DeclID overriddenID;
    uint8_t rawAccessLevel, rawSetterAccessLevel;
    uint8_t readImpl, writeImpl, readWriteImpl;
    unsigned numArgNames, numAccessors;
    ArrayRef<uint64_t> argNameAndDependencyIDs;

    decls_block::SubscriptLayout::readRecord(scratch, contextID,
                                             isImplicit, isObjC,
                                             isGetterMutating, isSetterMutating,
                                             readImpl, writeImpl, readWriteImpl,
                                             numAccessors,
                                             genericEnvID,
                                             interfaceTypeID,
                                             overriddenID, rawAccessLevel,
                                             rawSetterAccessLevel, numArgNames,
                                             argNameAndDependencyIDs);
    // Resolve the name ids.
    SmallVector<Identifier, 2> argNames;
    for (auto argNameID : argNameAndDependencyIDs.slice(0, numArgNames))
      argNames.push_back(getIdentifier(argNameID));
    DeclName name(ctx, DeclBaseName::createSubscript(), argNames);
    argNameAndDependencyIDs = argNameAndDependencyIDs.slice(numArgNames);

    // Exctract the accessor IDs.
    for (DeclID accessorID : argNameAndDependencyIDs.slice(0, numAccessors)) {
      accessors.IDs.push_back(accessorID);
    }
    argNameAndDependencyIDs = argNameAndDependencyIDs.slice(numAccessors);

    Expected<Decl *> overridden = getDeclChecked(overriddenID);
    if (!overridden) {
      llvm::consumeError(overridden.takeError());
      return llvm::make_error<OverrideError>(name);
    }

    for (TypeID dependencyID : argNameAndDependencyIDs) {
      auto dependency = getTypeChecked(dependencyID);
      if (!dependency) {
        return llvm::make_error<TypeError>(
            name, takeErrorInfo(dependency.takeError()));
      }
    }

    auto parent = getDeclContext(contextID);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto *genericParams = maybeReadGenericParams(parent);
    if (declOrOffset.isComplete())
      return declOrOffset;

    auto subscript = createDecl<SubscriptDecl>(name, SourceLoc(), nullptr,
                                               SourceLoc(), TypeLoc(),
                                               parent, genericParams);
    subscript->setIsGetterMutating(isGetterMutating);
    subscript->setIsSetterMutating(isSetterMutating);
    declOrOffset = subscript;

    configureGenericEnvironment(subscript, genericEnvID);

    subscript->setIndices(readParameterList());

    configureStorage(subscript, readImpl, writeImpl, readWriteImpl, accessors);

    if (auto accessLevel = getActualAccessLevel(rawAccessLevel)) {
      subscript->setAccess(*accessLevel);
    } else {
      error();
      return nullptr;
    }

    if (subscript->isSettable()) {
      if (auto setterAccess = getActualAccessLevel(rawSetterAccessLevel)) {
        subscript->setSetterAccess(*setterAccess);
      } else {
        error();
        return nullptr;
      }
    }

    auto interfaceType = getType(interfaceTypeID);
    subscript->setInterfaceType(interfaceType);

    if (isImplicit)
      subscript->setImplicit();
    subscript->setIsObjC(isObjC);
    if (auto overriddenSub = cast_or_null<SubscriptDecl>(overridden.get())) {
      subscript->setOverriddenDecl(overriddenSub);
      AddAttribute(new (ctx) OverrideAttr(SourceLoc()));
    }
    break;
  }

  case decls_block::EXTENSION_DECL: {
    TypeID baseID;
    DeclContextID contextID;
    bool isImplicit;
    GenericEnvironmentID genericEnvID;
    unsigned numConformances, numInherited;
    ArrayRef<uint64_t> inheritedAndDependencyIDs;

    decls_block::ExtensionLayout::readRecord(scratch, baseID, contextID,
                                             isImplicit, genericEnvID,
                                             numConformances, numInherited,
                                             inheritedAndDependencyIDs);

    auto DC = getDeclContext(contextID);

    for (TypeID dependencyID : inheritedAndDependencyIDs.slice(numInherited)) {
      auto dependency = getTypeChecked(dependencyID);
      if (!dependency) {
        return llvm::make_error<ExtensionError>(
            takeErrorInfo(dependency.takeError()));
      }
    }

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
    assert(!baseTy->hasUnboundGenericType());
    extension->getExtendedTypeLoc().setType(baseTy);

    if (isImplicit)
      extension->setImplicit();

    auto inheritedTypes = ctx.Allocate<TypeLoc>(numInherited);
    for_each(inheritedTypes, inheritedAndDependencyIDs.slice(0, numInherited),
             [this](TypeLoc &tl, uint64_t rawID) {
      tl = TypeLoc::withoutLoc(getType(rawID));
    });
    extension->setInherited(inheritedTypes);

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

    auto dtor = createDecl<DestructorDecl>(SourceLoc(), /*selfpat*/nullptr, DC);
    declOrOffset = dtor;

    configureGenericEnvironment(dtor, genericEnvID);

    dtor->setAccess(std::max(cast<ClassDecl>(DC)->getFormalAccess(),
                             AccessLevel::Internal));
    auto *selfDecl = ParamDecl::createSelf(SourceLoc(), DC,
                                           /*static*/ false,
                                           /*mutating*/ false);
    selfDecl->setImplicit();
    dtor->setParameters(selfDecl, ParameterList::createEmpty(ctx));

    auto interfaceType = getType(interfaceID);
    dtor->setInterfaceType(interfaceType);

    if (isImplicit)
      dtor->setImplicit();
    dtor->setIsObjC(isObjC);

    break;
  }

  case decls_block::XREF: {
    assert(DAttrs == nullptr);
    ModuleID baseModuleID;
    uint32_t pathLen;
    decls_block::XRefLayout::readRecord(scratch, baseModuleID, pathLen);
    auto resolved = resolveCrossReference(getModule(baseModuleID), pathLen);
    if (!resolved)
      return resolved;
    declOrOffset = resolved.get();
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
  decl->setValidationToChecked();
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

/// Translate from the Serialization coroutine kind enum values to the AST
/// strongly-typed enum.
///
/// The former is guaranteed to be stable, but may not reflect this version of
/// the AST.
static Optional<swift::SILCoroutineKind>
getActualSILCoroutineKind(uint8_t rep) {
  switch (rep) {
#define CASE(KIND) \
  case (uint8_t)serialization::SILCoroutineKind::KIND: \
    return swift::SILCoroutineKind::KIND;
  CASE(None)
  CASE(YieldOnce)
  CASE(YieldMany)
#undef CASE
  default:
    return None;
  }
}

/// Translate from the serialization ReferenceOwnership enumerators, which are
/// guaranteed to be stable, to the AST ones.
static Optional<swift::ReferenceOwnership>
getActualReferenceOwnership(serialization::ReferenceOwnership raw) {
  switch (raw) {
  case serialization::ReferenceOwnership::Strong:
    return swift::ReferenceOwnership::Strong;
#define REF_STORAGE(Name, ...) \
  case serialization::ReferenceOwnership::Name: \
    return swift::ReferenceOwnership::Name;
#include "swift/AST/ReferenceStorage.def"
  }
  return None;
}

/// Translate from the serialization ValueOwnership enumerators, which are
/// guaranteed to be stable, to the AST ones.
static Optional<swift::ValueOwnership>
getActualValueOwnership(serialization::ValueOwnership raw) {
  switch (raw) {
#define CASE(ID) \
  case serialization::ValueOwnership::ID: \
    return swift::ValueOwnership::ID;
  CASE(Default)
  CASE(InOut)
  CASE(Shared)
  CASE(Owned)
#undef CASE
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
  CASE(Indirect_In_Constant)
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
  Expected<Type> deserialized = getTypeChecked(TID);
  if (!deserialized) {
    fatal(deserialized.takeError());
  }
  return deserialized.get();
}

Expected<Type> ModuleFile::getTypeChecked(TypeID TID) {
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

  if (auto s = ctx.Stats)
    s->getFrontendCounters().NumTypesDeserialized++;

  switch (recordID) {
  case decls_block::BUILTIN_ALIAS_TYPE: {
    DeclID underlyingID;
    TypeID canonicalTypeID;
    decls_block::BuiltinAliasTypeLayout::readRecord(scratch, underlyingID,
                                                    canonicalTypeID);
    auto aliasOrError = getDeclChecked(underlyingID);
    if (!aliasOrError)
      return aliasOrError.takeError();
    auto alias = dyn_cast<TypeAliasDecl>(aliasOrError.get());

    if (ctx.LangOpts.EnableDeserializationRecovery) {
      Expected<Type> expectedType = getTypeChecked(canonicalTypeID);
      if (!expectedType)
        return expectedType.takeError();
      if (expectedType.get()) {
        if (!alias ||
            !alias->getDeclaredInterfaceType()->isEqual(expectedType.get())) {
          // Fall back to the canonical type.
          typeOrOffset = expectedType.get();
          break;
        }
      }
    }

    // Look through compatibility aliases that are now unavailable.
    if (alias->getAttrs().isUnavailable(ctx) &&
        alias->isCompatibilityAlias()) {
      typeOrOffset = alias->getUnderlyingTypeLoc().getType();
      break;
    }

    typeOrOffset = alias->getDeclaredInterfaceType();
    break;
  }

  case decls_block::NAME_ALIAS_TYPE: {
    DeclID typealiasID;
    TypeID parentTypeID;
    TypeID underlyingTypeID;
    SubstitutionMapID substitutionsID;
    decls_block::NameAliasTypeLayout::readRecord(scratch, typealiasID,
                                                 parentTypeID,
                                                 underlyingTypeID,
                                                 substitutionsID);
    auto aliasOrError = getDeclChecked(typealiasID);
    if (!aliasOrError)
      return aliasOrError.takeError();
    auto alias = dyn_cast<TypeAliasDecl>(aliasOrError.get());

    Type underlyingType;
    if (ctx.LangOpts.EnableDeserializationRecovery) {
      Expected<Type> expectedType = getTypeChecked(underlyingTypeID);
      if (!expectedType)
        return expectedType.takeError();
      if (expectedType.get()) {
        if (!alias ||
            !alias->getDeclaredInterfaceType()->isEqual(expectedType.get())) {
          // Fall back to the canonical type.
          typeOrOffset = expectedType.get()->getCanonicalType();
          break;
        }
      }

      underlyingType = expectedType.get();
    } else {
      underlyingType = getType(underlyingTypeID);
    }

    Type parentType = getType(parentTypeID);

    // Read the substitutions.
    SubstitutionMap subMap = getSubstitutionMap(substitutionsID);

    // Look through compatibility aliases that are now unavailable.
    if (alias->getAttrs().isUnavailable(ctx) &&
        alias->isCompatibilityAlias()) {
      typeOrOffset = alias->getUnderlyingTypeLoc().getType();
      break;
    }

    typeOrOffset = NameAliasType::get(alias, parentType, subMap,
                                      underlyingType);
    break;
  }
  case decls_block::NOMINAL_TYPE: {
    DeclID declID;
    TypeID parentID;
    decls_block::NominalTypeLayout::readRecord(scratch, declID, parentID);

    Expected<Type> parentTy = getTypeChecked(parentID);
    if (!parentTy)
      return parentTy.takeError();

    auto nominalOrError = getDeclChecked(declID);
    if (!nominalOrError)
      return nominalOrError.takeError();

    // Look through compatibility aliases.
    if (auto *alias = dyn_cast<TypeAliasDecl>(nominalOrError.get())) {
      // Reminder: TypeBase::getAs will look through sugar. But we don't want to
      // do that here, so we do isa<> checks on the TypeBase itself instead of
      // using the Type wrapper.
      const TypeBase *underlyingTy = nullptr;
      while (alias->isCompatibilityAlias()) {
        underlyingTy = alias->getUnderlyingTypeLoc().getType().getPointer();

        // If the underlying type is itself a typealias, it might be another
        // compatibility alias, meaning we need to go around the loop again.
        auto aliasTy = dyn_cast<NameAliasType>(underlyingTy);
        if (!aliasTy)
          break;
        alias = aliasTy->getDecl();
      }

      // We only want to use the type we found if it's a simple non-generic
      // nominal type.
      if (auto simpleNominalTy = dyn_cast_or_null<NominalType>(underlyingTy)) {
        nominalOrError = simpleNominalTy->getDecl();
        (void)!nominalOrError; // "Check" the llvm::Expected<> value.
      }
    }

    auto nominal = dyn_cast<NominalTypeDecl>(nominalOrError.get());
    if (!nominal) {
      XRefTracePath tinyTrace{*nominalOrError.get()->getModuleContext()};
      DeclName fullName = cast<ValueDecl>(nominalOrError.get())->getFullName();
      tinyTrace.addValue(fullName.getBaseIdentifier());
      return llvm::make_error<XRefError>("declaration is not a nominal type",
                                         tinyTrace, fullName);
    }
    typeOrOffset = NominalType::get(nominal, parentTy.get(), ctx);

    assert(typeOrOffset.isComplete());
    break;
  }

  case decls_block::PAREN_TYPE: {
    TypeID underlyingID;
    bool isVariadic, isAutoClosure, isEscaping;
    unsigned rawOwnership;
    decls_block::ParenTypeLayout::readRecord(scratch, underlyingID, isVariadic,
                                             isAutoClosure, isEscaping,
                                             rawOwnership);
    auto ownership =
        getActualValueOwnership((serialization::ValueOwnership)rawOwnership);
    if (!ownership) {
      error();
      return nullptr;
    }

    auto underlyingTy = getTypeChecked(underlyingID);
    if (!underlyingTy)
      return underlyingTy.takeError();

    typeOrOffset = ParenType::get(
        ctx, underlyingTy.get()->getInOutObjectType(),
        ParameterTypeFlags(isVariadic, isAutoClosure, isEscaping, *ownership));
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
      unsigned rawOwnership;
      decls_block::TupleTypeEltLayout::readRecord(scratch, nameID, typeID,
                                                  isVariadic, isAutoClosure,
                                                  isEscaping, rawOwnership);

      auto ownership =
          getActualValueOwnership((serialization::ValueOwnership)rawOwnership);
      if (!ownership) {
        error();
        return nullptr;
      }

      auto elementTy = getTypeChecked(typeID);
      if (!elementTy)
        return elementTy.takeError();

      elements.emplace_back(elementTy.get()->getInOutObjectType(),
                            getIdentifier(nameID),
                            ParameterTypeFlags(isVariadic, isAutoClosure,
                                               isEscaping, *ownership));
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
    
    auto info = FunctionType::ExtInfo(*representation, autoClosure, noescape,
                                      throws);

    auto inputTy = getTypeChecked(inputID);
    if (!inputTy)
      return inputTy.takeError();
    auto resultTy = getTypeChecked(resultID);
    if (!resultTy)
      return resultTy.takeError();

    typeOrOffset = FunctionType::get(inputTy.get(), resultTy.get(), info);
    break;
  }

  case decls_block::EXISTENTIAL_METATYPE_TYPE: {
    TypeID instanceID;
    uint8_t repr;
    decls_block::ExistentialMetatypeTypeLayout::readRecord(scratch,
                                                           instanceID, repr);
    auto instanceType = getTypeChecked(instanceID);
    if (!instanceType)
      return instanceType.takeError();

    switch (repr) {
    case serialization::MetatypeRepresentation::MR_None:
      typeOrOffset = ExistentialMetatypeType::get(instanceType.get());
      break;

    case serialization::MetatypeRepresentation::MR_Thin:
      error();
      break;

    case serialization::MetatypeRepresentation::MR_Thick:
      typeOrOffset = ExistentialMetatypeType::get(instanceType.get(),
                                       MetatypeRepresentation::Thick);
      break;

    case serialization::MetatypeRepresentation::MR_ObjC:
      typeOrOffset = ExistentialMetatypeType::get(instanceType.get(),
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

    auto instanceType = getTypeChecked(instanceID);
    if (!instanceType)
      return instanceType.takeError();

    switch (repr) {
    case serialization::MetatypeRepresentation::MR_None:
      typeOrOffset = MetatypeType::get(instanceType.get());
      break;

    case serialization::MetatypeRepresentation::MR_Thin:
      typeOrOffset = MetatypeType::get(instanceType.get(),
                                       MetatypeRepresentation::Thin);
      break;

    case serialization::MetatypeRepresentation::MR_Thick:
      typeOrOffset = MetatypeType::get(instanceType.get(),
                                       MetatypeRepresentation::Thick);
      break;

    case serialization::MetatypeRepresentation::MR_ObjC:
      typeOrOffset = MetatypeType::get(instanceType.get(),
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

  case decls_block::INOUT_TYPE: {
    TypeID objectTypeID;
    decls_block::InOutTypeLayout::readRecord(scratch, objectTypeID);

    auto objectTy = getTypeChecked(objectTypeID);
    if (!objectTy)
      return objectTy.takeError();

    typeOrOffset = InOutType::get(objectTy.get());
    break;
  }

  case decls_block::REFERENCE_STORAGE_TYPE: {
    uint8_t rawOwnership;
    TypeID objectTypeID;
    decls_block::ReferenceStorageTypeLayout::readRecord(scratch, rawOwnership,
                                                        objectTypeID);

    auto ownership = getActualReferenceOwnership(
        (serialization::ReferenceOwnership)rawOwnership);
    if (!ownership.hasValue()) {
      error();
      break;
    }

    auto objectTy = getTypeChecked(objectTypeID);
    if (!objectTy)
      return objectTy.takeError();

    typeOrOffset = ReferenceStorageType::get(objectTy.get(),
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
    bool hasExplicitAnyObject;
    ArrayRef<uint64_t> rawProtocolIDs;

    decls_block::ProtocolCompositionTypeLayout::readRecord(scratch,
                                                           hasExplicitAnyObject,
                                                           rawProtocolIDs);
    SmallVector<Type, 4> protocols;
    for (TypeID protoID : rawProtocolIDs) {
      auto protoTy = getTypeChecked(protoID);
      if (!protoTy)
        return protoTy.takeError();
      protocols.push_back(protoTy.get());
    }

    typeOrOffset = ProtocolCompositionType::get(ctx, protocols,
                                                hasExplicitAnyObject);
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

    auto nominalOrError = getDeclChecked(declID);
    if (!nominalOrError)
      return nominalOrError.takeError();
    auto nominal = cast<NominalTypeDecl>(nominalOrError.get());

    // FIXME: Check this?
    auto parentTy = getType(parentID);

    SmallVector<Type, 8> genericArgs;
    for (TypeID ID : rawArgumentIDs) {
      auto argTy = getTypeChecked(ID);
      if (!argTy)
        return argTy.takeError();

      genericArgs.push_back(argTy.get());
    }

    auto boundTy = BoundGenericType::get(nominal, parentTy, genericArgs);
    typeOrOffset = boundTy;
    break;
  }

  case decls_block::GENERIC_FUNCTION_TYPE: {
    TypeID inputID;
    TypeID resultID;
    uint8_t rawRep;
    bool throws = false;
    GenericSignatureID rawGenericSig;

    decls_block::GenericFunctionTypeLayout::readRecord(scratch,
                                                       inputID,
                                                       resultID,
                                                       rawRep,
                                                       throws,
                                                       rawGenericSig);
    auto rep = getActualFunctionTypeRepresentation(rawRep);
    if (!rep.hasValue()) {
      error();
      return nullptr;
    }

    auto sig = getGenericSignature(rawGenericSig);
    auto info = GenericFunctionType::ExtInfo(*rep, throws);
    
    auto inputTy = getTypeChecked(inputID);
    if (!inputTy)
      return inputTy.takeError();
    auto resultTy = getTypeChecked(resultID);
    if (!resultTy)
      return resultTy.takeError();

    typeOrOffset = GenericFunctionType::get(sig, inputTy.get(), resultTy.get(),
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
    SubstitutionMapID subMapID;
    decls_block::SILBoxTypeLayout::readRecord(scratch, layoutID, subMapID);
    
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

    auto subMap = getSubstitutionMap(subMapID);
    typeOrOffset = SILBoxType::get(getContext(), layout, subMap);
    break;
  }
      
  case decls_block::SIL_FUNCTION_TYPE: {
    uint8_t rawCoroutineKind;
    uint8_t rawCalleeConvention;
    uint8_t rawRepresentation;
    bool pseudogeneric = false;
    bool noescape;
    bool hasErrorResult;
    unsigned numParams;
    unsigned numYields;
    unsigned numResults;
    GenericSignatureID rawGenericSig;
    ArrayRef<uint64_t> variableData;

    decls_block::SILFunctionTypeLayout::readRecord(scratch,
                                             rawCoroutineKind,
                                             rawCalleeConvention,
                                             rawRepresentation,
                                             pseudogeneric,
                                             noescape,
                                             hasErrorResult,
                                             numParams,
                                             numYields,
                                             numResults,
                                             rawGenericSig,
                                             variableData);

    // Process the ExtInfo.
    auto representation
      = getActualSILFunctionTypeRepresentation(rawRepresentation);
    if (!representation.hasValue()) {
      error();
      return nullptr;
    }
    SILFunctionType::ExtInfo extInfo(*representation, pseudogeneric, noescape);

    // Process the coroutine kind.
    auto coroutineKind = getActualSILCoroutineKind(rawCoroutineKind);
    if (!coroutineKind.hasValue()) {
      error();
      return nullptr;
    }

    // Process the callee convention.
    auto calleeConvention = getActualParameterConvention(rawCalleeConvention);
    if (!calleeConvention.hasValue()) {
      error();
      return nullptr;
    }

    auto processParameter = [&](TypeID typeID, uint64_t rawConvention)
                                  -> llvm::Expected<SILParameterInfo> {
      auto convention = getActualParameterConvention(rawConvention);
      if (!convention) {
        error();
        llvm_unreachable("an error is a fatal exit at this point");
      }
      auto type = getTypeChecked(typeID);
      if (!type)
        return type.takeError();
      return SILParameterInfo(type.get()->getCanonicalType(), *convention);
    };

    auto processYield = [&](TypeID typeID, uint64_t rawConvention)
                                  -> llvm::Expected<SILYieldInfo> {
      auto convention = getActualParameterConvention(rawConvention);
      if (!convention) {
        error();
        llvm_unreachable("an error is a fatal exit at this point");
      }
      auto type = getTypeChecked(typeID);
      if (!type)
        return type.takeError();
      return SILYieldInfo(type.get()->getCanonicalType(), *convention);
    };

    auto processResult = [&](TypeID typeID, uint64_t rawConvention)
                               -> llvm::Expected<SILResultInfo> {
      auto convention = getActualResultConvention(rawConvention);
      if (!convention) {
        error();
        llvm_unreachable("an error is a fatal exit at this point");
      }
      auto type = getTypeChecked(typeID);
      if (!type)
        return type.takeError();
      return SILResultInfo(type.get()->getCanonicalType(), *convention);
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
      if (!param)
        return param.takeError();
      allParams.push_back(param.get());
    }

    // Process the yields.
    SmallVector<SILYieldInfo, 8> allYields;
    allYields.reserve(numYields);
    for (unsigned i = 0; i != numYields; ++i) {
      auto typeID = variableData[nextVariableDataIndex++];
      auto rawConvention = variableData[nextVariableDataIndex++];
      auto yield = processYield(typeID, rawConvention);
      if (!yield)
        return yield.takeError();
      allYields.push_back(yield.get());
    }

    // Process the results.
    SmallVector<SILResultInfo, 8> allResults;
    allParams.reserve(numResults);
    for (unsigned i = 0; i != numResults; ++i) {
      auto typeID = variableData[nextVariableDataIndex++];
      auto rawConvention = variableData[nextVariableDataIndex++];
      auto result = processResult(typeID, rawConvention);
      if (!result)
        return result.takeError();
      allResults.push_back(result.get());
    }

    // Process the error result.
    Optional<SILResultInfo> errorResult;
    if (hasErrorResult) {
      auto typeID = variableData[nextVariableDataIndex++];
      auto rawConvention = variableData[nextVariableDataIndex++];
      auto maybeErrorResult = processResult(typeID, rawConvention);
      if (!maybeErrorResult)
        return maybeErrorResult.takeError();
      errorResult = maybeErrorResult.get();
    }

    Optional<ProtocolConformanceRef> witnessMethodConformance;
    if (*representation == SILFunctionTypeRepresentation::WitnessMethod) {
      witnessMethodConformance = readConformance(DeclTypeCursor);
    }

    GenericSignature *genericSig = getGenericSignature(rawGenericSig);

    typeOrOffset = SILFunctionType::get(genericSig, extInfo,
                                        coroutineKind.getValue(),
                                        calleeConvention.getValue(),
                                        allParams, allYields, allResults,
                                        errorResult,
                                        ctx, witnessMethodConformance);
    break;
  }

  case decls_block::ARRAY_SLICE_TYPE: {
    TypeID baseID;
    decls_block::ArraySliceTypeLayout::readRecord(scratch, baseID);

    auto baseTy = getTypeChecked(baseID);
    if (!baseTy)
      return baseTy.takeError();

    typeOrOffset = ArraySliceType::get(baseTy.get());
    break;
  }

  case decls_block::DICTIONARY_TYPE: {
    TypeID keyID, valueID;
    decls_block::DictionaryTypeLayout::readRecord(scratch, keyID, valueID);

    auto keyTy = getTypeChecked(keyID);
    if (!keyTy)
      return keyTy.takeError();

    auto valueTy = getTypeChecked(valueID);
    if (!valueTy)
      return valueTy.takeError();

    typeOrOffset = DictionaryType::get(keyTy.get(), valueTy.get());
    break;
  }

  case decls_block::OPTIONAL_TYPE: {
    TypeID baseID;
    decls_block::OptionalTypeLayout::readRecord(scratch, baseID);

    auto baseTy = getTypeChecked(baseID);
    if (!baseTy)
      return baseTy.takeError();

    typeOrOffset = OptionalType::get(baseTy.get());
    break;
  }

  case decls_block::UNBOUND_GENERIC_TYPE: {
    DeclID genericID;
    TypeID parentID;
    decls_block::UnboundGenericTypeLayout::readRecord(scratch,
                                                      genericID, parentID);

    auto nominalOrError = getDeclChecked(genericID);
    if (!nominalOrError)
      return nominalOrError.takeError();
    auto genericDecl = cast<GenericTypeDecl>(nominalOrError.get());

    // FIXME: Check this?
    auto parentTy = getType(parentID);

    typeOrOffset = UnboundGenericType::get(genericDecl, parentTy, ctx);
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

Decl *handleErrorAndSupplyMissingClassMember(ASTContext &context,
                                             llvm::Error &&error,
                                             ClassDecl *containingClass) {
  Decl *suppliedMissingMember = nullptr;
  auto handleMissingClassMember = [&](const DeclDeserializationError &error) {
    if (error.isDesignatedInitializer())
      containingClass->setHasMissingDesignatedInitializers();
    if (error.needsVTableEntry() || error.needsAllocatingVTableEntry())
      containingClass->setHasMissingVTableEntries();

    if (error.getName().getBaseName() == DeclBaseName::createConstructor()) {
      suppliedMissingMember = MissingMemberDecl::forInitializer(
          context, containingClass, error.getName(), error.needsVTableEntry(),
          error.needsAllocatingVTableEntry());
    } else if (error.needsVTableEntry()) {
      suppliedMissingMember = MissingMemberDecl::forMethod(
          context, containingClass, error.getName(), error.needsVTableEntry());
    } else if (error.needsFieldOffsetVectorEntry()) {
      suppliedMissingMember = MissingMemberDecl::forStoredProperty(
          context, containingClass, error.getName());
    }
    // FIXME: Handle other kinds of missing members: properties,
    // subscripts, and methods that don't need vtable entries.
  };
  llvm::handleAllErrors(std::move(error), handleMissingClassMember);
  return suppliedMissingMember;
}

Decl *handleErrorAndSupplyMissingProtoMember(ASTContext &context,
                                             llvm::Error &&error,
                                             ProtocolDecl *containingProto) {
  Decl *suppliedMissingMember = nullptr;

  auto handleMissingProtocolMember =
      [&](const DeclDeserializationError &error) {
        assert(!error.needsAllocatingVTableEntry());
        if (error.needsVTableEntry())
          containingProto->setHasMissingRequirements(true);

        if (error.getName().getBaseName() == DeclBaseName::createConstructor()) {
          suppliedMissingMember = MissingMemberDecl::forInitializer(
              context, containingProto, error.getName(),
              error.needsVTableEntry(), error.needsAllocatingVTableEntry());
              return;
        }
        if (error.needsVTableEntry()) {
          suppliedMissingMember = MissingMemberDecl::forMethod(
              context, containingProto, error.getName(),
              error.needsVTableEntry());
        }
        // FIXME: Handle other kinds of missing members: properties,
        // subscripts, and methods that don't need vtable entries.
      };
  llvm::handleAllErrors(std::move(error), handleMissingProtocolMember);
  return suppliedMissingMember;
}

Decl *handleErrorAndSupplyMissingMiscMember(llvm::Error &&error) {
  llvm::consumeError(std::move(error));
  return nullptr;
}

Decl *handleErrorAndSupplyMissingMember(ASTContext &context, Decl *container,
                                        llvm::Error &&error) {
  // Drop the member if it had a problem.
  // FIXME: Handle overridable members in class extensions too, someday.
  if (auto *containingClass = dyn_cast<ClassDecl>(container)) {
    return handleErrorAndSupplyMissingClassMember(context, std::move(error),
                                                  containingClass);
  }
  if (auto *containingProto = dyn_cast<ProtocolDecl>(container)) {
    return handleErrorAndSupplyMissingProtoMember(context, std::move(error),
                                                  containingProto);
  }
  return handleErrorAndSupplyMissingMiscMember(std::move(error));
}

void ModuleFile::loadAllMembers(Decl *container, uint64_t contextData) {
  PrettyStackTraceDecl trace("loading members for", container);
  ++NumMemberListsLoaded;

  IterableDeclContext *IDC;
  if (auto *nominal = dyn_cast<NominalTypeDecl>(container))
    IDC = nominal;
  else
    IDC = cast<ExtensionDecl>(container);

  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(contextData);
  auto entry = DeclTypeCursor.advance();
  if (entry.Kind != llvm::BitstreamEntry::Record) {
    error();
    return;
  }

  SmallVector<uint64_t, 16> memberIDBuffer;

  unsigned kind = DeclTypeCursor.readRecord(entry.ID, memberIDBuffer);
  assert(kind == decls_block::MEMBERS);
  (void)kind;

  ArrayRef<uint64_t> rawMemberIDs;
  decls_block::MembersLayout::readRecord(memberIDBuffer, rawMemberIDs);

  if (rawMemberIDs.empty())
    return;

  SmallVector<Decl *, 16> members;
  members.reserve(rawMemberIDs.size());
  for (DeclID rawID : rawMemberIDs) {
    Expected<Decl *> next = getDeclChecked(rawID);
    if (next) {
      assert(next.get() && "unchecked error deserializing next member");
      members.push_back(next.get());
    } else {
      if (!getContext().LangOpts.EnableDeserializationRecovery)
        fatal(next.takeError());

      Decl *suppliedMissingMember = handleErrorAndSupplyMissingMember(
          getContext(), container, next.takeError());
      if (suppliedMissingMember)
        members.push_back(suppliedMissingMember);
    }
  }

  for (auto member : members)
    IDC->addMember(member);

  if (auto *proto = dyn_cast<ProtocolDecl>(container)) {
    PrettyStackTraceDecl trace("reading default witness table for", proto);
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

  PrettyStackTraceModuleFile traceModule("While reading from", *this);
  PrettyStackTraceType trace(getAssociatedModule()->getASTContext(),
                             "finishing conformance for",
                             conformance->getType());
  PrettyStackTraceDecl traceTo("... to", conformance->getProtocol());
  ++NumNormalProtocolConformancesCompleted;

  assert(conformance->isComplete());

  conformance->setState(ProtocolConformanceState::Incomplete);
  SWIFT_DEFER { conformance->setState(ProtocolConformanceState::Complete); };

  // Find the conformance record.
  BCOffsetRAII restoreOffset(DeclTypeCursor);
  DeclTypeCursor.JumpToBit(contextData);
  auto entry = DeclTypeCursor.advance();
  assert(entry.Kind == llvm::BitstreamEntry::Record &&
         "registered lazy loader incorrectly");

  DeclID protoID;
  DeclContextID contextID;
  unsigned valueCount, typeCount, conformanceCount;
  ArrayRef<uint64_t> rawIDs;
  SmallVector<uint64_t, 16> scratch;

  unsigned kind = DeclTypeCursor.readRecord(entry.ID, scratch);
  (void) kind;
  assert(kind == NORMAL_PROTOCOL_CONFORMANCE &&
         "registered lazy loader incorrectly");
  NormalProtocolConformanceLayout::readRecord(scratch, protoID,
                                              contextID, typeCount,
                                              valueCount, conformanceCount,
                                              rawIDs);

  // Read requirement signature conformances.
  const ProtocolDecl *proto = conformance->getProtocol();
  SmallVector<ProtocolConformanceRef, 4> reqConformances;

  if (proto->isObjC() && getContext().LangOpts.EnableDeserializationRecovery) {
    // Don't crash if inherited protocols are added or removed.
    // This is limited to Objective-C protocols because we know their only
    // conformance requirements are on Self. This isn't actually a /safe/ change
    // even in Objective-C, but we mostly just don't want to crash.

    // FIXME: DenseMap requires that its value type be default-constructible,
    // which ProtocolConformanceRef is not, hence the extra Optional.
    llvm::SmallDenseMap<ProtocolDecl *, Optional<ProtocolConformanceRef>, 16>
        conformancesForProtocols;
    while (conformanceCount--) {
      ProtocolConformanceRef nextConformance = readConformance(DeclTypeCursor);
      ProtocolDecl *confProto = nextConformance.getRequirement();
      conformancesForProtocols[confProto] = nextConformance;
    }

    for (const auto &req : proto->getRequirementSignature()) {
      if (req.getKind() != RequirementKind::Conformance)
        continue;
      ProtocolDecl *proto =
          req.getSecondType()->castTo<ProtocolType>()->getDecl();
      auto iter = conformancesForProtocols.find(proto);
      if (iter != conformancesForProtocols.end()) {
        reqConformances.push_back(iter->getSecond().getValue());
      } else {
        // Put in an abstract conformance as a placeholder. This is a lie, but
        // there's not much better we can do. We're relying on the fact that
        // the rest of the compiler doesn't actually need to check the
        // conformance to an Objective-C protocol for anything important.
        // There are no associated types and we don't emit a Swift conformance
        // record.
        reqConformances.push_back(ProtocolConformanceRef(proto));
      }
    }

  } else {
    auto isConformanceReq = [](const Requirement &req) {
      return req.getKind() == RequirementKind::Conformance;
    };
    if (conformanceCount != llvm::count_if(proto->getRequirementSignature(),
                                           isConformanceReq)) {
      fatal(llvm::make_error<llvm::StringError>(
          "serialized conformances do not match requirement signature",
          llvm::inconvertibleErrorCode()));
    }
    while (conformanceCount--)
      reqConformances.push_back(readConformance(DeclTypeCursor));
  }
  conformance->setSignatureConformances(reqConformances);

  ArrayRef<uint64_t>::iterator rawIDIter = rawIDs.begin();

  TypeWitnessMap typeWitnesses;
  while (typeCount--) {
    // FIXME: We don't actually want to allocate an archetype here; we just
    // want to get an access path within the protocol.
    auto first = cast<AssociatedTypeDecl>(getDecl(*rawIDIter++));
    auto second = getType(*rawIDIter++);
    auto third = cast_or_null<TypeDecl>(getDecl(*rawIDIter++));
    if (third &&
        isa<TypeAliasDecl>(third) &&
        third->getModuleContext() != getAssociatedModule() &&
        !third->getDeclaredInterfaceType()->isEqual(second)) {
      // Conservatively drop references to typealiases in other modules
      // that may have changed. This may also drop references to typealiases
      // that /haven't/ changed but just happen to have generics in them, but
      // in practice having a declaration here isn't actually required by the
      // rest of the compiler.
      third = nullptr;
    }
    typeWitnesses[first] = std::make_pair(second, third);
  }
  assert(rawIDIter <= rawIDs.end() && "read too much");

  // Set type witnesses.
  for (auto typeWitness : typeWitnesses) {
    conformance->setTypeWitness(typeWitness.first, typeWitness.second.first,
                                typeWitness.second.second);
  }

  // An imported requirement may have changed type between Swift versions.
  // In this situation we need to do a post-pass to fill in missing
  // requirements with opaque witnesses.
  bool needToFillInOpaqueValueWitnesses = false;
  while (valueCount--) {
    ValueDecl *req;
    
    auto trySetWitness = [&](Witness w) {
      if (req)
        conformance->setWitness(req, w);
    };
    
    auto deserializedReq = getDeclChecked(*rawIDIter++);
    if (deserializedReq) {
      req = cast_or_null<ValueDecl>(*deserializedReq);
    } else if (getContext().LangOpts.EnableDeserializationRecovery) {
      consumeError(deserializedReq.takeError());
      req = nullptr;
      needToFillInOpaqueValueWitnesses = true;
    } else {
      fatal(deserializedReq.takeError());
    }
    
    bool isOpaque = false;
    ValueDecl *witness;
    auto deserializedWitness = getDeclChecked(*rawIDIter++);
    if (deserializedWitness) {
      witness = cast_or_null<ValueDecl>(*deserializedWitness);
    // Across language compatibility versions, the witnessing decl may have
    // changed its signature as seen by the current compatibility version.
    // In that case, we want the conformance to still be available, but
    // we can't make use of the relationship to the underlying decl.
    } else if (getContext().LangOpts.EnableDeserializationRecovery) {
      consumeError(deserializedWitness.takeError());
      isOpaque = true;
      witness = nullptr;
    } else {
      fatal(deserializedWitness.takeError());
    }
    
    assert(!req || isOpaque || witness ||
           req->getAttrs().hasAttribute<OptionalAttr>() ||
           req->getAttrs().isUnavailable(getContext()));
    if (!witness && !isOpaque) {
      trySetWitness(Witness());
      continue;
    }

    // Generic environment.
    GenericEnvironment *syntheticEnv = nullptr;
    
    auto trySetOpaqueWitness = [&]{
      if (!req)
        return;
      
      // We shouldn't yet need to worry about generic requirements, since
      // an imported ObjC method should never be generic.
      assert(syntheticEnv == nullptr &&
             "opaque witness shouldn't be generic yet. when this is "
             "possible, it should use forwarding substitutions");
      conformance->setWitness(req, Witness::forOpaque(req));
    };

    // Requirement -> synthetic map.
    if (auto syntheticSig = getGenericSignature(*rawIDIter++)) {
      // Create the synthetic environment.
      syntheticEnv = syntheticSig->createGenericEnvironment();
    }

    // Requirement -> synthetic substitutions.
    SubstitutionMap reqToSyntheticSubs = getSubstitutionMap(*rawIDIter++);

    // Witness substitutions.
    SubstitutionMap witnessSubstitutions = getSubstitutionMap(*rawIDIter++);

    // Handle opaque witnesses that couldn't be deserialized.
    if (isOpaque) {
      trySetOpaqueWitness();
      continue;
    }

    // Set the witness.
    trySetWitness(Witness(witness, witnessSubstitutions,
                          syntheticEnv, reqToSyntheticSubs));
  }
  assert(rawIDIter <= rawIDs.end() && "read too much");
  
  // Fill in opaque value witnesses if we need to.
  if (needToFillInOpaqueValueWitnesses) {
    for (auto member : proto->getMembers()) {
      // We only care about non-associated-type requirements.
      auto valueMember = dyn_cast<ValueDecl>(member);
      if (!valueMember || !valueMember->isProtocolRequirement()
          || isa<AssociatedTypeDecl>(valueMember))
        continue;
      
      if (!conformance->hasWitness(valueMember))
        conformance->setWitness(valueMember, Witness::forOpaque(valueMember));
    }
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
