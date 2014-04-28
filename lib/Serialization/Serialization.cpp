//===--- Serialization.cpp - Read and write Swift modules -----------------===//
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

#include "swift/Serialization/ModuleFormat.h"
#include "Serialization.h"
#include "SILFormat.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/RawComment.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Dwarf.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Serialization/BCRecordLayout.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Config/config.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/OnDiskHashTable.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::serialization;
using namespace llvm::support;

namespace {
  /// Used to serialize the on-disk decl hash table.
  class DeclTableInfo {
  public:
    using key_type = Identifier;
    using key_type_ref = key_type;
    using data_type = Serializer::DeclTableData;
    using data_type_ref = const data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      assert(!key.empty());
      return llvm::HashString(key.str());
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = key.str().size();
      uint32_t dataLength = (sizeof(DeclID) + 1) * data.size();
      endian::Writer<little> writer(out);
      writer.write<uint16_t>(keyLength);
      writer.write<uint16_t>(dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      out << key.str();
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(sizeof(DeclID) <= 4, "DeclID too large");
      endian::Writer<little> writer(out);
      for (auto entry : data) {
        writer.write<uint8_t>(entry.first);
        writer.write<uint32_t>(entry.second);
      }
    }
  };

} // end anonymous namespace

namespace llvm {
  template<> struct DenseMapInfo<Serializer::DeclTypeUnion> {
    using DeclTypeUnion = Serializer::DeclTypeUnion;
    static inline DeclTypeUnion getEmptyKey() { return nullptr; }
    static inline DeclTypeUnion getTombstoneKey() { return swift::Type(); }
    static unsigned getHashValue(const DeclTypeUnion &val) {
      return DenseMapInfo<const void *>::getHashValue(val.getOpaqueValue());
    }
    static bool isEqual(const DeclTypeUnion &lhs, const DeclTypeUnion &rhs) {
      return lhs == rhs;
    }
  };
}

static Module *getModule(ModuleOrSourceFile DC) {
  if (auto M = DC.dyn_cast<Module *>())
    return M;
  return DC.get<SourceFile *>()->getParentModule();
}

static ASTContext &getContext(ModuleOrSourceFile DC) {
  return getModule(DC)->Ctx;
}

static const Decl *getDeclForContext(const DeclContext *DC) {
  switch (DC->getContextKind()) {
  case DeclContextKind::Module:
    // Use a null decl to represent the module.
    return nullptr;
  case DeclContextKind::FileUnit:
    return getDeclForContext(DC->getParent());
  case DeclContextKind::Initializer:
  case DeclContextKind::AbstractClosureExpr:
    // FIXME: What about default functions?
    llvm_unreachable("shouldn't serialize decls from anonymous closures");
  case DeclContextKind::NominalTypeDecl:
    return cast<NominalTypeDecl>(DC);
  case DeclContextKind::ExtensionDecl:
    return cast<ExtensionDecl>(DC);
  case DeclContextKind::TopLevelCodeDecl:
    llvm_unreachable("shouldn't serialize the main module");
  case DeclContextKind::AbstractFunctionDecl:
    return cast<AbstractFunctionDecl>(DC);
  }
}

DeclID Serializer::addDeclRef(const Decl *D, bool forceSerialization) {
  if (!D)
    return 0;

  DeclIDAndForce &id = DeclIDs[D];
  if (id.first != 0) {
    if (forceSerialization && !id.second)
      id.second = true;
    return id.first;
  }

  // Record any generic parameters that come from this decl, so that we can use
  // the decl to refer to the parameters later.
  const GenericParamList *paramList = nullptr;
  switch (D->getKind()) {
  case DeclKind::Constructor:
    paramList = cast<ConstructorDecl>(D)->getGenericParams();
    break;
  case DeclKind::Func:
    paramList = cast<FuncDecl>(D)->getGenericParams();
    break;
  case DeclKind::Class:
  case DeclKind::Struct:
  case DeclKind::Enum:
  case DeclKind::Protocol:
    paramList = cast<NominalTypeDecl>(D)->getGenericParams();
    break;
  default:
    break;
  }
  if (paramList)
    GenericContexts[paramList] = D;

  id = { ++LastDeclID, forceSerialization };
  DeclsAndTypesToWrite.push(D);
  return id.first;
}

TypeID Serializer::addTypeRef(Type ty) {
  if (!ty)
    return 0;

  auto &id = DeclIDs[ty];
  if (id.first != 0)
    return id.first;

  id = { ++LastTypeID, true };
  DeclsAndTypesToWrite.push(ty);
  return id.first;
}

IdentifierID Serializer::addIdentifierRef(Identifier ident) {
  if (ident.empty())
    return 0;

  IdentifierID &id = IdentifierIDs[ident];
  if (id != 0)
    return id;

  id = ++LastIdentifierID;
  IdentifiersToWrite.push_back(ident);
  return id;
}

IdentifierID Serializer::addModuleRef(const Module *M) {
  if (M == this->M->Ctx.TheBuiltinModule)
    return BUILTIN_MODULE_ID;
  if (M == this->M)
    return CURRENT_MODULE_ID;

  assert(!M->Name.empty());
  return addIdentifierRef(M->Name);
}

const Decl *Serializer::getGenericContext(const GenericParamList *paramList) {
  auto contextDecl = GenericContexts.lookup(paramList);
  return contextDecl;
}

/// Record the name of a block.
static void emitBlockID(llvm::BitstreamWriter &out, unsigned ID,
                        StringRef name,
                        SmallVectorImpl<unsigned char> &nameBuffer) {
  SmallVector<unsigned, 1> idBuffer;
  idBuffer.push_back(ID);
  out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETBID, idBuffer);

  // Emit the block name if present.
  if (name.empty())
    return;
  nameBuffer.resize(name.size());
  memcpy(nameBuffer.data(), name.data(), name.size());
  out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_BLOCKNAME, nameBuffer);
}

/// Record the name of a record within a block.
static void emitRecordID(llvm::BitstreamWriter &out, unsigned ID,
                         StringRef name,
                         SmallVectorImpl<unsigned char> &nameBuffer) {
  assert(ID < 256 && "can't fit record ID in next to name");
  nameBuffer.resize(name.size()+1);
  nameBuffer[0] = ID;
  memcpy(nameBuffer.data()+1, name.data(), name.size());
  out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETRECORDNAME, nameBuffer);
}

void Serializer::writeBlockInfoBlock() {
  BCBlockRAII restoreBlock(Out, llvm::bitc::BLOCKINFO_BLOCK_ID, 2);

  SmallVector<unsigned char, 64> nameBuffer;
#define BLOCK(X) emitBlockID(Out, X ## _ID, #X, nameBuffer)
#define BLOCK_RECORD(K, X) emitRecordID(Out, K::X, #X, nameBuffer)

  BLOCK(MODULE_BLOCK);

  BLOCK(CONTROL_BLOCK);
  BLOCK_RECORD(control_block, METADATA);
  BLOCK_RECORD(control_block, MODULE_NAME);

  BLOCK(INPUT_BLOCK);
  BLOCK_RECORD(input_block, SOURCE_FILE);
  BLOCK_RECORD(input_block, IMPORTED_MODULE);
  BLOCK_RECORD(input_block, LINK_LIBRARY);

  BLOCK(DECLS_AND_TYPES_BLOCK);
#define RECORD(X) BLOCK_RECORD(decls_block, X);
#include "swift/Serialization/DeclTypeRecordNodes.def"

  BLOCK(IDENTIFIER_DATA_BLOCK);
  BLOCK_RECORD(identifier_block, IDENTIFIER_DATA);

  BLOCK(INDEX_BLOCK);
  BLOCK_RECORD(index_block, TYPE_OFFSETS);
  BLOCK_RECORD(index_block, DECL_OFFSETS);
  BLOCK_RECORD(index_block, IDENTIFIER_OFFSETS);
  BLOCK_RECORD(index_block, TOP_LEVEL_DECLS);
  BLOCK_RECORD(index_block, OPERATORS);
  BLOCK_RECORD(index_block, EXTENSIONS);
  BLOCK_RECORD(index_block, CLASS_MEMBERS);
  BLOCK_RECORD(index_block, OPERATOR_METHODS);

  BLOCK(SIL_BLOCK);
  BLOCK_RECORD(sil_block, SIL_FUNCTION);
  BLOCK_RECORD(sil_block, SIL_BASIC_BLOCK);
  BLOCK_RECORD(sil_block, SIL_ONE_VALUE_ONE_OPERAND);
  BLOCK_RECORD(sil_block, SIL_ONE_TYPE);
  BLOCK_RECORD(sil_block, SIL_ONE_OPERAND);
  BLOCK_RECORD(sil_block, SIL_ONE_TYPE_ONE_OPERAND);
  BLOCK_RECORD(sil_block, SIL_ONE_TYPE_VALUES);
  BLOCK_RECORD(sil_block, SIL_TWO_OPERANDS);
  BLOCK_RECORD(sil_block, SIL_INST_APPLY);
  BLOCK_RECORD(sil_block, SIL_INST_NO_OPERAND);
  BLOCK_RECORD(sil_block, SIL_VTABLE);
  BLOCK_RECORD(sil_block, SIL_VTABLE_ENTRY);
  BLOCK_RECORD(sil_block, SIL_GLOBALVAR);
  BLOCK_RECORD(sil_block, SIL_INST_CAST);
  BLOCK_RECORD(sil_block, SIL_INIT_EXISTENTIAL);
  BLOCK_RECORD(sil_block, SIL_WITNESSTABLE);
  BLOCK_RECORD(sil_block, SIL_WITNESS_METHOD_ENTRY);
  BLOCK_RECORD(sil_block, SIL_WITNESS_BASE_ENTRY);
  BLOCK_RECORD(sil_block, SIL_WITNESS_ASSOC_PROTOCOL);
  BLOCK_RECORD(sil_block, SIL_WITNESS_ASSOC_ENTRY);
  BLOCK_RECORD(sil_block, SIL_GENERIC_OUTER_PARAMS);

  // These layouts can exist in both decl blocks and sil blocks.
#define BLOCK_RECORD_WITH_NAMESPACE(K, X) emitRecordID(Out, X, #X, nameBuffer)
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::BOUND_GENERIC_SUBSTITUTION);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::NO_CONFORMANCE);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::NORMAL_PROTOCOL_CONFORMANCE);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::SPECIALIZED_PROTOCOL_CONFORMANCE);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::INHERITED_PROTOCOL_CONFORMANCE);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::GENERIC_PARAM_LIST);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::GENERIC_PARAM);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::GENERIC_REQUIREMENT);
  BLOCK_RECORD_WITH_NAMESPACE(sil_block,
                              decls_block::LAST_GENERIC_REQUIREMENT);

  BLOCK(SIL_INDEX_BLOCK);
  BLOCK_RECORD(sil_index_block, SIL_FUNC_NAMES);
  BLOCK_RECORD(sil_index_block, SIL_FUNC_OFFSETS);
  BLOCK_RECORD(sil_index_block, SIL_VTABLE_NAMES);
  BLOCK_RECORD(sil_index_block, SIL_VTABLE_OFFSETS);
  BLOCK_RECORD(sil_index_block, SIL_GLOBALVAR_NAMES);
  BLOCK_RECORD(sil_index_block, SIL_GLOBALVAR_OFFSETS);
  BLOCK_RECORD(sil_index_block, SIL_WITNESSTABLE_NAMES);
  BLOCK_RECORD(sil_index_block, SIL_WITNESSTABLE_OFFSETS);

  BLOCK(KNOWN_PROTOCOL_BLOCK);
#define PROTOCOL(Id) BLOCK_RECORD(index_block, Id);
#include "swift/AST/KnownProtocols.def"

#undef BLOCK
#undef BLOCK_RECORD
}

void Serializer::writeDocBlockInfoBlock() {
  BCBlockRAII restoreBlock(Out, llvm::bitc::BLOCKINFO_BLOCK_ID, 2);

  SmallVector<unsigned char, 64> nameBuffer;
#define BLOCK(X) emitBlockID(Out, X ## _ID, #X, nameBuffer)
#define BLOCK_RECORD(K, X) emitRecordID(Out, K::X, #X, nameBuffer)

  BLOCK(MODULE_DOC_BLOCK);

  BLOCK(CONTROL_BLOCK);
  BLOCK_RECORD(control_block, METADATA);
  BLOCK_RECORD(control_block, MODULE_NAME);

  BLOCK(COMMENT_BLOCK);
  BLOCK_RECORD(comment_block, DECL_COMMENTS);

#undef BLOCK
#undef BLOCK_RECORD
}

void Serializer::writeHeader(const Module *M) {
  {
    BCBlockRAII restoreBlock(Out, CONTROL_BLOCK_ID, 3);
    control_block::ModuleNameLayout ModuleName(Out);
    control_block::MetadataLayout Metadata(Out);

    ModuleName.emit(ScratchRecord, M->Name.str());

    // FIXME: put a real version in here.
#ifdef LLVM_VERSION_INFO
# define EXTRA_VERSION_STRING PACKAGE_STRING LLVM_VERSION_INFO
#else
# define EXTRA_VERSION_STRING PACKAGE_STRING
#endif
    Metadata.emit(ScratchRecord,
                  VERSION_MAJOR, VERSION_MINOR, EXTRA_VERSION_STRING);
#undef EXTRA_VERSION_STRING
  }
}

static void
removeDuplicateImports(SmallVectorImpl<Module::ImportedModule> &imports) {
  std::sort(imports.begin(), imports.end(),
            [](const Module::ImportedModule &lhs,
               const Module::ImportedModule &rhs) -> bool {
    // Arbitrarily sort by name to get a deterministic order.
    // FIXME: Submodules don't get sorted properly here.
    if (lhs.second != rhs.second)
      return lhs.second->Name.str() < rhs.second->Name.str();
    using AccessPathElem = std::pair<Identifier, SourceLoc>;
    return std::lexicographical_compare(lhs.first.begin(), lhs.first.end(),
                                        rhs.first.begin(), rhs.first.end(),
                                        [](const AccessPathElem &lElem,
                                           const AccessPathElem &rElem) {
      return lElem.first.str() < rElem.first.str();
    });
  });
  auto last = std::unique(imports.begin(), imports.end(),
                          [](const Module::ImportedModule &lhs,
                             const Module::ImportedModule &rhs) -> bool {
    if (lhs.second != rhs.second)
      return false;
    return Module::isSameAccessPath(lhs.first, rhs.first);
  });
  imports.erase(last, imports.end());
}

using ImportPathBlob = llvm::SmallString<64>;
static void flattenImportPath(const Module::ImportedModule &import,
                              ImportPathBlob &out) {
  // FIXME: Submodules?
  out.append(import.second->Name.str());

  if (import.first.empty())
    return;

  out.push_back('\0');
  assert(import.first.size() == 1 && "can only handle top-level decl imports");
  auto accessPathElem = import.first.front();
  out.append(accessPathElem.first.str());
}

void Serializer::writeInputFiles(const Module *M,
                                 FilenamesTy inputFiles,
                                 StringRef moduleLinkName) {
  BCBlockRAII restoreBlock(Out, INPUT_BLOCK_ID, 3);
  input_block::SourceFileLayout SourceFile(Out);
  input_block::ImportedModuleLayout ImportedModule(Out);
  input_block::LinkLibraryLayout LinkLibrary(Out);

  for (auto filename : inputFiles) {
    llvm::SmallString<128> path(filename);

    llvm::error_code err;
    err = llvm::sys::fs::make_absolute(path);
    if (err)
      continue;

    SourceFile.emit(ScratchRecord, path);
  }

  // FIXME: Having to deal with private imports as a superset of public imports
  // is inefficient.
  SmallVector<Module::ImportedModule, 8> publicImports;
  SmallVector<Module::ImportedModule, 8> allImports;
  for (auto file : M->getFiles()) {
    file->getImportedModules(publicImports, Module::ImportFilter::Public);
    file->getImportedModules(allImports, Module::ImportFilter::All);
  }

  llvm::SmallSet<Module::ImportedModule, 8, Module::OrderImportedModules>
    publicImportSet;
  publicImportSet.insert(publicImports.begin(), publicImports.end());

  removeDuplicateImports(allImports);
  for (auto import : allImports) {
    if (import.second == M->Ctx.TheBuiltinModule)
      continue;

    ImportPathBlob importPath;
    flattenImportPath(import, importPath);
    ImportedModule.emit(ScratchRecord, publicImportSet.count(import),
                        importPath);
  }

  if (!moduleLinkName.empty()) {
    LinkLibrary.emit(ScratchRecord, serialization::LibraryKind::Library,
                     moduleLinkName);
  }
}

/// Translate AST default argument kind to the Serialization enum values, which
/// are guaranteed to be stable.
static uint8_t getRawStableDefaultArgumentKind(swift::DefaultArgumentKind kind) {
  switch (kind) {
  case swift::DefaultArgumentKind::None:
    return serialization::DefaultArgumentKind::None;
  case swift::DefaultArgumentKind::Normal:
    return serialization::DefaultArgumentKind::Normal;
  case swift::DefaultArgumentKind::Inherited:
    return serialization::DefaultArgumentKind::Inherited;
  case swift::DefaultArgumentKind::Column:
    return serialization::DefaultArgumentKind::Column;
  case swift::DefaultArgumentKind::File:
    return serialization::DefaultArgumentKind::File;
  case swift::DefaultArgumentKind::Line:
    return serialization::DefaultArgumentKind::Line;
  case swift::DefaultArgumentKind::Function:
    return serialization::DefaultArgumentKind::Function;
  }
}

static uint8_t getRawStableMetatypeRepresentation(AnyMetatypeType *metatype) {
  if (!metatype->hasRepresentation()) {
    return serialization::MetatypeRepresentation::MR_None;
  }

  switch (metatype->getRepresentation()) {
  case swift::MetatypeRepresentation::Thin:
    return serialization::MetatypeRepresentation::MR_Thin;
  case swift::MetatypeRepresentation::Thick:
    return serialization::MetatypeRepresentation::MR_Thick;
  case swift::MetatypeRepresentation::ObjC:
    return serialization::MetatypeRepresentation::MR_ObjC;
  }
  llvm_unreachable("bad representation");
}

void Serializer::writePattern(const Pattern *pattern) {
  using namespace decls_block;

  assert(pattern && "null pattern");
  switch (pattern->getKind()) {
  case PatternKind::Paren: {
    unsigned abbrCode = DeclTypeAbbrCodes[ParenPatternLayout::Code];
    ParenPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   pattern->isImplicit());
    writePattern(cast<ParenPattern>(pattern)->getSubPattern());
    break;
  }
  case PatternKind::Tuple: {
    auto tuple = cast<TuplePattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[TuplePatternLayout::Code];
    TuplePatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(tuple->getType()),
                                   tuple->getNumFields(), tuple->hasVararg(),
                                   tuple->isImplicit());

    abbrCode = DeclTypeAbbrCodes[TuplePatternEltLayout::Code];
    for (auto &elt : tuple->getFields()) {
      // FIXME: Default argument expressions?
      TuplePatternEltLayout::emitRecord(
        Out, ScratchRecord, abbrCode,
        getRawStableDefaultArgumentKind(elt.getDefaultArgKind()));
      writePattern(elt.getPattern());
    }
    break;
  }
  case PatternKind::Named: {
    auto named = cast<NamedPattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[NamedPatternLayout::Code];
    NamedPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addDeclRef(named->getDecl()),
                                   named->isImplicit());
    break;
  }
  case PatternKind::Any: {
    unsigned abbrCode = DeclTypeAbbrCodes[AnyPatternLayout::Code];
    AnyPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addTypeRef(pattern->getType()),
                                 pattern->isImplicit());
    break;
  }
  case PatternKind::Typed: {
    auto typed = cast<TypedPattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[TypedPatternLayout::Code];
    TypedPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(typed->getType()),
                                   typed->isImplicit());
    writePattern(typed->getSubPattern());
    break;
  }
  case PatternKind::Isa: {
    auto isa = cast<IsaPattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[IsaPatternLayout::Code];
    IsaPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addTypeRef(isa->getCastTypeLoc().getType()),
                                 isa->isImplicit());
    break;
  }
  case PatternKind::NominalType: {
    auto nom = cast<NominalTypePattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[NominalTypePatternLayout::Code];
    auto castTy = nom->getCastTypeLoc().getType();
    NominalTypePatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                         addTypeRef(castTy),
                                         nom->getElements().size(),
                                         nom->isImplicit());
    abbrCode = DeclTypeAbbrCodes[NominalTypePatternEltLayout::Code];
    for (auto &elt : nom->getElements()) {
      NominalTypePatternEltLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                              addDeclRef(elt.getProperty()));
      writePattern(elt.getSubPattern());
    }
    break;
  }
  case PatternKind::EnumElement:
  case PatternKind::Expr:
    llvm_unreachable("FIXME: not implemented");

  case PatternKind::Var: {
    auto var = cast<VarPattern>(pattern);

    unsigned abbrCode = DeclTypeAbbrCodes[VarPatternLayout::Code];
    VarPatternLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 var->isImplicit());
    writePattern(var->getSubPattern());
    break;
  }
  }
}

/// Translate from the requirement kind to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableRequirementKind(RequirementKind kind) {
#define CASE(KIND)            \
  case RequirementKind::KIND: \
    return GenericRequirementKind::KIND;

  switch (kind) {
  CASE(Conformance)
  CASE(SameType)
  CASE(WitnessMarker)
  }
#undef CASE
}

void Serializer::writeRequirements(ArrayRef<Requirement> requirements) {
  using namespace decls_block;

  if (requirements.empty())
    return;

  auto reqAbbrCode = DeclTypeAbbrCodes[GenericRequirementLayout::Code];
  for (const auto &req : requirements) { {
    switch (req.getKind()) {
    case RequirementKind::Conformance:
    case RequirementKind::SameType:
      GenericRequirementLayout::emitRecord(
        Out, ScratchRecord, reqAbbrCode,
        getRawStableRequirementKind(req.getKind()),
        addTypeRef(req.getFirstType()),
        addTypeRef(req.getSecondType()));
      break;

    case RequirementKind::WitnessMarker:
      GenericRequirementLayout::emitRecord(
        Out, ScratchRecord, reqAbbrCode,
        getRawStableRequirementKind(req.getKind()),
        llvm::makeArrayRef(addTypeRef(req.getFirstType())));
      break;
    }
  }
  }
}

bool Serializer::writeGenericParams(const GenericParamList *genericParams,
                                  const std::array<unsigned, 256> &abbrCodes) {
  using namespace decls_block;

  // Don't write anything if there are no generic params.
  if (!genericParams)
    return true;

  SmallVector<TypeID, 8> archetypeIDs;
  for (auto archetype : genericParams->getAllArchetypes())
    archetypeIDs.push_back(addTypeRef(archetype));

  unsigned abbrCode = abbrCodes[GenericParamListLayout::Code];
  GenericParamListLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     archetypeIDs);

  abbrCode = abbrCodes[GenericParamLayout::Code];
  for (auto next : genericParams->getParams()) {
    GenericParamLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addDeclRef(next.getDecl()));
  }

  abbrCode = abbrCodes[GenericRequirementLayout::Code];
  for (auto next : genericParams->getRequirements()) {
    switch (next.getKind()) {
    case RequirementKind::Conformance:
      GenericRequirementLayout::emitRecord(
                                      Out, ScratchRecord, abbrCode,
                                      GenericRequirementKind::Conformance,
                                      addTypeRef(next.getSubject()),
                                      addTypeRef(next.getConstraint()));
      break;
    case RequirementKind::SameType:
      GenericRequirementLayout::emitRecord(
                                      Out, ScratchRecord, abbrCode,
                                      GenericRequirementKind::SameType,
                                      addTypeRef(next.getFirstType()),
                                      addTypeRef(next.getSecondType()));
      break;
    case RequirementKind::WitnessMarker:
      llvm_unreachable("Can't show up in requirement representations");
      break;
    }
  }

  abbrCode = abbrCodes[LastGenericRequirementLayout::Code];
  uint8_t dummy = 0;
  LastGenericRequirementLayout::emitRecord(Out, ScratchRecord, abbrCode, dummy);
  return true;
}

bool
Serializer::encodeReferencedConformance(const ProtocolConformance *conformance,
                                        DeclID &typeID,
                                        ModuleID &moduleID,
                                        bool allowReferencingCurrentModule) {
  bool append = !isa<NormalProtocolConformance>(conformance);
  if (!allowReferencingCurrentModule)
    append |= conformance->getDeclContext()->getParentModule() == M;
  if (append) {
    // Encode the type in typeID. Set moduleID to BUILTIN_MODULE_ID to indicate
    // that the underlying conformance will follow. This is safe because there
    // should never be any conformances in the Builtin module.
    typeID = addTypeRef(conformance->getType());
    moduleID = serialization::BUILTIN_MODULE_ID;
  } else {
    typeID = addDeclRef(conformance->getType()->getAnyNominal());
    assert(typeID && "Missing nominal type for specialized conformance");

    // BUILTIN_MODULE_ID is a sentinel for a trailing underlying conformance
    // record.
    moduleID = addModuleRef(conformance->getDeclContext()->getParentModule());
    assert(moduleID != serialization::BUILTIN_MODULE_ID);
  }

  return append;
}

void
Serializer::writeConformance(const ProtocolDecl *protocol,
                             const ProtocolConformance *conformance,
                             const Decl *associatedDecl,
                             const std::array<unsigned, 256> &abbrCodes,
                             bool writeIncomplete) {
  using namespace decls_block;

  if (!conformance) {
    unsigned abbrCode = abbrCodes[NoConformanceLayout::Code];
    NoConformanceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    addDeclRef(protocol));
    return;
  }

  if (associatedDecl) {
    if (auto protoKind = protocol->getKnownProtocolKind()) {
      auto index = static_cast<unsigned>(protoKind.getValue());
      KnownProtocolAdopters[index].push_back(addDeclRef(associatedDecl));
    }
  }

  switch (conformance->getKind()) {
  case ProtocolConformanceKind::Normal: {
    auto conf = cast<NormalProtocolConformance>(conformance);

    SmallVector<DeclID, 16> data;
    unsigned numValueWitnesses = 0;
    unsigned numTypeWitnesses = 0;
    unsigned numDefaultedDefinitions = 0;
    conformance->forEachValueWitness(nullptr,
                                     [&](ValueDecl *req,
                                         ConcreteDeclRef witness) {
      data.push_back(addDeclRef(req));
      data.push_back(addDeclRef(witness.getDecl()));
      // The substitution records are serialized later.
      data.push_back(witness.getSubstitutions().size());
      ++numValueWitnesses;
    });

    conformance->forEachTypeWitness(/*resolver=*/nullptr,
                                    [&](AssociatedTypeDecl *assocType,
                                        const Substitution &witness) {
       data.push_back(addDeclRef(assocType));
       // The substitution record is serialized later.
       ++numTypeWitnesses;
      return false;
    });

    for (auto defaulted : conf->getDefaultedDefinitions()) {
      data.push_back(addDeclRef(defaulted));
      ++numDefaultedDefinitions;
    }

    if (writeIncomplete)
      data.clear();

    unsigned numInheritedConformances = conf->getInheritedConformances().size();
    unsigned abbrCode
      = abbrCodes[NormalProtocolConformanceLayout::Code];
    NormalProtocolConformanceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                                addDeclRef(protocol),
                                                numValueWitnesses,
                                                numTypeWitnesses,
                                                numInheritedConformances,
                                                numDefaultedDefinitions,
                                                writeIncomplete,
                                                data);

    // FIXME: Unfortunate to have to copy these.
    SmallVector<ProtocolDecl *, 8> inheritedProtos;
    SmallVector<ProtocolConformance *, 8> inheritedConformance;
    for (auto inheritedMapping : conf->getInheritedConformances()) {
      inheritedProtos.push_back(inheritedMapping.first);
      inheritedConformance.push_back(inheritedMapping.second);
    }
    writeConformances(inheritedProtos, inheritedConformance, associatedDecl,
                      abbrCodes, writeIncomplete);
    if (writeIncomplete)
      break;
      
    conformance->forEachValueWitness(nullptr,
                                     [&](ValueDecl *req,
                                         ConcreteDeclRef witness) {
      writeSubstitutions(witness.getSubstitutions(), abbrCodes);
    });
    conformance->forEachTypeWitness(/*resolver=*/nullptr,
                                    [&](AssociatedTypeDecl *assocType,
                                        const Substitution &witness) {
      writeSubstitutions(witness, abbrCodes);
      return false;
    });

    break;
  }

  case ProtocolConformanceKind::Specialized: {
    auto conf = cast<SpecializedProtocolConformance>(conformance);
    auto substitutions = conf->getGenericSubstitutions();
    unsigned abbrCode
      = abbrCodes[SpecializedProtocolConformanceLayout::Code];
    DeclID typeID;
    ModuleID moduleID;

    bool appendGenericConformance
      = encodeReferencedConformance(conf->getGenericConformance(),
                                    typeID, moduleID, true);

    SpecializedProtocolConformanceLayout::emitRecord(Out, ScratchRecord,
                                                     abbrCode,
                                                     addDeclRef(protocol),
                                                     typeID,
                                                     moduleID,
                                                     substitutions.size());
    writeSubstitutions(substitutions, abbrCodes);

    if (appendGenericConformance) {
      writeConformance(protocol, conf->getGenericConformance(), nullptr,
                       abbrCodes);
    }
    break;
  }

  case ProtocolConformanceKind::Inherited: {
    auto conf = cast<InheritedProtocolConformance>(conformance);
    unsigned abbrCode
      = abbrCodes[InheritedProtocolConformanceLayout::Code];
    DeclID typeID;
    ModuleID moduleID;

    bool appendInheritedConformance
      = encodeReferencedConformance(conf->getInheritedConformance(),
                                    typeID, moduleID, true);

    InheritedProtocolConformanceLayout::emitRecord(Out, ScratchRecord,
                                                   abbrCode,
                                                   addDeclRef(protocol),
                                                   typeID,
                                                   moduleID);
    if (appendInheritedConformance) {
      writeConformance(protocol, conf->getInheritedConformance(), nullptr,
                       abbrCodes);
    }
    break;
  }
  }
}

void
Serializer::writeConformances(ArrayRef<ProtocolDecl *> protocols,
                              ArrayRef<ProtocolConformance *> conformances,
                              const Decl *associatedDecl,
                              const std::array<unsigned, 256> &abbrCodes,
                              bool writeIncomplete) {
  using namespace decls_block;

  for_each(protocols, conformances,
           [&](const ProtocolDecl *proto, const ProtocolConformance *conf) {
    writeConformance(proto, conf, associatedDecl, abbrCodes, writeIncomplete);
  });
}

void
Serializer::writeSubstitutions(ArrayRef<Substitution> substitutions,
                               const std::array<unsigned, 256> &abbrCodes) {
  using namespace decls_block;
  auto abbrCode = abbrCodes[BoundGenericSubstitutionLayout::Code];
  for (auto &sub : substitutions) {
    SmallVector<DeclID, 16> conformanceData;
    SmallVector<const ProtocolConformance *, 8> conformancesToWrite;

    for (const ProtocolConformance *conformance : sub.Conformance) {
      DeclID typeID;
      ModuleID moduleID;
      if (!conformance) {
        typeID = addDeclRef(nullptr);
        moduleID = BUILTIN_MODULE_ID;
      } else if (encodeReferencedConformance(conformance, typeID, moduleID,
                                             false)) {
        conformancesToWrite.push_back(conformance);
      }
      conformanceData.push_back(typeID);
      conformanceData.push_back(moduleID);
    }

    BoundGenericSubstitutionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                               addTypeRef(sub.Archetype),
                                               addTypeRef(sub.Replacement),
                                               conformanceData);

    for (const ProtocolConformance *conformance : conformancesToWrite) {
      writeConformance(conformance->getProtocol(), conformance, nullptr,
                       abbrCodes, /*writeIncomplete=*/true);
    }
  }
}

static bool shouldSerializeMember(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::TopLevelCode:
  case DeclKind::Extension:
    llvm_unreachable("decl should never be a member");

  case DeclKind::IfConfig:
    return false;

  case DeclKind::EnumCase:
    return false;

  case DeclKind::Constructor: {
    // Never serialize a constructor with a stub implementation.
    auto ctor = cast<ConstructorDecl>(D);
    return !ctor->hasStubImplementation();
  }

  case DeclKind::EnumElement:
  case DeclKind::Protocol:
  case DeclKind::Destructor:
  case DeclKind::PatternBinding:
  case DeclKind::Subscript:
  case DeclKind::TypeAlias:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Var:
  case DeclKind::Param:
  case DeclKind::Func:
    return true;
  }
}

void Serializer::writeMembers(DeclRange members, bool isClass) {
  using namespace decls_block;

  unsigned abbrCode = DeclTypeAbbrCodes[DeclContextLayout::Code];
  SmallVector<DeclID, 16> memberIDs;
  for (auto member : members) {
    if (!shouldSerializeMember(member))
      continue;

    DeclID memberID = addDeclRef(member);
    memberIDs.push_back(memberID);

    if (isClass) {
      if (auto VD = dyn_cast<ValueDecl>(member)) {
        if (VD->canBeAccessedByDynamicLookup()) {
          auto &list = ClassMembersByName[VD->getName()];
          list.push_back({getKindForTable(VD), memberID});
        }
      }
    }
  }
  DeclContextLayout::emitRecord(Out, ScratchRecord, abbrCode, memberIDs);
}

static serialization::AccessorKind getStableAccessorKind(swift::AccessorKind K){
  switch (K) {
  case swift::AccessorKind::NotAccessor:
    llvm_unreachable("should only be called for actual accessors");
#define CASE(NAME) \
  case swift::AccessorKind::Is##NAME: return serialization::NAME;
  CASE(Getter)
  CASE(Setter)
  CASE(WillSet)
  CASE(DidSet)
#undef CASE
  }
}

void Serializer::writeCrossReference(const DeclContext *DC, uint32_t pathLen) {
  using namespace decls_block;

  unsigned abbrCode;

  switch (DC->getContextKind()) {
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::Initializer:
  case DeclContextKind::TopLevelCodeDecl:
    llvm_unreachable("cannot cross-reference this context");

  case DeclContextKind::FileUnit:
    DC = cast<FileUnit>(DC)->getParentModule();
    SWIFT_FALLTHROUGH;

  case DeclContextKind::Module:
    abbrCode = DeclTypeAbbrCodes[XRefLayout::Code];
    XRefLayout::emitRecord(Out, ScratchRecord, abbrCode,
                           addModuleRef(cast<Module>(DC)), pathLen);
    break;

  case DeclContextKind::NominalTypeDecl: {
    writeCrossReference(DC->getParent(), pathLen + 1);

    auto nominal = cast<NominalTypeDecl>(DC);
    abbrCode = DeclTypeAbbrCodes[XRefTypePathPieceLayout::Code];
    XRefTypePathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                        addIdentifierRef(nominal->getName()));
    break;
  }

  case DeclContextKind::ExtensionDecl: {
    Type baseTy = cast<ExtensionDecl>(DC)->getExtendedType();
    writeCrossReference(baseTy->getAnyNominal(), pathLen + 1);

    abbrCode = DeclTypeAbbrCodes[XRefExtensionPathPieceLayout::Code];
    XRefExtensionPathPieceLayout::emitRecord(
        Out, ScratchRecord, abbrCode, addModuleRef(DC->getParentModule()));
    break;
  }

  case DeclContextKind::AbstractFunctionDecl: {
    if (auto fn = dyn_cast<FuncDecl>(DC)) {
      if (auto storage = fn->getAccessorStorageDecl()) {
        writeCrossReference(storage->getDeclContext(), pathLen + 2);

        Type ty = storage->getInterfaceType()->getCanonicalType();
        auto nameID = addIdentifierRef(storage->getName());
        abbrCode = DeclTypeAbbrCodes[XRefValuePathPieceLayout::Code];
        XRefValuePathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                             addTypeRef(ty), nameID);

        abbrCode =
          DeclTypeAbbrCodes[XRefOperatorOrAccessorPathPieceLayout::Code];
        auto emptyID = addIdentifierRef(Identifier());
        auto accessorKind = getStableAccessorKind(fn->getAccessorKind());
        assert(!fn->isObservingAccessor() &&
               "cannot form cross-reference to observing accessors");
        XRefOperatorOrAccessorPathPieceLayout::emitRecord(Out, ScratchRecord,
                                                          abbrCode, emptyID,
                                                          accessorKind);
        break;
      }
    }

    auto fn = cast<AbstractFunctionDecl>(DC);
    writeCrossReference(DC->getParent(), pathLen + 1 + fn->isOperator());

    Type ty = fn->getInterfaceType()->getCanonicalType();
    abbrCode = DeclTypeAbbrCodes[XRefValuePathPieceLayout::Code];
    XRefValuePathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                         addTypeRef(ty),
                                         addIdentifierRef(fn->getName()));

    if (fn->isOperator()) {
      // Encode the fixity as a filter on the func decls, to distinguish prefix
      // and postfix operators.
      auto op = cast<FuncDecl>(fn)->getOperatorDecl();
      assert(op);
      abbrCode = DeclTypeAbbrCodes[XRefOperatorOrAccessorPathPieceLayout::Code];
      auto emptyID = addIdentifierRef(Identifier());
      auto fixity = getStableFixity(op->getKind());
      XRefOperatorOrAccessorPathPieceLayout::emitRecord(Out, ScratchRecord,
                                                        abbrCode, emptyID,
                                                        fixity);
    }
    break;
  }
  }
}

void Serializer::writeCrossReference(const Decl *D) {
  using namespace decls_block;

  unsigned abbrCode;

  if (auto op = dyn_cast<OperatorDecl>(D)) {
    writeCrossReference(op->getModuleContext());

    abbrCode = DeclTypeAbbrCodes[XRefOperatorOrAccessorPathPieceLayout::Code];
    auto nameID = addIdentifierRef(op->getName());
    auto fixity = getStableFixity(op->getKind());
    XRefOperatorOrAccessorPathPieceLayout::emitRecord(Out, ScratchRecord,
                                                      abbrCode, nameID,
                                                      fixity);
    return;
  }

  if (auto fn = dyn_cast<AbstractFunctionDecl>(D)) {
    // Functions are special because they might be operators.
    writeCrossReference(fn, 0);
    return;
  }

  writeCrossReference(D->getDeclContext());

  if (auto genericParam = dyn_cast<GenericTypeParamDecl>(D)) {
    abbrCode = DeclTypeAbbrCodes[XRefGenericParamPathPieceLayout::Code];
    XRefGenericParamPathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                                genericParam->getIndex());
    return;
  }

  if (auto type = dyn_cast<TypeDecl>(D)) {
    abbrCode = DeclTypeAbbrCodes[XRefTypePathPieceLayout::Code];
    XRefTypePathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                        addIdentifierRef(type->getName()));
    return;
  }

  auto val = cast<ValueDecl>(D);
  auto ty = val->getInterfaceType()->getCanonicalType();
  abbrCode = DeclTypeAbbrCodes[XRefValuePathPieceLayout::Code];
  XRefValuePathPieceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                       addTypeRef(ty),
                                       addIdentifierRef(val->getName()));
}

/// Translate from the AST associativity enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableAssociativity(swift::Associativity assoc) {
  switch (assoc) {
  case swift::Associativity::Left:
    return serialization::Associativity::LeftAssociative;
  case swift::Associativity::Right:
    return serialization::Associativity::RightAssociative;
  case swift::Associativity::None:
    return serialization::Associativity::NonAssociative;
  }
}

static serialization::StaticSpellingKind
getStableStaticSpelling(swift::StaticSpellingKind SS) {
  switch (SS) {
  case swift::StaticSpellingKind::None:
    return serialization::StaticSpellingKind::None;
  case swift::StaticSpellingKind::KeywordStatic:
    return serialization::StaticSpellingKind::KeywordStatic;
  case swift::StaticSpellingKind::KeywordClass:
    return serialization::StaticSpellingKind::KeywordClass;
  }
}

/// Asserts if the declaration has any attributes other than the ones
/// specified in the template parameters.
///
/// This is a no-op in release builds.
template <AttrKind ...KINDS>
static void checkAllowedAttributes(const Decl *D) {
#ifndef NDEBUG
  DeclAttributes attrs = D->getAttrs();

  for (AttrKind AK : std::vector<AttrKind>({ KINDS... }))
    attrs.clearAttribute(AK);

  if (attrs.containsTraditionalAttributes()) {
    llvm::errs() << "Serialization: unhandled attributes ";
    attrs.print(llvm::errs());
    llvm::errs() << "\n";
    llvm_unreachable("TODO: handle the above attributes");
  }
#endif
}

#ifndef NDEBUG
#define DEF_VERIFY_ATTR(DECL)\
static void verifyAttrSerializable(const DECL ## Decl *D) {\
  for (auto Attr : D->getAttrs()) {\
  if (!Attr->canAppearOn ## DECL())\
    llvm_unreachable("attribute cannot appear on a " #DECL);\
}\
}

DEF_VERIFY_ATTR(Func)
DEF_VERIFY_ATTR(Extension)
DEF_VERIFY_ATTR(PatternBinding)
DEF_VERIFY_ATTR(Operator)
DEF_VERIFY_ATTR(TypeAlias)
DEF_VERIFY_ATTR(Type)
DEF_VERIFY_ATTR(Struct)
DEF_VERIFY_ATTR(Enum)
DEF_VERIFY_ATTR(Class)
DEF_VERIFY_ATTR(Protocol)
DEF_VERIFY_ATTR(Var)
DEF_VERIFY_ATTR(Subscript)
DEF_VERIFY_ATTR(Constructor)
DEF_VERIFY_ATTR(Destructor)

#undef DEF_VERIFY_ATTR
#else
static void verifyAttrSerializable(const Decl *D) {}
#endif

static bool isForced(const Decl *D,
                     const llvm::DenseMap<Serializer::DeclTypeUnion,
                                          Serializer::DeclIDAndForce> &table) {
  if (table.lookup(D).second)
    return true;
  for (const DeclContext *DC = D->getDeclContext(); !DC->isModuleScopeContext();
       DC = DC->getParent())
    if (table.lookup(getDeclForContext(DC)).second)
      return true;
  return false;
}

void Serializer::writeDeclAttribute(const DeclAttribute *DA) {
  using namespace decls_block;

  switch (DA->getKind()) {
  case DAK_Count:
    llvm_unreachable("cannot serialize DAK_Count");
    return;

#define SIMPLE_DECL_ATTR(NAME, CLASS, ...)\
  case DAK_##NAME: { \
    auto abbrCode = DeclTypeAbbrCodes[CLASS##DeclAttrLayout::Code]; \
    CLASS##DeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode, \
                                      DA->isImplicit()); \
    return; \
  }
#include "swift/AST/Attr.def"

  case DAK_asmname: {
    auto *theAttr = cast<AsmnameAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[AsmnameDeclAttrLayout::Code];
    AsmnameDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      theAttr->isImplicit(),
                                      theAttr->Name);
    return;
  }
  case DAK_availability: {
    auto *theAttr = cast<AvailabilityAttr>(DA);
    llvm::SmallString<32> blob;
    blob.append(theAttr->Platform);
    blob.append(theAttr->Message);
    auto abbrCode = DeclTypeAbbrCodes[AvailabilityDeclAttrLayout::Code];
    AvailabilityDeclAttrLayout::emitRecord(
        Out, ScratchRecord, abbrCode,
        theAttr->isImplicit(),
        theAttr->IsUnvailable,
        theAttr->Platform.size(),
        theAttr->Message.size(),
        blob);
    return;
  }
  case DAK_objc: {
    auto *theAttr = cast<ObjCAttr>(DA);
    SmallVector<IdentifierID, 4> pieces;
    unsigned numArgs = 0;
    if (auto name = theAttr->getName()) {
      numArgs = name->getNumArgs() + 1;
      for (auto piece : name->getSelectorPieces()) {
        pieces.push_back(addIdentifierRef(piece));
      }
    }
    auto abbrCode = DeclTypeAbbrCodes[ObjCDeclAttrLayout::Code];
    ObjCDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   theAttr->isImplicit(), numArgs, pieces);
    return;
  }
  case DAK_override:
    // No need to serialize 'override'.
    return;
  case DAK_raw_doc_comment:
    // Serialized in a separate table.
    return;
  }
}

bool Serializer::isDeclXRef(const Decl *D) const {
  const DeclContext *topLevel = D->getDeclContext()->getModuleScopeContext();
  return (topLevel->getParentModule() != M ||
          (SF && topLevel != SF && !isForced(D, DeclIDs)));
}

static serialization::CtorInitializerKind
getStableCtorInitializerKind(swift::CtorInitializerKind K){
  switch (K) {
  case swift::CtorInitializerKind::ConvenienceFactory:
    llvm_unreachable("Convenience factory initializers cannot be uttered");

  case swift::CtorInitializerKind::Factory:
    llvm_unreachable("Factory initializers cannot be uttered");

#define CASE(NAME) \
  case swift::CtorInitializerKind::NAME: return serialization::NAME;
      CASE(Designated)
      CASE(Convenience)
#undef CASE
  }
}

void Serializer::writeDecl(const Decl *D) {
  using namespace decls_block;

  assert(!D->isInvalid() && "cannot create a module with an invalid decl");
  if (isDeclXRef(D)) {
    writeCrossReference(D);
    return;
  }

  assert(!D->hasClangNode() && "imported decls should use cross-references");

  // Emit attributes (if any).
  auto &Attrs = D->getAttrs();
  if (Attrs.begin() != Attrs.end()) {
    for (auto Attr : Attrs)
      writeDeclAttribute(Attr);
  }

  switch (D->getKind()) {
  case DeclKind::Import:
    llvm_unreachable("import decls should not be serialized");

  case DeclKind::IfConfig:
    llvm_unreachable("#if block declarations should not be serialized");

  case DeclKind::Extension: {
    auto extension = cast<ExtensionDecl>(D);

    // @transparent on extensions is propagated down to the methods and
    // constructors during serialization.
    checkAllowedAttributes<AK_transparent>(extension);
    verifyAttrSerializable(extension);

    const Decl *DC = getDeclForContext(extension->getDeclContext());
    Type baseTy = extension->getExtendedType();

    SmallVector<DeclID, 8> protocols;
    for (auto proto : extension->getProtocols())
      protocols.push_back(addDeclRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[ExtensionLayout::Code];
    ExtensionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addTypeRef(baseTy),
                                addDeclRef(DC),
                                extension->isImplicit(),
                                protocols);

    bool isClassExtension = false;
    if (auto baseNominal = baseTy->getAnyNominal()) {
      isClassExtension = isa<ClassDecl>(baseNominal) ||
                         isa<ProtocolDecl>(baseNominal);
    }

    writeMembers(extension->getMembers(), isClassExtension);
    writeConformances(extension->getProtocols(), extension->getConformances(),
                      extension, DeclTypeAbbrCodes);

    break;
  }

  case DeclKind::EnumCase:
    llvm_unreachable("enum case decls should not be serialized");

  case DeclKind::PatternBinding: {
    auto binding = cast<PatternBindingDecl>(D);
    checkAllowedAttributes<>(binding);
    verifyAttrSerializable(binding);

    const Decl *DC = getDeclForContext(binding->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[PatternBindingLayout::Code];
    PatternBindingLayout::emitRecord(
        Out, ScratchRecord, abbrCode, addDeclRef(DC), binding->isImplicit(),
        binding->isStatic(),
        uint8_t(getStableStaticSpelling(binding->getStaticSpelling())));

    writePattern(binding->getPattern());
    // Ignore initializer; external clients don't need to know about it.

    break;
  }

  case DeclKind::TopLevelCode:
    // Top-level code is ignored; external clients don't need to know about it.
    break;

  case DeclKind::InfixOperator: {
    auto op = cast<InfixOperatorDecl>(D);
    checkAllowedAttributes<>(op);
    verifyAttrSerializable(op);

    const Decl *DC = getDeclForContext(op->getDeclContext());
    auto associativity = getRawStableAssociativity(op->getAssociativity());

    unsigned abbrCode = DeclTypeAbbrCodes[InfixOperatorLayout::Code];
    InfixOperatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    addIdentifierRef(op->getName()),
                                    addDeclRef(DC),
                                    associativity,
                                    op->getPrecedence());
    break;
  }

  case DeclKind::PrefixOperator: {
    auto op = cast<PrefixOperatorDecl>(D);
    checkAllowedAttributes<>(op);
    verifyAttrSerializable(op);

    const Decl *DC = getDeclForContext(op->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[PrefixOperatorLayout::Code];
    PrefixOperatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addIdentifierRef(op->getName()),
                                     addDeclRef(DC));
    break;
  }

  case DeclKind::PostfixOperator: {
    auto op = cast<PostfixOperatorDecl>(D);
    checkAllowedAttributes<>(op);
    verifyAttrSerializable(op);

    const Decl *DC = getDeclForContext(op->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[PostfixOperatorLayout::Code];
    PostfixOperatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addIdentifierRef(op->getName()),
                                      addDeclRef(DC));
    break;
  }

  case DeclKind::TypeAlias: {
    auto typeAlias = cast<TypeAliasDecl>(D);
    assert(!typeAlias->isObjC() && "ObjC typealias is not meaningful");
    assert(typeAlias->getProtocols().empty() &&
           "concrete typealiases cannot have protocols");
    checkAllowedAttributes<>(typeAlias);
    verifyAttrSerializable(typeAlias);

    const Decl *DC = getDeclForContext(typeAlias->getDeclContext());

    Type underlying;
    if (typeAlias->hasUnderlyingType())
      underlying = typeAlias->getUnderlyingType();

    unsigned abbrCode = DeclTypeAbbrCodes[TypeAliasLayout::Code];
    TypeAliasLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addIdentifierRef(typeAlias->getName()),
                                addDeclRef(DC),
                                addTypeRef(underlying),
                                addTypeRef(typeAlias->getInterfaceType()),
                                typeAlias->isImplicit());
    break;
  }

  case DeclKind::GenericTypeParam: {
    auto genericParam = cast<GenericTypeParamDecl>(D);
    checkAllowedAttributes<>(genericParam);
    verifyAttrSerializable(genericParam);

    const Decl *DC = getDeclForContext(genericParam->getDeclContext());

    SmallVector<DeclID, 4> protocols;
    for (auto proto : genericParam->getProtocols())
      protocols.push_back(addDeclRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[GenericTypeParamDeclLayout::Code];
    GenericTypeParamDeclLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addIdentifierRef(genericParam->getName()),
                                addDeclRef(DC),
                                genericParam->isImplicit(),
                                genericParam->getDepth(),
                                genericParam->getIndex(),
                                addTypeRef(genericParam->getSuperclass()),
                                addTypeRef(genericParam->getArchetype()),
                                protocols);
    break;
  }

  case DeclKind::AssociatedType: {
    auto assocType = cast<AssociatedTypeDecl>(D);
    checkAllowedAttributes<>(assocType);
    verifyAttrSerializable(assocType);

    const Decl *DC = getDeclForContext(assocType->getDeclContext());

    SmallVector<DeclID, 4> protocols;
    for (auto proto : assocType->getProtocols())
      protocols.push_back(addDeclRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[AssociatedTypeDeclLayout::Code];
    AssociatedTypeDeclLayout::emitRecord(
      Out, ScratchRecord, abbrCode,
      addIdentifierRef(assocType->getName()),
      addDeclRef(DC),
      addTypeRef(assocType->getSuperclass()),
      addTypeRef(assocType->getArchetype()),
      addTypeRef(assocType->getDefaultDefinitionType()),
      assocType->isImplicit(),
      protocols);
    break;
  }

  case DeclKind::Struct: {
    auto theStruct = cast<StructDecl>(D);
    checkAllowedAttributes<>(theStruct);
    verifyAttrSerializable(theStruct);

    const Decl *DC = getDeclForContext(theStruct->getDeclContext());

    SmallVector<DeclID, 8> protocols;
    for (auto proto : theStruct->getProtocols())
      protocols.push_back(addDeclRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[StructLayout::Code];
    StructLayout::emitRecord(Out, ScratchRecord, abbrCode,
                             addIdentifierRef(theStruct->getName()),
                             addDeclRef(DC),
                             theStruct->isImplicit(),
                             protocols);


    writeGenericParams(theStruct->getGenericParams(), DeclTypeAbbrCodes);
    writeRequirements(theStruct->getGenericRequirements());
    writeMembers(theStruct->getMembers(), false);
    writeConformances(theStruct->getProtocols(), theStruct->getConformances(),
                      theStruct, DeclTypeAbbrCodes);
    break;
  }

  case DeclKind::Enum: {
    auto theEnum = cast<EnumDecl>(D);
    checkAllowedAttributes<>(theEnum);
    verifyAttrSerializable(theEnum);

    const Decl *DC = getDeclForContext(theEnum->getDeclContext());

    SmallVector<DeclID, 8> protocols;
    for (auto proto : theEnum->getProtocols())
      protocols.push_back(addDeclRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[EnumLayout::Code];
    EnumLayout::emitRecord(Out, ScratchRecord, abbrCode,
                            addIdentifierRef(theEnum->getName()),
                            addDeclRef(DC),
                            theEnum->isImplicit(),
                            addTypeRef(theEnum->getRawType()),
                            protocols);

    writeGenericParams(theEnum->getGenericParams(), DeclTypeAbbrCodes);
    writeRequirements(theEnum->getGenericRequirements());
    writeMembers(theEnum->getMembers(), false);
    writeConformances(theEnum->getProtocols(), theEnum->getConformances(),
                      theEnum, DeclTypeAbbrCodes);
    break;
  }

  case DeclKind::Class: {
    auto theClass = cast<ClassDecl>(D);
    checkAllowedAttributes<
      AK_requires_stored_property_inits>(theClass);
    verifyAttrSerializable(theClass);

    const Decl *DC = getDeclForContext(theClass->getDeclContext());

    SmallVector<DeclID, 8> protocols;
    for (auto proto : theClass->getProtocols())
      protocols.push_back(addDeclRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[ClassLayout::Code];
    ClassLayout::emitRecord(Out, ScratchRecord, abbrCode,
                            addIdentifierRef(theClass->getName()),
                            addDeclRef(DC),
                            theClass->isImplicit(),
                            theClass->isObjC(),
                            theClass->getAttrs().requiresStoredPropertyInits(),
                            theClass->requiresStoredPropertyInits(),
                            addTypeRef(theClass->getSuperclass()),
                            protocols);

    writeGenericParams(theClass->getGenericParams(), DeclTypeAbbrCodes);
    writeRequirements(theClass->getGenericRequirements());
    writeMembers(theClass->getMembers(), true);
    writeConformances(theClass->getProtocols(), theClass->getConformances(),
                      theClass, DeclTypeAbbrCodes);
    break;
  }


  case DeclKind::Protocol: {
    auto proto = cast<ProtocolDecl>(D);
    verifyAttrSerializable(proto);

    const Decl *DC = getDeclForContext(proto->getDeclContext());

    SmallVector<DeclID, 8> protocols;
    for (auto proto : proto->getProtocols())
      protocols.push_back(addDeclRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[ProtocolLayout::Code];
    ProtocolLayout::emitRecord(Out, ScratchRecord, abbrCode,
                               addIdentifierRef(proto->getName()),
                               addDeclRef(DC),
                               proto->isImplicit(),
                               proto->isObjC(),
                               protocols);

    writeGenericParams(proto->getGenericParams(), DeclTypeAbbrCodes);
    writeRequirements(proto->getGenericRequirements());
    writeMembers(proto->getMembers(), true);
    break;
  }

  case DeclKind::Var: {
    auto var = cast<VarDecl>(D);
    checkAllowedAttributes<
      AK_optional, AK_unowned, AK_unowned_unsafe, AK_weak, AK_transparent
    >(var);
    verifyAttrSerializable(var);

    const Decl *DC = getDeclForContext(var->getDeclContext());
    Type type = var->hasType() ? var->getType() : nullptr;

    FuncDecl *WillSet = nullptr, *DidSet = nullptr;

    VarDeclStorageKind StorageKind;
    switch (var->getStorageKind()) {
    case VarDecl::Stored:
      StorageKind = VarDeclStorageKind::Stored;
      break;
    case VarDecl::StoredWithTrivialAccessors:
      StorageKind = VarDeclStorageKind::StoredWithTrivialAccessors;
      break;
    case VarDecl::Computed:
      StorageKind = VarDeclStorageKind::Computed;
      break;
    case VarDecl::Observing:
      StorageKind = VarDeclStorageKind::Observing;
      WillSet = var->getWillSetFunc();
      DidSet = var->getDidSetFunc();
      break;
    }

    unsigned abbrCode = DeclTypeAbbrCodes[VarLayout::Code];
    VarLayout::emitRecord(Out, ScratchRecord, abbrCode,
                          addIdentifierRef(var->getName()),
                          addDeclRef(DC),
                          var->isImplicit(),
                          var->isObjC(),
                          var->getAttrs().isOptional(),
                          var->isStatic(),
                          var->isLet(),
                          uint8_t(StorageKind),
                          addTypeRef(type),
                          addTypeRef(var->getInterfaceType()),
                          addDeclRef(var->getGetter()),
                          addDeclRef(var->getSetter()),
                          addDeclRef(WillSet),
                          addDeclRef(DidSet),
                          addDeclRef(var->getOverriddenDecl()));
    break;
  }

  case DeclKind::Param: {
    auto param = cast<ParamDecl>(D);
    checkAllowedAttributes<
      AK_unowned, AK_unowned_unsafe, AK_weak
    >(param);
    verifyAttrSerializable(param);

    const Decl *DC = getDeclForContext(param->getDeclContext());
    Type type = param->hasType() ? param->getType() : nullptr;

    unsigned abbrCode = DeclTypeAbbrCodes[ParamLayout::Code];
    ParamLayout::emitRecord(Out, ScratchRecord, abbrCode,
                            addIdentifierRef(param->getArgumentName()),
                            addIdentifierRef(param->getName()),
                            addDeclRef(DC),
                            param->isLet(),
                            addTypeRef(type),
                            addTypeRef(param->getInterfaceType()));
    break;
  }

  case DeclKind::Func: {
    auto fn = cast<FuncDecl>(D);
    checkAllowedAttributes<
      AK_conversion, AK_infix,
      AK_optional, AK_postfix, AK_prefix, AK_transparent,
      AK_mutating
    >(fn);
    verifyAttrSerializable(fn);

    const Decl *DC = getDeclForContext(fn->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[FuncLayout::Code];
    SmallVector<IdentifierID, 4> nameComponents;
    nameComponents.push_back(addIdentifierRef(fn->getFullName().getBaseName()));
    for (auto argName : fn->getFullName().getArgumentNames())
      nameComponents.push_back(addIdentifierRef(argName));
      
    FuncLayout::emitRecord(Out, ScratchRecord, abbrCode,
                           addDeclRef(DC),
                           fn->isImplicit(),
                           fn->isStatic(),
                           uint8_t(getStableStaticSpelling(fn->getStaticSpelling())),
                           fn->getAttrs().isConversion(),
                           fn->isObjC(),
                           fn->isTransparent(),
                           fn->isMutating(),
                           fn->hasDynamicSelf(),
                           fn->getAttrs().isOptional(),
                           fn->getBodyParamPatterns().size(),
                           addTypeRef(fn->getType()),
                           addTypeRef(fn->getInterfaceType()),
                           addDeclRef(fn->getOperatorDecl()),
                           addDeclRef(fn->getOverriddenDecl()),
                           addDeclRef(fn->getAccessorStorageDecl()),
                           !fn->getFullName().isSimpleName(),
                           nameComponents);

    writeGenericParams(fn->getGenericParams(), DeclTypeAbbrCodes);

    // Write the body parameters.
    for (auto pattern : fn->getBodyParamPatterns())
      writePattern(pattern);

    break;
  }

  case DeclKind::EnumElement: {
    auto elem = cast<EnumElementDecl>(D);
    checkAllowedAttributes<>(elem);

    const Decl *DC = getDeclForContext(elem->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[EnumElementLayout::Code];
    EnumElementLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  addIdentifierRef(elem->getName()),
                                  addDeclRef(DC),
                                  addTypeRef(elem->getArgumentType()),
                                  addTypeRef(elem->getType()),
                                  addTypeRef(elem->getInterfaceType()),
                                  elem->isImplicit());
    break;
  }

  case DeclKind::Subscript: {
    auto subscript = cast<SubscriptDecl>(D);
    checkAllowedAttributes<AK_optional>(subscript);
    verifyAttrSerializable(subscript);

    const Decl *DC = getDeclForContext(subscript->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[SubscriptLayout::Code];
    SubscriptLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addDeclRef(DC),
                                subscript->isImplicit(),
                                subscript->isObjC(),
                                subscript->getAttrs().isOptional(),
                                addTypeRef(subscript->getType()),
                                addTypeRef(subscript->getElementType()),
                                addTypeRef(subscript->getInterfaceType()),
                                addDeclRef(subscript->getGetter()),
                                addDeclRef(subscript->getSetter()),
                                addDeclRef(subscript->getOverriddenDecl()));

    writePattern(subscript->getIndices());
    break;
  }


  case DeclKind::Constructor: {
    auto ctor = cast<ConstructorDecl>(D);
    checkAllowedAttributes<AK_transparent>(ctor);
    verifyAttrSerializable(ctor);

    const Decl *DC = getDeclForContext(ctor->getDeclContext());

    SmallVector<IdentifierID, 4> nameComponents;
    for (auto argName : ctor->getFullName().getArgumentNames())
      nameComponents.push_back(addIdentifierRef(argName));

    unsigned abbrCode = DeclTypeAbbrCodes[ConstructorLayout::Code];
    ConstructorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  addDeclRef(DC),
                                  ctor->isImplicit(),
                                  ctor->isObjC(),
                                  ctor->isTransparent(),
                                  getStableCtorInitializerKind(
                                    ctor->getInitKind()),
                                  addTypeRef(ctor->getType()),
                                  addTypeRef(ctor->getInterfaceType()),
                                  addDeclRef(ctor->getOverriddenDecl()),
                                  nameComponents);

    writeGenericParams(ctor->getGenericParams(), DeclTypeAbbrCodes);
    assert(ctor->getBodyParamPatterns().size() == 2);
    for (auto pattern : ctor->getBodyParamPatterns())
      writePattern(pattern);
    break;
  }

  case DeclKind::Destructor: {
    auto dtor = cast<DestructorDecl>(D);
    checkAllowedAttributes<>(dtor);
    verifyAttrSerializable(dtor);

    const Decl *DC = getDeclForContext(dtor->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[DestructorLayout::Code];
    DestructorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addDeclRef(DC),
                                 dtor->isImplicit(),
                                 dtor->isObjC(),
                                 addTypeRef(dtor->getType()));
    assert(dtor->getBodyParamPatterns().size() == 1);
    for (auto pattern : dtor->getBodyParamPatterns())
      writePattern(pattern);
    break;
  }
  }
}

#define SIMPLE_CASE(TYPENAME, VALUE) \
  case swift::TYPENAME::VALUE: return uint8_t(serialization::TYPENAME::VALUE);

/// Translate from the AST calling convention enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableCC(swift::AbstractCC cc) {
  switch (cc) {
  SIMPLE_CASE(AbstractCC, C)
  SIMPLE_CASE(AbstractCC, ObjCMethod)
  SIMPLE_CASE(AbstractCC, Freestanding)
  SIMPLE_CASE(AbstractCC, Method)
  SIMPLE_CASE(AbstractCC, WitnessMethod)
  }
  llvm_unreachable("bad calling convention");
}

/// Translate from the AST ownership enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableOwnership(swift::Ownership ownership) {
  switch (ownership) {
  SIMPLE_CASE(Ownership, Strong)
  SIMPLE_CASE(Ownership, Weak)
  SIMPLE_CASE(Ownership, Unowned)
  SIMPLE_CASE(Ownership, Unmanaged)
  }
  llvm_unreachable("bad ownership kind");
}

/// Translate from the AST ParameterConvention enum to the
/// Serialization enum values, which are guaranteed to be stable.
static uint8_t getRawStableParameterConvention(swift::ParameterConvention pc) {
  switch (pc) {
  SIMPLE_CASE(ParameterConvention, Indirect_In)
  SIMPLE_CASE(ParameterConvention, Indirect_Out)
  SIMPLE_CASE(ParameterConvention, Indirect_Inout)
  SIMPLE_CASE(ParameterConvention, Direct_Owned)
  SIMPLE_CASE(ParameterConvention, Direct_Unowned)
  SIMPLE_CASE(ParameterConvention, Direct_Guaranteed)
  }
  llvm_unreachable("bad parameter convention kind");
}

/// Translate from the AST ResultConvention enum to the
/// Serialization enum values, which are guaranteed to be stable.
static uint8_t getRawStableResultConvention(swift::ResultConvention rc) {
  switch (rc) {
  SIMPLE_CASE(ResultConvention, Owned)
  SIMPLE_CASE(ResultConvention, Unowned)
  SIMPLE_CASE(ResultConvention, UnownedInnerPointer)
  SIMPLE_CASE(ResultConvention, Autoreleased)
  }
  llvm_unreachable("bad result convention kind");
}

#undef SIMPLE_CASE

/// Find the typealias given a builtin type.
static TypeAliasDecl *findTypeAliasForBuiltin(ASTContext &Ctx,
                                              BuiltinType *Bt) {
  /// Get the type name by chopping off "Builtin.".
  llvm::SmallString<32> FullName;
  llvm::raw_svector_ostream OS(FullName);
  Bt->print(OS);
  OS.flush();
  assert(FullName.startswith("Builtin."));
  StringRef TypeName = FullName.substr(8);

  SmallVector<ValueDecl*, 4> CurModuleResults;
  Ctx.TheBuiltinModule->lookupValue(Module::AccessPathTy(),
                                    Ctx.getIdentifier(TypeName),
                                    NLKind::QualifiedLookup,
                                    CurModuleResults);
  assert(CurModuleResults.size() == 1);
  return cast<TypeAliasDecl>(CurModuleResults[0]);
}

void Serializer::writeType(Type ty) {
  using namespace decls_block;

  switch (ty.getPointer()->getKind()) {
  case TypeKind::Error:
    llvm_unreachable("should not serialize an error type");

  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinNativeObject:
  case TypeKind::BuiltinUnknownObject:
  case TypeKind::BuiltinVector: {
    TypeAliasDecl *typeAlias =
      findTypeAliasForBuiltin(M->Ctx, ty->castTo<BuiltinType>());

    unsigned abbrCode = DeclTypeAbbrCodes[NameAliasTypeLayout::Code];
    NameAliasTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    addDeclRef(typeAlias));
    break;
  }
  case TypeKind::NameAlias: {
    auto nameAlias = cast<NameAliasType>(ty.getPointer());
    const TypeAliasDecl *typeAlias = nameAlias->getDecl();

    unsigned abbrCode = DeclTypeAbbrCodes[NameAliasTypeLayout::Code];
    NameAliasTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    addDeclRef(typeAlias));
    break;
  }

  case TypeKind::Paren: {
    auto parenTy = cast<ParenType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[ParenTypeLayout::Code];
    ParenTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addTypeRef(parenTy->getUnderlyingType()));
    break;
  }

  case TypeKind::Tuple: {
    auto tupleTy = cast<TupleType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[TupleTypeLayout::Code];
    TupleTypeLayout::emitRecord(Out, ScratchRecord, abbrCode);

    abbrCode = DeclTypeAbbrCodes[TupleTypeEltLayout::Code];
    for (auto &elt : tupleTy->getFields()) {
      uint8_t rawDefaultArg
        = getRawStableDefaultArgumentKind(elt.getDefaultArgKind());
      TupleTypeEltLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addIdentifierRef(elt.getName()),
                                     addTypeRef(elt.getType()),
                                     rawDefaultArg,
                                     elt.isVararg());
    }

    break;
  }

  case TypeKind::Struct:
  case TypeKind::Enum:
  case TypeKind::Class:
  case TypeKind::Protocol: {
    auto nominalTy = cast<NominalType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[NominalTypeLayout::Code];
    NominalTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  addDeclRef(nominalTy->getDecl()),
                                  addTypeRef(nominalTy->getParent()));
    break;
  }

  case TypeKind::ExistentialMetatype: {
    auto metatypeTy = cast<ExistentialMetatypeType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[ExistentialMetatypeTypeLayout::Code];

    // Map the metatype representation.
    auto repr = getRawStableMetatypeRepresentation(metatypeTy);
    ExistentialMetatypeTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addTypeRef(metatypeTy->getInstanceType()),
                                              static_cast<uint8_t>(repr));
    break;
  }

  case TypeKind::Metatype: {
    auto metatypeTy = cast<MetatypeType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[MetatypeTypeLayout::Code];

    // Map the metatype representation.
    auto repr = getRawStableMetatypeRepresentation(metatypeTy);
    MetatypeTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(metatypeTy->getInstanceType()),
                                   static_cast<uint8_t>(repr));
    break;
  }

  case TypeKind::Module:
    llvm_unreachable("modules are currently not first-class values");

  case TypeKind::DynamicSelf: {
    auto dynamicSelfTy = cast<DynamicSelfType>(ty.getPointer());
    unsigned abbrCode = DeclTypeAbbrCodes[DynamicSelfTypeLayout::Code];
    DynamicSelfTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addTypeRef(dynamicSelfTy->getSelfType()));
    break;
  }

  case TypeKind::Archetype: {
    auto archetypeTy = cast<ArchetypeType>(ty.getPointer());

    // Opened existential types use a separate layout.
    if (auto existentialTy = archetypeTy->getOpenedExistentialType()) {
      unsigned abbrCode = DeclTypeAbbrCodes[OpenedExistentialTypeLayout::Code];
      OpenedExistentialTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                              addTypeRef(existentialTy));
      break;
    }

    TypeID indexOrParentID;
    if (archetypeTy->isPrimary())
      indexOrParentID = archetypeTy->getPrimaryIndex();
    else
      indexOrParentID = addTypeRef(archetypeTy->getParent());

    SmallVector<DeclID, 4> conformances;
    for (auto proto : archetypeTy->getConformsTo())
      conformances.push_back(addDeclRef(proto));

    DeclID assocTypeOrProtoID;
    if (auto assocType = archetypeTy->getAssocType())
      assocTypeOrProtoID = addDeclRef(assocType);
    else
      assocTypeOrProtoID = addDeclRef(archetypeTy->getSelfProtocol());

    unsigned abbrCode = DeclTypeAbbrCodes[ArchetypeTypeLayout::Code];
    ArchetypeTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    addIdentifierRef(archetypeTy->getName()),
                                    archetypeTy->isPrimary(),
                                    indexOrParentID,
                                    assocTypeOrProtoID,
                                    addTypeRef(archetypeTy->getSuperclass()),
                                    conformances);

    SmallVector<IdentifierID, 4> nestedTypeNames;
    SmallVector<TypeID, 4> nestedTypes;
    SmallVector<bool, 4> areArchetypes;
    for (auto next : archetypeTy->getNestedTypes()) {
      nestedTypeNames.push_back(addIdentifierRef(next.first));
      nestedTypes.push_back(
                    addTypeRef(ArchetypeType::getNestedTypeValue(next.second)));
      areArchetypes.push_back(next.second.is<ArchetypeType*>());
    }

    abbrCode = DeclTypeAbbrCodes[ArchetypeNestedTypeNamesLayout::Code];
    ArchetypeNestedTypeNamesLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                               nestedTypeNames);

    abbrCode = DeclTypeAbbrCodes[ArchetypeNestedTypesAreArchetypesLayout::Code];
    ArchetypeNestedTypesAreArchetypesLayout::emitRecord(Out, ScratchRecord,
                                                        abbrCode, areArchetypes);

    abbrCode = DeclTypeAbbrCodes[ArchetypeNestedTypesLayout::Code];
    ArchetypeNestedTypesLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           nestedTypes);

    break;
  }

  case TypeKind::GenericTypeParam: {
    auto genericParam = cast<GenericTypeParamType>(ty.getPointer());
    unsigned abbrCode = DeclTypeAbbrCodes[GenericTypeParamTypeLayout::Code];
    DeclID declIDOrDepth;
    unsigned indexPlusOne;
    if (genericParam->getDecl()) {
      declIDOrDepth = addDeclRef(genericParam->getDecl());
      indexPlusOne = 0;
    } else {
      declIDOrDepth = genericParam->getDepth();
      indexPlusOne = genericParam->getIndex() + 1;
    }
    GenericTypeParamTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           declIDOrDepth, indexPlusOne);
    break;
  }

  case TypeKind::AssociatedType: {
    auto assocType = cast<AssociatedTypeType>(ty.getPointer());
    unsigned abbrCode = DeclTypeAbbrCodes[AssociatedTypeTypeLayout::Code];
    AssociatedTypeTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                         addDeclRef(assocType->getDecl()));
    break;
  }

  case TypeKind::Substituted: {
    auto subTy = cast<SubstitutedType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[SubstitutedTypeLayout::Code];
    SubstitutedTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addTypeRef(subTy->getOriginal()),
                                      addTypeRef(subTy->getReplacementType()));
    break;
  }

  case TypeKind::DependentMember: {
    auto dependent = cast<DependentMemberType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[DependentMemberTypeLayout::Code];
    assert(dependent->getAssocType() && "Unchecked dependent member type");
    DependentMemberTypeLayout::emitRecord(
      Out, ScratchRecord, abbrCode,
      addTypeRef(dependent->getBase()),
      addDeclRef(dependent->getAssocType()));
    break;
  }

  case TypeKind::Function: {
    auto fnTy = cast<FunctionType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[FunctionTypeLayout::Code];
    FunctionTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
           addTypeRef(fnTy->getInput()),
           addTypeRef(fnTy->getResult()),
           getRawStableCC(fnTy->getAbstractCC()),
           fnTy->isAutoClosure(),
           fnTy->getRepresentation() == AnyFunctionType::Representation::Thin,
           fnTy->isNoReturn(),
           fnTy->getRepresentation() == AnyFunctionType::Representation::Block);
    break;
  }

  case TypeKind::PolymorphicFunction: {
    auto fnTy = cast<PolymorphicFunctionType>(ty.getPointer());
    const Decl *genericContext = getGenericContext(&fnTy->getGenericParams());
    auto callingConvention = fnTy->getAbstractCC();
    DeclID dID = genericContext ? addDeclRef(genericContext) : DeclID(0);

    unsigned abbrCode = DeclTypeAbbrCodes[PolymorphicFunctionTypeLayout::Code];
    PolymorphicFunctionTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
            addTypeRef(fnTy->getInput()),
            addTypeRef(fnTy->getResult()),
            dID,
            getRawStableCC(callingConvention),
            fnTy->getRepresentation() == AnyFunctionType::Representation::Thin,
            fnTy->isNoReturn());
    if (!genericContext)
      writeGenericParams(&fnTy->getGenericParams(), DeclTypeAbbrCodes);
    break;
  }

  case TypeKind::GenericFunction: {
    auto fnTy = cast<GenericFunctionType>(ty.getPointer());
    unsigned abbrCode = DeclTypeAbbrCodes[GenericFunctionTypeLayout::Code];
    auto callingConvention = fnTy->getAbstractCC();
    SmallVector<TypeID, 4> genericParams;
    for (auto param : fnTy->getGenericParams())
      genericParams.push_back(addTypeRef(param));
    GenericFunctionTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
            addTypeRef(fnTy->getInput()),
            addTypeRef(fnTy->getResult()),
            getRawStableCC(callingConvention),
            fnTy->getRepresentation() == AnyFunctionType::Representation::Thin,
            fnTy->isNoReturn(),
            genericParams);
    
    // Write requirements.
    writeRequirements(fnTy->getRequirements());
    break;
  }
      
  case TypeKind::SILBlockStorage: {
    auto storageTy = cast<SILBlockStorageType>(ty.getPointer());
    
    unsigned abbrCode = DeclTypeAbbrCodes[SILBlockStorageTypeLayout::Code];
    SILBlockStorageTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addTypeRef(storageTy->getCaptureType()));
    break;
  }
      
  case TypeKind::SILFunction: {
    auto fnTy = cast<SILFunctionType>(ty.getPointer());

    auto callingConvention = fnTy->getAbstractCC();
    auto interfaceResult = fnTy->getInterfaceResult();
    auto stableInterfaceResultConvention =
      getRawStableResultConvention(interfaceResult.getConvention());

    SmallVector<TypeID, 8> paramTypes;
    for (auto param : fnTy->getInterfaceParameters()) {
      paramTypes.push_back(addTypeRef(param.getType()));
      unsigned conv = getRawStableParameterConvention(param.getConvention());
      paramTypes.push_back(TypeID(conv));
    }

    auto sig = fnTy->getGenericSignature();
    if (sig) {
      for (auto param : sig->getGenericParams())
        paramTypes.push_back(addTypeRef(param));
    }

    auto stableCalleeConvention =
      getRawStableParameterConvention(fnTy->getCalleeConvention());

    unsigned abbrCode = DeclTypeAbbrCodes[SILFunctionTypeLayout::Code];
    SILFunctionTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
          addTypeRef(interfaceResult.getType()),
          stableInterfaceResultConvention,
          stableCalleeConvention,
          getRawStableCC(callingConvention),
          fnTy->getRepresentation() == AnyFunctionType::Representation::Thin,
          fnTy->getRepresentation() == AnyFunctionType::Representation::Block,
          fnTy->isNoReturn(),
          sig ? sig->getGenericParams().size() : 0,
          paramTypes);
    if (sig)
      writeRequirements(sig->getRequirements());
    else
      writeRequirements({});
    break;
  }
      
  case TypeKind::ArraySlice: {
    auto sliceTy = cast<ArraySliceType>(ty.getPointer());

    Type base = sliceTy->getBaseType();

    unsigned abbrCode = DeclTypeAbbrCodes[ArraySliceTypeLayout::Code];
    ArraySliceTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addTypeRef(base));
    break;
  }

  case TypeKind::Optional: {
    auto sliceTy = cast<OptionalType>(ty.getPointer());

    Type base = sliceTy->getBaseType();

    unsigned abbrCode = DeclTypeAbbrCodes[OptionalTypeLayout::Code];
    OptionalTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                   addTypeRef(base));
    break;
  }

  case TypeKind::UncheckedOptional: {
    auto sliceTy = cast<UncheckedOptionalType>(ty.getPointer());

    Type base = sliceTy->getBaseType();

    unsigned abbrCode = DeclTypeAbbrCodes[UncheckedOptionalTypeLayout::Code];
    UncheckedOptionalTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                            addTypeRef(base));
    break;
  }

  case TypeKind::ProtocolComposition: {
    auto composition = cast<ProtocolCompositionType>(ty.getPointer());

    SmallVector<TypeID, 4> protocols;
    for (auto proto : composition->getProtocols())
      protocols.push_back(addTypeRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[ProtocolCompositionTypeLayout::Code];
    ProtocolCompositionTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                              protocols);
    break;
  }

  case TypeKind::LValue: {
    auto lValueTy = cast<LValueType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[LValueTypeLayout::Code];
    LValueTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 addTypeRef(lValueTy->getObjectType()));
    break;
  }
  case TypeKind::InOut: {
    auto iotTy = cast<InOutType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[InOutTypeLayout::Code];
    InOutTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addTypeRef(iotTy->getObjectType()));
    break;
  }

  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
  case TypeKind::WeakStorage: {
    auto refTy = cast<ReferenceStorageType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[ReferenceStorageTypeLayout::Code];
    auto stableOwnership = getRawStableOwnership(refTy->getOwnership());
    ReferenceStorageTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           stableOwnership,
                                  addTypeRef(refTy->getReferentType()));
    break;
  }

  case TypeKind::UnboundGeneric: {
    auto generic = cast<UnboundGenericType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[UnboundGenericTypeLayout::Code];
    UnboundGenericTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                         addDeclRef(generic->getDecl()),
                                         addTypeRef(generic->getParent()));
    break;
  }

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct: {
    auto generic = cast<BoundGenericType>(ty.getPointer());

    // We don't want two copies of Archetype being serialized, one by
    // serializing genericArgs, the other by serializaing the Decl. The reason
    // is that it is likely the Decl's Archetype can be serialized in
    // a different module, causing two copies being constructed at
    // deserialization, one in the other module, one in this module as
    // genericArgs. The fix is to check if genericArgs exist in the Decl's
    // Archetypes, if they all exist, we use indices to the Decl's
    // Archetypes..
    bool allGenericArgsInDecl = true;
#ifndef NDEBUG
    bool someGenericArgsInDecl = false;
#endif
    SmallVector<TypeID, 8> genericArgIDs;
    // Push in a special number to say that IDs are indices to the Archetypes.
    if (!generic->getGenericArgs().empty())
      genericArgIDs.push_back(INT32_MAX);
    for (auto next : generic->getGenericArgs()) {
      bool found = false;
      if (auto arche = dyn_cast<ArchetypeType>(next.getPointer())) {
        auto genericParams = generic->getDecl()->getGenericParams();
        unsigned idx = 0;
        // Check if next exisits in the Decl.
        for (auto archetype : genericParams->getAllArchetypes()) {
          if (archetype == arche) {
            found = true;
            genericArgIDs.push_back(idx);
#ifndef NDEBUG
            someGenericArgsInDecl = true;
#endif
            break;
          }
          idx++;
        }
      }

      if (!found) {
        allGenericArgsInDecl = false;
        break;
      }
    }

    if (!allGenericArgsInDecl) {
#ifndef NDEBUG
      if (someGenericArgsInDecl && isDeclXRef(generic->getDecl()))
        // Emit warning message. 
        llvm::errs() << "Serialization: we may have two copied of Archetype\n";
#endif
      genericArgIDs.clear();
      for (auto next : generic->getGenericArgs())
        genericArgIDs.push_back(addTypeRef(next));
    }

    unsigned abbrCode = DeclTypeAbbrCodes[BoundGenericTypeLayout::Code];
    BoundGenericTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                       addDeclRef(generic->getDecl()),
                                       addTypeRef(generic->getParent()),
                                       genericArgIDs);
    break;
  }

  case TypeKind::TypeVariable:
    llvm_unreachable("type variables should not escape the type checker");
  }
}

void Serializer::writeAllDeclsAndTypes() {
  BCBlockRAII restoreBlock(Out, DECLS_AND_TYPES_BLOCK_ID, 8);

  {
    using namespace decls_block;
    registerDeclTypeAbbr<NameAliasTypeLayout>();
    registerDeclTypeAbbr<GenericTypeParamDeclLayout>();
    registerDeclTypeAbbr<AssociatedTypeDeclLayout>();
    registerDeclTypeAbbr<NominalTypeLayout>();
    registerDeclTypeAbbr<ParenTypeLayout>();
    registerDeclTypeAbbr<TupleTypeLayout>();
    registerDeclTypeAbbr<TupleTypeEltLayout>();
    registerDeclTypeAbbr<FunctionTypeLayout>();
    registerDeclTypeAbbr<MetatypeTypeLayout>();
    registerDeclTypeAbbr<ExistentialMetatypeTypeLayout>();
    registerDeclTypeAbbr<LValueTypeLayout>();
    registerDeclTypeAbbr<InOutTypeLayout>();
    registerDeclTypeAbbr<ArchetypeTypeLayout>();
    registerDeclTypeAbbr<ArchetypeNestedTypeNamesLayout>();
    registerDeclTypeAbbr<ArchetypeNestedTypesAreArchetypesLayout>();
    registerDeclTypeAbbr<ArchetypeNestedTypesLayout>();
    registerDeclTypeAbbr<ProtocolCompositionTypeLayout>();
    registerDeclTypeAbbr<SubstitutedTypeLayout>();
    registerDeclTypeAbbr<BoundGenericTypeLayout>();
    registerDeclTypeAbbr<BoundGenericSubstitutionLayout>();
    registerDeclTypeAbbr<PolymorphicFunctionTypeLayout>();
    registerDeclTypeAbbr<GenericFunctionTypeLayout>();
    registerDeclTypeAbbr<SILBlockStorageTypeLayout>();
    registerDeclTypeAbbr<SILFunctionTypeLayout>();
    registerDeclTypeAbbr<ArraySliceTypeLayout>();
    registerDeclTypeAbbr<ReferenceStorageTypeLayout>();
    registerDeclTypeAbbr<UnboundGenericTypeLayout>();
    registerDeclTypeAbbr<OptionalTypeLayout>();
    registerDeclTypeAbbr<UncheckedOptionalTypeLayout>();
    registerDeclTypeAbbr<DynamicSelfTypeLayout>();
    registerDeclTypeAbbr<OpenedExistentialTypeLayout>();

    registerDeclTypeAbbr<TypeAliasLayout>();
    registerDeclTypeAbbr<GenericTypeParamTypeLayout>();
    registerDeclTypeAbbr<AssociatedTypeTypeLayout>();
    registerDeclTypeAbbr<DependentMemberTypeLayout>();
    registerDeclTypeAbbr<StructLayout>();
    registerDeclTypeAbbr<ConstructorLayout>();
    registerDeclTypeAbbr<VarLayout>();
    registerDeclTypeAbbr<ParamLayout>();
    registerDeclTypeAbbr<FuncLayout>();
    registerDeclTypeAbbr<PatternBindingLayout>();
    registerDeclTypeAbbr<ProtocolLayout>();
    registerDeclTypeAbbr<PrefixOperatorLayout>();
    registerDeclTypeAbbr<PostfixOperatorLayout>();
    registerDeclTypeAbbr<InfixOperatorLayout>();
    registerDeclTypeAbbr<ClassLayout>();
    registerDeclTypeAbbr<EnumLayout>();
    registerDeclTypeAbbr<EnumElementLayout>();
    registerDeclTypeAbbr<SubscriptLayout>();
    registerDeclTypeAbbr<ExtensionLayout>();
    registerDeclTypeAbbr<DestructorLayout>();

    registerDeclTypeAbbr<ParenPatternLayout>();
    registerDeclTypeAbbr<TuplePatternLayout>();
    registerDeclTypeAbbr<TuplePatternEltLayout>();
    registerDeclTypeAbbr<NamedPatternLayout>();
    registerDeclTypeAbbr<VarPatternLayout>();
    registerDeclTypeAbbr<AnyPatternLayout>();
    registerDeclTypeAbbr<TypedPatternLayout>();

    registerDeclTypeAbbr<GenericParamListLayout>();
    registerDeclTypeAbbr<GenericParamLayout>();
    registerDeclTypeAbbr<GenericRequirementLayout>();
    registerDeclTypeAbbr<LastGenericRequirementLayout>();

    registerDeclTypeAbbr<XRefTypePathPieceLayout>();
    registerDeclTypeAbbr<XRefValuePathPieceLayout>();
    registerDeclTypeAbbr<XRefExtensionPathPieceLayout>();
    registerDeclTypeAbbr<XRefOperatorOrAccessorPathPieceLayout>();
    registerDeclTypeAbbr<XRefGenericParamPathPieceLayout>();

    registerDeclTypeAbbr<NoConformanceLayout>();
    registerDeclTypeAbbr<NormalProtocolConformanceLayout>();
    registerDeclTypeAbbr<SpecializedProtocolConformanceLayout>();
    registerDeclTypeAbbr<InheritedProtocolConformanceLayout>();
    registerDeclTypeAbbr<DeclContextLayout>();
    registerDeclTypeAbbr<XRefLayout>();

#define DECL_ATTR(X, NAME, ...) \
    registerDeclTypeAbbr<NAME##DeclAttrLayout>();
#define VIRTUAL_DECL_ATTR(X, NAME, ...)
#include "swift/AST/Attr.def"
  }

  while (!DeclsAndTypesToWrite.empty()) {
    DeclTypeUnion next = DeclsAndTypesToWrite.front();
    DeclsAndTypesToWrite.pop();

    auto id = DeclIDs[next].first;
    assert(id != 0 && "decl or type not referenced properly");
    (void)id;

    auto &offsets = next.isDecl() ? DeclOffsets : TypeOffsets;
    assert((id - 1) == offsets.size());

    offsets.push_back(Out.GetCurrentBitNo());

    if (next.isDecl())
      writeDecl(next.getDecl());
    else
      writeType(next.getType());
  }
}

void Serializer::writeAllIdentifiers() {
  BCBlockRAII restoreBlock(Out, IDENTIFIER_DATA_BLOCK_ID, 3);
  identifier_block::IdentifierDataLayout IdentifierData(Out);

  llvm::SmallString<4096> stringData;

  // Make sure no identifier has an offset of 0.
  stringData.push_back('\0');

  for (Identifier ident : IdentifiersToWrite) {
    IdentifierOffsets.push_back(stringData.size());
    stringData.append(ident.get());
    stringData.push_back('\0');
  }

  IdentifierData.emit(ScratchRecord, stringData.str());
}

void Serializer::writeOffsets(const index_block::OffsetsLayout &Offsets,
                              const std::vector<BitOffset> &values) {
  Offsets.emit(ScratchRecord, getOffsetRecordCode(values), values);
}

/// Writes an in-memory decl table to an on-disk representation, using the
/// given layout.
static void writeDeclTable(const index_block::DeclListLayout &DeclList,
                           index_block::RecordKind kind,
                           const Serializer::DeclTable &table) {
  if (table.empty())
    return;

  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::OnDiskChainedHashTableGenerator<DeclTableInfo> generator;
    for (auto &entry : table)
      generator.insert(entry.first, entry.second);

    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::Writer<little>(blobStream).write<uint32_t>(0);
    tableOffset = generator.Emit(blobStream);
  }

  DeclList.emit(scratch, kind, tableOffset, hashTableBlob);
}

namespace {

struct DeclCommentTableData {
  StringRef Brief;
  RawComment Raw;
};

class DeclCommentTableInfo {
public:
  using key_type = StringRef;
  using key_type_ref = key_type;
  using data_type = DeclCommentTableData;
  using data_type_ref = const data_type &;
  using hash_value_type = uint32_t;
  using offset_type = unsigned;

  hash_value_type ComputeHash(key_type_ref key) {
    assert(!key.empty());
    return llvm::HashString(key);
  }

  std::pair<unsigned, unsigned>
  EmitKeyDataLength(raw_ostream &out, key_type_ref key, data_type_ref data) {
    uint32_t keyLength = key.size();

    // Data consists of brief comment length and brief comment text,
    uint32_t dataLength = 4 + data.Brief.size();
    // number of raw comments,
    dataLength += 4;
    // for each raw comment: column number of the first line, length of each
    // raw comment and its text.
    for (auto C : data.Raw.Comments)
      dataLength += 4 + 4 + C.RawText.size();

    endian::Writer<little> writer(out);
    writer.write<uint32_t>(keyLength);
    writer.write<uint32_t>(dataLength);
    return { keyLength, dataLength };
  }

  void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
    out << key;
  }

  void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                unsigned len) {
    endian::Writer<little> writer(out);
    writer.write<uint32_t>(data.Brief.size());
    out << data.Brief;
    writer.write<uint32_t>(data.Raw.Comments.size());
    for (auto C : data.Raw.Comments) {
      writer.write<uint32_t>(C.StartColumn);
      writer.write<uint32_t>(C.RawText.size());
      out << C.RawText;
    }
  }
};

} // end unnamed namespace

static void writeDeclCommentTable(
    const comment_block::DeclCommentListLayout &DeclCommentList,
    const SourceFile *SF, const Module *M) {

  struct DeclCommentTableWriter : public ASTWalker {
    llvm::BumpPtrAllocator Arena;
    llvm::SmallString<512> USRBuffer;
    llvm::OnDiskChainedHashTableGenerator<DeclCommentTableInfo> generator;

    StringRef copyString(StringRef String) {
      char *Mem = static_cast<char *>(Arena.Allocate(String.size(), 1));
      std::copy(String.begin(), String.end(), Mem);
      return StringRef(Mem, String.size());
    }

    bool walkToDeclPre(Decl *D) override {
      auto *VD = dyn_cast<ValueDecl>(D);
      if (!VD)
        return true;

      // Skip the decl if it does not have a comment.
      RawComment Raw = VD->getRawComment();
      if (Raw.Comments.empty())
        return true;

      // Compute USR.
      {
        USRBuffer.clear();
        llvm::raw_svector_ostream OS(USRBuffer);
        if (ide::printDeclUSR(VD, OS))
          return true;
      }

      generator.insert(copyString(USRBuffer.str()),
                       { VD->getBriefComment(), Raw });
      return true;
    }
  };

  DeclCommentTableWriter Writer;

  ArrayRef<const FileUnit *> files = SF ? SF : M->getFiles();
  for (auto nextFile : files)
    const_cast<FileUnit *>(nextFile)->walk(Writer);

  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<32> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::raw_svector_ostream blobStream(hashTableBlob);
    // Make sure that no bucket is at offset 0
    endian::Writer<little>(blobStream).write<uint32_t>(0);
    tableOffset = Writer.generator.Emit(blobStream);
  }

  DeclCommentList.emit(scratch, tableOffset, hashTableBlob);
}

/// Translate from the AST known protocol enum to the Serialization enum
/// values, which are guaranteed to be stable.
static uint8_t getRawStableKnownProtocolKind(KnownProtocolKind kind) {
  switch (kind) {
#define PROTOCOL(Id) \
  case KnownProtocolKind::Id: return index_block::Id;
#include "swift/AST/KnownProtocols.def"
  }
}

/// Writes a list of decls known to conform to the given compiler-known
/// protocol.
static void
writeKnownProtocolList(const index_block::KnownProtocolLayout &AdopterList,
                       KnownProtocolKind kind, ArrayRef<DeclID> adopters) {
  if (adopters.empty())
    return;

  SmallVector<uint32_t, 32> scratch;
  AdopterList.emit(scratch, getRawStableKnownProtocolKind(kind), adopters);
}

/// Add operator methods from the given declaration type.
///
/// Recursively walks the members and derived global decls of any nested
/// nominal types.
template<typename Range>
static void addOperatorsAndTopLevel(Serializer &S, Range members,
                                    Serializer::DeclTable &operatorMethodDecls,
                                    Serializer::DeclTable &topLevelDecls,
                                    bool isDerivedTopLevel) {
  for (const Decl *member : members) {
    auto memberValue = dyn_cast<ValueDecl>(member);
    if (!memberValue)
      continue;

    if (isDerivedTopLevel) {
      topLevelDecls[memberValue->getName()].push_back({
        /*ignored*/0,
        S.addDeclRef(memberValue, /*force=*/true)
      });
    } else if (memberValue->isOperator()) {
      // Add operator methods.
      // Note that we don't have to add operators that are already in the
      // top-level list.
      operatorMethodDecls[memberValue->getName()].push_back({
        /*ignored*/0,
        S.addDeclRef(memberValue)
      });
    }

    // Recurse into nested types.
    if (auto nominal = dyn_cast<NominalTypeDecl>(member)) {
      addOperatorsAndTopLevel(S, nominal->getMembers(),
                             operatorMethodDecls, topLevelDecls, false);
      addOperatorsAndTopLevel(S, nominal->getDerivedGlobalDecls(),
                             operatorMethodDecls, topLevelDecls, true);
    }
  }
}

void Serializer::writeAST(ModuleOrSourceFile DC) {
  DeclTable topLevelDecls, extensionDecls, operatorDecls, operatorMethodDecls;
  ArrayRef<const FileUnit *> files = SF ? SF : M->getFiles();
  for (auto nextFile : files) {
    // FIXME: Switch to a visitor interface?
    SmallVector<Decl *, 32> fileDecls;
    nextFile->getTopLevelDecls(fileDecls);

    for (auto D : fileDecls) {
      if (isa<ImportDecl>(D))
        continue;
      else if (auto VD = dyn_cast<ValueDecl>(D)) {
        if (!VD->hasName())
          continue;
        topLevelDecls[VD->getName()]
          .push_back({ getKindForTable(D), addDeclRef(D) });

        // Add operator methods from nominal types.
        if (auto nominal = dyn_cast<NominalTypeDecl>(VD)) {
          addOperatorsAndTopLevel(*this, nominal->getMembers(),
                                 operatorMethodDecls, topLevelDecls, false);
          addOperatorsAndTopLevel(*this, nominal->getDerivedGlobalDecls(),
                                 operatorMethodDecls, topLevelDecls, true);
        }
      } else if (auto ED = dyn_cast<ExtensionDecl>(D)) {
        Type extendedTy = ED->getExtendedType();
        const NominalTypeDecl *extendedNominal = extendedTy->getAnyNominal();
        extensionDecls[extendedNominal->getName()]
          .push_back({ getKindForTable(extendedNominal), addDeclRef(D) });

        // Add operator methods from extensions.
        addOperatorsAndTopLevel(*this, ED->getMembers(),
                               operatorMethodDecls, topLevelDecls, false);

      } else if (auto OD = dyn_cast<OperatorDecl>(D)) {
        operatorDecls[OD->getName()]
          .push_back({ getStableFixity(OD->getKind()), addDeclRef(D) });
      }
    }
  }

  writeAllDeclsAndTypes();
  writeAllIdentifiers();

  {
    BCBlockRAII restoreBlock(Out, INDEX_BLOCK_ID, 4);

    index_block::OffsetsLayout Offsets(Out);
    writeOffsets(Offsets, DeclOffsets);
    writeOffsets(Offsets, TypeOffsets);
    writeOffsets(Offsets, IdentifierOffsets);

    index_block::DeclListLayout DeclList(Out);
    writeDeclTable(DeclList, index_block::TOP_LEVEL_DECLS, topLevelDecls);
    writeDeclTable(DeclList, index_block::OPERATORS, operatorDecls);
    writeDeclTable(DeclList, index_block::EXTENSIONS, extensionDecls);
    writeDeclTable(DeclList, index_block::CLASS_MEMBERS, ClassMembersByName);
    writeDeclTable(DeclList, index_block::OPERATOR_METHODS, operatorMethodDecls);

    {
      BCBlockRAII subBlock(Out, KNOWN_PROTOCOL_BLOCK_ID, 3);
      index_block::KnownProtocolLayout AdopterList(Out);

      for (unsigned i = 0; i < NumKnownProtocols; ++i) {
        writeKnownProtocolList(AdopterList, static_cast<KnownProtocolKind>(i),
                               KnownProtocolAdopters[i]);
      }
    }
  }
}

void Serializer::writeToStream(raw_ostream &os, ModuleOrSourceFile DC,
                               const SILModule *SILMod, bool serializeAllSIL,
                               FilenamesTy inputFiles,
                               StringRef moduleLinkName) {
  // Write the signature through the BitstreamWriter for alignment purposes.
  for (unsigned char byte : MODULE_SIGNATURE)
    Out.Emit(byte, 8);

  // FIXME: This is only really needed for debugging. We don't actually use it.
  writeBlockInfoBlock();

  {
    assert(!this->M && "already serializing a module");
    this->M = getModule(DC);
    this->SF = DC.dyn_cast<SourceFile *>();

    BCBlockRAII moduleBlock(Out, MODULE_BLOCK_ID, 2);
    writeHeader(M);
    writeInputFiles(M, inputFiles, moduleLinkName);
    writeSIL(SILMod, serializeAllSIL);
    writeAST(DC);
    Out.FlushToWord();

#ifndef NDEBUG
    this->M = nullptr;
    this->SF = nullptr;
#endif
  }

  os.write(Buffer.data(), Buffer.size());
  os.flush();
  Buffer.clear();
}

void Serializer::writeDocToStream(raw_ostream &os, ModuleOrSourceFile DC) {
  // Write the signature through the BitstreamWriter for alignment purposes.
  for (unsigned char byte : MODULE_DOC_SIGNATURE)
    Out.Emit(byte, 8);

  // FIXME: This is only really needed for debugging. We don't actually use it.
  writeDocBlockInfoBlock();

  {
    assert(!this->M && "already serializing a module");
    this->M = getModule(DC);
    this->SF = DC.dyn_cast<SourceFile *>();

    BCBlockRAII moduleBlock(Out, MODULE_DOC_BLOCK_ID, 2);
    writeHeader(M);
    {
      BCBlockRAII restoreBlock(Out, COMMENT_BLOCK_ID, 4);

      comment_block::DeclCommentListLayout DeclCommentList(Out);
      writeDeclCommentTable(DeclCommentList, SF, M);
    }
    Out.FlushToWord();

#ifndef NDEBUG
    this->M = nullptr;
    this->SF = nullptr;
#endif
  }

  os.write(Buffer.data(), Buffer.size());
  os.flush();
  Buffer.clear();
}

void swift::serialize(ModuleOrSourceFile DC, const char *outputPath,
                      const SILModule *M, bool serializeAllSIL,
                      FilenamesTy inputFiles, StringRef moduleLinkName) {
  std::string errorInfo;
  llvm::raw_fd_ostream out(outputPath, errorInfo, llvm::sys::fs::F_None);

  if (out.has_error() || !errorInfo.empty()) {
    getContext(DC).Diags.diagnose(SourceLoc(), diag::error_opening_output,
                                  outputPath, errorInfo);
    out.clear_error();
    return;
  }

  serializeToStream(DC, out, M, serializeAllSIL, inputFiles, moduleLinkName);
}

void swift::serializeToStream(ModuleOrSourceFile DC, raw_ostream &out,
                              const SILModule *M, bool serializeAllSIL,
                              FilenamesTy inputFiles,
                              StringRef moduleLinkName) {
  Serializer S;
  S.writeToStream(out, DC, M, serializeAllSIL, inputFiles, moduleLinkName);
}

void swift::serializeModuleDoc(ModuleOrSourceFile DC, const char *outputPath) {
  std::string errorInfo;
  llvm::raw_fd_ostream out(outputPath, errorInfo, llvm::sys::fs::F_None);

  if (out.has_error() || !errorInfo.empty()) {
    getContext(DC).Diags.diagnose(SourceLoc(), diag::error_opening_output,
                                  outputPath, errorInfo);
    out.clear_error();
    return;
  }

  Serializer S;
  S.writeDocToStream(out, DC);
}
