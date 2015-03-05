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

#include "Serialization.h"
#include "SILFormat.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/RawComment.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Dwarf.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Serialization/SerializationOptions.h"

#include "clang/Basic/Module.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Bitcode/RecordLayout.h"
#include "llvm/Config/config.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/OnDiskHashTable.h"
#include "llvm/Support/raw_ostream.h"

#include <vector>

using namespace swift;
using namespace swift::serialization;
using namespace llvm::support;
using llvm::BCBlockRAII;

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

  class LocalDeclTableInfo {
  public:
    using key_type = std::string;
    using key_type_ref = StringRef;
    using data_type = std::pair<DeclID, unsigned>; // ID, local discriminator
    using data_type_ref = const data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      assert(!key.empty());
      return llvm::HashString(key);
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      uint32_t keyLength = key.size();
      uint32_t dataLength = sizeof(DeclID) + sizeof(unsigned);
      endian::Writer<little> writer(out);
      writer.write<uint16_t>(keyLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      out << key;
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(sizeof(DeclID) <= 4, "DeclID too large");
      endian::Writer<little> writer(out);
      writer.write<DeclID>(data.first);
      writer.write<unsigned>(data.second);
    }
  };

  using LocalTypeHashTableGenerator =
    llvm::OnDiskChainedHashTableGenerator<LocalDeclTableInfo>;

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

static bool shouldSerializeAsLocalContext(const DeclContext *DC) {
  return DC->isLocalContext() && !isa<AbstractFunctionDecl>(DC);
}

static const Decl *getDeclForContext(const DeclContext *DC) {
  switch (DC->getContextKind()) {
  case DeclContextKind::Module:
    // Use a null decl to represent the module.
    return nullptr;
  case DeclContextKind::FileUnit:
    return getDeclForContext(DC->getParent());
  case DeclContextKind::SerializedLocal:
    llvm_unreachable("Serialized local contexts should only come from deserialization");
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

namespace {
  struct Accessors {
    StorageKind Kind;
    FuncDecl *Get = nullptr, *Set = nullptr, *MaterializeForSet = nullptr;
    FuncDecl *Address = nullptr, *MutableAddress = nullptr;
    FuncDecl *WillSet = nullptr, *DidSet = nullptr;
  };
}

static StorageKind getRawStorageKind(AbstractStorageDecl::StorageKindTy kind) {
  switch (kind) {
#define CASE(KIND) case AbstractStorageDecl::KIND: return StorageKind::KIND
  CASE(Stored);
  CASE(StoredWithTrivialAccessors);
  CASE(StoredWithObservers);
  CASE(InheritedWithObservers);
  CASE(Computed);
  CASE(ComputedWithMutableAddress);
  CASE(Addressed);
  CASE(AddressedWithTrivialAccessors);
  CASE(AddressedWithObservers);
#undef CASE
  }
  llvm_unreachable("bad storage kind");
}

static Accessors getAccessors(const AbstractStorageDecl *storage) {
  Accessors accessors;
  accessors.Kind = getRawStorageKind(storage->getStorageKind());
  switch (auto storageKind = storage->getStorageKind()) {
  case AbstractStorageDecl::Stored:
    return accessors;

  case AbstractStorageDecl::Addressed:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::ComputedWithMutableAddress:
    accessors.Address = storage->getAddressor();
    accessors.MutableAddress = storage->getMutableAddressor();
    if (storageKind == AbstractStorageDecl::Addressed)
      return accessors;
    goto getset;

  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::AddressedWithObservers:
    accessors.WillSet = storage->getWillSetFunc();
    accessors.DidSet = storage->getDidSetFunc();
    goto getset;

  case AbstractStorageDecl::StoredWithTrivialAccessors:
  case AbstractStorageDecl::Computed:
  getset:
    accessors.Get = storage->getGetter();
    accessors.Set = storage->getSetter();
    accessors.MaterializeForSet = storage->getMaterializeForSetFunc();
    return accessors;
  }
  llvm_unreachable("bad storage kind");
}

DeclID Serializer::addLocalDeclContextRef(const DeclContext *DC) {
  assert(DC->isLocalContext() && "Expected a local DeclContext");
  auto &id = LocalDeclContextIDs[DC];
  if (id != 0)
    return id;

  id = { ++LastLocalDeclContextID };
  LocalDeclContextsToWrite.push(DC);
  return id;
}

DeclContextID Serializer::addDeclContextRef(const DeclContext *DC) {
  switch (DC->getContextKind()) {
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit: // Skip up to the module
    return 0;
  default:
    break;
  }

  // If this decl context is a plain old serializable decl, queue it up for
  // normal serialization.
  if (shouldSerializeAsLocalContext(DC))
    addLocalDeclContextRef(DC);
  else
    addDeclRef(getDeclForContext(DC));

  auto &id = DeclContextIDs[DC];
  if (id)
    return id;

  id = { ++LastDeclContextID };
  DeclContextsToWrite.push(DC);

  return id;
}

DeclID Serializer::addDeclRef(const Decl *D, bool forceSerialization) {
  if (!D)
    return 0;

  DeclIDAndForce &id = DeclAndTypeIDs[D];
  if (id.first != 0) {
    if (forceSerialization && !id.second)
      id.second = true;
    return id.first;
  }

  assert((!isDeclXRef(D) || isa<ValueDecl>(D) || isa<OperatorDecl>(D)) &&
         "cannot cross-reference this decl");

  // Record any generic parameters that come from this decl, so that we can use
  // the decl to refer to the parameters later.
  const GenericParamList *paramList = nullptr;
  if (auto fn = dyn_cast<AbstractFunctionDecl>(D))
    paramList = fn->getGenericParams();
  else if (auto nominal = dyn_cast<NominalTypeDecl>(D))
    paramList = nominal->getGenericParams();
  if (paramList)
    GenericContexts[paramList] = D;

  id = { ++LastDeclID, forceSerialization };
  DeclsAndTypesToWrite.push(D);
  return id.first;
}

TypeID Serializer::addTypeRef(Type ty) {
  if (!ty)
    return 0;

  auto &id = DeclAndTypeIDs[ty];
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
  if (M == this->M)
    return CURRENT_MODULE_ID;
  if (M == this->M->Ctx.TheBuiltinModule)
    return BUILTIN_MODULE_ID;

  auto clangImporter =
    static_cast<ClangImporter *>(this->M->Ctx.getClangModuleLoader());
  if (M == clangImporter->getImportedHeaderModule())
    return OBJC_HEADER_MODULE_ID;

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
  BLOCK_RECORD(control_block, TARGET);

  BLOCK(OPTIONS_BLOCK);
  BLOCK_RECORD(options_block, SDK_PATH);
  BLOCK_RECORD(options_block, XCC);
  BLOCK_RECORD(options_block, IS_SIB);

  BLOCK(INPUT_BLOCK);
  BLOCK_RECORD(input_block, IMPORTED_MODULE);
  BLOCK_RECORD(input_block, LINK_LIBRARY);
  BLOCK_RECORD(input_block, IMPORTED_HEADER);
  BLOCK_RECORD(input_block, IMPORTED_HEADER_CONTENTS);
  BLOCK_RECORD(input_block, MODULE_FLAGS);
  BLOCK_RECORD(input_block, SEARCH_PATH);

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
  BLOCK_RECORD(index_block, OBJC_METHODS);
  BLOCK_RECORD(index_block, ENTRY_POINT);
  BLOCK_RECORD(index_block, LOCAL_DECL_CONTEXT_OFFSETS);
  BLOCK_RECORD(index_block, DECL_CONTEXT_OFFSETS);
  BLOCK_RECORD(index_block, LOCAL_TYPE_DECLS);

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
  BLOCK_RECORD(sil_block, SIL_INST_WITNESS_METHOD);

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
  BLOCK_RECORD(control_block, TARGET);

  BLOCK(COMMENT_BLOCK);
  BLOCK_RECORD(comment_block, DECL_COMMENTS);

#undef BLOCK
#undef BLOCK_RECORD
}

void Serializer::writeHeader(const SerializationOptions &options) {
  {
    BCBlockRAII restoreBlock(Out, CONTROL_BLOCK_ID, 3);
    control_block::ModuleNameLayout ModuleName(Out);
    control_block::MetadataLayout Metadata(Out);
    control_block::TargetLayout Target(Out);

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

    Target.emit(ScratchRecord, M->Ctx.LangOpts.Target.str());

    {
      llvm::BCBlockRAII restoreBlock(Out, OPTIONS_BLOCK_ID, 3);

      options_block::IsSIBLayout IsSIB(Out);
      IsSIB.emit(ScratchRecord, options.IsSIB);

      if (options.SerializeOptionsForDebugging) {
        options_block::SDKPathLayout SDKPath(Out);
        options_block::XCCLayout XCC(Out);

        SDKPath.emit(ScratchRecord, M->Ctx.SearchPathOpts.SDKPath);
        for (const std::string &arg : options.ExtraClangOptions) {
          XCC.emit(ScratchRecord, arg);
        }
      }
    }
  }
}

void Serializer::writeDocHeader() {
  {
    BCBlockRAII restoreBlock(Out, CONTROL_BLOCK_ID, 3);
    control_block::ModuleNameLayout ModuleName(Out);
    control_block::MetadataLayout Metadata(Out);
    control_block::TargetLayout Target(Out);

    // FIXME: put a real version in here.
#ifdef LLVM_VERSION_INFO
# define EXTRA_VERSION_STRING PACKAGE_STRING LLVM_VERSION_INFO
#else
# define EXTRA_VERSION_STRING PACKAGE_STRING
#endif
    Metadata.emit(ScratchRecord,
                  VERSION_MAJOR, VERSION_MINOR, EXTRA_VERSION_STRING);
#undef EXTRA_VERSION_STRING

    Target.emit(ScratchRecord, M->Ctx.LangOpts.Target.str());
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
  ArrayRef<FileUnit *> files = import.second->getFiles();
  if (auto clangModule = dyn_cast<ClangModuleUnit>(files.front())) {
    // FIXME: This is an awful hack to handle Clang submodules.
    // Once Swift has a native notion of submodules, this can go away.
    const clang::Module *submodule = clangModule->getClangModule();
    SmallVector<StringRef, 4> submoduleNames;
    do {
      submoduleNames.push_back(submodule->Name);
      submodule = submodule->Parent;
    } while (submodule);
    interleave(submoduleNames.rbegin(), submoduleNames.rend(),
               [&out](StringRef next) { out.append(next); },
               [&out] { out.push_back('\0'); });
  } else {
    out.append(import.second->Name.str());
  }

  if (import.first.empty())
    return;

  out.push_back('\0');
  assert(import.first.size() == 1 && "can only handle top-level decl imports");
  auto accessPathElem = import.first.front();
  out.append(accessPathElem.first.str());
}

void Serializer::writeInputBlock(const SerializationOptions &options) {
  BCBlockRAII restoreBlock(Out, INPUT_BLOCK_ID, 4);
  input_block::ImportedModuleLayout ImportedModule(Out);
  input_block::LinkLibraryLayout LinkLibrary(Out);
  input_block::ImportedHeaderLayout ImportedHeader(Out);
  input_block::ImportedHeaderContentsLayout ImportedHeaderContents(Out);
  input_block::ModuleFlagsLayout ModuleFlags(Out);
  input_block::SearchPathLayout SearchPath(Out);

  if (options.SerializeOptionsForDebugging) {
    const SearchPathOptions &searchPathOpts = M->Ctx.SearchPathOpts;
    for (auto &path : searchPathOpts.ImportSearchPaths)
      SearchPath.emit(ScratchRecord, /*framework=*/false, path);
    for (auto &path : searchPathOpts.FrameworkSearchPaths)
      SearchPath.emit(ScratchRecord, /*framework=*/true, path);
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
  auto clangImporter =
    static_cast<ClangImporter *>(M->Ctx.getClangModuleLoader());
  Module *importedHeaderModule = clangImporter->getImportedHeaderModule();
  Module *theBuiltinModule = M->Ctx.TheBuiltinModule;
  for (auto import : allImports) {
    if (import.second == theBuiltinModule)
      continue;

    if (import.second == importedHeaderModule) {
      off_t importedHeaderSize = 0;
      time_t importedHeaderModTime = 0;
      std::string contents;
      if (!options.ImportedHeader.empty())
        contents = clangImporter->getBridgingHeaderContents(
            options.ImportedHeader, importedHeaderSize, importedHeaderModTime);
      ImportedHeader.emit(ScratchRecord, publicImportSet.count(import),
                          importedHeaderSize, importedHeaderModTime,
                          options.ImportedHeader);
      if (!contents.empty()) {
        contents.push_back('\0');
        ImportedHeaderContents.emit(ScratchRecord, contents);
      }
      continue;
    }

    ImportPathBlob importPath;
    flattenImportPath(import, importPath);
    ImportedModule.emit(ScratchRecord, publicImportSet.count(import),
                        !import.first.empty(), importPath);
  }

  if (!options.ModuleLinkName.empty()) {
    LinkLibrary.emit(ScratchRecord, serialization::LibraryKind::Library,
                     options.AutolinkForceLoad, options.ModuleLinkName);
  }

  ModuleFlags.emit(ScratchRecord, options.HasUnderlyingModule);
}

/// Translate AST default argument kind to the Serialization enum values, which
/// are guaranteed to be stable.
static uint8_t getRawStableDefaultArgumentKind(swift::DefaultArgumentKind kind) {
  switch (kind) {
#define CASE(X) \
  case swift::DefaultArgumentKind::X: \
    return static_cast<uint8_t>(serialization::DefaultArgumentKind::X);
  CASE(None)
  CASE(Normal)
  CASE(Inherited)
  CASE(Column)
  CASE(File)
  CASE(Line)
  CASE(Function)
  CASE(DSOHandle)
#undef CASE
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

static uint8_t getRawStableAddressorKind(swift::AddressorKind kind) {
  switch (kind) {
  case swift::AddressorKind::NotAddressor:
    return uint8_t(serialization::AddressorKind::NotAddressor);
  case swift::AddressorKind::Unsafe:
    return uint8_t(serialization::AddressorKind::Unsafe);
  case swift::AddressorKind::Owning:
    return uint8_t(serialization::AddressorKind::Owning);
  case swift::AddressorKind::NativeOwning:
    return uint8_t(serialization::AddressorKind::NativeOwning);
  case swift::AddressorKind::NativePinning:
    return uint8_t(serialization::AddressorKind::NativePinning);
  }
  llvm_unreachable("bad addressor kind");
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
                                   addDeclRef(next));
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
  if (!conformance) {
    typeID = addDeclRef(nullptr);
    moduleID = serialization::BUILTIN_MODULE_ID;
    return false;
  }

  bool append = !isa<NormalProtocolConformance>(conformance);
  if (!append)
    append = getDeclForContext(conformance->getDeclContext())->hasClangNode();
  if (!append && !allowReferencingCurrentModule)
    append = conformance->getDeclContext()->getParentModule() == M;

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

  switch (conformance->getKind()) {
  case ProtocolConformanceKind::Normal: {
    auto conf = cast<NormalProtocolConformance>(conformance);

    SmallVector<DeclID, 32> data;
    unsigned numValueWitnesses = 0;
    unsigned numTypeWitnesses = 0;
    unsigned numDefaultedDefinitions = 0;

    SmallVector<ProtocolDecl *, 8> inheritedProtos;
    SmallVector<ProtocolConformance *, 8> inheritedConformance;
    for (auto inheritedMapping : conf->getInheritedConformances()) {
      DeclID typeDeclID;
      ModuleID confModuleID;
      if (encodeReferencedConformance(inheritedMapping.second, typeDeclID,
                                      confModuleID,
                                      /*allowReferencingCurrent=*/false)) {
        inheritedProtos.push_back(inheritedMapping.first);
        inheritedConformance.push_back(inheritedMapping.second);
      }
      data.push_back(addDeclRef(inheritedMapping.first));
      data.push_back(typeDeclID);
      data.push_back(confModuleID);
    }

    // If we have an incomplete conformance, it might have just not been
    // deserialized. That's okay if this is just a reference to it.
    if (conformance->isIncomplete()) {
      assert(writeIncomplete && "trying to write an incomplete conformance");
    } else {
      size_t initialSize = data.size();
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

      if (writeIncomplete) {
        data.resize(initialSize);
        numValueWitnesses = numTypeWitnesses = numDefaultedDefinitions = 0;
      }
    }

    unsigned numInheritedConformances = conf->getInheritedConformances().size();
    unsigned abbrCode
      = abbrCodes[NormalProtocolConformanceLayout::Code];
    auto ownerID = addDeclContextRef(conf->getDeclContext());
    NormalProtocolConformanceLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                                addDeclRef(protocol), ownerID,
                                                numValueWitnesses,
                                                numTypeWitnesses,
                                                numInheritedConformances,
                                                numDefaultedDefinitions,
                                                writeIncomplete,
                                                data);

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
    unsigned abbrCode = abbrCodes[SpecializedProtocolConformanceLayout::Code];
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

    for (const ProtocolConformance *conformance : sub.getConformances()) {
      DeclID typeID;
      ModuleID moduleID;
      if (encodeReferencedConformance(conformance, typeID, moduleID, false))
        conformancesToWrite.push_back(conformance);
      conformanceData.push_back(typeID);
      conformanceData.push_back(moduleID);
    }

    BoundGenericSubstitutionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                               addTypeRef(sub.getArchetype()),
                                               addTypeRef(sub.getReplacement()),
                                               conformanceData);

    for (const ProtocolConformance *conformance : conformancesToWrite) {
      writeConformance(conformance->getProtocol(), conformance, nullptr,
                       abbrCodes, /*writeIncomplete=*/true);
    }
  }
}

static uint8_t getRawStableOptionalTypeKind(swift::OptionalTypeKind kind) {
  switch (kind) {
  case swift::OTK_None:
    return static_cast<uint8_t>(serialization::OptionalTypeKind::None);
  case swift::OTK_Optional:
    return static_cast<uint8_t>(serialization::OptionalTypeKind::Optional);
  case swift::OTK_ImplicitlyUnwrappedOptional:
    return static_cast<uint8_t>(
             serialization::OptionalTypeKind::ImplicitlyUnwrappedOptional);
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

  case DeclKind::EnumElement:
  case DeclKind::Protocol:
  case DeclKind::Constructor:
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

  unsigned abbrCode = DeclTypeAbbrCodes[MembersLayout::Code];
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
  MembersLayout::emitRecord(Out, ScratchRecord, abbrCode, memberIDs);
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
  CASE(MaterializeForSet)
  CASE(Addressor)
  CASE(MutableAddressor)
#undef CASE
  }
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

void Serializer::writeCrossReference(const DeclContext *DC, uint32_t pathLen) {
  using namespace decls_block;

  unsigned abbrCode;

  switch (DC->getContextKind()) {
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::Initializer:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::SerializedLocal:
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

    if (auto ctor = dyn_cast<ConstructorDecl>(DC)) {
      abbrCode = DeclTypeAbbrCodes[XRefInitializerPathPieceLayout::Code];
      XRefInitializerPathPieceLayout::emitRecord(
        Out, ScratchRecord, abbrCode, addTypeRef(ty),
        getStableCtorInitializerKind(ctor->getInitKind()));
      break;
    }

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
    assert(!D->getDeclContext()->isModuleScopeContext() &&
           "Cannot cross reference a generic type decl at module scope.");
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

static uint8_t getRawStableAccessibility(Accessibility access) {
  switch (access) {
#define CASE(NAME) \
  case Accessibility::NAME: \
    return static_cast<uint8_t>(serialization::AccessibilityKind::NAME);
  CASE(Private)
  CASE(Internal)
  CASE(Public)
#undef CASE
  }
}

#ifndef NDEBUG
#define DEF_VERIFY_ATTR(DECL)\
static void verifyAttrSerializable(const DECL ## Decl *D) {\
  for (auto Attr : D->getAttrs()) {\
    assert(Attr->canAppearOnDecl(D) && "attribute cannot appear on a " #DECL);\
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

static inline unsigned getOptionalOrZero(const llvm::Optional<unsigned> &X) {
  if (X.hasValue())
    return X.getValue();
  return 0;
}

void Serializer::writeDeclAttribute(const DeclAttribute *DA) {
  using namespace decls_block;

  // Completely ignore attributes that aren't serialized.
  if (DA->isNotSerialized())
    return;

  switch (DA->getKind()) {
  case DAK_RawDocComment:
  case DAK_Ownership: // Serialized as part of the type.
  case DAK_Accessibility:
  case DAK_SetterAccessibility:
  case DAK_ObjCBridged:
  case DAK_Count:
    llvm_unreachable("cannot serialize DAK_Count");
    return;

#define SIMPLE_DECL_ATTR(_, CLASS, ...)\
  case DAK_##CLASS: { \
    auto abbrCode = DeclTypeAbbrCodes[CLASS##DeclAttrLayout::Code]; \
    CLASS##DeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode, \
                                      DA->isImplicit()); \
    return; \
  }
#include "swift/AST/Attr.def"

  case DAK_Asmname: {
    auto *theAttr = cast<AsmnameAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[AsmnameDeclAttrLayout::Code];
    AsmnameDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      theAttr->isImplicit(),
                                      theAttr->Name);
    return;
  }
  case DAK_Semantics: {
    auto *theAttr = cast<SemanticsAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[SemanticsDeclAttrLayout::Code];
    SemanticsDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      theAttr->isImplicit(),
                                      theAttr->Value);
    return;
  }

  case DAK_Inline: {
    auto *theAttr = cast<InlineAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[InlineDeclAttrLayout::Code];
    InlineDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     (unsigned)theAttr->getKind());
    return;
  }

  case DAK_Effects: {
    auto *theAttr = cast<EffectsAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[EffectsDeclAttrLayout::Code];
    EffectsDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     (unsigned)theAttr->getKind());
    return;
  }

  case DAK_Availability: {
#define LIST_VER_TUPLE_PIECES(X)\
  X##_Major, X##_Minor, X##_Subminor, X##_HasMinor, X##_HasSubminor
#define DEF_VER_TUPLE_PIECES(X, X_Expr)\
  unsigned X##_Major = 0, X##_Minor = 0, X##_Subminor = 0,\
           X##_HasMinor = 0, X##_HasSubminor = 0;\
  const auto &X##_Val = X_Expr;\
  if (X##_Val.hasValue()) {\
    const auto &Y = X##_Val.getValue();\
    X##_Major = Y.getMajor();\
    X##_Minor = getOptionalOrZero(Y.getMinor());\
    X##_Subminor = getOptionalOrZero(Y.getSubminor());\
    X##_HasMinor = Y.getMinor().hasValue();\
    X##_HasSubminor = Y.getSubminor().hasValue();\
  }

    auto *theAttr = cast<AvailabilityAttr>(DA);
    DEF_VER_TUPLE_PIECES(Introduced, theAttr->Introduced)
    DEF_VER_TUPLE_PIECES(Deprecated, theAttr->Deprecated)
    DEF_VER_TUPLE_PIECES(Obsoleted, theAttr->Obsoleted)

    llvm::SmallString<32> blob;
    blob.append(theAttr->Message);
    blob.append(theAttr->Rename);
    auto abbrCode = DeclTypeAbbrCodes[AvailabilityDeclAttrLayout::Code];
    AvailabilityDeclAttrLayout::emitRecord(
        Out, ScratchRecord, abbrCode,
        theAttr->isImplicit(),
        theAttr->IsUnvailable,
        LIST_VER_TUPLE_PIECES(Introduced),
        LIST_VER_TUPLE_PIECES(Deprecated),
        LIST_VER_TUPLE_PIECES(Obsoleted),
        static_cast<unsigned>(theAttr->Platform),
        theAttr->Message.size(),
        theAttr->Rename.size(),
        blob);
    return;
#undef LIST_VER_TUPLE_PIECES
#undef DEF_VER_TUPLE_PIECES
  }

  case DAK_AutoClosure: {
    auto *theAttr = cast<AutoClosureAttr>(DA);
    auto abbrCode = DeclTypeAbbrCodes[AutoClosureDeclAttrLayout::Code];
    AutoClosureDeclAttrLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                          theAttr->isImplicit(),
                                          theAttr->isEscaping());
    return;
  }

  case DAK_ObjC: {
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
                                   theAttr->isImplicit(), 
                                   theAttr->isNameImplicit(), numArgs, pieces);
    return;
  }
  }
}

bool Serializer::isDeclXRef(const Decl *D) const {
  const DeclContext *topLevel = D->getDeclContext()->getModuleScopeContext();
  if (topLevel->getParentModule() != M)
    return true;
  if (!SF || topLevel == SF)
    return false;
  // Special-case for SIL generic parameter decls, which don't have a real
  // DeclContext.
  if (!isa<FileUnit>(topLevel)) {
    assert(isa<GenericTypeParamDecl>(D) && "unexpected decl kind");
    return false;
  }
  return !isForced(D, DeclAndTypeIDs);
}

void Serializer::writeDeclContext(const DeclContext *DC) {
  using namespace decls_block;
  auto isDecl = false;
  auto id = DeclContextIDs[DC];
  assert(id != 0 && "decl context not referenced properly");
  (void)id;

  assert((id - 1) == DeclContextOffsets.size());
  DeclContextOffsets.push_back(Out.GetCurrentBitNo());

  auto abbrCode = DeclTypeAbbrCodes[DeclContextLayout::Code];
  DeclContextID declOrDeclContextID = 0;

  switch (DC->getContextKind()) {
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::NominalTypeDecl:
  case DeclContextKind::ExtensionDecl:
    declOrDeclContextID = addDeclRef(getDeclForContext(DC));
    isDecl = true;
    break;

  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::Initializer:
  case DeclContextKind::SerializedLocal:
    declOrDeclContextID = addLocalDeclContextRef(DC);
    break;
  case DeclContextKind::Module:
    llvm_unreachable("References to the module are serialized implicitly");
  case DeclContextKind::FileUnit:
    llvm_unreachable("Can't serialize a FileUnit");
  }

  DeclContextLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                declOrDeclContextID, isDecl);
}

void Serializer::writePatternBindingInitializer(PatternBindingDecl *binding) {
  using namespace decls_block;
  auto abbrCode = DeclTypeAbbrCodes[PatternBindingInitializerLayout::Code];
  PatternBindingInitializerLayout::emitRecord(Out, ScratchRecord,
                                              abbrCode, addDeclRef(binding));
}

void
Serializer::writeDefaultArgumentInitializer(const DeclContext *parentContext,
                                            unsigned index) {
  using namespace decls_block;
  auto abbrCode = DeclTypeAbbrCodes[DefaultArgumentInitializerLayout::Code];
  DefaultArgumentInitializerLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                               addDeclContextRef(parentContext),
                                               index);
}

void Serializer::writeAbstractClosureExpr(const DeclContext *parentContext,
                                          Type Ty, bool isImplicit,
                                          unsigned discriminator) {
  using namespace decls_block;
  auto abbrCode = DeclTypeAbbrCodes[AbstractClosureExprLayout::Code];
  AbstractClosureExprLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                        addTypeRef(Ty), isImplicit,
                                        discriminator,
                                        addDeclContextRef(parentContext));
}

void Serializer::writeLocalDeclContext(const DeclContext *DC) {
  using namespace decls_block;

  assert(shouldSerializeAsLocalContext(DC) &&
         "Can't serialize as local context");

  auto id = LocalDeclContextIDs[DC];
  assert(id != 0 && "decl context not referenced properly");
  (void)id;

  assert((id - 1)== LocalDeclContextOffsets.size());
  LocalDeclContextOffsets.push_back(Out.GetCurrentBitNo());

  switch (DC->getContextKind()) {
  case DeclContextKind::AbstractClosureExpr: {
    auto ACE = cast<AbstractClosureExpr>(DC);
    writeAbstractClosureExpr(ACE->getParent(), ACE->getType(),
                             ACE->isImplicit(), ACE->getDiscriminator());
    break;
  }

  case DeclContextKind::Initializer: {
    if (auto PBI = dyn_cast<PatternBindingInitializer>(DC)) {
      writePatternBindingInitializer(PBI->getBinding());
    } else if (auto DAI = dyn_cast<DefaultArgumentInitializer>(DC)) {
      writeDefaultArgumentInitializer(DAI->getParent(), DAI->getIndex());
    }
    break;
  }

  case DeclContextKind::TopLevelCodeDecl: {
    auto abbrCode = DeclTypeAbbrCodes[TopLevelCodeDeclContextLayout::Code];
    TopLevelCodeDeclContextLayout::emitRecord(Out, ScratchRecord, abbrCode,
      addDeclContextRef(DC->getParent()));
    break;
  }

  // If we are merging already serialized modules with local decl contexts,
  // we handle them here in a similar fashion.
  case DeclContextKind::SerializedLocal: {
    auto local = cast<SerializedLocalDeclContext>(DC);
    switch (local->getLocalDeclContextKind()) {
    case LocalDeclContextKind::AbstractClosure: {
      auto SACE = cast<SerializedAbstractClosureExpr>(local);
      writeAbstractClosureExpr(SACE->getParent(), SACE->getType(),
                               SACE->isImplicit(), SACE->getDiscriminator());
      return;
    }
    case LocalDeclContextKind::DefaultArgumentInitializer: {
      auto DAI = cast<SerializedDefaultArgumentInitializer>(local);
      writeDefaultArgumentInitializer(DAI->getParent(), DAI->getIndex());
      return;
    }
    case LocalDeclContextKind::PatternBindingInitializer: {
      auto PBI = cast<SerializedPatternBindingInitializer>(local);
      writePatternBindingInitializer(PBI->getBinding());
      return;
    }
    case LocalDeclContextKind::TopLevelCodeDecl: {
      auto abbrCode = DeclTypeAbbrCodes[TopLevelCodeDeclContextLayout::Code];
      TopLevelCodeDeclContextLayout::emitRecord(Out, ScratchRecord,
        abbrCode, addDeclContextRef(DC->getParent()));
      return;
    }
    }
  }

  default:
    llvm_unreachable("Trying to write a DeclContext that isn't local");
  }
}

void Serializer::writeDecl(const Decl *D) {
  using namespace decls_block;

  auto id = DeclAndTypeIDs[D].first;
  assert(id != 0 && "decl or type not referenced properly");
  (void)id;

  assert((id - 1) == DeclOffsets.size());
  DeclOffsets.push_back(Out.GetCurrentBitNo());

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

  if (M->Ctx.LangOpts.UsePrivateDiscriminators) {
    if (auto *value = dyn_cast<ValueDecl>(D)) {
      if (value->hasAccessibility() &&
          value->getAccessibility() == Accessibility::Private &&
          !value->getDeclContext()->isLocalContext()) {
        // FIXME: We shouldn't need to encode this for /all/ private decls.
        // In theory we can follow the same rules as mangling and only include
        // the outermost private context.
        auto topLevelContext = value->getDeclContext()->getModuleScopeContext();
        if (auto *enclosingFile = dyn_cast<FileUnit>(topLevelContext)) {
          Identifier discriminator =
            enclosingFile->getDiscriminatorForPrivateValue(value);
          unsigned abbrCode =
            DeclTypeAbbrCodes[PrivateDiscriminatorLayout::Code];
          PrivateDiscriminatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                          addIdentifierRef(discriminator));
        }
      }
    }
  }

  if (auto *VD = dyn_cast<ValueDecl>(D)) {
    if (VD->getDeclContext()->isLocalContext()) {
      auto discriminator = VD->getLocalDiscriminator();
      auto abbrCode = DeclTypeAbbrCodes[LocalDiscriminatorLayout::Code];
      LocalDiscriminatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                           discriminator);
    }
  }

  switch (D->getKind()) {
  case DeclKind::Import:
    llvm_unreachable("import decls should not be serialized");

  case DeclKind::IfConfig:
    llvm_unreachable("#if block declarations should not be serialized");

  case DeclKind::Extension: {
    auto extension = cast<ExtensionDecl>(D);
    verifyAttrSerializable(extension);

    auto contextID = addDeclContextRef(extension->getDeclContext());
    Type baseTy = extension->getExtendedType();

    // Make sure the base type has registered itself as a provider of generic
    // parameters.
    (void)addDeclRef(baseTy->getAnyNominal());

    SmallVector<DeclID, 8> protocols;
    for (auto proto : extension->getProtocols())
      protocols.push_back(addDeclRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[ExtensionLayout::Code];
    ExtensionLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addTypeRef(baseTy),
                                contextID,
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
    verifyAttrSerializable(binding);

    auto contextID = addDeclContextRef(binding->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[PatternBindingLayout::Code];
    PatternBindingLayout::emitRecord(
        Out, ScratchRecord, abbrCode, contextID, binding->isImplicit(),
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
    verifyAttrSerializable(op);

    auto contextID = addDeclContextRef(op->getDeclContext());
    auto associativity = getRawStableAssociativity(op->getAssociativity());

    unsigned abbrCode = DeclTypeAbbrCodes[InfixOperatorLayout::Code];
    InfixOperatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                    addIdentifierRef(op->getName()),
                                    contextID,
                                    associativity,
                                    op->getPrecedence(),
                                    op->isAssignment(),
                                    op->isAssociativityImplicit(),
                                    op->isPrecedenceImplicit(),
                                    op->isAssignmentImplicit());
    break;
  }

  case DeclKind::PrefixOperator: {
    auto op = cast<PrefixOperatorDecl>(D);
    verifyAttrSerializable(op);

    auto contextID = addDeclContextRef(op->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[PrefixOperatorLayout::Code];
    PrefixOperatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addIdentifierRef(op->getName()),
                                     contextID);
    break;
  }

  case DeclKind::PostfixOperator: {
    auto op = cast<PostfixOperatorDecl>(D);
    verifyAttrSerializable(op);

    auto contextID = addDeclContextRef(op->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[PostfixOperatorLayout::Code];
    PostfixOperatorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                      addIdentifierRef(op->getName()),
                                      contextID);
    break;
  }

  case DeclKind::TypeAlias: {
    auto typeAlias = cast<TypeAliasDecl>(D);
    assert(!typeAlias->isObjC() && "ObjC typealias is not meaningful");
    assert(typeAlias->getProtocols().empty() &&
           "concrete typealiases cannot have protocols");
    verifyAttrSerializable(typeAlias);

    auto contextID = addDeclContextRef(typeAlias->getDeclContext());

    Type underlying;
    if (typeAlias->hasUnderlyingType())
      underlying = typeAlias->getUnderlyingType();

    uint8_t rawAccessLevel =
      getRawStableAccessibility(typeAlias->getAccessibility());

    unsigned abbrCode = DeclTypeAbbrCodes[TypeAliasLayout::Code];
    TypeAliasLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addIdentifierRef(typeAlias->getName()),
                                contextID,
                                addTypeRef(underlying),
                                addTypeRef(typeAlias->getInterfaceType()),
                                typeAlias->isImplicit(),
                                rawAccessLevel);
    break;
  }

  case DeclKind::GenericTypeParam: {
    auto genericParam = cast<GenericTypeParamDecl>(D);
    verifyAttrSerializable(genericParam);

    auto contextID = addDeclContextRef(genericParam->getDeclContext());

    SmallVector<DeclID, 4> protocols;
    for (auto proto : genericParam->getProtocols())
      protocols.push_back(addDeclRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[GenericTypeParamDeclLayout::Code];
    GenericTypeParamDeclLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                addIdentifierRef(genericParam->getName()),
                                contextID,
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
    verifyAttrSerializable(assocType);

    auto contextID = addDeclContextRef(assocType->getDeclContext());

    SmallVector<DeclID, 4> protocols;
    for (auto proto : assocType->getProtocols())
      protocols.push_back(addDeclRef(proto));

    unsigned abbrCode = DeclTypeAbbrCodes[AssociatedTypeDeclLayout::Code];
    AssociatedTypeDeclLayout::emitRecord(
      Out, ScratchRecord, abbrCode,
      addIdentifierRef(assocType->getName()),
      contextID,
      addTypeRef(assocType->getSuperclass()),
      addTypeRef(assocType->getArchetype()),
      addTypeRef(assocType->getDefaultDefinitionType()),
      assocType->isImplicit(),
      protocols);
    break;
  }

  case DeclKind::Struct: {
    auto theStruct = cast<StructDecl>(D);
    verifyAttrSerializable(theStruct);

    auto contextID = addDeclContextRef(theStruct->getDeclContext());

    SmallVector<DeclID, 8> protocols;
    for (auto proto : theStruct->getProtocols())
      protocols.push_back(addDeclRef(proto));

    uint8_t rawAccessLevel =
      getRawStableAccessibility(theStruct->getAccessibility());

    unsigned abbrCode = DeclTypeAbbrCodes[StructLayout::Code];
    StructLayout::emitRecord(Out, ScratchRecord, abbrCode,
                             addIdentifierRef(theStruct->getName()),
                             contextID,
                             theStruct->isImplicit(),
                             rawAccessLevel,
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
    verifyAttrSerializable(theEnum);

    auto contextID = addDeclContextRef(theEnum->getDeclContext());

    SmallVector<DeclID, 8> protocols;
    for (auto proto : theEnum->getProtocols())
      protocols.push_back(addDeclRef(proto));

    uint8_t rawAccessLevel =
      getRawStableAccessibility(theEnum->getAccessibility());

    unsigned abbrCode = DeclTypeAbbrCodes[EnumLayout::Code];
    EnumLayout::emitRecord(Out, ScratchRecord, abbrCode,
                            addIdentifierRef(theEnum->getName()),
                            contextID,
                            theEnum->isImplicit(),
                            addTypeRef(theEnum->getRawType()),
                            rawAccessLevel,
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
    verifyAttrSerializable(theClass);

    auto contextID = addDeclContextRef(theClass->getDeclContext());

    SmallVector<DeclID, 8> protocols;
    for (auto proto : theClass->getProtocols())
      protocols.push_back(addDeclRef(proto));

    uint8_t rawAccessLevel =
      getRawStableAccessibility(theClass->getAccessibility());

    unsigned abbrCode = DeclTypeAbbrCodes[ClassLayout::Code];
    ClassLayout::emitRecord(Out, ScratchRecord, abbrCode,
                            addIdentifierRef(theClass->getName()),
                            contextID,
                            theClass->isImplicit(),
                            theClass->isObjC(),
                            theClass->requiresStoredPropertyInits(),
                            theClass->isForeign(),
                            addTypeRef(theClass->getSuperclass()),
                            rawAccessLevel,
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

    auto contextID = addDeclContextRef(proto->getDeclContext());

    SmallVector<DeclID, 8> protocols;
    for (auto proto : proto->getProtocols())
      protocols.push_back(addDeclRef(proto));

    uint8_t rawAccessLevel =
      getRawStableAccessibility(proto->getAccessibility());

    unsigned abbrCode = DeclTypeAbbrCodes[ProtocolLayout::Code];
    ProtocolLayout::emitRecord(Out, ScratchRecord, abbrCode,
                               addIdentifierRef(proto->getName()),
                               contextID,
                               proto->isImplicit(),
                               const_cast<ProtocolDecl *>(proto)
                                 ->requiresClass(),
                               proto->isObjC(),
                               rawAccessLevel,
                               protocols);

    writeGenericParams(proto->getGenericParams(), DeclTypeAbbrCodes);
    writeRequirements(proto->getGenericRequirements());
    writeMembers(proto->getMembers(), true);
    break;
  }

  case DeclKind::Var: {
    auto var = cast<VarDecl>(D);
    verifyAttrSerializable(var);

    auto contextID = addDeclContextRef(var->getDeclContext());
    Type type = var->hasType() ? var->getType() : nullptr;

    Accessors accessors = getAccessors(var);
    uint8_t rawAccessLevel =
      getRawStableAccessibility(var->getAccessibility());
    uint8_t rawSetterAccessLevel = rawAccessLevel;
    if (var->isSettable(nullptr))
      rawSetterAccessLevel =
        getRawStableAccessibility(var->getSetterAccessibility());

    unsigned abbrCode = DeclTypeAbbrCodes[VarLayout::Code];
    VarLayout::emitRecord(Out, ScratchRecord, abbrCode,
                          addIdentifierRef(var->getName()),
                          contextID,
                          var->isImplicit(),
                          var->isObjC(),
                          var->isStatic(),
                          var->isLet(),
                          var->hasNonPatternBindingInit(),
                          (unsigned) accessors.Kind,
                          addTypeRef(type),
                          addTypeRef(var->getInterfaceType()),
                          addDeclRef(accessors.Get),
                          addDeclRef(accessors.Set),
                          addDeclRef(accessors.MaterializeForSet),
                          addDeclRef(accessors.Address),
                          addDeclRef(accessors.MutableAddress),
                          addDeclRef(accessors.WillSet),
                          addDeclRef(accessors.DidSet),
                          addDeclRef(var->getOverriddenDecl()),
                          rawAccessLevel, rawSetterAccessLevel);
    break;
  }

  case DeclKind::Param: {
    auto param = cast<ParamDecl>(D);
    verifyAttrSerializable(param);

    auto contextID = addDeclContextRef(param->getDeclContext());
    Type type = param->hasType() ? param->getType() : nullptr;

    unsigned abbrCode = DeclTypeAbbrCodes[ParamLayout::Code];
    ParamLayout::emitRecord(Out, ScratchRecord, abbrCode,
                            addIdentifierRef(param->getArgumentName()),
                            addIdentifierRef(param->getName()),
                            contextID,
                            param->isLet(),
                            addTypeRef(type),
                            addTypeRef(param->getInterfaceType()));
    break;
  }

  case DeclKind::Func: {
    auto fn = cast<FuncDecl>(D);
    verifyAttrSerializable(fn);

    auto contextID = addDeclContextRef(fn->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[FuncLayout::Code];
    SmallVector<IdentifierID, 4> nameComponents;
    nameComponents.push_back(addIdentifierRef(fn->getFullName().getBaseName()));
    for (auto argName : fn->getFullName().getArgumentNames())
      nameComponents.push_back(addIdentifierRef(argName));

    uint8_t rawAccessLevel =
      getRawStableAccessibility(fn->getAccessibility());
    uint8_t rawAddressorKind =
      getRawStableAddressorKind(fn->getAddressorKind());

    FuncLayout::emitRecord(Out, ScratchRecord, abbrCode,
                           contextID,
                           fn->isImplicit(),
                           fn->isStatic(),
                           uint8_t(getStableStaticSpelling(fn->getStaticSpelling())),
                           fn->isObjC(),
                           fn->isMutating(),
                           fn->hasDynamicSelf(),
                           fn->getBodyParamPatterns().size(),
                           addTypeRef(fn->getType()),
                           addTypeRef(fn->getInterfaceType()),
                           addDeclRef(fn->getOperatorDecl()),
                           addDeclRef(fn->getOverriddenDecl()),
                           addDeclRef(fn->getAccessorStorageDecl()),
                           !fn->getFullName().isSimpleName(),
                           rawAddressorKind,
                           rawAccessLevel,
                           nameComponents);

    writeGenericParams(fn->getGenericParams(), DeclTypeAbbrCodes);

    // Write the body parameters.
    for (auto pattern : fn->getBodyParamPatterns())
      writePattern(pattern);

    break;
  }

  case DeclKind::EnumElement: {
    auto elem = cast<EnumElementDecl>(D);
    auto contextID = addDeclContextRef(elem->getDeclContext());

    // We only serialize the raw values of @objc enums, because they're part
    // of the ABI. That isn't the case for Swift enums.
    auto RawValueKind = EnumElementRawValueKind::None;
    bool Negative = false;
    StringRef RawValueText;
    if (elem->getParentEnum()->isObjC()) {
      // Currently ObjC enums always have integer raw values.
      RawValueKind = EnumElementRawValueKind::IntegerLiteral;
      auto ILE = cast<IntegerLiteralExpr>(elem->getRawValueExpr());
      RawValueText = ILE->getDigitsText();
      Negative = ILE->isNegative();
    }

    unsigned abbrCode = DeclTypeAbbrCodes[EnumElementLayout::Code];
    EnumElementLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  addIdentifierRef(elem->getName()),
                                  contextID,
                                  addTypeRef(elem->getArgumentType()),
                                  addTypeRef(elem->getType()),
                                  addTypeRef(elem->getInterfaceType()),
                                  elem->isImplicit(),
                                  (unsigned)RawValueKind,
                                  Negative,
                                  RawValueText);
    break;
  }

  case DeclKind::Subscript: {
    auto subscript = cast<SubscriptDecl>(D);
    verifyAttrSerializable(subscript);

    auto contextID = addDeclContextRef(subscript->getDeclContext());

    SmallVector<IdentifierID, 4> nameComponents;
    for (auto argName : subscript->getFullName().getArgumentNames())
      nameComponents.push_back(addIdentifierRef(argName));

    Accessors accessors = getAccessors(subscript);
    uint8_t rawAccessLevel =
      getRawStableAccessibility(subscript->getAccessibility());
    uint8_t rawSetterAccessLevel = rawAccessLevel;
    if (subscript->isSettable())
      rawSetterAccessLevel =
        getRawStableAccessibility(subscript->getSetterAccessibility());

    unsigned abbrCode = DeclTypeAbbrCodes[SubscriptLayout::Code];
    SubscriptLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                contextID,
                                subscript->isImplicit(),
                                subscript->isObjC(),
                                (unsigned) accessors.Kind,
                                addTypeRef(subscript->getType()),
                                addTypeRef(subscript->getElementType()),
                                addTypeRef(subscript->getInterfaceType()),
                                addDeclRef(accessors.Get),
                                addDeclRef(accessors.Set),
                                addDeclRef(accessors.MaterializeForSet),
                                addDeclRef(accessors.Address),
                                addDeclRef(accessors.MutableAddress),
                                addDeclRef(accessors.WillSet),
                                addDeclRef(accessors.DidSet),
                                addDeclRef(subscript->getOverriddenDecl()),
                                rawAccessLevel,
                                rawSetterAccessLevel,
                                nameComponents);

    writePattern(subscript->getIndices());
    break;
  }


  case DeclKind::Constructor: {
    auto ctor = cast<ConstructorDecl>(D);
    verifyAttrSerializable(ctor);

    auto contextID = addDeclContextRef(ctor->getDeclContext());

    SmallVector<IdentifierID, 4> nameComponents;
    for (auto argName : ctor->getFullName().getArgumentNames())
      nameComponents.push_back(addIdentifierRef(argName));

    uint8_t rawAccessLevel =
      getRawStableAccessibility(ctor->getAccessibility());

    unsigned abbrCode = DeclTypeAbbrCodes[ConstructorLayout::Code];
    ConstructorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                  contextID,
                                  getRawStableOptionalTypeKind(
                                    ctor->getFailability()),
                                  ctor->isImplicit(),
                                  ctor->isObjC(),
                                  ctor->hasStubImplementation(),
                                  getStableCtorInitializerKind(
                                    ctor->getInitKind()),
                                  addTypeRef(ctor->getType()),
                                  addTypeRef(ctor->getInterfaceType()),
                                  addDeclRef(ctor->getOverriddenDecl()),
                                  rawAccessLevel,
                                  nameComponents);

    writeGenericParams(ctor->getGenericParams(), DeclTypeAbbrCodes);
    assert(ctor->getBodyParamPatterns().size() == 2);
    for (auto pattern : ctor->getBodyParamPatterns())
      writePattern(pattern);
    break;
  }

  case DeclKind::Destructor: {
    auto dtor = cast<DestructorDecl>(D);
    verifyAttrSerializable(dtor);

    auto contextID = addDeclContextRef(dtor->getDeclContext());

    unsigned abbrCode = DeclTypeAbbrCodes[DestructorLayout::Code];
    DestructorLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                 contextID,
                                 dtor->isImplicit(),
                                 dtor->isObjC(),
                                 addTypeRef(dtor->getType()),
                                 addTypeRef(dtor->getInterfaceType()));
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
  SIMPLE_CASE(ParameterConvention, Indirect_In_Guaranteed)
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

  auto id = DeclAndTypeIDs[ty].first;
  assert(id != 0 && "type not referenced properly");
  (void)id;

  assert((id - 1) == TypeOffsets.size());

  TypeOffsets.push_back(Out.GetCurrentBitNo());

  switch (ty.getPointer()->getKind()) {
  case TypeKind::Error:
    llvm_unreachable("should not serialize an error type");

  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinNativeObject:
  case TypeKind::BuiltinBridgeObject:
  case TypeKind::BuiltinUnknownObject:
  case TypeKind::BuiltinUnsafeValueBuffer:
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

    TypeID parentID = addTypeRef(archetypeTy->getParent());

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
                                    parentID,
                                    assocTypeOrProtoID,
                                    addTypeRef(archetypeTy->getSuperclass()),
                                    conformances);

    SmallVector<IdentifierID, 4> nestedTypeNames;
    SmallVector<TypeID, 4> nestedTypes;
    SmallVector<bool, 4> areArchetypes;
    for (auto next : archetypeTy->getNestedTypes()) {
      nestedTypeNames.push_back(addIdentifierRef(next.first));
      nestedTypes.push_back(addTypeRef(next.second.getValue()));
      areArchetypes.push_back(!next.second.isConcreteType());
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
           fnTy->getRepresentation() == AnyFunctionType::Representation::Block,
                                   fnTy->isNoEscape());
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
    auto interfaceResult = fnTy->getResult();
    auto stableInterfaceResultConvention =
      getRawStableResultConvention(interfaceResult.getConvention());

    SmallVector<TypeID, 8> paramTypes;
    for (auto param : fnTy->getParameters()) {
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
          fnTy->isNoReturn(), fnTy->isNoEscape(),
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

  case TypeKind::Dictionary: {
    auto dictTy = cast<DictionaryType>(ty.getPointer());

    unsigned abbrCode = DeclTypeAbbrCodes[DictionaryTypeLayout::Code];
    DictionaryTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
                                     addTypeRef(dictTy->getKeyType()),
                                     addTypeRef(dictTy->getValueType()));
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

  case TypeKind::ImplicitlyUnwrappedOptional: {
    auto sliceTy = cast<ImplicitlyUnwrappedOptionalType>(ty.getPointer());

    Type base = sliceTy->getBaseType();

    unsigned abbrCode = DeclTypeAbbrCodes[ImplicitlyUnwrappedOptionalTypeLayout::Code];
    ImplicitlyUnwrappedOptionalTypeLayout::emitRecord(Out, ScratchRecord, abbrCode,
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
  registerDeclTypeAbbr<DictionaryTypeLayout>();
  registerDeclTypeAbbr<ReferenceStorageTypeLayout>();
  registerDeclTypeAbbr<UnboundGenericTypeLayout>();
  registerDeclTypeAbbr<OptionalTypeLayout>();
  registerDeclTypeAbbr<ImplicitlyUnwrappedOptionalTypeLayout>();
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

  registerDeclTypeAbbr<DeclContextLayout>();
  registerDeclTypeAbbr<AbstractClosureExprLayout>();
  registerDeclTypeAbbr<PatternBindingInitializerLayout>();
  registerDeclTypeAbbr<DefaultArgumentInitializerLayout>();
  registerDeclTypeAbbr<TopLevelCodeDeclContextLayout>();

  registerDeclTypeAbbr<XRefTypePathPieceLayout>();
  registerDeclTypeAbbr<XRefValuePathPieceLayout>();
  registerDeclTypeAbbr<XRefExtensionPathPieceLayout>();
  registerDeclTypeAbbr<XRefOperatorOrAccessorPathPieceLayout>();
  registerDeclTypeAbbr<XRefGenericParamPathPieceLayout>();
  registerDeclTypeAbbr<XRefInitializerPathPieceLayout>();

  registerDeclTypeAbbr<NoConformanceLayout>();
  registerDeclTypeAbbr<NormalProtocolConformanceLayout>();
  registerDeclTypeAbbr<SpecializedProtocolConformanceLayout>();
  registerDeclTypeAbbr<InheritedProtocolConformanceLayout>();

  registerDeclTypeAbbr<LocalDiscriminatorLayout>();
  registerDeclTypeAbbr<PrivateDiscriminatorLayout>();
  registerDeclTypeAbbr<MembersLayout>();
  registerDeclTypeAbbr<XRefLayout>();

#define DECL_ATTR(X, NAME, ...) \
  registerDeclTypeAbbr<NAME##DeclAttrLayout>();
#include "swift/AST/Attr.def"

  while (!DeclsAndTypesToWrite.empty()) {
    auto next = DeclsAndTypesToWrite.front();
    DeclsAndTypesToWrite.pop();

    if (next.isDecl())
      writeDecl(next.getDecl());
    else
      writeType(next.getType());

    while (!DeclContextsToWrite.empty()) {
      auto next = DeclContextsToWrite.front();
      DeclContextsToWrite.pop();
      writeDeclContext(next);

      while (!LocalDeclContextsToWrite.empty()) {
        auto next = LocalDeclContextsToWrite.front();
        LocalDeclContextsToWrite.pop();
        writeLocalDeclContext(next);
      }
    }
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

static void writeLocalDeclTable(const index_block::DeclListLayout &DeclList,
                                index_block::RecordKind kind,
                                LocalTypeHashTableGenerator &generator) {
  SmallVector<uint64_t, 8> scratch;
  llvm::SmallString<4096> hashTableBlob;
  uint32_t tableOffset;
  {
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

namespace {
  /// Used to serialize the on-disk Objective-C method hash table.
  class ObjCMethodTableInfo {
  public:
    using key_type = ObjCSelector;
    using key_type_ref = key_type;
    using data_type = Serializer::ObjCMethodTableData;
    using data_type_ref = const data_type &;
    using hash_value_type = uint32_t;
    using offset_type = unsigned;

    hash_value_type ComputeHash(key_type_ref key) {
      llvm::SmallString<32> scratch;
      return llvm::HashString(key.getString(scratch));
    }

    std::pair<unsigned, unsigned> EmitKeyDataLength(raw_ostream &out,
                                                    key_type_ref key,
                                                    data_type_ref data) {
      llvm::SmallString<32> scratch;
      uint32_t keyLength = key.getString(scratch).size();
      uint32_t dataLength = (sizeof(TypeID) + 1 + sizeof(DeclID)) * data.size();

      endian::Writer<little> writer(out);
      writer.write<uint16_t>(keyLength);
      writer.write<uint16_t>(dataLength);
      return { keyLength, dataLength };
    }

    void EmitKey(raw_ostream &out, key_type_ref key, unsigned len) {
      out << key;
    }

    void EmitData(raw_ostream &out, key_type_ref key, data_type_ref data,
                  unsigned len) {
      static_assert(sizeof(DeclID) <= 4, "DeclID too large");
      endian::Writer<little> writer(out);
      for (auto entry : data) {
        writer.write<uint32_t>(std::get<0>(entry));
        writer.write<uint8_t>(std::get<1>(entry));
        writer.write<uint32_t>(std::get<2>(entry));
      }
    }
  };
} // end anonymous namespace

static void writeObjCMethodTable(const index_block::ObjCMethodTableLayout &out,
                                 Serializer::ObjCMethodTable &objcMethods) {
  // Collect all of the Objective-C selectors in the method table.
  std::vector<ObjCSelector> selectors;
  for (const auto &entry : objcMethods) {
    selectors.push_back(entry.first);
  }

  // Sort the Objective-C selectors so we emit them in a stable order.
  llvm::array_pod_sort(selectors.begin(), selectors.end());

  // Create the on-disk hash table.
  llvm::OnDiskChainedHashTableGenerator<ObjCMethodTableInfo> generator;
  llvm::SmallString<32> hashTableBlob;
  uint32_t tableOffset;
  {
    llvm::raw_svector_ostream blobStream(hashTableBlob);
    for (auto selector : selectors) {
      generator.insert(selector, objcMethods[selector]);
    }

    // Make sure that no bucket is at offset 0
    endian::Writer<little>(blobStream).write<uint32_t>(0);
    tableOffset = generator.Emit(blobStream);
  }

  SmallVector<uint64_t, 8> scratch;
  out.emit(scratch, tableOffset, hashTableBlob);
}

/// Add operator methods from the given declaration type.
///
/// Recursively walks the members and derived global decls of any nested
/// nominal types.
template<typename Range>
static void addOperatorsAndTopLevel(Serializer &S, Range members,
                                    Serializer::DeclTable &operatorMethodDecls,
                                    Serializer::DeclTable &topLevelDecls,
                                    Serializer::ObjCMethodTable &objcMethods,
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
                             operatorMethodDecls, topLevelDecls, objcMethods,
                             false);
      addOperatorsAndTopLevel(S, nominal->getDerivedGlobalDecls(),
                             operatorMethodDecls, topLevelDecls, objcMethods,
                             true);
    }

    // Record Objective-C methods.
    if (auto func = dyn_cast<AbstractFunctionDecl>(member)) {
      if (func->isObjC()) {
        TypeID owningTypeID
          = S.addTypeRef(func->getDeclContext()->getDeclaredInterfaceType());
        objcMethods[func->getObjCSelector()].push_back(
          std::make_tuple(owningTypeID,
                          func->isObjCInstanceMethod(),
                          S.addDeclRef(memberValue)));
      }
    }
  }
}

void Serializer::writeAST(ModuleOrSourceFile DC) {
  DeclTable topLevelDecls, extensionDecls, operatorDecls, operatorMethodDecls;
  ObjCMethodTable objcMethods;
  LocalTypeHashTableGenerator localTypeGenerator;
  bool hasLocalTypes = false;

  Optional<DeclID> entryPointClassID;

  ArrayRef<const FileUnit *> files = SF ? SF : M->getFiles();
  for (auto nextFile : files) {
    if (nextFile->hasEntryPoint())
      entryPointClassID = addDeclRef(nextFile->getMainClass());

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
                                  operatorMethodDecls, topLevelDecls,
                                  objcMethods, false);
          addOperatorsAndTopLevel(*this, nominal->getDerivedGlobalDecls(),
                                  operatorMethodDecls, topLevelDecls,
                                  objcMethods, true);
        }
      } else if (auto ED = dyn_cast<ExtensionDecl>(D)) {
        Type extendedTy = ED->getExtendedType();
        const NominalTypeDecl *extendedNominal = extendedTy->getAnyNominal();
        extensionDecls[extendedNominal->getName()]
          .push_back({ getKindForTable(extendedNominal), addDeclRef(D) });

        // Add operator methods from extensions.
        addOperatorsAndTopLevel(*this, ED->getMembers(),
                                operatorMethodDecls, topLevelDecls, objcMethods,
                                false);

      } else if (auto OD = dyn_cast<OperatorDecl>(D)) {
        operatorDecls[OD->getName()]
          .push_back({ getStableFixity(OD->getKind()), addDeclRef(D) });
      }
    }

    SmallVector<TypeDecl *, 16> localTypeDecls;
    nextFile->getLocalTypeDecls(localTypeDecls);

    for (auto TD : localTypeDecls) {
      hasLocalTypes = true;

      SmallString<32> MangledName;
      llvm::raw_svector_ostream Stream(MangledName);
      Mangle::Mangler DebugMangler(Stream, false);
      DebugMangler.mangleType(TD->getDeclaredType(),
                              ResilienceExpansion::Minimal, 0);
      Stream.flush();
      assert(!MangledName.empty() && "Mangled type came back empty!");
      localTypeGenerator.insert(MangledName, {
        addDeclRef(TD), TD->getLocalDiscriminator()
      });
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
    writeOffsets(Offsets, DeclContextOffsets);
    writeOffsets(Offsets, LocalDeclContextOffsets);

    index_block::DeclListLayout DeclList(Out);
    writeDeclTable(DeclList, index_block::TOP_LEVEL_DECLS, topLevelDecls);
    writeDeclTable(DeclList, index_block::OPERATORS, operatorDecls);
    writeDeclTable(DeclList, index_block::EXTENSIONS, extensionDecls);
    writeDeclTable(DeclList, index_block::CLASS_MEMBERS, ClassMembersByName);
    writeDeclTable(DeclList, index_block::OPERATOR_METHODS, operatorMethodDecls);
    if (hasLocalTypes)
      writeLocalDeclTable(DeclList, index_block::LOCAL_TYPE_DECLS,
                          localTypeGenerator);

    index_block::ObjCMethodTableLayout ObjCMethodTable(Out);
    writeObjCMethodTable(ObjCMethodTable, objcMethods);

    if (entryPointClassID.hasValue()) {
      index_block::EntryPointLayout EntryPoint(Out);
      EntryPoint.emit(ScratchRecord, entryPointClassID.getValue());
    }
  }
}

void Serializer::writeToStream(raw_ostream &os) {
  os.write(Buffer.data(), Buffer.size());
  os.flush();
}

template <size_t N>
Serializer::Serializer(const unsigned char (&signature)[N],
                       ModuleOrSourceFile DC) {
  for (unsigned char byte : signature)
    Out.Emit(byte, 8);

  this->M = getModule(DC);
  this->SF = DC.dyn_cast<SourceFile *>();
}


void Serializer::writeToStream(raw_ostream &os, ModuleOrSourceFile DC,
                               const SILModule *SILMod,
                               const SerializationOptions &options) {
  Serializer S{MODULE_SIGNATURE, DC};

  // FIXME: This is only really needed for debugging. We don't actually use it.
  S.writeBlockInfoBlock();

  {
    BCBlockRAII moduleBlock(S.Out, MODULE_BLOCK_ID, 2);
    S.writeHeader(options);
    S.writeInputBlock(options);
    S.writeSIL(SILMod, options.SerializeAllSIL);
    S.writeAST(DC);
  }

  S.writeToStream(os);
}

void Serializer::writeDocToStream(raw_ostream &os, ModuleOrSourceFile DC) {
  Serializer S{MODULE_DOC_SIGNATURE, DC};

  // FIXME: This is only really needed for debugging. We don't actually use it.
  S.writeDocBlockInfoBlock();

  {
    BCBlockRAII moduleBlock(S.Out, MODULE_DOC_BLOCK_ID, 2);
    S.writeDocHeader();
    {
      BCBlockRAII restoreBlock(S.Out, COMMENT_BLOCK_ID, 4);

      comment_block::DeclCommentListLayout DeclCommentList(S.Out);
      writeDeclCommentTable(DeclCommentList, S.SF, S.M);
    }
  }

  S.writeToStream(os);
}

void swift::serialize(ModuleOrSourceFile DC,
                      const SerializationOptions &options,
                      const SILModule *M) {
  assert(options.OutputPath && options.OutputPath[0] != '\0');

  std::error_code EC;
  llvm::raw_fd_ostream out(options.OutputPath, EC, llvm::sys::fs::F_None);

  if (out.has_error() || EC) {
    getContext(DC).Diags.diagnose(SourceLoc(), diag::error_opening_output,
                                  options.OutputPath, EC.message());
    out.clear_error();
    return;
  }

  Serializer::writeToStream(out, DC, M, options);

  if (options.DocOutputPath && options.DocOutputPath[0] != '\0') {
    std::error_code EC;
    llvm::raw_fd_ostream docOut(options.DocOutputPath, EC,
                                llvm::sys::fs::F_None);

    if (docOut.has_error() || EC) {
      getContext(DC).Diags.diagnose(SourceLoc(), diag::error_opening_output,
                                    options.DocOutputPath, EC.message());
      docOut.clear_error();
      return;
    }

    Serializer::writeDocToStream(docOut, DC);
  }
}
