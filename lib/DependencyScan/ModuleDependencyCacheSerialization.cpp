//=== ModuleDependencyCacheSerialization.cpp - serialized format -*- C++ -*-==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/FileSystem.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "swift/Basic/Version.h"
#include "swift/DependencyScan/SerializedModuleDependencyCacheFormat.h"
#include "llvm/ADT/DenseMap.h"
#include <unordered_map>

using namespace swift;
using namespace dependencies;
using namespace module_dependency_cache_serialization;

// MARK: Deserialization
namespace {

class Deserializer {
  std::vector<std::string> Identifiers;
  std::vector<std::vector<uint64_t>> ArraysOfIdentifierIDs;
  llvm::BitstreamCursor Cursor;
  SmallVector<uint64_t, 64> Scratch;
  StringRef BlobData;

  // These return true if there was an error.
  bool readSignature();
  bool enterGraphBlock();
  bool readMetadata();
  bool readGraph(ModuleDependenciesCache &cache);

  llvm::Optional<std::string> getIdentifier(unsigned n);
  llvm::Optional<std::vector<std::string>> getArray(unsigned n);

public:
  Deserializer(llvm::MemoryBufferRef Data) : Cursor(Data) {}
  bool readInterModuleDependenciesCache(ModuleDependenciesCache &cache);
};

} // end namespace

/// Read in the expected signature: IMDC
bool Deserializer::readSignature() {
  for (unsigned char byte : MODULE_DEPENDENCY_CACHE_FORMAT_SIGNATURE) {
    if (Cursor.AtEndOfStream())
      return true;
    if (auto maybeRead = Cursor.Read(8)) {
      if (maybeRead.get() != byte)
        return true;
    } else {
      return true;
    }
  }
  return false;
}

/// Read in the info block and enter the top-level block which represents the
/// graph
bool Deserializer::enterGraphBlock() {
  // Read the BLOCKINFO_BLOCK, which contains metadata used when dumping
  // the binary data with llvm-bcanalyzer.
  {
    auto next = Cursor.advance();
    if (!next) {
      consumeError(next.takeError());
      return true;
    }

    if (next->Kind != llvm::BitstreamEntry::SubBlock)
      return true;

    if (next->ID != llvm::bitc::BLOCKINFO_BLOCK_ID)
      return true;

    if (!Cursor.ReadBlockInfoBlock())
      return true;
  }

  // Enters our top-level subblock,
  // which contains the actual module dependency information.
  {
    auto next = Cursor.advance();
    if (!next) {
      consumeError(next.takeError());
      return true;
    }

    if (next->Kind != llvm::BitstreamEntry::SubBlock)
      return true;

    if (next->ID != GRAPH_BLOCK_ID)
      return true;

    if (auto err = Cursor.EnterSubBlock(GRAPH_BLOCK_ID)) {
      consumeError(std::move(err));
      return true;
    }
  }
  return false;
}

/// Read in the serialized file's format version, error/exit if not matching
/// current version.
bool Deserializer::readMetadata() {
  using namespace graph_block;

  auto entry = Cursor.advance();
  if (!entry) {
    consumeError(entry.takeError());
    return true;
  }

  if (entry->Kind != llvm::BitstreamEntry::Record)
    return true;

  auto recordID = Cursor.readRecord(entry->ID, Scratch, &BlobData);
  if (!recordID) {
    consumeError(recordID.takeError());
    return true;
  }

  if (*recordID != METADATA)
    return true;

  unsigned majorVersion, minorVersion;

  MetadataLayout::readRecord(Scratch, majorVersion, minorVersion);
  if (majorVersion != MODULE_DEPENDENCY_CACHE_FORMAT_VERSION_MAJOR ||
      minorVersion != MODULE_DEPENDENCY_CACHE_FORMAT_VERSION_MINOR) {
    return true;
  }

  return false;
}

/// Read in the top-level block's graph structure by first reading in
/// all of the file's identifiers and arrays of identifiers, followed by
/// consuming individual module info records and registering them into the
/// cache.
bool Deserializer::readGraph(ModuleDependenciesCache &cache) {
  using namespace graph_block;

  bool hasCurrentModule = false;
  std::string currentModuleName;
  llvm::Optional<std::vector<std::string>> currentModuleDependencies;

  while (!Cursor.AtEndOfStream()) {
    auto entry = cantFail(Cursor.advance(), "Advance bitstream cursor");

    if (entry.Kind == llvm::BitstreamEntry::EndBlock) {
      Cursor.ReadBlockEnd();
      assert(Cursor.GetCurrentBitNo() % CHAR_BIT == 0);
      break;
    }

    if (entry.Kind != llvm::BitstreamEntry::Record)
      llvm::report_fatal_error("Bad bitstream entry kind");

    Scratch.clear();
    unsigned recordID =
        cantFail(Cursor.readRecord(entry.ID, Scratch, &BlobData),
                 "Read bitstream record");

    switch (recordID) {
    case METADATA: {
      // METADATA must appear at the beginning and is read by readMetadata().
      llvm::report_fatal_error("Unexpected METADATA record");
      break;
    }

    case IDENTIFIER_NODE: {
      // IDENTIFIER_NODE must come before MODULE_NODEs.
      if (hasCurrentModule)
        llvm::report_fatal_error("Unexpected IDENTIFIER_NODE record");
      IdentifierNodeLayout::readRecord(Scratch);
      Identifiers.push_back(BlobData.str());
      break;
    }

    case IDENTIFIER_ARRAY_NODE: {
      // IDENTIFIER_ARRAY_NODE must come before MODULE_NODEs.
      if (hasCurrentModule)
        llvm::report_fatal_error("Unexpected IDENTIFIER_NODE record");
      ArrayRef<uint64_t> identifierIDs;
      IdentifierArrayLayout::readRecord(Scratch, identifierIDs);
      ArraysOfIdentifierIDs.push_back(identifierIDs.vec());
      break;
    }

    case MODULE_NODE: {
      hasCurrentModule = true;
      unsigned moduleNameID, moduleDependenciesArrayID;
      ModuleInfoLayout::readRecord(Scratch, moduleNameID,
                                   moduleDependenciesArrayID);
      auto moduleName = getIdentifier(moduleNameID);
      if (!moduleName)
        llvm::report_fatal_error("Bad module name");
      currentModuleName = *moduleName;

      currentModuleDependencies = getArray(moduleDependenciesArrayID);
      if (!currentModuleDependencies)
        llvm::report_fatal_error("Bad direct dependencies");
      break;
    }

    case SWIFT_TEXTUAL_MODULE_DETAILS_NODE: {
      if (!hasCurrentModule)
        llvm::report_fatal_error(
            "Unexpected SWIFT_TEXTUAL_MODULE_DETAILS_NODE record");
      unsigned interfaceFileID, compiledModuleCandidatesArrayID,
          buildCommandLineArrayID, extraPCMArgsArrayID, contextHashID,
          isFramework, bridgingHeaderFileID, sourceFilesArrayID,
          bridgingSourceFilesArrayID, bridgingModuleDependenciesArrayID;
      SwiftTextualModuleDetailsLayout::readRecord(
          Scratch, interfaceFileID, compiledModuleCandidatesArrayID,
          buildCommandLineArrayID, extraPCMArgsArrayID, contextHashID,
          isFramework, bridgingHeaderFileID, sourceFilesArrayID,
          bridgingSourceFilesArrayID, bridgingModuleDependenciesArrayID);

      Optional<std::string> optionalSwiftInterfaceFile;
      if (interfaceFileID != 0) {
        auto swiftInterfaceFile = getIdentifier(interfaceFileID);
        if (!swiftInterfaceFile)
          llvm::report_fatal_error("Bad swift interface file path");
        optionalSwiftInterfaceFile = *swiftInterfaceFile;
      }
      auto compiledModuleCandidates = getArray(compiledModuleCandidatesArrayID);
      if (!compiledModuleCandidates)
        llvm::report_fatal_error("Bad compiled module candidates");
      auto commandLine = getArray(buildCommandLineArrayID);
      if (!commandLine)
        llvm::report_fatal_error("Bad command line");
      auto extraPCMArgs = getArray(extraPCMArgsArrayID);
      if (!extraPCMArgs)
        llvm::report_fatal_error("Bad PCM Args set");
      auto contextHash = getIdentifier(contextHashID);
      if (!contextHash)
        llvm::report_fatal_error("Bad context hash");

      // forSwiftInterface API demands references here.
      std::vector<StringRef> buildCommandRefs;
      for (auto &arg : *commandLine)
        buildCommandRefs.push_back(arg);
      std::vector<StringRef> extraPCMRefs;
      for (auto &arg : *extraPCMArgs)
        extraPCMRefs.push_back(arg);

      // Form the dependencies storage object
      auto moduleDep = ModuleDependencies::forSwiftTextualModule(
          optionalSwiftInterfaceFile, *compiledModuleCandidates,
          buildCommandRefs, extraPCMRefs, *contextHash, isFramework);

      // Add dependencies of this module
      for (const auto &moduleName : *currentModuleDependencies)
        moduleDep.addModuleDependency(moduleName);

      // Add bridging header file path
      if (bridgingHeaderFileID != 0) {
        auto bridgingHeaderFile = getIdentifier(bridgingHeaderFileID);
        if (!bridgingHeaderFile)
          llvm::report_fatal_error("Bad bridging header path");

        moduleDep.addBridgingHeader(*bridgingHeaderFile);
      }

      // Add bridging source files
      auto bridgingSourceFiles = getArray(bridgingSourceFilesArrayID);
      if (!bridgingSourceFiles)
        llvm::report_fatal_error("Bad bridging source files");
      for (const auto &file : *bridgingSourceFiles)
        moduleDep.addBridgingSourceFile(file);

      // Add source files
      auto sourceFiles = getArray(sourceFilesArrayID);
      if (!sourceFiles)
        llvm::report_fatal_error("Bad bridging source files");
      for (const auto &file : *sourceFiles)
        moduleDep.addSourceFile(file);

      // Add bridging module dependencies
      auto bridgingModuleDeps = getArray(bridgingModuleDependenciesArrayID);
      if (!bridgingModuleDeps)
        llvm::report_fatal_error("Bad bridging module dependencies");
      llvm::StringSet<> alreadyAdded;
      for (const auto &mod : *bridgingModuleDeps)
        moduleDep.addBridgingModuleDependency(mod, alreadyAdded);

      cache.recordDependencies(currentModuleName, std::move(moduleDep));
      hasCurrentModule = false;
      break;
    }

    case SWIFT_BINARY_MODULE_DETAILS_NODE: {
      if (!hasCurrentModule)
        llvm::report_fatal_error(
            "Unexpected SWIFT_BINARY_MODULE_DETAILS_NODE record");
      unsigned compiledModulePathID, moduleDocPathID, moduleSourceInfoPathID,
          isFramework;
      SwiftBinaryModuleDetailsLayout::readRecord(
          Scratch, compiledModulePathID, moduleDocPathID,
          moduleSourceInfoPathID, isFramework);

      auto compiledModulePath = getIdentifier(compiledModulePathID);
      if (!compiledModulePath)
        llvm::report_fatal_error("Bad compiled module path");
      auto moduleDocPath = getIdentifier(moduleDocPathID);
      if (!moduleDocPath)
        llvm::report_fatal_error("Bad module doc path");
      auto moduleSourceInfoPath = getIdentifier(moduleSourceInfoPathID);
      if (!moduleSourceInfoPath)
        llvm::report_fatal_error("Bad module source info path");

      // Form the dependencies storage object
      auto moduleDep = ModuleDependencies::forSwiftBinaryModule(
          *compiledModulePath, *moduleDocPath, *moduleSourceInfoPath,
          isFramework);
      // Add dependencies of this module
      for (const auto &moduleName : *currentModuleDependencies)
        moduleDep.addModuleDependency(moduleName);

      cache.recordDependencies(currentModuleName, std::move(moduleDep));
      hasCurrentModule = false;
      break;
    }

    case SWIFT_PLACEHOLDER_MODULE_DETAILS_NODE: {
      if (!hasCurrentModule)
        llvm::report_fatal_error(
            "Unexpected SWIFT_PLACEHOLDER_MODULE_DETAILS_NODE record");
      unsigned compiledModulePathID, moduleDocPathID, moduleSourceInfoPathID;
      SwiftPlaceholderModuleDetailsLayout::readRecord(
          Scratch, compiledModulePathID, moduleDocPathID,
          moduleSourceInfoPathID);

      auto compiledModulePath = getIdentifier(compiledModulePathID);
      if (!compiledModulePath)
        llvm::report_fatal_error("Bad compiled module path");
      auto moduleDocPath = getIdentifier(moduleDocPathID);
      if (!moduleDocPath)
        llvm::report_fatal_error("Bad module doc path");
      auto moduleSourceInfoPath = getIdentifier(moduleSourceInfoPathID);
      if (!moduleSourceInfoPath)
        llvm::report_fatal_error("Bad module source info path");

      // Form the dependencies storage object
      auto moduleDep = ModuleDependencies::forPlaceholderSwiftModuleStub(
          *compiledModulePath, *moduleDocPath, *moduleSourceInfoPath);
      // Add dependencies of this module
      for (const auto &moduleName : *currentModuleDependencies)
        moduleDep.addModuleDependency(moduleName);

      cache.recordDependencies(currentModuleName, std::move(moduleDep));
      hasCurrentModule = false;
      break;
    }

    case CLANG_MODULE_DETAILS_NODE: {
      if (!hasCurrentModule)
        llvm::report_fatal_error("Unexpected CLANG_MODULE_DETAILS_NODE record");
      unsigned moduleMapPathID, contextHashID, commandLineArrayID,
          fileDependenciesArrayID;
      ClangModuleDetailsLayout::readRecord(Scratch, moduleMapPathID,
                                           contextHashID, commandLineArrayID,
                                           fileDependenciesArrayID);
      auto moduleMapPath = getIdentifier(moduleMapPathID);
      if (!moduleMapPath)
        llvm::report_fatal_error("Bad module map path");
      auto contextHash = getIdentifier(contextHashID);
      if (!contextHash)
        llvm::report_fatal_error("Bad context hash");
      auto commandLineArgs = getArray(commandLineArrayID);
      if (!commandLineArgs)
        llvm::report_fatal_error("Bad command line");
      auto fileDependencies = getArray(fileDependenciesArrayID);
      if (!fileDependencies)
        llvm::report_fatal_error("Bad file dependencies");

      // Form the dependencies storage object
      auto moduleDep = ModuleDependencies::forClangModule(
          *moduleMapPath, *contextHash, *commandLineArgs, *fileDependencies);
      // Add dependencies of this module
      for (const auto &moduleName : *currentModuleDependencies)
        moduleDep.addModuleDependency(moduleName);

      cache.recordDependencies(currentModuleName, std::move(moduleDep));
      hasCurrentModule = false;
      break;
    }

    default: {
      llvm::report_fatal_error("Unknown record ID");
    }
    }
  }

  return false;
}

bool Deserializer::readInterModuleDependenciesCache(
    ModuleDependenciesCache &cache) {
  using namespace graph_block;

  if (readSignature())
    return true;

  if (enterGraphBlock())
    return true;

  if (readMetadata())
    return true;

  if (readGraph(cache))
    return true;

  return false;
}

llvm::Optional<std::string> Deserializer::getIdentifier(unsigned n) {
  if (n == 0)
    return std::string();

  --n;
  if (n >= Identifiers.size())
    return None;

  return Identifiers[n];
}

llvm::Optional<std::vector<std::string>> Deserializer::getArray(unsigned n) {
  if (n == 0)
    return std::vector<std::string>();

  --n;
  if (n >= ArraysOfIdentifierIDs.size())
    return None;

  auto &identifierIDs = ArraysOfIdentifierIDs[n];

  auto IDtoStringMap = [this](unsigned id) {
    auto identifier = getIdentifier(id);
    if (!identifier)
      llvm::report_fatal_error("Bad identifier array element");
    return *identifier;
  };
  std::vector<std::string> result;
  result.reserve(identifierIDs.size());
  std::transform(identifierIDs.begin(), identifierIDs.end(),
                 std::back_inserter(result), IDtoStringMap);
  return result;
}

bool swift::dependencies::module_dependency_cache_serialization::
    readInterModuleDependenciesCache(llvm::MemoryBuffer &buffer,
                                     ModuleDependenciesCache &cache) {
  Deserializer deserializer(buffer.getMemBufferRef());
  return deserializer.readInterModuleDependenciesCache(cache);
}

bool swift::dependencies::module_dependency_cache_serialization::
    readInterModuleDependenciesCache(StringRef path,
                                     ModuleDependenciesCache &cache) {
  PrettyStackTraceStringAction stackTrace(
      "loading inter-module dependency graph", path);
  auto buffer = llvm::MemoryBuffer::getFile(path);
  if (!buffer)
    return true;

  return readInterModuleDependenciesCache(*buffer.get(), cache);
}

// MARK: Serialization

/// Kinds of arrays that we track being serialized. Used to query serialized
/// array ID for a given module.
enum ModuleIdentifierArrayKind : uint8_t {
  Empty = 0,
  DirectDependencies,
  CompiledModuleCandidates,
  BuildCommandLine,
  ExtraPCMArgs,
  SourceFiles,
  BridgingSourceFiles,
  BridgingModuleDependencies,
  NonPathCommandLine,
  FileDependencies,
  LastArrayKind
};

using ModuleIdentifierArrayKey =
    std::pair<ModuleDependencyID, ModuleIdentifierArrayKind>;

// DenseMap Infos for hashing of ModuleIdentifierArrayKind
template <>
struct llvm::DenseMapInfo<ModuleIdentifierArrayKind> {
  using UnderlyingType = std::underlying_type<ModuleIdentifierArrayKind>::type;
  using UnerlyingInfo = DenseMapInfo<UnderlyingType>;

  static inline ModuleIdentifierArrayKind getEmptyKey() {
    return ModuleIdentifierArrayKind::Empty;
  }
  static inline ModuleIdentifierArrayKind getTombstoneKey() {
    return ModuleIdentifierArrayKind::LastArrayKind;
  }
  static unsigned getHashValue(const ModuleIdentifierArrayKind &arrKind) {
    auto underlyingValue = static_cast<UnderlyingType>(arrKind);
    return UnerlyingInfo::getHashValue(underlyingValue);
  }
  static bool isEqual(const ModuleIdentifierArrayKind &LHS,
                      const ModuleIdentifierArrayKind &RHS) {
    return LHS == RHS;
  }
};

namespace std {
template <>
struct hash<ModuleDependencyID> {
  using UnderlyingKindType = std::underlying_type<ModuleDependenciesKind>::type;
  std::size_t operator()(const ModuleDependencyID &id) const {
    auto underlyingKindValue = static_cast<UnderlyingKindType>(id.second);

    return (hash<string>()(id.first) ^
            (hash<UnderlyingKindType>()(underlyingKindValue)));
  }
};
} // namespace std

namespace {

class Serializer {
  llvm::StringMap<unsigned, llvm::BumpPtrAllocator> IdentifierIDs;
  std::unordered_map<ModuleDependencyID,
                     llvm::DenseMap<ModuleIdentifierArrayKind, unsigned>>
      ArrayIDs;
  unsigned LastIdentifierID = 0;
  unsigned LastArrayID = 0;
  std::vector<StringRef> Identifiers;
  std::vector<std::vector<unsigned>> ArraysOfIdentifiers;

  llvm::BitstreamWriter &Out;

  /// A reusable buffer for emitting records.
  SmallVector<uint64_t, 64> ScratchRecord;
  std::array<unsigned, 256> AbbrCodes;

  // Returns the identifier ID of the added identifier, either
  // new or previously-hashed
  unsigned addIdentifier(const std::string &str);
  unsigned getIdentifier(const std::string &str) const;

  // Returns the array ID of the added array
  void addArray(ModuleDependencyID moduleID,
                ModuleIdentifierArrayKind arrayKind,
                const std::vector<std::string> &vec);
  unsigned getArray(ModuleDependencyID moduleID,
                    ModuleIdentifierArrayKind arrayKind) const;

  template <typename Layout>
  void registerRecordAbbr() {
    using AbbrArrayTy = decltype(AbbrCodes);
    static_assert(Layout::Code <= std::tuple_size<AbbrArrayTy>::value,
                  "layout has invalid record code");
    AbbrCodes[Layout::Code] = Layout::emitAbbrev(Out);
  }

  void collectStringsAndArrays(const ModuleDependenciesCache &cache);

  void emitBlockID(unsigned ID, StringRef name,
                   SmallVectorImpl<unsigned char> &nameBuffer);

  void emitRecordID(unsigned ID, StringRef name,
                    SmallVectorImpl<unsigned char> &nameBuffer);

  void writeSignature();
  void writeBlockInfoBlock();

  void writeMetadata();
  void writeIdentifiers();
  void writeArraysOfIdentifiers();

  void writeModuleInfo(ModuleDependencyID moduleID,
                       const ModuleDependencies &dependencyInfo);

public:
  Serializer(llvm::BitstreamWriter &ExistingOut) : Out(ExistingOut) {}

public:
  void writeInterModuleDependenciesCache(const ModuleDependenciesCache &cache);
};

} // end namespace

/// Record the name of a block.
void Serializer::emitBlockID(unsigned ID, StringRef name,
                             SmallVectorImpl<unsigned char> &nameBuffer) {
  SmallVector<unsigned, 1> idBuffer;
  idBuffer.push_back(ID);
  Out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETBID, idBuffer);

  // Emit the block name if present.
  if (name.empty())
    return;
  nameBuffer.resize(name.size());
  memcpy(nameBuffer.data(), name.data(), name.size());
  Out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_BLOCKNAME, nameBuffer);
}

/// Record the name of a record.
void Serializer::emitRecordID(unsigned ID, StringRef name,
                              SmallVectorImpl<unsigned char> &nameBuffer) {
  assert(ID < 256 && "can't fit record ID in next to name");
  nameBuffer.resize(name.size() + 1);
  nameBuffer[0] = ID;
  memcpy(nameBuffer.data() + 1, name.data(), name.size());
  Out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETRECORDNAME, nameBuffer);
}

void Serializer::writeBlockInfoBlock() {
  llvm::BCBlockRAII restoreBlock(Out, llvm::bitc::BLOCKINFO_BLOCK_ID, 2);

  SmallVector<unsigned char, 64> nameBuffer;
#define BLOCK(X) emitBlockID(X##_ID, #X, nameBuffer)
#define BLOCK_RECORD(K, X) emitRecordID(K::X, #X, nameBuffer)

  BLOCK(GRAPH_BLOCK);
  BLOCK_RECORD(graph_block, METADATA);
  BLOCK_RECORD(graph_block, IDENTIFIER_NODE);
  BLOCK_RECORD(graph_block, IDENTIFIER_ARRAY_NODE);

  BLOCK_RECORD(graph_block, MODULE_NODE);
  BLOCK_RECORD(graph_block, SWIFT_TEXTUAL_MODULE_DETAILS_NODE);
  BLOCK_RECORD(graph_block, SWIFT_BINARY_MODULE_DETAILS_NODE);
  BLOCK_RECORD(graph_block, SWIFT_PLACEHOLDER_MODULE_DETAILS_NODE);
  BLOCK_RECORD(graph_block, CLANG_MODULE_DETAILS_NODE);
}

void Serializer::writeSignature() {
  for (auto c : MODULE_DEPENDENCY_CACHE_FORMAT_SIGNATURE)
    Out.Emit((unsigned)c, 8);
}

void Serializer::writeMetadata() {
  using namespace graph_block;

  MetadataLayout::emitRecord(Out, ScratchRecord,
                             AbbrCodes[MetadataLayout::Code],
                             MODULE_DEPENDENCY_CACHE_FORMAT_VERSION_MAJOR,
                             MODULE_DEPENDENCY_CACHE_FORMAT_VERSION_MINOR,
                             version::getSwiftFullVersion());
}

void Serializer::writeIdentifiers() {
  using namespace graph_block;
  for (auto str : Identifiers) {
    IdentifierNodeLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[IdentifierNodeLayout::Code], str);
  }
}

void Serializer::writeArraysOfIdentifiers() {
  using namespace graph_block;
  for (auto vec : ArraysOfIdentifiers) {
    IdentifierArrayLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[IdentifierArrayLayout::Code], vec);
  }
}

void Serializer::writeModuleInfo(ModuleDependencyID moduleID,
                                 const ModuleDependencies &dependencyInfo) {
  using namespace graph_block;

  ModuleInfoLayout::emitRecord(
      Out, ScratchRecord, AbbrCodes[ModuleInfoLayout::Code],
      getIdentifier(moduleID.first),
      getArray(moduleID, ModuleIdentifierArrayKind::DirectDependencies));

  switch (dependencyInfo.getKind()) {
  case swift::ModuleDependenciesKind::SwiftTextual: {
    auto swiftTextDeps = dependencyInfo.getAsSwiftTextualModule();
    assert(swiftTextDeps);
    unsigned swiftInterfaceFileId =
        swiftTextDeps->swiftInterfaceFile
            ? getIdentifier(swiftTextDeps->swiftInterfaceFile.getValue())
            : 0;
    unsigned bridgingHeaderFileId =
        swiftTextDeps->bridgingHeaderFile
            ? getIdentifier(swiftTextDeps->bridgingHeaderFile.getValue())
            : 0;
    SwiftTextualModuleDetailsLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[SwiftTextualModuleDetailsLayout::Code],
        swiftInterfaceFileId,
        getArray(moduleID, ModuleIdentifierArrayKind::CompiledModuleCandidates),
        getArray(moduleID, ModuleIdentifierArrayKind::BuildCommandLine),
        getArray(moduleID, ModuleIdentifierArrayKind::ExtraPCMArgs),
        getIdentifier(swiftTextDeps->contextHash), swiftTextDeps->isFramework,
        bridgingHeaderFileId,
        getArray(moduleID, ModuleIdentifierArrayKind::SourceFiles),
        getArray(moduleID, ModuleIdentifierArrayKind::BridgingSourceFiles),
        getArray(moduleID,
                 ModuleIdentifierArrayKind::BridgingModuleDependencies));
    break;
  }
  case swift::ModuleDependenciesKind::SwiftBinary: {
    auto swiftBinDeps = dependencyInfo.getAsSwiftBinaryModule();
    assert(swiftBinDeps);
    SwiftBinaryModuleDetailsLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[SwiftBinaryModuleDetailsLayout::Code],
        getIdentifier(swiftBinDeps->compiledModulePath),
        getIdentifier(swiftBinDeps->moduleDocPath),
        getIdentifier(swiftBinDeps->sourceInfoPath), swiftBinDeps->isFramework);

    break;
  }
  case swift::ModuleDependenciesKind::SwiftPlaceholder: {
    auto swiftPHDeps = dependencyInfo.getAsPlaceholderDependencyModule();
    assert(swiftPHDeps);
    SwiftPlaceholderModuleDetailsLayout::emitRecord(
        Out, ScratchRecord,
        AbbrCodes[SwiftPlaceholderModuleDetailsLayout::Code],
        getIdentifier(swiftPHDeps->compiledModulePath),
        getIdentifier(swiftPHDeps->moduleDocPath),
        getIdentifier(swiftPHDeps->sourceInfoPath));
    break;
  }
  case swift::ModuleDependenciesKind::Clang: {
    auto clangDeps = dependencyInfo.getAsClangModule();
    assert(clangDeps);
    ClangModuleDetailsLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[ClangModuleDetailsLayout::Code],
        getIdentifier(clangDeps->moduleMapFile),
        getIdentifier(clangDeps->contextHash),
        getArray(moduleID, ModuleIdentifierArrayKind::NonPathCommandLine),
        getArray(moduleID, ModuleIdentifierArrayKind::FileDependencies));

    break;
  }
  }
}

unsigned Serializer::addIdentifier(const std::string &str) {
  if (str.empty())
    return 0;

  decltype(IdentifierIDs)::iterator iter;
  bool isNew;
  std::tie(iter, isNew) = IdentifierIDs.insert({str, LastIdentifierID + 1});

  if (!isNew)
    return iter->getValue();

  ++LastIdentifierID;
  // Note that we use the string data stored in the StringMap.
  Identifiers.push_back(iter->getKey());
  return iter->getValue();
}

unsigned Serializer::getIdentifier(const std::string &str) const {
  if (str.empty())
    return 0;

  auto iter = IdentifierIDs.find(str);
  assert(iter != IdentifierIDs.end());
  assert(iter->second != 0);
  return iter->second;
}

void Serializer::addArray(ModuleDependencyID moduleID,
                          ModuleIdentifierArrayKind arrayKind,
                          const std::vector<std::string> &vec) {
  if (ArrayIDs.find(moduleID) != ArrayIDs.end()) {
    // Already have arrays for this module
    llvm::DenseMap<ModuleIdentifierArrayKind, unsigned>::iterator iter;
    bool isNew;
    std::tie(iter, isNew) =
        ArrayIDs[moduleID].insert({arrayKind, LastArrayID + 1});
    if (!isNew)
      return;
  } else {
    // Do not yet have any arrays for this module
    ArrayIDs[moduleID] = llvm::DenseMap<ModuleIdentifierArrayKind, unsigned>();
    ArrayIDs[moduleID].insert({arrayKind, LastArrayID + 1});
  }

  ++LastArrayID;

  // Add in the individual identifiers in the array
  std::vector<unsigned> identifierIDs;
  identifierIDs.reserve(vec.size());
  for (const auto &id : vec) {
    identifierIDs.push_back(addIdentifier(id));
  }

  ArraysOfIdentifiers.push_back(identifierIDs);
  return;
}

unsigned Serializer::getArray(ModuleDependencyID moduleID,
                              ModuleIdentifierArrayKind arrayKind) const {
  auto iter = ArrayIDs.find(moduleID);
  assert(iter != ArrayIDs.end());
  auto &innerMap = iter->second;
  auto arrayIter = innerMap.find(arrayKind);
  assert(arrayIter != innerMap.end());
  return arrayIter->second;
}

void Serializer::collectStringsAndArrays(const ModuleDependenciesCache &cache) {
  for (auto &moduleID : cache.getAllModules()) {
    auto dependencyInfos = cache.findAllDependenciesIrrespectiveOfSearchPaths(
        moduleID.first, moduleID.second);
    assert(dependencyInfos.hasValue() && "Expected dependency info.");
    for (auto &dependencyInfo : *dependencyInfos) {
      // Add the module's name
      addIdentifier(moduleID.first);
      // Add the module's dependencies
      addArray(moduleID, ModuleIdentifierArrayKind::DirectDependencies,
               dependencyInfo.getModuleDependencies());

      // Add the dependency-kind-specific data
      switch (dependencyInfo.getKind()) {
      case swift::ModuleDependenciesKind::SwiftTextual: {
        auto swiftTextDeps = dependencyInfo.getAsSwiftTextualModule();
        assert(swiftTextDeps);
        if (swiftTextDeps->swiftInterfaceFile)
          addIdentifier(swiftTextDeps->swiftInterfaceFile.getValue());
        addArray(moduleID, ModuleIdentifierArrayKind::CompiledModuleCandidates,
                 swiftTextDeps->compiledModuleCandidates);
        addArray(moduleID, ModuleIdentifierArrayKind::BuildCommandLine,
                 swiftTextDeps->buildCommandLine);
        addArray(moduleID, ModuleIdentifierArrayKind::ExtraPCMArgs,
                 swiftTextDeps->extraPCMArgs);
        addIdentifier(swiftTextDeps->contextHash);
        if (swiftTextDeps->bridgingHeaderFile)
          addIdentifier(swiftTextDeps->bridgingHeaderFile.getValue());
        addArray(moduleID, ModuleIdentifierArrayKind::SourceFiles,
                 swiftTextDeps->sourceFiles);
        addArray(moduleID, ModuleIdentifierArrayKind::BridgingSourceFiles,
                 swiftTextDeps->bridgingSourceFiles);
        addArray(moduleID,
                 ModuleIdentifierArrayKind::BridgingModuleDependencies,
                 swiftTextDeps->bridgingModuleDependencies);
        break;
      }
      case swift::ModuleDependenciesKind::SwiftBinary: {
        auto swiftBinDeps = dependencyInfo.getAsSwiftBinaryModule();
        assert(swiftBinDeps);
        addIdentifier(swiftBinDeps->compiledModulePath);
        addIdentifier(swiftBinDeps->moduleDocPath);
        addIdentifier(swiftBinDeps->sourceInfoPath);
        break;
      }
      case swift::ModuleDependenciesKind::SwiftPlaceholder: {
        auto swiftPHDeps = dependencyInfo.getAsPlaceholderDependencyModule();
        assert(swiftPHDeps);
        addIdentifier(swiftPHDeps->compiledModulePath);
        addIdentifier(swiftPHDeps->moduleDocPath);
        addIdentifier(swiftPHDeps->sourceInfoPath);
        break;
      }
      case swift::ModuleDependenciesKind::Clang: {
        auto clangDeps = dependencyInfo.getAsClangModule();
        assert(clangDeps);
        addIdentifier(clangDeps->moduleMapFile);
        addIdentifier(clangDeps->contextHash);
        addArray(moduleID, ModuleIdentifierArrayKind::NonPathCommandLine,
                 clangDeps->nonPathCommandLine);
        addArray(moduleID, ModuleIdentifierArrayKind::FileDependencies,
                 clangDeps->fileDependencies);
        break;
      }
      }
    }
  }
}

void Serializer::writeInterModuleDependenciesCache(
    const ModuleDependenciesCache &cache) {
  // Write the header
  writeSignature();
  writeBlockInfoBlock();

  // Enter the main graph block
  unsigned blockID = GRAPH_BLOCK_ID;
  llvm::BCBlockRAII restoreBlock(Out, blockID, 8);

  using namespace graph_block;

  registerRecordAbbr<MetadataLayout>();
  registerRecordAbbr<IdentifierNodeLayout>();
  registerRecordAbbr<IdentifierArrayLayout>();
  registerRecordAbbr<ModuleInfoLayout>();
  registerRecordAbbr<SwiftTextualModuleDetailsLayout>();
  registerRecordAbbr<SwiftBinaryModuleDetailsLayout>();
  registerRecordAbbr<SwiftPlaceholderModuleDetailsLayout>();
  registerRecordAbbr<ClangModuleDetailsLayout>();

  // Make a pass to collect all unique strings and arrays
  // of strings
  collectStringsAndArrays(cache);

  // Write the version information
  writeMetadata();

  // Write the strings
  writeIdentifiers();

  // Write the arrays
  writeArraysOfIdentifiers();

  // Write the core graph
  for (auto &moduleID : cache.getAllModules()) {
    auto dependencyInfos = cache.findAllDependenciesIrrespectiveOfSearchPaths(
        moduleID.first, moduleID.second);
    assert(dependencyInfos.hasValue() && "Expected dependency info.");
    for (auto &dependencyInfo : *dependencyInfos) {
      writeModuleInfo(moduleID, dependencyInfo);
    }
  }

  return;
}

void swift::dependencies::module_dependency_cache_serialization::
    writeInterModuleDependenciesCache(llvm::BitstreamWriter &Out,
                                      const ModuleDependenciesCache &cache) {
  Serializer serializer{Out};
  serializer.writeInterModuleDependenciesCache(cache);
}

bool swift::dependencies::module_dependency_cache_serialization::
    writeInterModuleDependenciesCache(DiagnosticEngine &diags, StringRef path,
                                      const ModuleDependenciesCache &cache) {
  PrettyStackTraceStringAction stackTrace(
      "saving inter-module dependency graph", path);
  return withOutputFile(diags, path, [&](llvm::raw_ostream &out) {
    SmallVector<char, 0> Buffer;
    llvm::BitstreamWriter Writer{Buffer};
    writeInterModuleDependenciesCache(Writer, cache);
    out.write(Buffer.data(), Buffer.size());
    out.flush();
    return false;
  });
}
