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
#include "swift/Basic/Assertions.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "swift/Basic/Version.h"
#include "swift/DependencyScan/SerializedModuleDependencyCacheFormat.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/VirtualOutputBackend.h"
#include <unordered_map>

using namespace swift;
using namespace dependencies;
using namespace module_dependency_cache_serialization;

// MARK: Deserialization
namespace swift {

class ModuleDependenciesCacheDeserializer {
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

  std::optional<std::string> getIdentifier(unsigned n);
  std::optional<std::vector<std::string>> getStringArray(unsigned n);
  std::optional<std::vector<ModuleDependencyID>>
  getModuleDependencyIDArray(unsigned n);

public:
  ModuleDependenciesCacheDeserializer(llvm::MemoryBufferRef Data) : Cursor(Data) {}
  bool readInterModuleDependenciesCache(ModuleDependenciesCache &cache);
};

} // end namespace

/// Read in the expected signature: IMDC
bool ModuleDependenciesCacheDeserializer::readSignature() {
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
bool ModuleDependenciesCacheDeserializer::enterGraphBlock() {
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
bool ModuleDependenciesCacheDeserializer::readMetadata() {
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
bool ModuleDependenciesCacheDeserializer::readGraph(ModuleDependenciesCache &cache) {
  using namespace graph_block;

  bool hasCurrentModule = false;
  std::string currentModuleName;
  unsigned currentContextHashID;
  std::vector<ScannerImportStatementInfo> currentModuleImports;
  std::vector<ScannerImportStatementInfo> currentOptionalModuleImports;
  std::vector<ModuleDependencyID> currentModuleDependencyIDs;

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
      unsigned moduleNameID, contextHashID,
               moduleImportsArrayID, optionalModuleImportsArrayID,
               moduleDependencyIDArrayID;
      ModuleInfoLayout::readRecord(Scratch, moduleNameID, contextHashID,
                                   moduleImportsArrayID,
                                   optionalModuleImportsArrayID,
                                   moduleDependencyIDArrayID);
      auto moduleName = getIdentifier(moduleNameID);
      if (!moduleName)
        llvm::report_fatal_error("Bad module name");
      currentModuleName = *moduleName;
      currentContextHashID = contextHashID;
      auto importStrings = getStringArray(moduleImportsArrayID);
      auto optionalImportStrings = getStringArray(optionalModuleImportsArrayID);
      if (importStrings.has_value()) {
        for (const auto &is : importStrings.value())
          currentModuleImports.push_back(is);
      }

      if (optionalImportStrings.has_value()) {
        for (const auto &ois : optionalImportStrings.value())
          currentOptionalModuleImports.push_back(ois);
      }

      auto optionalCurrentModuleDependencyIDs = getModuleDependencyIDArray(moduleDependencyIDArrayID);
      if (!optionalCurrentModuleDependencyIDs)
        llvm::report_fatal_error("Bad direct dependencies: no qualified dependencies");
      currentModuleDependencyIDs = optionalCurrentModuleDependencyIDs.value();
      break;
    }

    case SWIFT_INTERFACE_MODULE_DETAILS_NODE: {
      if (!hasCurrentModule)
        llvm::report_fatal_error(
            "Unexpected SWIFT_TEXTUAL_MODULE_DETAILS_NODE record");
      unsigned outputPathFileID, interfaceFileID,
          compiledModuleCandidatesArrayID, buildCommandLineArrayID,
          extraPCMArgsArrayID, contextHashID, isFramework, isStatic, bridgingHeaderFileID,
          sourceFilesArrayID, bridgingSourceFilesArrayID,
          bridgingModuleDependenciesArrayID, overlayDependencyIDArrayID,
          CASFileSystemRootID, bridgingHeaderIncludeTreeID, moduleCacheKeyID,
          userModuleVersionID;
      SwiftInterfaceModuleDetailsLayout::readRecord(
          Scratch, outputPathFileID, interfaceFileID,
          compiledModuleCandidatesArrayID, buildCommandLineArrayID,
          extraPCMArgsArrayID, contextHashID, isFramework, isStatic, bridgingHeaderFileID,
          sourceFilesArrayID, bridgingSourceFilesArrayID,
          bridgingModuleDependenciesArrayID, overlayDependencyIDArrayID,
          CASFileSystemRootID, bridgingHeaderIncludeTreeID, moduleCacheKeyID, userModuleVersionID);

      auto outputModulePath = getIdentifier(outputPathFileID);
      if (!outputModulePath)
         llvm::report_fatal_error("Bad .swiftmodule output path");
      std::optional<std::string> optionalSwiftInterfaceFile;
      if (interfaceFileID != 0) {
        auto swiftInterfaceFile = getIdentifier(interfaceFileID);
        if (!swiftInterfaceFile)
          llvm::report_fatal_error("Bad swift interface file path");
        optionalSwiftInterfaceFile = *swiftInterfaceFile;
      }
      auto compiledModuleCandidates = getStringArray(compiledModuleCandidatesArrayID);
      if (!compiledModuleCandidates)
        llvm::report_fatal_error("Bad compiled module candidates");
      auto commandLine = getStringArray(buildCommandLineArrayID);
      if (!commandLine)
        llvm::report_fatal_error("Bad command line");
      auto extraPCMArgs = getStringArray(extraPCMArgsArrayID);
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
      std::vector<StringRef> compiledCandidatesRefs;
      for (auto &cc : compiledCandidatesRefs)
        compiledCandidatesRefs.push_back(cc);

      auto rootFileSystemID = getIdentifier(CASFileSystemRootID);
      if (!rootFileSystemID)
        llvm::report_fatal_error("Bad CASFileSystem RootID");
      auto moduleCacheKey = getIdentifier(moduleCacheKeyID);
      if (!moduleCacheKey)
        llvm::report_fatal_error("Bad moduleCacheKey");
      auto userModuleVersion = getIdentifier(userModuleVersionID);
      if (!userModuleVersion)
        llvm::report_fatal_error("Bad userModuleVersion");

      // TODO: LinkLibraries, MacroDependencies
      // Form the dependencies storage object
      auto moduleDep = ModuleDependencyInfo::forSwiftInterfaceModule(
          outputModulePath.value(), optionalSwiftInterfaceFile.value(),
          compiledCandidatesRefs, buildCommandRefs, {}, extraPCMRefs,
          *contextHash, isFramework, isStatic, *rootFileSystemID, *moduleCacheKey,
          *userModuleVersion);

      // Add imports of this module
      for (const auto &moduleName : currentModuleImports)
        moduleDep.addModuleImport(moduleName.importIdentifier);
      // Add optional imports of this module
      for (const auto &moduleName : currentOptionalModuleImports)
        moduleDep.addOptionalModuleImport(moduleName.importIdentifier);

      // Add qualified dependencies of this module
      std::vector<ModuleDependencyID> swiftDeps;
      std::vector<ModuleDependencyID> clangDeps;
      for (const auto &mID : currentModuleDependencyIDs) {
        if (mID.Kind == ModuleDependencyKind::Clang)
          clangDeps.push_back(mID);
        else
          swiftDeps.push_back(mID);
      }
      moduleDep.setImportedSwiftDependencies(swiftDeps);
      moduleDep.setImportedClangDependencies(clangDeps);

      // Add bridging header file path
      if (bridgingHeaderFileID != 0) {
        auto bridgingHeaderFile = getIdentifier(bridgingHeaderFileID);
        if (!bridgingHeaderFile)
          llvm::report_fatal_error("Bad bridging header path");

        moduleDep.addBridgingHeader(*bridgingHeaderFile);
      }

      // Add bridging source files
      auto bridgingSourceFiles = getStringArray(bridgingSourceFilesArrayID);
      if (!bridgingSourceFiles)
        llvm::report_fatal_error("Bad bridging source files");
      for (const auto &file : *bridgingSourceFiles)
        moduleDep.addHeaderSourceFile(file);

      // Add source files
      auto sourceFiles = getStringArray(sourceFilesArrayID);
      if (!sourceFiles)
        llvm::report_fatal_error("Bad bridging source files");
      for (const auto &file : *sourceFiles)
        moduleDep.addSourceFile(file);

      // Add bridging module dependencies
      auto bridgingModuleDeps = getStringArray(bridgingModuleDependenciesArrayID);
      if (!bridgingModuleDeps)
        llvm::report_fatal_error("Bad bridging module dependencies");
      llvm::StringSet<> alreadyAdded;

      std::vector<ModuleDependencyID> bridgingModuleDepIDs;
      for (const auto &mod : bridgingModuleDeps.value())
        bridgingModuleDepIDs.push_back(ModuleDependencyID{mod, ModuleDependencyKind::Clang});
      moduleDep.setHeaderClangDependencies(bridgingModuleDepIDs);

      // Add Swift overlay dependencies
      auto overlayModuleDependencyIDs = getModuleDependencyIDArray(overlayDependencyIDArrayID);
      if (!overlayModuleDependencyIDs.has_value())
        llvm::report_fatal_error("Bad overlay dependencies: no qualified dependencies");
      moduleDep.setSwiftOverlayDependencies(overlayModuleDependencyIDs.value());

      // Add bridging header include tree
      auto bridgingHeaderIncludeTree =
          getIdentifier(bridgingHeaderIncludeTreeID);
      if (!bridgingHeaderIncludeTree)
        llvm::report_fatal_error("Bad bridging header include tree");
      if (!bridgingHeaderIncludeTree->empty())
        moduleDep.addBridgingHeaderIncludeTree(*bridgingHeaderIncludeTree);

      cache.recordDependency(currentModuleName, std::move(moduleDep));
      hasCurrentModule = false;
      break;
    }

    case SWIFT_SOURCE_MODULE_DETAILS_NODE: {
      if (!hasCurrentModule)
        llvm::report_fatal_error(
            "Unexpected SWIFT_SOURCE_MODULE_DETAILS_NODE record");
      // Expected context hash ID is 0
      if (currentContextHashID)
        llvm::report_fatal_error(
            "Unexpected context hash on MODULE_NODE corresponding to a "
            "SWIFT_SOURCE_MODULE_DETAILS_NODE record");
      unsigned extraPCMArgsArrayID, bridgingHeaderFileID, sourceFilesArrayID,
          bridgingSourceFilesArrayID, bridgingModuleDependenciesArrayID,
          overlayDependencyIDArrayID, CASFileSystemRootID,
          bridgingHeaderIncludeTreeID, buildCommandLineArrayID,
          bridgingHeaderBuildCommandLineArrayID;
      SwiftSourceModuleDetailsLayout::readRecord(
          Scratch, extraPCMArgsArrayID, bridgingHeaderFileID,
          sourceFilesArrayID, bridgingSourceFilesArrayID,
          bridgingModuleDependenciesArrayID, overlayDependencyIDArrayID,
          CASFileSystemRootID, bridgingHeaderIncludeTreeID,
          buildCommandLineArrayID, bridgingHeaderBuildCommandLineArrayID);

      auto extraPCMArgs = getStringArray(extraPCMArgsArrayID);
      if (!extraPCMArgs)
        llvm::report_fatal_error("Bad PCM Args set");
      std::vector<StringRef> extraPCMRefs;
      for (auto &arg : *extraPCMArgs)
        extraPCMRefs.push_back(arg);

      auto rootFileSystemID = getIdentifier(CASFileSystemRootID);
      if (!rootFileSystemID)
        llvm::report_fatal_error("Bad CASFileSystem RootID");
      auto commandLine = getStringArray(buildCommandLineArrayID);
      if (!commandLine)
        llvm::report_fatal_error("Bad command line");
      std::vector<StringRef> buildCommandRefs;
      for (auto &arg : *commandLine)
        buildCommandRefs.push_back(arg);
      std::vector<StringRef> bridgingHeaderBuildCommandRefs;
      auto bridgingHeaderCommandLine =
          getStringArray(bridgingHeaderBuildCommandLineArrayID);
      if (!bridgingHeaderCommandLine)
        llvm::report_fatal_error("Bad bridging header command line");
      for (auto &arg : *bridgingHeaderCommandLine)
        bridgingHeaderBuildCommandRefs.push_back(arg);

      // Form the dependencies storage object
      auto moduleDep = ModuleDependencyInfo::forSwiftSourceModule(
          *rootFileSystemID, buildCommandRefs, bridgingHeaderBuildCommandRefs,
          extraPCMRefs);

      // Add dependencies of this module
      for (const auto &moduleName : currentModuleImports)
        moduleDep.addModuleImport(moduleName.importIdentifier);
      // Add optional imports of this module
      for (const auto &moduleName : currentOptionalModuleImports)
        moduleDep.addOptionalModuleImport(moduleName.importIdentifier);

      // Add bridging header file path
      if (bridgingHeaderFileID != 0) {
        auto bridgingHeaderFile = getIdentifier(bridgingHeaderFileID);
        if (!bridgingHeaderFile)
          llvm::report_fatal_error("Bad bridging header path");

        moduleDep.addBridgingHeader(*bridgingHeaderFile);
      }

      // Add bridging source files
      auto bridgingSourceFiles = getStringArray(bridgingSourceFilesArrayID);
      if (!bridgingSourceFiles)
        llvm::report_fatal_error("Bad bridging source files");
      for (const auto &file : *bridgingSourceFiles)
        moduleDep.addHeaderSourceFile(file);

      // Add source files
      auto sourceFiles = getStringArray(sourceFilesArrayID);
      if (!sourceFiles)
        llvm::report_fatal_error("Bad bridging source files");
      for (const auto &file : *sourceFiles)
        moduleDep.addSourceFile(file);

      // Add bridging module dependencies
      auto bridgingModuleDeps = getStringArray(bridgingModuleDependenciesArrayID);
      if (!bridgingModuleDeps)
        llvm::report_fatal_error("Bad bridging module dependencies");
      llvm::StringSet<> alreadyAdded;

      std::vector<ModuleDependencyID> headerDependencyIDs;
      for (const auto &mod : *bridgingModuleDeps)
        headerDependencyIDs.push_back({mod, ModuleDependencyKind::Clang});
      moduleDep.setHeaderClangDependencies(headerDependencyIDs);

      // Add Swift overlay dependencies
      auto overlayModuleDependencyIDs = getModuleDependencyIDArray(overlayDependencyIDArrayID);
      if (!overlayModuleDependencyIDs.has_value())
        llvm::report_fatal_error("Bad overlay dependencies: no qualified dependencies");
      moduleDep.setSwiftOverlayDependencies(overlayModuleDependencyIDs.value());

      // Add bridging header include tree
      auto bridgingHeaderIncludeTree =
          getIdentifier(bridgingHeaderIncludeTreeID);
      if (!bridgingHeaderIncludeTree)
        llvm::report_fatal_error("Bad bridging header include tree");
      if (!bridgingHeaderIncludeTree->empty())
        moduleDep.addBridgingHeaderIncludeTree(*bridgingHeaderIncludeTree);

      cache.recordDependency(currentModuleName, std::move(moduleDep));
      hasCurrentModule = false;
      break;
    }

    case SWIFT_BINARY_MODULE_DETAILS_NODE: {
      if (!hasCurrentModule)
        llvm::report_fatal_error(
            "Unexpected SWIFT_BINARY_MODULE_DETAILS_NODE record");
      unsigned compiledModulePathID, moduleDocPathID, moduleSourceInfoPathID,
               overlayDependencyIDArrayID, headerImportID,
               headerModuleDependenciesArrayID,
               headerImportsSourceFilesArrayID, isFramework, isStatic,
               moduleCacheKeyID, userModuleVersionID;
      SwiftBinaryModuleDetailsLayout::readRecord(
          Scratch, compiledModulePathID, moduleDocPathID,
          moduleSourceInfoPathID, overlayDependencyIDArrayID,
          headerImportID, headerModuleDependenciesArrayID,
          headerImportsSourceFilesArrayID, isFramework, isStatic,
          moduleCacheKeyID, userModuleVersionID);

      auto compiledModulePath = getIdentifier(compiledModulePathID);
      if (!compiledModulePath)
        llvm::report_fatal_error("Bad compiled module path");
      auto moduleDocPath = getIdentifier(moduleDocPathID);
      if (!moduleDocPath)
        llvm::report_fatal_error("Bad module doc path");
      auto moduleSourceInfoPath = getIdentifier(moduleSourceInfoPathID);
      if (!moduleSourceInfoPath)
        llvm::report_fatal_error("Bad module source info path");
      auto moduleCacheKey = getIdentifier(moduleCacheKeyID);
      if (!moduleCacheKey)
        llvm::report_fatal_error("Bad moduleCacheKey");
      auto userModuleVersion = getIdentifier(userModuleVersionID);
      if (!userModuleVersion)
        llvm::report_fatal_error("Bad userModuleVersion");
      auto headerImport = getIdentifier(headerImportID);
      if (!headerImport)
        llvm::report_fatal_error("Bad binary direct dependencies: no header import");

      // TODO: LinkLibraries, DefiningModulePath
      // Form the dependencies storage object
      auto moduleDep = ModuleDependencyInfo::forSwiftBinaryModule(
           *compiledModulePath, *moduleDocPath, *moduleSourceInfoPath,
           currentModuleImports, currentOptionalModuleImports, {},
           *headerImport, "", isFramework, isStatic, *moduleCacheKey, *userModuleVersion);

      auto headerModuleDependencies = getStringArray(headerModuleDependenciesArrayID);
      if (!headerModuleDependencies)
        llvm::report_fatal_error("Bad binary direct dependencies: no header import module dependencies");
      llvm::StringSet<> alreadyAdded;

      std::vector<ModuleDependencyID> clangHeaderDependencyIDs;
      for (const auto &headerDepName : *headerModuleDependencies)
        clangHeaderDependencyIDs.push_back(ModuleDependencyID{headerDepName,
                                                              ModuleDependencyKind::Clang});

      moduleDep.setHeaderClangDependencies(clangHeaderDependencyIDs);

      auto headerImportsSourceFiles = getStringArray(headerImportsSourceFilesArrayID);
      if (!headerImportsSourceFiles)
        llvm::report_fatal_error("Bad binary direct dependencies: no header import source files");
      for (const auto &depSource : *headerImportsSourceFiles)
        moduleDep.addHeaderSourceFile(depSource);

      // Add Swift overlay dependencies
      auto overlayModuleDependencyIDs = getModuleDependencyIDArray(overlayDependencyIDArrayID);
      if (!overlayModuleDependencyIDs.has_value())
        llvm::report_fatal_error("Bad overlay dependencies: no qualified dependencies");

      moduleDep.setSwiftOverlayDependencies(*overlayModuleDependencyIDs);
      cache.recordDependency(currentModuleName, std::move(moduleDep));
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
      auto moduleDep = ModuleDependencyInfo::forPlaceholderSwiftModuleStub(
          *compiledModulePath, *moduleDocPath, *moduleSourceInfoPath);

      // Add dependencies of this module
      for (const auto &moduleName : currentModuleImports)
        moduleDep.addModuleImport(moduleName.importIdentifier);
      // Add optional imports of this module
      for (const auto &moduleName : currentOptionalModuleImports)
        moduleDep.addOptionalModuleImport(moduleName.importIdentifier);

      cache.recordDependency(currentModuleName, std::move(moduleDep));
      hasCurrentModule = false;
      break;
    }

    case CLANG_MODULE_DETAILS_NODE: {
      if (!hasCurrentModule)
        llvm::report_fatal_error("Unexpected CLANG_MODULE_DETAILS_NODE record");
      unsigned pcmOutputPathID, mappedPCMPathID, moduleMapPathID, contextHashID,
          commandLineArrayID, fileDependenciesArrayID, capturedPCMArgsArrayID,
          CASFileSystemRootID, clangIncludeTreeRootID, moduleCacheKeyID, isSystem;
      ClangModuleDetailsLayout::readRecord(
          Scratch, pcmOutputPathID, mappedPCMPathID, moduleMapPathID,
          contextHashID, commandLineArrayID, fileDependenciesArrayID,
          capturedPCMArgsArrayID, CASFileSystemRootID, clangIncludeTreeRootID,
          moduleCacheKeyID, isSystem);
      auto pcmOutputPath = getIdentifier(pcmOutputPathID);
      if (!pcmOutputPath)
        llvm::report_fatal_error("Bad pcm output path");
      auto mappedPCMPath = getIdentifier(mappedPCMPathID);
      if (!mappedPCMPath)
        llvm::report_fatal_error("Bad mapped pcm path");
      auto moduleMapPath = getIdentifier(moduleMapPathID);
      if (!moduleMapPath)
        llvm::report_fatal_error("Bad module map path");
      auto contextHash = getIdentifier(contextHashID);
      if (!contextHash)
        llvm::report_fatal_error("Bad context hash");
      auto commandLineArgs = getStringArray(commandLineArrayID);
      if (!commandLineArgs)
        llvm::report_fatal_error("Bad command line");
      auto fileDependencies = getStringArray(fileDependenciesArrayID);
      if (!fileDependencies)
        llvm::report_fatal_error("Bad file dependencies");
      auto capturedPCMArgs = getStringArray(capturedPCMArgsArrayID);
      if (!capturedPCMArgs)
        llvm::report_fatal_error("Bad captured PCM Args");
      auto rootFileSystemID = getIdentifier(CASFileSystemRootID);
      if (!rootFileSystemID)
        llvm::report_fatal_error("Bad CASFileSystem RootID");
      auto clangIncludeTreeRoot = getIdentifier(clangIncludeTreeRootID);
      if (!clangIncludeTreeRoot)
        llvm::report_fatal_error("Bad clang include tree ID");
      auto moduleCacheKey = getIdentifier(moduleCacheKeyID);
      if (!moduleCacheKey)
        llvm::report_fatal_error("Bad moduleCacheKey");

      // TODO: LinkLibraries
      // Form the dependencies storage object
      auto moduleDep = ModuleDependencyInfo::forClangModule(
          *pcmOutputPath, *mappedPCMPath, *moduleMapPath, *contextHash,
          *commandLineArgs, *fileDependencies, *capturedPCMArgs, {},
          *rootFileSystemID, *clangIncludeTreeRoot, *moduleCacheKey, isSystem);

      // Add dependencies of this module
      for (const auto &moduleName : currentModuleImports)
        moduleDep.addModuleImport(moduleName.importIdentifier);
      // Add optional imports of this module
      for (const auto &moduleName : currentOptionalModuleImports)
        moduleDep.addOptionalModuleImport(moduleName.importIdentifier);

      cache.recordDependency(currentModuleName, std::move(moduleDep));
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

bool ModuleDependenciesCacheDeserializer::readInterModuleDependenciesCache(
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

std::optional<std::string>
ModuleDependenciesCacheDeserializer::getIdentifier(unsigned n) {
  if (n == 0)
    return std::string();

  --n;
  if (n >= Identifiers.size())
    return std::nullopt;

  return Identifiers[n];
}

std::optional<std::vector<std::string>>
ModuleDependenciesCacheDeserializer::getStringArray(unsigned n) {
  if (n == 0)
    return std::vector<std::string>();

  --n;
  if (n >= ArraysOfIdentifierIDs.size())
    return std::nullopt;

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

std::optional<std::vector<ModuleDependencyID>>
ModuleDependenciesCacheDeserializer::getModuleDependencyIDArray(unsigned n) {
  auto encodedIdentifierStringArray = getStringArray(n);
  if (encodedIdentifierStringArray.has_value()) {
    static const std::string textualPrefix("swiftTextual");
    static const std::string binaryPrefix("swiftBinary");
    static const std::string placeholderPrefix("swiftPlaceholder");
    static const std::string clangPrefix("clang");
    std::vector<ModuleDependencyID> result;
    for (const auto &encodedIdentifierString : *encodedIdentifierStringArray) {
      ModuleDependencyID id;
      if (!encodedIdentifierString.compare(0, textualPrefix.size(), textualPrefix)) {
        auto moduleName = encodedIdentifierString.substr(textualPrefix.size() + 1);
        id = {moduleName, ModuleDependencyKind::SwiftInterface};
      } else if (!encodedIdentifierString.compare(0, binaryPrefix.size(), binaryPrefix)) {
        auto moduleName = encodedIdentifierString.substr(binaryPrefix.size() + 1);
        id = {moduleName, ModuleDependencyKind::SwiftBinary};
      } else if (!encodedIdentifierString.compare(0, placeholderPrefix.size(), placeholderPrefix)) {
        auto moduleName = encodedIdentifierString.substr(placeholderPrefix.size() + 1);
        id = {moduleName, ModuleDependencyKind::SwiftPlaceholder};
      } else {
        auto moduleName = encodedIdentifierString.substr(clangPrefix.size() + 1);
        id = {moduleName, ModuleDependencyKind::Clang};
      }
      result.push_back(id);
    }
    return result;
  }

  return std::nullopt;
}

bool swift::dependencies::module_dependency_cache_serialization::
    readInterModuleDependenciesCache(llvm::MemoryBuffer &buffer,
                                     ModuleDependenciesCache &cache) {
  ModuleDependenciesCacheDeserializer deserializer(buffer.getMemBufferRef());
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
  DependencyImports,
  OptionalDependencyImports,
  DependencyHeaders,
  QualifiedModuleDependencyIDs,
  CompiledModuleCandidates,
  BuildCommandLine,
  ExtraPCMArgs,
  SourceFiles,
  BridgingSourceFiles,
  BridgingModuleDependencies,
  HeaderInputDependencySourceFiles,
  HeaderInputModuleDependencies,
  SwiftOverlayDependencyIDs,
  BridgingHeaderBuildCommandLine,
  NonPathCommandLine,
  FileDependencies,
  CapturedPCMArgs,
  LastArrayKind
};

using ModuleIdentifierArrayKey =
    std::pair<ModuleDependencyID, ModuleIdentifierArrayKind>;

// DenseMap Infos for hashing of ModuleIdentifierArrayKind
template <>
struct llvm::DenseMapInfo<ModuleIdentifierArrayKind> {
  using UnderlyingType = std::underlying_type<ModuleIdentifierArrayKind>::type;
  using UnderlyingInfo = DenseMapInfo<UnderlyingType>;

  static inline ModuleIdentifierArrayKind getEmptyKey() {
    return ModuleIdentifierArrayKind::Empty;
  }
  static inline ModuleIdentifierArrayKind getTombstoneKey() {
    return ModuleIdentifierArrayKind::LastArrayKind;
  }
  static unsigned getHashValue(const ModuleIdentifierArrayKind &arrKind) {
    auto underlyingValue = static_cast<UnderlyingType>(arrKind);
    return UnderlyingInfo::getHashValue(underlyingValue);
  }
  static bool isEqual(const ModuleIdentifierArrayKind &LHS,
                      const ModuleIdentifierArrayKind &RHS) {
    return LHS == RHS;
  }
};

namespace swift {

class ModuleDependenciesCacheSerializer {
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
  void addStringArray(ModuleDependencyID moduleID,
                      ModuleIdentifierArrayKind arrayKind,
                      const std::vector<std::string> &vec);
  void addDependencyIDArray(ModuleDependencyID moduleID,
                            ModuleIdentifierArrayKind arrayKind,
                            const std::vector<ModuleDependencyID> &vec);
  unsigned getArrayID(ModuleDependencyID moduleID,
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
                       const ModuleDependencyInfo &dependencyInfo);

public:
  ModuleDependenciesCacheSerializer(llvm::BitstreamWriter &ExistingOut) : Out(ExistingOut) {}

public:
  void
  writeInterModuleDependenciesCache(const ModuleDependenciesCache &cache);
};

} // end namespace

/// Record the name of a block.
void ModuleDependenciesCacheSerializer::emitBlockID(unsigned ID, StringRef name,
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
void ModuleDependenciesCacheSerializer::emitRecordID(unsigned ID, StringRef name,
                              SmallVectorImpl<unsigned char> &nameBuffer) {
  assert(ID < 256 && "can't fit record ID in next to name");
  nameBuffer.resize(name.size() + 1);
  nameBuffer[0] = ID;
  memcpy(nameBuffer.data() + 1, name.data(), name.size());
  Out.EmitRecord(llvm::bitc::BLOCKINFO_CODE_SETRECORDNAME, nameBuffer);
}

void ModuleDependenciesCacheSerializer::writeBlockInfoBlock() {
  llvm::BCBlockRAII restoreBlock(Out, llvm::bitc::BLOCKINFO_BLOCK_ID, 2);

  SmallVector<unsigned char, 64> nameBuffer;
#define BLOCK(X) emitBlockID(X##_ID, #X, nameBuffer)
#define BLOCK_RECORD(K, X) emitRecordID(K::X, #X, nameBuffer)

  BLOCK(GRAPH_BLOCK);
  BLOCK_RECORD(graph_block, METADATA);
  BLOCK_RECORD(graph_block, IDENTIFIER_NODE);
  BLOCK_RECORD(graph_block, IDENTIFIER_ARRAY_NODE);

  BLOCK_RECORD(graph_block, MODULE_NODE);
  BLOCK_RECORD(graph_block, SWIFT_INTERFACE_MODULE_DETAILS_NODE);
  BLOCK_RECORD(graph_block, SWIFT_SOURCE_MODULE_DETAILS_NODE);
  BLOCK_RECORD(graph_block, SWIFT_BINARY_MODULE_DETAILS_NODE);
  BLOCK_RECORD(graph_block, SWIFT_PLACEHOLDER_MODULE_DETAILS_NODE);
  BLOCK_RECORD(graph_block, CLANG_MODULE_DETAILS_NODE);
}

void ModuleDependenciesCacheSerializer::writeSignature() {
  for (auto c : MODULE_DEPENDENCY_CACHE_FORMAT_SIGNATURE)
    Out.Emit((unsigned)c, 8);
}

void ModuleDependenciesCacheSerializer::writeMetadata() {
  using namespace graph_block;

  MetadataLayout::emitRecord(Out, ScratchRecord,
                             AbbrCodes[MetadataLayout::Code],
                             MODULE_DEPENDENCY_CACHE_FORMAT_VERSION_MAJOR,
                             MODULE_DEPENDENCY_CACHE_FORMAT_VERSION_MINOR,
                             version::getSwiftFullVersion());
}

void ModuleDependenciesCacheSerializer::writeIdentifiers() {
  using namespace graph_block;
  for (auto str : Identifiers) {
    IdentifierNodeLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[IdentifierNodeLayout::Code], str);
  }
}

void ModuleDependenciesCacheSerializer::writeArraysOfIdentifiers() {
  using namespace graph_block;
  for (auto vec : ArraysOfIdentifiers) {
    IdentifierArrayLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[IdentifierArrayLayout::Code], vec);
  }
}

void ModuleDependenciesCacheSerializer::writeModuleInfo(
    ModuleDependencyID moduleID,
    const ModuleDependencyInfo &dependencyInfo) {
  using namespace graph_block;
  // TODO: Eliminate per-module context hash
  auto contextHashStrID = 0;

  ModuleInfoLayout::emitRecord(
      Out, ScratchRecord, AbbrCodes[ModuleInfoLayout::Code],
      getIdentifier(moduleID.ModuleName), contextHashStrID,
      getArrayID(moduleID, ModuleIdentifierArrayKind::DependencyImports),
      getArrayID(moduleID, ModuleIdentifierArrayKind::OptionalDependencyImports),
      getArrayID(moduleID, ModuleIdentifierArrayKind::QualifiedModuleDependencyIDs));

  switch (dependencyInfo.getKind()) {
  case swift::ModuleDependencyKind::SwiftInterface: {
    auto swiftTextDeps = dependencyInfo.getAsSwiftInterfaceModule();
    assert(swiftTextDeps);
    unsigned outputModulePathFileId =
        getIdentifier(swiftTextDeps->moduleOutputPath);
    unsigned swiftInterfaceFileId =
        getIdentifier(swiftTextDeps->swiftInterfaceFile);
    unsigned bridgingHeaderFileId =
        swiftTextDeps->textualModuleDetails.bridgingHeaderFile
            ? getIdentifier(swiftTextDeps->textualModuleDetails
                                .bridgingHeaderFile.value())
            : 0;
    SwiftInterfaceModuleDetailsLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[SwiftInterfaceModuleDetailsLayout::Code],
        outputModulePathFileId, swiftInterfaceFileId,
        getArrayID(moduleID,
                   ModuleIdentifierArrayKind::CompiledModuleCandidates),
        getArrayID(moduleID, ModuleIdentifierArrayKind::BuildCommandLine),
        getArrayID(moduleID, ModuleIdentifierArrayKind::ExtraPCMArgs),
        getIdentifier(swiftTextDeps->contextHash), 
        swiftTextDeps->isFramework, swiftTextDeps->isStatic,
        bridgingHeaderFileId,
        getArrayID(moduleID, ModuleIdentifierArrayKind::SourceFiles),
        getArrayID(moduleID, ModuleIdentifierArrayKind::BridgingSourceFiles),
        getArrayID(moduleID,
                   ModuleIdentifierArrayKind::BridgingModuleDependencies),
        getArrayID(moduleID,
                   ModuleIdentifierArrayKind::SwiftOverlayDependencyIDs),
        getIdentifier(swiftTextDeps->textualModuleDetails.CASFileSystemRootID),
        getIdentifier(swiftTextDeps->textualModuleDetails
                          .CASBridgingHeaderIncludeTreeRootID),
        getIdentifier(swiftTextDeps->moduleCacheKey),
        getIdentifier(swiftTextDeps->userModuleVersion));
    break;
  }
  case swift::ModuleDependencyKind::SwiftSource: {
    auto swiftSourceDeps = dependencyInfo.getAsSwiftSourceModule();
    assert(swiftSourceDeps);
    unsigned bridgingHeaderFileId =
        swiftSourceDeps->textualModuleDetails.bridgingHeaderFile
            ? getIdentifier(swiftSourceDeps->textualModuleDetails
                                .bridgingHeaderFile.value())
            : 0;
    SwiftSourceModuleDetailsLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[SwiftSourceModuleDetailsLayout::Code],
        getArrayID(moduleID, ModuleIdentifierArrayKind::ExtraPCMArgs),
        bridgingHeaderFileId,
        getArrayID(moduleID, ModuleIdentifierArrayKind::SourceFiles),
        getArrayID(moduleID, ModuleIdentifierArrayKind::BridgingSourceFiles),
        getArrayID(moduleID,
                   ModuleIdentifierArrayKind::BridgingModuleDependencies),
        getArrayID(moduleID,
                   ModuleIdentifierArrayKind::SwiftOverlayDependencyIDs),
        getIdentifier(
            swiftSourceDeps->textualModuleDetails.CASFileSystemRootID),
        getIdentifier(swiftSourceDeps->textualModuleDetails
                          .CASBridgingHeaderIncludeTreeRootID),
        getArrayID(moduleID, ModuleIdentifierArrayKind::BuildCommandLine),
        getArrayID(moduleID,
                   ModuleIdentifierArrayKind::BridgingHeaderBuildCommandLine));
    break;
  }
  case swift::ModuleDependencyKind::SwiftBinary: {
    auto swiftBinDeps = dependencyInfo.getAsSwiftBinaryModule();
    assert(swiftBinDeps);
    SwiftBinaryModuleDetailsLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[SwiftBinaryModuleDetailsLayout::Code],
        getIdentifier(swiftBinDeps->compiledModulePath),
        getIdentifier(swiftBinDeps->moduleDocPath),
        getIdentifier(swiftBinDeps->sourceInfoPath),
        getArrayID(moduleID, ModuleIdentifierArrayKind::SwiftOverlayDependencyIDs),
        getArrayID(moduleID, ModuleIdentifierArrayKind::DependencyHeaders),
        getArrayID(moduleID, ModuleIdentifierArrayKind::HeaderInputModuleDependencies),
        getArrayID(moduleID, ModuleIdentifierArrayKind::HeaderInputDependencySourceFiles),
        swiftBinDeps->isFramework, swiftBinDeps->isStatic,
        getIdentifier(swiftBinDeps->moduleCacheKey),
        getIdentifier(swiftBinDeps->userModuleVersion));

    break;
  }
  case swift::ModuleDependencyKind::SwiftPlaceholder: {
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
  case swift::ModuleDependencyKind::Clang: {
    auto clangDeps = dependencyInfo.getAsClangModule();
    assert(clangDeps);
    ClangModuleDetailsLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[ClangModuleDetailsLayout::Code],
        getIdentifier(clangDeps->pcmOutputPath),
        getIdentifier(clangDeps->mappedPCMPath),
        getIdentifier(clangDeps->moduleMapFile),
        getIdentifier(clangDeps->contextHash),
        getArrayID(moduleID, ModuleIdentifierArrayKind::NonPathCommandLine),
        getArrayID(moduleID, ModuleIdentifierArrayKind::FileDependencies),
        getArrayID(moduleID, ModuleIdentifierArrayKind::CapturedPCMArgs),
        getIdentifier(clangDeps->CASFileSystemRootID),
        getIdentifier(clangDeps->CASClangIncludeTreeRootID),
        getIdentifier(clangDeps->moduleCacheKey), clangDeps->IsSystem);

    break;
  }
  default:
    llvm_unreachable("Unhandled dependency kind.");
  }
}

unsigned ModuleDependenciesCacheSerializer::addIdentifier(const std::string &str) {
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

unsigned ModuleDependenciesCacheSerializer::getIdentifier(const std::string &str) const {
  if (str.empty())
    return 0;

  auto iter = IdentifierIDs.find(str);
  assert(iter != IdentifierIDs.end());
  assert(iter->second != 0);
  return iter->second;
}

void ModuleDependenciesCacheSerializer::addDependencyIDArray(ModuleDependencyID moduleID,
                                                             ModuleIdentifierArrayKind arrayKind,
                                                             const std::vector<ModuleDependencyID> &vec) {
  std::vector<std::string> encodedDependencyIDs;
  for (const auto &moduleID : vec)
    encodedDependencyIDs.push_back(createEncodedModuleKindAndName(moduleID));
  addStringArray(moduleID, arrayKind, encodedDependencyIDs);
  return;
}

void ModuleDependenciesCacheSerializer::addStringArray(ModuleDependencyID moduleID,
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

unsigned ModuleDependenciesCacheSerializer::getArrayID(ModuleDependencyID moduleID,
                                                       ModuleIdentifierArrayKind arrayKind) const {
  auto iter = ArrayIDs.find(moduleID);
  assert(iter != ArrayIDs.end());
  auto &innerMap = iter->second;
  auto arrayIter = innerMap.find(arrayKind);
  assert(arrayIter != innerMap.end());
  return arrayIter->second;
}

void ModuleDependenciesCacheSerializer::collectStringsAndArrays(
    const ModuleDependenciesCache &cache) {
  addIdentifier(cache.scannerContextHash);
  // TODO: Serialize *all* modules
  for (auto &moduleID : cache.getAllDependencies({cache.mainScanModuleName,
                                                  ModuleDependencyKind::SwiftSource})) {
    auto optionalDependencyInfo =
        cache.findDependency(moduleID.ModuleName, moduleID.Kind);
    assert(optionalDependencyInfo.has_value() && "Expected dependency info.");
    auto dependencyInfo = optionalDependencyInfo.value();
    // Add the module's name
    addIdentifier(moduleID.ModuleName);

    // Map import infos to their respective module identifiers
    auto importInfoArrayToIdentifier =
      [](const auto &importInfo) -> std::string {
        return importInfo.importIdentifier;
      };

    // Add the module's dependencies
    std::vector<std::string> importIdentifiers;
    llvm::transform(dependencyInfo->getModuleImports(),
                    std::back_inserter(importIdentifiers),
                    importInfoArrayToIdentifier);
    std::vector<std::string> optionalImportIdentifiers;
    llvm::transform(dependencyInfo->getOptionalModuleImports(),
                    std::back_inserter(optionalImportIdentifiers),
                    importInfoArrayToIdentifier);

    addStringArray(moduleID, ModuleIdentifierArrayKind::DependencyImports,
                   importIdentifiers);
    addStringArray(moduleID, ModuleIdentifierArrayKind::OptionalDependencyImports,
                   optionalImportIdentifiers);
    
    ModuleDependencyIDSetVector allDependencies;
    if (dependencyInfo->isSwiftModule()) {
      auto swiftImportedDepsRef = dependencyInfo->getImportedSwiftDependencies();
      auto headerClangDepsRef = dependencyInfo->getHeaderClangDependencies();
      auto overlayDependenciesRef = dependencyInfo->getSwiftOverlayDependencies();
      allDependencies.insert(swiftImportedDepsRef.begin(),
                    swiftImportedDepsRef.end());
      allDependencies.insert(headerClangDepsRef.begin(),
                    headerClangDepsRef.end());
      allDependencies.insert(overlayDependenciesRef.begin(),
                    overlayDependenciesRef.end());
    }

    if (dependencyInfo->isSwiftSourceModule()) {
      auto crossImportOverlayDepsRef = dependencyInfo->getCrossImportOverlayDependencies();
      allDependencies.insert(crossImportOverlayDepsRef.begin(),
                    crossImportOverlayDepsRef.end());
    }

    auto clangImportedDepsRef = dependencyInfo->getImportedClangDependencies();
    allDependencies.insert(clangImportedDepsRef.begin(),
                  clangImportedDepsRef.end());

    addDependencyIDArray(
        moduleID, ModuleIdentifierArrayKind::QualifiedModuleDependencyIDs,
        allDependencies.getArrayRef());

    std::vector<std::string> clangHeaderDependencyNames;
    for (const auto &headerDepID :
        dependencyInfo->getHeaderClangDependencies())
      clangHeaderDependencyNames.push_back(headerDepID.ModuleName);

    // Add the dependency-kind-specific data
    switch (dependencyInfo->getKind()) {
    case swift::ModuleDependencyKind::SwiftInterface: {
      auto swiftTextDeps = dependencyInfo->getAsSwiftInterfaceModule();
      assert(swiftTextDeps);
      addIdentifier(swiftTextDeps->moduleOutputPath);
      addIdentifier(swiftTextDeps->swiftInterfaceFile);
      addStringArray(moduleID,
               ModuleIdentifierArrayKind::CompiledModuleCandidates,
               swiftTextDeps->compiledModuleCandidates);
      addStringArray(moduleID, ModuleIdentifierArrayKind::BuildCommandLine,
               swiftTextDeps->textualModuleDetails.buildCommandLine);
      addStringArray(moduleID, ModuleIdentifierArrayKind::ExtraPCMArgs,
               swiftTextDeps->textualModuleDetails.extraPCMArgs);
      addIdentifier(swiftTextDeps->contextHash);
      if (swiftTextDeps->textualModuleDetails.bridgingHeaderFile.has_value())
        addIdentifier(swiftTextDeps->textualModuleDetails.bridgingHeaderFile
                      .value());
      addStringArray(moduleID, ModuleIdentifierArrayKind::SourceFiles,
               std::vector<std::string>());
      addStringArray(moduleID, ModuleIdentifierArrayKind::BridgingSourceFiles,
               swiftTextDeps->textualModuleDetails.bridgingSourceFiles);
      addStringArray(
               moduleID, ModuleIdentifierArrayKind::BridgingModuleDependencies,
               clangHeaderDependencyNames);
      addDependencyIDArray(
          moduleID, ModuleIdentifierArrayKind::SwiftOverlayDependencyIDs,
          swiftTextDeps->swiftOverlayDependencies);
      addIdentifier(swiftTextDeps->textualModuleDetails.CASFileSystemRootID);
      addIdentifier(swiftTextDeps->textualModuleDetails
                        .CASBridgingHeaderIncludeTreeRootID);
      addIdentifier(swiftTextDeps->moduleCacheKey);
      break;
    }
    case swift::ModuleDependencyKind::SwiftBinary: {
      auto swiftBinDeps = dependencyInfo->getAsSwiftBinaryModule();
      assert(swiftBinDeps);
      addIdentifier(swiftBinDeps->compiledModulePath);
      addIdentifier(swiftBinDeps->moduleDocPath);
      addIdentifier(swiftBinDeps->sourceInfoPath);
      addIdentifier(swiftBinDeps->moduleCacheKey);
      addIdentifier(swiftBinDeps->headerImport);
      addStringArray(moduleID, ModuleIdentifierArrayKind::HeaderInputModuleDependencies,
                     clangHeaderDependencyNames);
      addStringArray(moduleID, ModuleIdentifierArrayKind::HeaderInputDependencySourceFiles,
                     swiftBinDeps->headerSourceFiles);
      addDependencyIDArray(
          moduleID, ModuleIdentifierArrayKind::SwiftOverlayDependencyIDs,
          swiftBinDeps->swiftOverlayDependencies);
      break;
    }
    case swift::ModuleDependencyKind::SwiftPlaceholder: {
      auto swiftPHDeps = dependencyInfo->getAsPlaceholderDependencyModule();
      assert(swiftPHDeps);
      addIdentifier(swiftPHDeps->compiledModulePath);
      addIdentifier(swiftPHDeps->moduleDocPath);
      addIdentifier(swiftPHDeps->sourceInfoPath);
      break;
    }
    case swift::ModuleDependencyKind::SwiftSource: {
      auto swiftSourceDeps = dependencyInfo->getAsSwiftSourceModule();
      assert(swiftSourceDeps);
      addStringArray(moduleID, ModuleIdentifierArrayKind::ExtraPCMArgs,
                     swiftSourceDeps->textualModuleDetails.extraPCMArgs);
      if (swiftSourceDeps->textualModuleDetails.bridgingHeaderFile
              .has_value())
        addIdentifier(
            swiftSourceDeps->textualModuleDetails.bridgingHeaderFile.value());
      addStringArray(moduleID, ModuleIdentifierArrayKind::SourceFiles,
                     swiftSourceDeps->sourceFiles);
      addStringArray(
          moduleID, ModuleIdentifierArrayKind::BridgingSourceFiles,
          swiftSourceDeps->textualModuleDetails.bridgingSourceFiles);
      addStringArray(
          moduleID, ModuleIdentifierArrayKind::BridgingModuleDependencies,
          clangHeaderDependencyNames);
      addDependencyIDArray(
          moduleID, ModuleIdentifierArrayKind::SwiftOverlayDependencyIDs,
          swiftSourceDeps->swiftOverlayDependencies);
      addStringArray(
          moduleID, ModuleIdentifierArrayKind::BuildCommandLine,
          swiftSourceDeps->textualModuleDetails.buildCommandLine);
      addStringArray(
          moduleID, ModuleIdentifierArrayKind::BridgingHeaderBuildCommandLine,
          swiftSourceDeps->bridgingHeaderBuildCommandLine);
      addIdentifier(
          swiftSourceDeps->textualModuleDetails.CASFileSystemRootID);
      break;
    }
    case swift::ModuleDependencyKind::Clang: {
      auto clangDeps = dependencyInfo->getAsClangModule();
      assert(clangDeps);
      addIdentifier(clangDeps->pcmOutputPath);
      addIdentifier(clangDeps->mappedPCMPath);
      addIdentifier(clangDeps->moduleMapFile);
      addIdentifier(clangDeps->contextHash);
      addStringArray(moduleID, ModuleIdentifierArrayKind::NonPathCommandLine,
                     clangDeps->buildCommandLine);
      addStringArray(moduleID, ModuleIdentifierArrayKind::FileDependencies,
                     clangDeps->fileDependencies);
      addStringArray(moduleID, ModuleIdentifierArrayKind::CapturedPCMArgs,
                     clangDeps->capturedPCMArgs);
      addIdentifier(clangDeps->CASFileSystemRootID);
      addIdentifier(clangDeps->CASClangIncludeTreeRootID);
      addIdentifier(clangDeps->moduleCacheKey);
      break;
    }
    default:
      llvm_unreachable("Unhandled dependency kind.");
    }
  }
}

void ModuleDependenciesCacheSerializer::writeInterModuleDependenciesCache(
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
  registerRecordAbbr<SwiftSourceModuleDetailsLayout>();
  registerRecordAbbr<SwiftInterfaceModuleDetailsLayout>();
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
  // TODO: Serialize *all* modules
  for (auto &moduleID : cache.getAllDependencies({cache.mainScanModuleName,
                                                  ModuleDependencyKind::SwiftSource})) {
    auto dependencyInfo = cache.findDependency(moduleID.ModuleName, moduleID.Kind);
    assert(dependencyInfo.has_value() && "Expected dependency info.");
    writeModuleInfo(moduleID, **dependencyInfo);
  }
  return;
}

void swift::dependencies::module_dependency_cache_serialization::
    writeInterModuleDependenciesCache(
        llvm::BitstreamWriter &Out,
        const ModuleDependenciesCache &cache) {
  ModuleDependenciesCacheSerializer serializer{Out};
  serializer.writeInterModuleDependenciesCache(cache);
}

bool swift::dependencies::module_dependency_cache_serialization::
    writeInterModuleDependenciesCache(
        DiagnosticEngine &diags, llvm::vfs::OutputBackend &backend,
        StringRef path, const ModuleDependenciesCache &cache) {
  PrettyStackTraceStringAction stackTrace(
      "saving inter-module dependency graph", path);
  return withOutputPath(diags, backend, path, [&](llvm::raw_ostream &out) {
    SmallVector<char, 0> Buffer;
    llvm::BitstreamWriter Writer{Buffer};
    writeInterModuleDependenciesCache(Writer, cache);
    out.write(Buffer.data(), Buffer.size());
    out.flush();
    return false;
  });
}
