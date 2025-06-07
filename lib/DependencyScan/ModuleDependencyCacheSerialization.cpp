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

namespace {
ModuleDependencyKind &operator++(ModuleDependencyKind &e) {
  if (e == ModuleDependencyKind::LastKind) {
    llvm_unreachable(
        "Attempting to increment last enum value on ModuleDependencyKind");
  }
  e = ModuleDependencyKind(
      static_cast<std::underlying_type<ModuleDependencyKind>::type>(e) + 1);
  return e;
}
} // namespace

// MARK: Deserialization
namespace swift {

class ModuleDependenciesCacheDeserializer {
  std::vector<std::string> Identifiers;
  std::vector<std::vector<uint64_t>> ArraysOfIdentifierIDs;
  std::vector<LinkLibrary> LinkLibraries;
  std::vector<std::vector<uint64_t>> ArraysOfLinkLibraryIDs;
  std::vector<std::pair<std::string, MacroPluginDependency>> MacroDependencies;
  std::vector<std::vector<uint64_t>> ArraysOfMacroDependenciesIDs;
  std::vector<ScannerImportStatementInfo> ImportStatements;
  std::vector<std::vector<uint64_t>> ArraysOfImportStatementIDs;
  std::vector<std::vector<uint64_t>> ArraysOfOptionalImportStatementIDs;

  llvm::BitstreamCursor Cursor;
  SmallVector<uint64_t, 64> Scratch;
  StringRef BlobData;

  // These return true if there was an error.
  bool readSignature();
  bool enterGraphBlock();
  bool readMetadata(StringRef scannerContextHash);
  bool readSerializationTime(llvm::sys::TimePoint<> &SerializationTimeStamp);
  bool readGraph(ModuleDependenciesCache &cache);

  std::optional<std::string> getIdentifier(unsigned n);
  std::optional<std::vector<std::string>> getStringArray(unsigned n);
  std::optional<std::vector<LinkLibrary>> getLinkLibraryArray(unsigned n);
  std::optional<std::vector<std::pair<std::string, MacroPluginDependency>>>
  getMacroDependenciesArray(unsigned n);
  std::optional<std::vector<ScannerImportStatementInfo>>
  getImportStatementInfoArray(unsigned n);
  std::optional<std::vector<ScannerImportStatementInfo>>
  getOptionalImportStatementInfoArray(unsigned n);

  std::optional<std::vector<ModuleDependencyID>>
  getModuleDependencyIDArray(unsigned n);

public:
  ModuleDependenciesCacheDeserializer(llvm::MemoryBufferRef Data)
      : Cursor(Data) {}
  bool readInterModuleDependenciesCache(ModuleDependenciesCache &cache,
                                        llvm::sys::TimePoint<> &serializedCacheTimeStamp);
};

} // namespace swift

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
bool ModuleDependenciesCacheDeserializer::readMetadata(StringRef scannerContextHash) {
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
      minorVersion != MODULE_DEPENDENCY_CACHE_FORMAT_VERSION_MINOR)
    return true;

  std::string readScannerContextHash = BlobData.str();
  if (readScannerContextHash != scannerContextHash)
    return true;

  return false;
}

bool ModuleDependenciesCacheDeserializer::readSerializationTime(llvm::sys::TimePoint<> &SerializationTimeStamp) {
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

  if (*recordID != TIME_NODE)
    return true;
  
  TimeLayout::readRecord(Scratch);
  std::string serializedTimeStamp = BlobData.str();
  
  SerializationTimeStamp =
    llvm::sys::TimePoint<>(llvm::sys::TimePoint<>::duration(std::stoll(serializedTimeStamp)));
  return SerializationTimeStamp == llvm::sys::TimePoint<>();
}

/// Read in the top-level block's graph structure by first reading in
/// all of the file's identifiers and arrays of identifiers, followed by
/// consuming individual module info records and registering them into the
/// cache.
bool ModuleDependenciesCacheDeserializer::readGraph(
    ModuleDependenciesCache &cache) {
  using namespace graph_block;

  bool hasCurrentModule = false;
  std::string currentModuleName;
  std::vector<ModuleDependencyID> importedSwiftDependenciesIDs;
  std::vector<ModuleDependencyID> importedClangDependenciesIDs;
  std::vector<ModuleDependencyID> crossImportOverlayDependenciesIDs;
  std::vector<ModuleDependencyID> swiftOverlayDependenciesIDs;

  std::vector<LinkLibrary> linkLibraries;
  std::vector<std::pair<std::string, MacroPluginDependency>> macroDependencies;

  std::vector<ScannerImportStatementInfo> importStatements;
  std::vector<ScannerImportStatementInfo> optionalImportStatements;

  auto addCommonDependencyInfo =
      [&importedClangDependenciesIDs, &macroDependencies]
      (ModuleDependencyInfo &moduleDep) {
        // Add qualified dependencies of this module
        moduleDep.setImportedClangDependencies(importedClangDependenciesIDs);

        // Add macro dependencies
        for (const auto &md : macroDependencies)
          moduleDep.addMacroDependency(md.first, md.second.LibraryPath,
                                       md.second.ExecutablePath);

        moduleDep.setIsFinalized(true);
      };

  auto addSwiftCommonDependencyInfo =
      [&importedSwiftDependenciesIDs, &crossImportOverlayDependenciesIDs,
       &swiftOverlayDependenciesIDs](ModuleDependencyInfo &moduleDep) {
        moduleDep.setImportedSwiftDependencies(importedSwiftDependenciesIDs);
        moduleDep.setCrossImportOverlayDependencies(
            crossImportOverlayDependenciesIDs);
        moduleDep.setSwiftOverlayDependencies(swiftOverlayDependenciesIDs);
      };

  auto addSwiftTextualDependencyInfo =
      [this](ModuleDependencyInfo &moduleDep, unsigned bridgingHeaderFileID,
             unsigned bridgingSourceFilesArrayID,
             unsigned bridgingModuleDependenciesArrayID,
             unsigned bridgingHeaderIncludeTreeID) {
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
        moduleDep.setHeaderSourceFiles(*bridgingSourceFiles);

        // Add bridging module dependencies
        auto bridgingModuleDeps =
            getStringArray(bridgingModuleDependenciesArrayID);
        if (!bridgingModuleDeps)
          llvm::report_fatal_error("Bad bridging module dependencies");
        llvm::StringSet<> alreadyAdded;
        std::vector<ModuleDependencyID> bridgingModuleDepIDs;
        for (const auto &mod : *bridgingModuleDeps)
          bridgingModuleDepIDs.push_back(
              ModuleDependencyID{mod, ModuleDependencyKind::Clang});
        moduleDep.setHeaderClangDependencies(bridgingModuleDepIDs);

        // Add bridging header include tree
        auto bridgingHeaderIncludeTree =
            getIdentifier(bridgingHeaderIncludeTreeID);
        if (!bridgingHeaderIncludeTree)
          llvm::report_fatal_error("Bad bridging header include tree");
        if (!bridgingHeaderIncludeTree->empty())
          moduleDep.addBridgingHeaderIncludeTree(*bridgingHeaderIncludeTree);
      };

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

    case LINK_LIBRARY_NODE: {
      unsigned libraryIdentifierID;
      bool isFramework, isStatic, shouldForceLoad;
      LinkLibraryLayout::readRecord(Scratch, libraryIdentifierID, isFramework,
                                    isStatic, shouldForceLoad);
      auto libraryIdentifier = getIdentifier(libraryIdentifierID);
      if (!libraryIdentifier)
        llvm::report_fatal_error("Bad link library identifier");

      LinkLibraries.emplace_back(
          *libraryIdentifier,
                      isFramework ? LibraryKind::Framework
                                  : LibraryKind::Library,
                      isStatic, shouldForceLoad);
      break;
    }

    case LINK_LIBRARY_ARRAY_NODE: {
      ArrayRef<uint64_t> identifierIDs;
      LinkLibraryArrayLayout::readRecord(Scratch, identifierIDs);
      ArraysOfLinkLibraryIDs.push_back(identifierIDs.vec());
      break;
    }

    case MACRO_DEPENDENCY_NODE: {
      unsigned macroModuleNameID, libraryPathID, executablePathID;
      MacroDependencyLayout::readRecord(Scratch, macroModuleNameID,
                                        libraryPathID, executablePathID);
      auto macroModuleName = getIdentifier(macroModuleNameID);
      if (!macroModuleName)
        llvm::report_fatal_error("Bad macro dependency: no module name");

      auto libraryPath = getIdentifier(libraryPathID);
      if (!libraryPath)
        llvm::report_fatal_error("Bad macro dependency: no library path");

      auto executablePath = getIdentifier(executablePathID);
      if (!executablePath)
        llvm::report_fatal_error("Bad macro dependency: no executable path");

      MacroDependencies.push_back(
          {*macroModuleName,
           MacroPluginDependency{*libraryPath, *executablePath}});
      break;
    }

    case MACRO_DEPENDENCY_ARRAY_NODE: {
      ArrayRef<uint64_t> identifierIDs;
      MacroDependencyArrayLayout::readRecord(Scratch, identifierIDs);
      ArraysOfMacroDependenciesIDs.push_back(identifierIDs.vec());
      break;
    }

    case IMPORT_STATEMENT_NODE: {
      unsigned importIdentifierID, bufferIdentifierID;
      unsigned lineNumber, columnNumber;
      bool isOptional, isExported;
      ImportStatementLayout::readRecord(Scratch, importIdentifierID,
                                        bufferIdentifierID, lineNumber,
                                        columnNumber, isOptional, isExported);
      auto importIdentifier = getIdentifier(importIdentifierID);
      if (!importIdentifier)
        llvm::report_fatal_error("Bad import statement info: no import name");

      auto bufferIdentifier = getIdentifier(bufferIdentifierID);
      if (!bufferIdentifier)
        llvm::report_fatal_error(
            "Bad import statement info: no buffer identifier");
      if (bufferIdentifier->empty())
        ImportStatements.push_back(ScannerImportStatementInfo(
            *importIdentifier, isExported));
      else
        ImportStatements.push_back(ScannerImportStatementInfo(
            *importIdentifier, isExported,
            ScannerImportStatementInfo::ImportDiagnosticLocationInfo(
                *bufferIdentifier, lineNumber, columnNumber)));
      break;
    }

    case IMPORT_STATEMENT_ARRAY_NODE: {
      ArrayRef<uint64_t> identifierIDs;
      ImportStatementArrayLayout::readRecord(Scratch, identifierIDs);
      ArraysOfImportStatementIDs.push_back(identifierIDs.vec());
      break;
    }

    case OPTIONAL_IMPORT_STATEMENT_ARRAY_NODE: {
      ArrayRef<uint64_t> identifierIDs;
      OptionalImportStatementArrayLayout::readRecord(Scratch, identifierIDs);
      ArraysOfOptionalImportStatementIDs.push_back(identifierIDs.vec());
      break;
    }

    case MODULE_NODE: {
      hasCurrentModule = true;
      unsigned moduleNameID, moduleImportsArrayID, optionalImportsArrayID,
          linkLibraryArrayID, macroDependencyArrayID,
          importedSwiftDependenciesIDsArrayID,
          importedClangDependenciesIDsArrayID,
          crossImportOverlayDependenciesIDsArrayID,
          swiftOverlayDependenciesIDsArrayID, moduleCacheKeyID;

      ModuleInfoLayout::readRecord(Scratch, moduleNameID, moduleImportsArrayID,
                                   optionalImportsArrayID, linkLibraryArrayID,
                                   macroDependencyArrayID,
                                   importedSwiftDependenciesIDsArrayID,
                                   importedClangDependenciesIDsArrayID,
                                   crossImportOverlayDependenciesIDsArrayID,
                                   swiftOverlayDependenciesIDsArrayID,
                                   moduleCacheKeyID);
      auto moduleName = getIdentifier(moduleNameID);
      if (!moduleName)
        llvm::report_fatal_error("Bad module name");
      currentModuleName = *moduleName;

      auto optionalImportStatementInfos =
          getImportStatementInfoArray(moduleImportsArrayID);
      if (!optionalImportStatementInfos)
        llvm::report_fatal_error("Bad direct Swift dependencies: no imports");
      importStatements = *optionalImportStatementInfos;

      auto optionalOptionalImportStatementInfos =
          getOptionalImportStatementInfoArray(optionalImportsArrayID);
      if (optionalOptionalImportStatementInfos)
        optionalImportStatements = *optionalOptionalImportStatementInfos;

      auto optionalImportedSwiftDependenciesIDs =
          getModuleDependencyIDArray(importedSwiftDependenciesIDsArrayID);
      if (!optionalImportedSwiftDependenciesIDs)
        llvm::report_fatal_error(
            "Bad direct Swift dependencies: no qualified dependencies");
      importedSwiftDependenciesIDs =
          *optionalImportedSwiftDependenciesIDs;

      auto optionalImportedClangDependenciesIDs =
          getModuleDependencyIDArray(importedClangDependenciesIDsArrayID);
      if (!optionalImportedClangDependenciesIDs)
        llvm::report_fatal_error(
            "Bad direct Clang dependencies: no qualified dependencies");
      importedClangDependenciesIDs =
          *optionalImportedClangDependenciesIDs;

      auto optionalCrossImportOverlayDependenciesIDs =
          getModuleDependencyIDArray(crossImportOverlayDependenciesIDsArrayID);
      if (!optionalCrossImportOverlayDependenciesIDs)
        llvm::report_fatal_error(
            "Bad Cross-Import Overlay dependencies: no qualified dependencies");
      crossImportOverlayDependenciesIDs =
          *optionalCrossImportOverlayDependenciesIDs;

      auto optionalSwiftOverlayDependenciesIDs =
          getModuleDependencyIDArray(swiftOverlayDependenciesIDsArrayID);
      if (!optionalSwiftOverlayDependenciesIDs)
        llvm::report_fatal_error(
            "Bad Swift Overlay dependencies: no qualified dependencies");
      swiftOverlayDependenciesIDs = *optionalSwiftOverlayDependenciesIDs;

      auto optionalLinkLibraries = getLinkLibraryArray(linkLibraryArrayID);
      if (!optionalLinkLibraries)
        llvm::report_fatal_error("Bad Link Libraries info");
      linkLibraries = *optionalLinkLibraries;

      auto optionalMacroDependencies =
          getMacroDependenciesArray(macroDependencyArrayID);
      if (!optionalMacroDependencies)
        llvm::report_fatal_error("Bad Macro Dependencies info");
      macroDependencies = *optionalMacroDependencies;
      break;
    }

    case SWIFT_INTERFACE_MODULE_DETAILS_NODE: {
      if (!hasCurrentModule)
        llvm::report_fatal_error(
            "Unexpected SWIFT_TEXTUAL_MODULE_DETAILS_NODE record");
      unsigned outputPathFileID, interfaceFileID,
          compiledModuleCandidatesArrayID, buildCommandLineArrayID,
          contextHashID, isFramework, isStatic,
          bridgingHeaderFileID, sourceFilesArrayID, bridgingSourceFilesArrayID,
          bridgingModuleDependenciesArrayID, CASFileSystemRootID,
          bridgingHeaderIncludeTreeID, moduleCacheKeyID, userModuleVersionID;
      SwiftInterfaceModuleDetailsLayout::readRecord(
          Scratch, outputPathFileID, interfaceFileID,
          compiledModuleCandidatesArrayID, buildCommandLineArrayID,
          contextHashID, isFramework, isStatic,
          bridgingHeaderFileID, sourceFilesArrayID, bridgingSourceFilesArrayID,
          bridgingModuleDependenciesArrayID, CASFileSystemRootID,
          bridgingHeaderIncludeTreeID, moduleCacheKeyID, userModuleVersionID);

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
      auto compiledModuleCandidates =
          getStringArray(compiledModuleCandidatesArrayID);
      if (!compiledModuleCandidates)
        llvm::report_fatal_error("Bad compiled module candidates");
      auto commandLine = getStringArray(buildCommandLineArrayID);
      if (!commandLine)
        llvm::report_fatal_error("Bad command line");
      auto contextHash = getIdentifier(contextHashID);
      if (!contextHash)
        llvm::report_fatal_error("Bad context hash");

      // forSwiftInterface API demands references here.
      std::vector<StringRef> buildCommandRefs;
      for (auto &arg : *commandLine)
        buildCommandRefs.push_back(arg);
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

      // Form the dependencies storage object
      auto moduleDep = ModuleDependencyInfo::forSwiftInterfaceModule(
          *optionalSwiftInterfaceFile, compiledCandidatesRefs,
          buildCommandRefs, importStatements, optionalImportStatements,
          linkLibraries, isFramework, isStatic, *rootFileSystemID,
          *moduleCacheKey, *userModuleVersion);

      moduleDep.setOutputPathAndHash(*outputModulePath, *contextHash);
      addCommonDependencyInfo(moduleDep);
      addSwiftCommonDependencyInfo(moduleDep);
      addSwiftTextualDependencyInfo(
          moduleDep, bridgingHeaderFileID, bridgingSourceFilesArrayID,
          bridgingModuleDependenciesArrayID, bridgingHeaderIncludeTreeID);

      cache.recordDependency(currentModuleName, std::move(moduleDep));
      hasCurrentModule = false;
      break;
    }

    case SWIFT_SOURCE_MODULE_DETAILS_NODE: {
      if (!hasCurrentModule)
        llvm::report_fatal_error(
            "Unexpected SWIFT_SOURCE_MODULE_DETAILS_NODE record");
      unsigned bridgingHeaderFileID, sourceFilesArrayID,
          bridgingSourceFilesArrayID, bridgingModuleDependenciesArrayID,
          CASFileSystemRootID, bridgingHeaderIncludeTreeID,
          buildCommandLineArrayID, bridgingHeaderBuildCommandLineArrayID,
          chainedBridgingHeaderPathID, chainedBridgingHeaderContentID;
      SwiftSourceModuleDetailsLayout::readRecord(
          Scratch, bridgingHeaderFileID,
          sourceFilesArrayID, bridgingSourceFilesArrayID,
          bridgingModuleDependenciesArrayID, CASFileSystemRootID,
          bridgingHeaderIncludeTreeID, buildCommandLineArrayID,
          bridgingHeaderBuildCommandLineArrayID, chainedBridgingHeaderPathID,
          chainedBridgingHeaderContentID);

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
          *rootFileSystemID, buildCommandRefs, importStatements,
          optionalImportStatements, bridgingHeaderBuildCommandRefs);

      // Add source files
      auto sourceFiles = getStringArray(sourceFilesArrayID);
      if (!sourceFiles)
        llvm::report_fatal_error("Bad bridging source files");
      for (const auto &file : *sourceFiles)
        moduleDep.addSourceFile(file);

      // Add chained bridging header.
      auto chainedBridgingHeaderPath =
          getIdentifier(chainedBridgingHeaderPathID);
      auto chainedBridgingHeaderContent =
          getIdentifier(chainedBridgingHeaderContentID);
      if (chainedBridgingHeaderPath && chainedBridgingHeaderContent)
        moduleDep.setChainedBridgingHeaderBuffer(*chainedBridgingHeaderPath,
                                                 *chainedBridgingHeaderContent);

      addCommonDependencyInfo(moduleDep);
      addSwiftCommonDependencyInfo(moduleDep);
      addSwiftTextualDependencyInfo(
          moduleDep, bridgingHeaderFileID, bridgingSourceFilesArrayID,
          bridgingModuleDependenciesArrayID, bridgingHeaderIncludeTreeID);

      cache.recordDependency(currentModuleName, std::move(moduleDep));
      hasCurrentModule = false;
      break;
    }

    case SWIFT_BINARY_MODULE_DETAILS_NODE: {
      if (!hasCurrentModule)
        llvm::report_fatal_error(
            "Unexpected SWIFT_BINARY_MODULE_DETAILS_NODE record");
      unsigned compiledModulePathID, moduleDocPathID, moduleSourceInfoPathID,
          headerImportID, definingInterfacePathID,
          headerModuleDependenciesArrayID, headerImportsSourceFilesArrayID,
          isFramework, isStatic, moduleCacheKeyID, userModuleVersionID;
      SwiftBinaryModuleDetailsLayout::readRecord(
          Scratch, compiledModulePathID, moduleDocPathID,
          moduleSourceInfoPathID, headerImportID, definingInterfacePathID,
          headerModuleDependenciesArrayID, headerImportsSourceFilesArrayID,
          isFramework, isStatic, moduleCacheKeyID, userModuleVersionID);

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
        llvm::report_fatal_error(
            "Bad binary direct dependencies: no header import");
      auto definingInterfacePath = getIdentifier(definingInterfacePathID);
      if (!definingInterfacePath)
        llvm::report_fatal_error(
            "Bad binary direct dependencies: no defining interface path");

      // Form the dependencies storage object
      auto moduleDep = ModuleDependencyInfo::forSwiftBinaryModule(
          *compiledModulePath, *moduleDocPath, *moduleSourceInfoPath,
          importStatements, optionalImportStatements, linkLibraries,
          *headerImport, *definingInterfacePath, isFramework, isStatic,
          *moduleCacheKey, *userModuleVersion);

      addCommonDependencyInfo(moduleDep);
      addSwiftCommonDependencyInfo(moduleDep);

      auto headerModuleDependencies =
          getStringArray(headerModuleDependenciesArrayID);
      if (!headerModuleDependencies)
        llvm::report_fatal_error("Bad binary direct dependencies: no header "
                                 "import module dependencies");
      llvm::StringSet<> alreadyAdded;

      std::vector<ModuleDependencyID> clangHeaderDependencyIDs;
      for (const auto &headerDepName : *headerModuleDependencies)
        clangHeaderDependencyIDs.push_back(
            ModuleDependencyID{headerDepName, ModuleDependencyKind::Clang});

      moduleDep.setHeaderClangDependencies(clangHeaderDependencyIDs);

      auto headerImportsSourceFiles =
          getStringArray(headerImportsSourceFilesArrayID);
      if (!headerImportsSourceFiles)
        llvm::report_fatal_error(
            "Bad binary direct dependencies: no header import source files");
      moduleDep.setHeaderSourceFiles(*headerImportsSourceFiles);

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

      cache.recordDependency(currentModuleName, std::move(moduleDep));
      hasCurrentModule = false;
      break;
    }

    case CLANG_MODULE_DETAILS_NODE: {
      if (!hasCurrentModule)
        llvm::report_fatal_error("Unexpected CLANG_MODULE_DETAILS_NODE record");
      unsigned pcmOutputPathID, mappedPCMPathID, moduleMapPathID, contextHashID,
          commandLineArrayID, fileDependenciesArrayID,
          CASFileSystemRootID, clangIncludeTreeRootID, moduleCacheKeyID,
          isSystem;
      ClangModuleDetailsLayout::readRecord(
          Scratch, pcmOutputPathID, mappedPCMPathID, moduleMapPathID,
          contextHashID, commandLineArrayID, fileDependenciesArrayID,
          CASFileSystemRootID, clangIncludeTreeRootID,
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
      auto rootFileSystemID = getIdentifier(CASFileSystemRootID);
      if (!rootFileSystemID)
        llvm::report_fatal_error("Bad CASFileSystem RootID");
      auto clangIncludeTreeRoot = getIdentifier(clangIncludeTreeRootID);
      if (!clangIncludeTreeRoot)
        llvm::report_fatal_error("Bad clang include tree ID");
      auto moduleCacheKey = getIdentifier(moduleCacheKeyID);
      if (!moduleCacheKey)
        llvm::report_fatal_error("Bad moduleCacheKey");

      // Form the dependencies storage object
      auto moduleDep = ModuleDependencyInfo::forClangModule(
          *pcmOutputPath, *mappedPCMPath, *moduleMapPath, *contextHash,
          *commandLineArgs, *fileDependencies, linkLibraries,
          *rootFileSystemID, *clangIncludeTreeRoot, *moduleCacheKey, isSystem);
      addCommonDependencyInfo(moduleDep);

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
    ModuleDependenciesCache &cache,
    llvm::sys::TimePoint<> &serializedCacheTimeStamp) {
  using namespace graph_block;

  if (readSignature())
    return true;

  if (enterGraphBlock())
    return true;

  if (readMetadata(cache.scannerContextHash))
    return true;
  
  if (readSerializationTime(serializedCacheTimeStamp))
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

std::optional<std::vector<LinkLibrary>>
ModuleDependenciesCacheDeserializer::getLinkLibraryArray(unsigned n) {
  if (n == 0)
    return std::vector<LinkLibrary>();

  --n;
  if (n >= ArraysOfLinkLibraryIDs.size())
    return std::nullopt;

  auto &llIDs = ArraysOfLinkLibraryIDs[n];

  auto IDtoLLMap = [this](unsigned index) { return LinkLibraries[index]; };
  std::vector<LinkLibrary> result;
  result.reserve(llIDs.size());
  std::transform(llIDs.begin(), llIDs.end(), std::back_inserter(result),
                 IDtoLLMap);
  return result;
}

std::optional<std::vector<std::pair<std::string, MacroPluginDependency>>>
ModuleDependenciesCacheDeserializer::getMacroDependenciesArray(unsigned n) {
  if (n == 0)
    return std::vector<std::pair<std::string, MacroPluginDependency>>();

  --n;
  if (n >= ArraysOfMacroDependenciesIDs.size())
    return std::nullopt;

  auto &llIDs = ArraysOfMacroDependenciesIDs[n];

  auto IDtoLLMap = [this](unsigned index) { return MacroDependencies[index]; };
  std::vector<std::pair<std::string, MacroPluginDependency>> result;
  result.reserve(llIDs.size());
  std::transform(llIDs.begin(), llIDs.end(), std::back_inserter(result),
                 IDtoLLMap);
  return result;
}

std::optional<std::vector<ScannerImportStatementInfo>>
ModuleDependenciesCacheDeserializer::getImportStatementInfoArray(unsigned n) {
  if (n == 0)
    return std::vector<ScannerImportStatementInfo>();

  --n;
  if (n >= ArraysOfImportStatementIDs.size())
    return std::nullopt;

  auto &ISIDs = ArraysOfImportStatementIDs[n];
  llvm::StringMap<ScannerImportStatementInfo> addedImports;
  for (const auto &importID : ISIDs) {
    const auto &entry = ImportStatements[importID];
    if (addedImports.contains(entry.importIdentifier)) {
      auto entryIt = addedImports.find(entry.importIdentifier);
      for (const auto &importLoc : entry.importLocations)
        entryIt->second.addImportLocation(importLoc);
    } else
      addedImports.insert({entry.importIdentifier, entry});
  }

  std::vector<ScannerImportStatementInfo> result;
  result.reserve(addedImports.size());
  for (const auto &keyValPair : addedImports) {
    result.push_back(keyValPair.second);
  }

  return result;
}

std::optional<std::vector<ScannerImportStatementInfo>>
ModuleDependenciesCacheDeserializer::getOptionalImportStatementInfoArray(
    unsigned n) {
  if (n == 0)
    return std::vector<ScannerImportStatementInfo>();

  --n;
  if (n >= ArraysOfOptionalImportStatementIDs.size())
    return std::nullopt;

  auto &ISIDs = ArraysOfOptionalImportStatementIDs[n];

  auto IDtoISMap = [this](unsigned index) { return ImportStatements[index]; };
  std::vector<ScannerImportStatementInfo> result;
  result.reserve(ISIDs.size());
  std::transform(ISIDs.begin(), ISIDs.end(), std::back_inserter(result),
                 IDtoISMap);

  return result;
}

std::optional<std::vector<ModuleDependencyID>>
ModuleDependenciesCacheDeserializer::getModuleDependencyIDArray(unsigned n) {
  auto encodedIdentifierStringArray = getStringArray(n);
  if (encodedIdentifierStringArray) {
    static const std::string textualPrefix("swiftTextual");
    static const std::string binaryPrefix("swiftBinary");
    static const std::string placeholderPrefix("swiftPlaceholder");
    static const std::string clangPrefix("clang");
    std::vector<ModuleDependencyID> result;
    for (const auto &encodedIdentifierString : *encodedIdentifierStringArray) {
      ModuleDependencyID id;
      if (!encodedIdentifierString.compare(0, textualPrefix.size(),
                                           textualPrefix)) {
        auto moduleName =
            encodedIdentifierString.substr(textualPrefix.size() + 1);
        id = {moduleName, ModuleDependencyKind::SwiftInterface};
      } else if (!encodedIdentifierString.compare(0, binaryPrefix.size(),
                                                  binaryPrefix)) {
        auto moduleName =
            encodedIdentifierString.substr(binaryPrefix.size() + 1);
        id = {moduleName, ModuleDependencyKind::SwiftBinary};
      } else if (!encodedIdentifierString.compare(0, placeholderPrefix.size(),
                                                  placeholderPrefix)) {
        auto moduleName =
            encodedIdentifierString.substr(placeholderPrefix.size() + 1);
        id = {moduleName, ModuleDependencyKind::SwiftPlaceholder};
      } else {
        auto moduleName =
            encodedIdentifierString.substr(clangPrefix.size() + 1);
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
                                     ModuleDependenciesCache &cache,
                                     llvm::sys::TimePoint<> &serializedCacheTimeStamp) {
  ModuleDependenciesCacheDeserializer deserializer(buffer.getMemBufferRef());
  return deserializer.readInterModuleDependenciesCache(cache, serializedCacheTimeStamp);
}

bool swift::dependencies::module_dependency_cache_serialization::
    readInterModuleDependenciesCache(StringRef path,
                                     ModuleDependenciesCache &cache,
                                     llvm::sys::TimePoint<> &serializedCacheTimeStamp) {
  PrettyStackTraceStringAction stackTrace(
      "loading inter-module dependency graph", path);
  auto buffer = llvm::MemoryBuffer::getFile(path);
  if (!buffer)
    return true;

  return readInterModuleDependenciesCache(*buffer.get(), cache, serializedCacheTimeStamp);
}

// MARK: Serialization

/// Kinds of arrays that we track being serialized. Used to query serialized
/// array ID for a given module.
enum ModuleIdentifierArrayKind : uint8_t {
  Empty = 0,
  DependencyImports,
  OptionalDependencyImports,
  ImportedSwiftDependenciesIDs,
  ImportedClangDependenciesIDs,
  CrossImportOverlayDependenciesIDs,
  SwiftOverlayDependenciesIDs,
  CompiledModuleCandidates,
  BuildCommandLine,
  SourceFiles,
  BridgingSourceFiles,
  BridgingModuleDependencies,
  HeaderInputDependencySourceFiles,
  HeaderInputModuleDependencies,
  BridgingHeaderBuildCommandLine,
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
      IdentifierArrayIDsMap;
  unsigned LastIdentifierID = 0;
  unsigned LastIdentifierArrayID = 0;
  std::vector<StringRef> Identifiers;
  std::vector<std::vector<unsigned>> ArraysOfIdentifiers;

  std::unordered_map<ModuleDependencyID, unsigned> LinkLibraryArrayIDsMap;
  std::unordered_map<ModuleDependencyID, unsigned> MacroDependenciesArrayIDsMap;
  std::unordered_map<ModuleDependencyID, unsigned> ImportInfosArrayIDsMap;
  std::unordered_map<ModuleDependencyID, unsigned>
      OptionalImportInfosArrayIDsMap;

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
                            const ArrayRef<ModuleDependencyID> vec);
  unsigned getIdentifierArrayID(ModuleDependencyID moduleID,
                                ModuleIdentifierArrayKind arrayKind) const;
  unsigned getLinkLibrariesArrayID(ModuleDependencyID moduleID) const;
  unsigned getMacroDependenciesArrayID(ModuleDependencyID moduleID) const;
  unsigned getImportStatementsArrayID(ModuleDependencyID moduleID) const;
  unsigned
  getOptionalImportStatementsArrayID(ModuleDependencyID moduleID) const;

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

  void writeMetadata(StringRef scanningContextHash);
  void writeSerializationTime(const llvm::sys::TimePoint<> &scanInitializationTime);
  void writeIdentifiers();
  void writeArraysOfIdentifiers();

  void writeLinkLibraries(const ModuleDependenciesCache &cache);
  unsigned writeLinkLibraryInfos(const ModuleDependencyInfo &dependencyInfo);
  void writeLinkLibraryInfoArray(unsigned startIndex, unsigned count);

  void writeMacroDependencies(const ModuleDependenciesCache &cache);
  unsigned writeMacroDependencies(const ModuleDependencyInfo &dependencyInfo);
  void writeMacroDependenciesArray(unsigned startIndex, unsigned count);

  void writeImportStatementInfos(const ModuleDependenciesCache &cache);
  unsigned writeImportStatementInfos(const ModuleDependencyInfo &dependencyInfo,
                                     bool optional);
  void writeImportStatementInfosArray(unsigned startIndex, unsigned count);

  void writeModuleInfo(ModuleDependencyID moduleID,
                       const ModuleDependencyInfo &dependencyInfo);

public:
  ModuleDependenciesCacheSerializer(llvm::BitstreamWriter &ExistingOut)
      : Out(ExistingOut) {}

public:
  void writeInterModuleDependenciesCache(const ModuleDependenciesCache &cache);
};

} // namespace swift

/// Record the name of a block.
void ModuleDependenciesCacheSerializer::emitBlockID(
    unsigned ID, StringRef name, SmallVectorImpl<unsigned char> &nameBuffer) {
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
void ModuleDependenciesCacheSerializer::emitRecordID(
    unsigned ID, StringRef name, SmallVectorImpl<unsigned char> &nameBuffer) {
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
  BLOCK_RECORD(graph_block, TIME_NODE);
  BLOCK_RECORD(graph_block, IDENTIFIER_NODE);
  BLOCK_RECORD(graph_block, IDENTIFIER_ARRAY_NODE);

  BLOCK_RECORD(graph_block, LINK_LIBRARY_NODE);
  BLOCK_RECORD(graph_block, LINK_LIBRARY_ARRAY_NODE);
  BLOCK_RECORD(graph_block, MACRO_DEPENDENCY_NODE);
  BLOCK_RECORD(graph_block, MACRO_DEPENDENCY_ARRAY_NODE);
  BLOCK_RECORD(graph_block, IMPORT_STATEMENT_NODE);
  BLOCK_RECORD(graph_block, IMPORT_STATEMENT_ARRAY_NODE);
  BLOCK_RECORD(graph_block, OPTIONAL_IMPORT_STATEMENT_ARRAY_NODE);
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

void ModuleDependenciesCacheSerializer::writeMetadata(StringRef scanningContextHash) {
  using namespace graph_block;
  MetadataLayout::emitRecord(Out, ScratchRecord,
                             AbbrCodes[MetadataLayout::Code],
                             MODULE_DEPENDENCY_CACHE_FORMAT_VERSION_MAJOR,
                             MODULE_DEPENDENCY_CACHE_FORMAT_VERSION_MINOR,
                             scanningContextHash);
}

void ModuleDependenciesCacheSerializer::writeSerializationTime(const llvm::sys::TimePoint<> &scanInitializationTime) {
  using namespace graph_block;
  auto timeSinceEpoch = scanInitializationTime.time_since_epoch().count();
  std::string serializationData = std::to_string(timeSinceEpoch);
  TimeLayout::emitRecord(Out, ScratchRecord,
                         AbbrCodes[TimeLayout::Code],
                         serializationData);
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

void ModuleDependenciesCacheSerializer::writeLinkLibraries(
    const ModuleDependenciesCache &cache) {
  unsigned lastLLIndex = 0;
  std::map<ModuleDependencyID, std::pair<unsigned, unsigned>> moduleLLArrayMap;
  for (auto kind = ModuleDependencyKind::FirstKind;
       kind != ModuleDependencyKind::LastKind; ++kind) {
    auto modMap = cache.getDependenciesMap(kind);
    for (const auto &entry : modMap) {
      ModuleDependencyID moduleID = {entry.getKey().str(), kind};
      auto optionalDependencyInfo = cache.findDependency(moduleID);
      assert(optionalDependencyInfo && "Expected dependency info.");
      auto dependencyInfo = *optionalDependencyInfo;
      unsigned numLLs = writeLinkLibraryInfos(*dependencyInfo);
      moduleLLArrayMap.insert({moduleID, std::make_pair(lastLLIndex, numLLs)});
      lastLLIndex += numLLs;
    }
  }

  unsigned lastLLArrayIndex = 1;
  for (auto kind = ModuleDependencyKind::FirstKind;
       kind != ModuleDependencyKind::LastKind; ++kind) {
    auto modMap = cache.getDependenciesMap(kind);
    for (const auto &entry : modMap) {
      ModuleDependencyID moduleID = {entry.getKey().str(), kind};
      auto entries = moduleLLArrayMap.at(moduleID);
      if (entries.second == 0)
        continue;
      writeLinkLibraryInfoArray(entries.first, entries.second);
      LinkLibraryArrayIDsMap.insert({moduleID, lastLLArrayIndex++});
    }
  }
}

unsigned ModuleDependenciesCacheSerializer::writeLinkLibraryInfos(
    const ModuleDependencyInfo &dependencyInfo) {
  using namespace graph_block;
  for (auto &linkLibrary : dependencyInfo.getLinkLibraries())
    LinkLibraryLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[LinkLibraryLayout::Code],
        getIdentifier(linkLibrary.getName().str()),
        linkLibrary.getKind() == LibraryKind::Framework,
        linkLibrary.isStaticLibrary(), linkLibrary.shouldForceLoad());
  return dependencyInfo.getLinkLibraries().size();
}

void ModuleDependenciesCacheSerializer::writeLinkLibraryInfoArray(
    unsigned startIndex, unsigned count) {
  using namespace graph_block;
  std::vector<unsigned> vec(count);
  std::iota(vec.begin(), vec.end(), startIndex);
  LinkLibraryArrayLayout::emitRecord(
      Out, ScratchRecord, AbbrCodes[LinkLibraryArrayLayout::Code], vec);
}

void ModuleDependenciesCacheSerializer::writeMacroDependencies(
    const ModuleDependenciesCache &cache) {
  unsigned lastMDIndex = 0;
  std::map<ModuleDependencyID, std::pair<unsigned, unsigned>>
      moduleMacroDepArrayMap;
  for (auto kind = ModuleDependencyKind::FirstKind;
       kind != ModuleDependencyKind::LastKind; ++kind) {
    auto modMap = cache.getDependenciesMap(kind);
    for (const auto &entry : modMap) {
      ModuleDependencyID moduleID = {entry.getKey().str(), kind};
      auto optionalDependencyInfo = cache.findDependency(moduleID);
      assert(optionalDependencyInfo && "Expected dependency info.");
      auto dependencyInfo = *optionalDependencyInfo;
      unsigned numMDs = writeMacroDependencies(*dependencyInfo);
      moduleMacroDepArrayMap.insert(
          {moduleID, std::make_pair(lastMDIndex, numMDs)});
      lastMDIndex += numMDs;
    }
  }

  unsigned lastMDArrayIndex = 1;
  for (auto kind = ModuleDependencyKind::FirstKind;
       kind != ModuleDependencyKind::LastKind; ++kind) {
    auto modMap = cache.getDependenciesMap(kind);
    for (const auto &entry : modMap) {
      ModuleDependencyID moduleID = {entry.getKey().str(), kind};
      auto entries = moduleMacroDepArrayMap.at(moduleID);
      if (entries.second == 0)
        continue;
      writeMacroDependenciesArray(entries.first, entries.second);
      MacroDependenciesArrayIDsMap.insert({moduleID, lastMDArrayIndex++});
    }
  }
}

unsigned ModuleDependenciesCacheSerializer::writeMacroDependencies(
    const ModuleDependencyInfo &dependencyInfo) {
  using namespace graph_block;
  for (auto &macroDependency : dependencyInfo.getMacroDependencies()) {
    MacroDependencyLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[MacroDependencyLayout::Code],
        getIdentifier(macroDependency.first),
        getIdentifier(macroDependency.second.LibraryPath),
        getIdentifier(macroDependency.second.ExecutablePath));
  }
  return dependencyInfo.getMacroDependencies().size();
}

void ModuleDependenciesCacheSerializer::writeMacroDependenciesArray(
    unsigned startIndex, unsigned count) {
  using namespace graph_block;
  std::vector<unsigned> vec(count);
  std::iota(vec.begin(), vec.end(), startIndex);
  MacroDependencyArrayLayout::emitRecord(
      Out, ScratchRecord, AbbrCodes[MacroDependencyArrayLayout::Code], vec);
}

void ModuleDependenciesCacheSerializer::writeImportStatementInfos(
    const ModuleDependenciesCache &cache) {
  unsigned lastImportInfoIndex = 0;
  std::map<ModuleDependencyID, std::pair<unsigned, unsigned>>
      importInfoArrayMap;
  std::map<ModuleDependencyID, std::pair<unsigned, unsigned>>
      optionalImportInfoArrayMap;
  for (auto kind = ModuleDependencyKind::FirstKind;
       kind != ModuleDependencyKind::LastKind; ++kind) {
    auto modMap = cache.getDependenciesMap(kind);
    for (const auto &entry : modMap.keys()) {
      ModuleDependencyID moduleID = {entry.str(), kind};
      auto optionalDependencyInfo = cache.findDependency(moduleID);
      assert(optionalDependencyInfo && "Expected dependency info.");
      auto dependencyInfo = *optionalDependencyInfo;

      auto numImportInfos =
          writeImportStatementInfos(*dependencyInfo, /* optional */ false);
      importInfoArrayMap.insert(
          {moduleID, std::make_pair(lastImportInfoIndex, numImportInfos)});
      lastImportInfoIndex += numImportInfos;

      auto numOptionalImportInfos =
          writeImportStatementInfos(*dependencyInfo, /* optional */ true);
      optionalImportInfoArrayMap.insert(
          {moduleID, std::make_pair(lastImportInfoIndex, numOptionalImportInfos)});
      lastImportInfoIndex += numOptionalImportInfos;
    }
  }

  unsigned lastImportInfoArrayIndex = 1;
  unsigned lastOptionalImportInfoArrayIndex = 1;
  for (auto kind = ModuleDependencyKind::FirstKind;
       kind != ModuleDependencyKind::LastKind; ++kind) {
    auto modMap = cache.getDependenciesMap(kind);
    for (const auto &entry : modMap.keys()) {
      ModuleDependencyID moduleID = {entry.str(), kind};
      auto entries = importInfoArrayMap.at(moduleID);
      if (entries.second != 0) {
        writeImportStatementInfosArray(entries.first, entries.second);
        ImportInfosArrayIDsMap.insert({moduleID, lastImportInfoArrayIndex++});
      }
      auto optionalEntries = optionalImportInfoArrayMap.at(moduleID);
      if (optionalEntries.second != 0) {
        writeImportStatementInfosArray(optionalEntries.first, optionalEntries.second);
        OptionalImportInfosArrayIDsMap.insert({moduleID, lastOptionalImportInfoArrayIndex++});
      }
    }
  }
}

unsigned ModuleDependenciesCacheSerializer::writeImportStatementInfos(
    const ModuleDependencyInfo &dependencyInfo, bool optional) {
  using namespace graph_block;
  size_t count = 0;
  auto emitImportStatementInfo = [this, &count](const auto &importInfo,
                                                bool isOptional) {
    if (importInfo.importLocations.empty()) {
      ImportStatementLayout::emitRecord(
          Out, ScratchRecord, AbbrCodes[ImportStatementLayout::Code],
          getIdentifier(importInfo.importIdentifier),
          0, 0, 0, isOptional, importInfo.isExported);
      count++;
    } else {
      for (auto &importLoc : importInfo.importLocations) {
        ImportStatementLayout::emitRecord(
            Out, ScratchRecord, AbbrCodes[ImportStatementLayout::Code],
            getIdentifier(importInfo.importIdentifier),
            getIdentifier(importLoc.bufferIdentifier), importLoc.lineNumber,
            importLoc.columnNumber, isOptional, importInfo.isExported);
        count++;
      }
    }
  };

  if (!optional) {
    for (auto &importInfo : dependencyInfo.getModuleImports())
      emitImportStatementInfo(importInfo, false);
  } else {
    for (auto &importInfo : dependencyInfo.getOptionalModuleImports())
      emitImportStatementInfo(importInfo, true);
  }
  return count;
}

void ModuleDependenciesCacheSerializer::writeImportStatementInfosArray(
    unsigned startIndex, unsigned count) {
  using namespace graph_block;
  std::vector<unsigned> vec(count);
  std::iota(vec.begin(), vec.end(), startIndex);
  ImportStatementArrayLayout::emitRecord(
      Out, ScratchRecord, AbbrCodes[ImportStatementArrayLayout::Code], vec);
}

void ModuleDependenciesCacheSerializer::writeModuleInfo(
    ModuleDependencyID moduleID, const ModuleDependencyInfo &dependencyInfo) {
  using namespace graph_block;

  ModuleInfoLayout::emitRecord(
      Out, ScratchRecord, AbbrCodes[ModuleInfoLayout::Code],
      getIdentifier(moduleID.ModuleName), getImportStatementsArrayID(moduleID),
      getOptionalImportStatementsArrayID(moduleID),
      getLinkLibrariesArrayID(moduleID), getMacroDependenciesArrayID(moduleID),
      getIdentifierArrayID(
          moduleID, ModuleIdentifierArrayKind::ImportedSwiftDependenciesIDs),
      getIdentifierArrayID(
          moduleID, ModuleIdentifierArrayKind::ImportedClangDependenciesIDs),
      getIdentifierArrayID(
          moduleID,
          ModuleIdentifierArrayKind::CrossImportOverlayDependenciesIDs),
      getIdentifierArrayID(
          moduleID, ModuleIdentifierArrayKind::SwiftOverlayDependenciesIDs),
      getIdentifier(dependencyInfo.getModuleCacheKey()));

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
            ? getIdentifier(*(swiftTextDeps->textualModuleDetails
                                .bridgingHeaderFile))
            : 0;
    SwiftInterfaceModuleDetailsLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[SwiftInterfaceModuleDetailsLayout::Code],
        outputModulePathFileId, swiftInterfaceFileId,
        getIdentifierArrayID(
            moduleID, ModuleIdentifierArrayKind::CompiledModuleCandidates),
        getIdentifierArrayID(moduleID,
                             ModuleIdentifierArrayKind::BuildCommandLine),
        getIdentifier(swiftTextDeps->contextHash), swiftTextDeps->isFramework,
        swiftTextDeps->isStatic, bridgingHeaderFileId,
        getIdentifierArrayID(moduleID, ModuleIdentifierArrayKind::SourceFiles),
        getIdentifierArrayID(moduleID,
                             ModuleIdentifierArrayKind::BridgingSourceFiles),
        getIdentifierArrayID(
            moduleID, ModuleIdentifierArrayKind::BridgingModuleDependencies),
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
            ? getIdentifier(*(swiftSourceDeps->textualModuleDetails
                                .bridgingHeaderFile))
            : 0;
    SwiftSourceModuleDetailsLayout::emitRecord(
        Out, ScratchRecord, AbbrCodes[SwiftSourceModuleDetailsLayout::Code],
        bridgingHeaderFileId,
        getIdentifierArrayID(moduleID, ModuleIdentifierArrayKind::SourceFiles),
        getIdentifierArrayID(moduleID,
                             ModuleIdentifierArrayKind::BridgingSourceFiles),
        getIdentifierArrayID(
            moduleID, ModuleIdentifierArrayKind::BridgingModuleDependencies),
        getIdentifier(
            swiftSourceDeps->textualModuleDetails.CASFileSystemRootID),
        getIdentifier(swiftSourceDeps->textualModuleDetails
                          .CASBridgingHeaderIncludeTreeRootID),
        getIdentifierArrayID(moduleID,
                             ModuleIdentifierArrayKind::BuildCommandLine),
        getIdentifierArrayID(
            moduleID,
            ModuleIdentifierArrayKind::BridgingHeaderBuildCommandLine),
        getIdentifier(swiftSourceDeps->chainedBridgingHeaderPath),
        getIdentifier(swiftSourceDeps->chainedBridgingHeaderContent));
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
        getIdentifier(swiftBinDeps->headerImport),
        getIdentifier(swiftBinDeps->definingModuleInterfacePath),
        getIdentifierArrayID(
            moduleID, ModuleIdentifierArrayKind::HeaderInputModuleDependencies),
        getIdentifierArrayID(
            moduleID,
            ModuleIdentifierArrayKind::HeaderInputDependencySourceFiles),
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
        getIdentifierArrayID(moduleID,
                             ModuleIdentifierArrayKind::NonPathCommandLine),
        getIdentifierArrayID(moduleID,
                             ModuleIdentifierArrayKind::FileDependencies),
        getIdentifier(clangDeps->CASFileSystemRootID),
        getIdentifier(clangDeps->CASClangIncludeTreeRootID),
        getIdentifier(clangDeps->moduleCacheKey), clangDeps->IsSystem);

    break;
  }
  default:
    llvm_unreachable("Unhandled dependency kind.");
  }
}

unsigned
ModuleDependenciesCacheSerializer::addIdentifier(const std::string &str) {
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

unsigned
ModuleDependenciesCacheSerializer::getIdentifier(const std::string &str) const {
  if (str.empty())
    return 0;

  auto iter = IdentifierIDs.find(str);
  assert(iter != IdentifierIDs.end());
  assert(iter->second != 0);
  return iter->second;
}

void ModuleDependenciesCacheSerializer::addDependencyIDArray(
    ModuleDependencyID moduleID, ModuleIdentifierArrayKind arrayKind,
    const ArrayRef<ModuleDependencyID> vec) {
  std::vector<std::string> encodedDependencyIDs;
  for (const auto &moduleID : vec)
    encodedDependencyIDs.push_back(createEncodedModuleKindAndName(moduleID));
  addStringArray(moduleID, arrayKind, encodedDependencyIDs);
  return;
}

void ModuleDependenciesCacheSerializer::addStringArray(
    ModuleDependencyID moduleID, ModuleIdentifierArrayKind arrayKind,
    const std::vector<std::string> &vec) {
  if (IdentifierArrayIDsMap.find(moduleID) != IdentifierArrayIDsMap.end()) {
    // Already have arrays for this module
    llvm::DenseMap<ModuleIdentifierArrayKind, unsigned>::iterator iter;
    bool isNew;
    std::tie(iter, isNew) = IdentifierArrayIDsMap[moduleID].insert(
        {arrayKind, LastIdentifierArrayID + 1});
    if (!isNew)
      return;
  } else {
    // Do not yet have any arrays for this module
    IdentifierArrayIDsMap[moduleID] =
        llvm::DenseMap<ModuleIdentifierArrayKind, unsigned>();
    IdentifierArrayIDsMap[moduleID].insert(
        {arrayKind, LastIdentifierArrayID + 1});
  }

  ++LastIdentifierArrayID;

  // Add in the individual identifiers in the array
  std::vector<unsigned> identifierIDs;
  identifierIDs.reserve(vec.size());
  for (const auto &id : vec) {
    identifierIDs.push_back(addIdentifier(id));
  }

  ArraysOfIdentifiers.push_back(identifierIDs);
  return;
}

unsigned ModuleDependenciesCacheSerializer::getIdentifierArrayID(
    ModuleDependencyID moduleID, ModuleIdentifierArrayKind arrayKind) const {
  auto iter = IdentifierArrayIDsMap.find(moduleID);
  assert(iter != IdentifierArrayIDsMap.end());
  auto &innerMap = iter->second;
  auto arrayIter = innerMap.find(arrayKind);
  assert(arrayIter != innerMap.end());
  return arrayIter->second;
}

unsigned ModuleDependenciesCacheSerializer::getLinkLibrariesArrayID(
    ModuleDependencyID moduleID) const {
  auto iter = LinkLibraryArrayIDsMap.find(moduleID);
  if (iter == LinkLibraryArrayIDsMap.end())
    return 0;

  return iter->second;
}

unsigned ModuleDependenciesCacheSerializer::getMacroDependenciesArrayID(
    ModuleDependencyID moduleID) const {
  auto iter = MacroDependenciesArrayIDsMap.find(moduleID);
  if (iter == MacroDependenciesArrayIDsMap.end())
    return 0;

  return iter->second;
}

unsigned ModuleDependenciesCacheSerializer::getImportStatementsArrayID(
    ModuleDependencyID moduleID) const {
  auto iter = ImportInfosArrayIDsMap.find(moduleID);
  if (iter == ImportInfosArrayIDsMap.end())
    return 0;

  return iter->second;
}

unsigned ModuleDependenciesCacheSerializer::getOptionalImportStatementsArrayID(
    ModuleDependencyID moduleID) const {
  auto iter = OptionalImportInfosArrayIDsMap.find(moduleID);
  if (iter == OptionalImportInfosArrayIDsMap.end())
    return 0;

  return iter->second;
}

void ModuleDependenciesCacheSerializer::collectStringsAndArrays(
    const ModuleDependenciesCache &cache) {
  addIdentifier(cache.scannerContextHash);
  for (auto kind = ModuleDependencyKind::FirstKind;
       kind != ModuleDependencyKind::LastKind; ++kind) {
    auto modMap = cache.getDependenciesMap(kind);
    for (const auto &entry : modMap.keys()) {
      ModuleDependencyID moduleID = {entry.str(), kind};
      auto optionalDependencyInfo = cache.findDependency(moduleID);
      assert(optionalDependencyInfo && "Expected dependency info.");
      auto dependencyInfo = *optionalDependencyInfo;
      // Add the module's name
      addIdentifier(moduleID.ModuleName);

      for (const auto &ll : dependencyInfo->getLinkLibraries())
        addIdentifier(ll.getName().str());

      for (const auto &md : dependencyInfo->getMacroDependencies()) {
        addIdentifier(md.first);
        addIdentifier(md.second.LibraryPath);
        addIdentifier(md.second.ExecutablePath);
      }
      
      for (const auto &ii : dependencyInfo->getModuleImports()) {
        addIdentifier(ii.importIdentifier);
        for (const auto &il : ii.importLocations)
          addIdentifier(il.bufferIdentifier);
      }

      for (const auto &oii : dependencyInfo->getOptionalModuleImports()) {
        addIdentifier(oii.importIdentifier);
        for (const auto &oil : oii.importLocations)
          addIdentifier(oil.bufferIdentifier);
      }

      addDependencyIDArray(
          moduleID, ModuleIdentifierArrayKind::ImportedSwiftDependenciesIDs,
          dependencyInfo->getImportedSwiftDependencies());
      addDependencyIDArray(
          moduleID, ModuleIdentifierArrayKind::ImportedClangDependenciesIDs,
          dependencyInfo->getImportedClangDependencies());
      addDependencyIDArray(
          moduleID,
          ModuleIdentifierArrayKind::CrossImportOverlayDependenciesIDs,
          dependencyInfo->getCrossImportOverlayDependencies());
      addDependencyIDArray(
          moduleID, ModuleIdentifierArrayKind::SwiftOverlayDependenciesIDs,
          dependencyInfo->getSwiftOverlayDependencies());

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
        addIdentifier(swiftTextDeps->contextHash);
        if (swiftTextDeps->textualModuleDetails.bridgingHeaderFile)
          addIdentifier(
              *(swiftTextDeps->textualModuleDetails.bridgingHeaderFile));
        addStringArray(moduleID, ModuleIdentifierArrayKind::SourceFiles,
                       std::vector<std::string>());
        addStringArray(moduleID, ModuleIdentifierArrayKind::BridgingSourceFiles,
                       swiftTextDeps->textualModuleDetails.bridgingSourceFiles);
        addStringArray(moduleID,
                       ModuleIdentifierArrayKind::BridgingModuleDependencies,
                       clangHeaderDependencyNames);
        addIdentifier(swiftTextDeps->textualModuleDetails.CASFileSystemRootID);
        addIdentifier(swiftTextDeps->textualModuleDetails
                          .CASBridgingHeaderIncludeTreeRootID);
        addIdentifier(swiftTextDeps->moduleCacheKey);
        addIdentifier(swiftTextDeps->userModuleVersion);
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
        addIdentifier(swiftBinDeps->definingModuleInterfacePath);
        addIdentifier(swiftBinDeps->userModuleVersion);
        addIdentifier(swiftBinDeps->moduleCacheKey);
        addStringArray(moduleID,
                       ModuleIdentifierArrayKind::HeaderInputModuleDependencies,
                       clangHeaderDependencyNames);
        addStringArray(
            moduleID,
            ModuleIdentifierArrayKind::HeaderInputDependencySourceFiles,
            swiftBinDeps->headerSourceFiles);
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
        if (swiftSourceDeps->textualModuleDetails.bridgingHeaderFile)
          addIdentifier(
            *(swiftSourceDeps->textualModuleDetails.bridgingHeaderFile));
        addStringArray(moduleID, ModuleIdentifierArrayKind::SourceFiles,
                       swiftSourceDeps->sourceFiles);
        addStringArray(
            moduleID, ModuleIdentifierArrayKind::BridgingSourceFiles,
            swiftSourceDeps->textualModuleDetails.bridgingSourceFiles);
        addStringArray(moduleID,
                       ModuleIdentifierArrayKind::BridgingModuleDependencies,
                       clangHeaderDependencyNames);
        addStringArray(moduleID, ModuleIdentifierArrayKind::BuildCommandLine,
                       swiftSourceDeps->textualModuleDetails.buildCommandLine);
        addStringArray(
            moduleID, ModuleIdentifierArrayKind::BridgingHeaderBuildCommandLine,
            swiftSourceDeps->bridgingHeaderBuildCommandLine);
        addIdentifier(
            swiftSourceDeps->textualModuleDetails.CASFileSystemRootID);
        addIdentifier(swiftSourceDeps->chainedBridgingHeaderPath);
        addIdentifier(swiftSourceDeps->chainedBridgingHeaderContent);
        addIdentifier(swiftSourceDeps->moduleCacheKey);
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
  registerRecordAbbr<TimeLayout>();
  registerRecordAbbr<IdentifierNodeLayout>();
  registerRecordAbbr<IdentifierArrayLayout>();
  registerRecordAbbr<LinkLibraryLayout>();
  registerRecordAbbr<LinkLibraryArrayLayout>();
  registerRecordAbbr<MacroDependencyLayout>();
  registerRecordAbbr<MacroDependencyArrayLayout>();
  registerRecordAbbr<ImportStatementLayout>();
  registerRecordAbbr<ImportStatementArrayLayout>();
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
  writeMetadata(cache.scannerContextHash);

  // The current time-stamp
  writeSerializationTime(cache.scanInitializationTime);

  // Write the strings
  writeIdentifiers();

  // Write the arrays
  writeArraysOfIdentifiers();

  // Write all the import statement info
  writeImportStatementInfos(cache);

  // Write all the arrays of link library infos for this graph
  writeLinkLibraries(cache);

  // Write all the arrays of macro dependency infos for this graph
  writeMacroDependencies(cache);

  // Write the core graph
  for (auto kind = ModuleDependencyKind::FirstKind;
       kind != ModuleDependencyKind::LastKind; ++kind) {
    auto modMap = cache.getDependenciesMap(kind);
    for (const auto &modInfo : modMap) {
      writeModuleInfo({modInfo.getKey().str(), kind}, modInfo.second);
    }
  }
  return;
}

void swift::dependencies::module_dependency_cache_serialization::
    writeInterModuleDependenciesCache(llvm::BitstreamWriter &Out,
                                      const ModuleDependenciesCache &cache) {
  ModuleDependenciesCacheSerializer serializer{Out};
  serializer.writeInterModuleDependenciesCache(cache);
}

bool swift::dependencies::module_dependency_cache_serialization::
    writeInterModuleDependenciesCache(DiagnosticEngine &diags,
                                      llvm::vfs::OutputBackend &backend,
                                      StringRef path,
                                      const ModuleDependenciesCache &cache) {
  PrettyStackTraceStringAction stackTrace(
      "saving inter-module dependency graph", path);
  return withOutputPath(diags, backend, path, [&](llvm::raw_ostream &out) {
    SmallVector<char, 0> buffer;
    llvm::BitstreamWriter writer{buffer};
    writeInterModuleDependenciesCache(writer, cache);
    out.write(buffer.data(), buffer.size());
    out.flush();
    return false;
  });
}
