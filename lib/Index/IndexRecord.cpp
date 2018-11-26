//===--- IndexRecord.cpp --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Index/IndexRecord.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Index/Index.h"
#include "clang/Basic/FileManager.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Index/IndexingAction.h"
#include "clang/Index/IndexRecordWriter.h"
#include "clang/Index/IndexUnitWriter.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/Support/Path.h"

using namespace swift;
using namespace swift::index;
using clang::index::IndexUnitWriter;
using clang::index::IndexRecordWriter;
using clang::index::SymbolRole;
using clang::index::SymbolRoleSet;

//===----------------------------------------------------------------------===//
// Index data collection and record writing
//===----------------------------------------------------------------------===//

namespace {
class SymbolTracker {
public:
  struct SymbolRelation {
    size_t symbolIndex;
    SymbolRoleSet roles;

    llvm::hash_code hash() const { return llvm::hash_combine(symbolIndex, roles); }
  };
  struct SymbolOccurrence {
    size_t symbolIndex;
    SymbolRoleSet roles;
    unsigned line;
    unsigned column;
    SmallVector<SymbolRelation, 3> related;

    llvm::hash_code hash() const {
      auto hash = llvm::hash_combine(symbolIndex, roles, line, column);
      for (auto &relation : related) {
        hash = llvm::hash_combine(hash, relation.hash());
      }
      return hash;
    }
  };
  struct Symbol {
    StringRef name;
    StringRef USR;
    StringRef group;

    SymbolInfo symInfo;
    unsigned isTestCandidate : 1;

    llvm::hash_code hash() const {
      return llvm::hash_combine(
          name, USR, group,
          static_cast<unsigned>(symInfo.Kind),
          static_cast<unsigned>(symInfo.SubKind),
          symInfo.Properties, isTestCandidate);
    }
  };

  Symbol *getSymbol(size_t index) {
    assert(index < symbols.size());
    return &symbols[index];
  }

  ArrayRef<SymbolOccurrence> getOccurrences() {
    if (!sorted) {
      std::stable_sort(occurrences.begin(), occurrences.end(),
          [](const SymbolOccurrence &a, const SymbolOccurrence& b) {
        if (a.line < b.line)
          return true;
        if (b.line < a.line)
          return false;
        return a.column < b.column;
      });
      sorted = true;
    }
    return occurrences;
  }

  size_t addSymbol(const IndexRelation &indexSym) {
    auto pair = USRToSymbol.insert(std::make_pair(indexSym.USR.data(),
                                                  symbols.size()));
    if (pair.second) {
      Symbol symbol{indexSym.name,
                    indexSym.USR,
                    indexSym.group,
                    indexSym.symInfo,
                    0};
      recordHash = llvm::hash_combine(recordHash, symbol.hash());
      symbols.push_back(std::move(symbol));
    }

    return pair.first->second;
  }

  void addOccurrence(const IndexSymbol &indexOccur) {
    sorted = false;

    SmallVector<SymbolRelation, 3> relations;
    for(IndexRelation indexRel: indexOccur.Relations) {
      relations.push_back({addSymbol(indexRel), indexRel.roles});
    }

    occurrences.push_back({/*symbolIndex=*/addSymbol(indexOccur),
                           indexOccur.roles,
                           indexOccur.line,
                           indexOccur.column,
                           std::move(relations)});

    recordHash = llvm::hash_combine(recordHash, occurrences.back().hash());
  }

  llvm::hash_code hashRecord() const { return recordHash; }

private:
  llvm::DenseMap<const char *, size_t> USRToSymbol;
  std::vector<Symbol> symbols;
  std::vector<SymbolOccurrence> occurrences;
  bool sorted = false;
  llvm::hash_code recordHash = 0;
};

class IndexRecordingConsumer : public IndexDataConsumer {
  SymbolTracker record;
  // Keep a USR map to uniquely identify Decls.
  // FIXME: if we just passed the original Decl * through we could use that,
  // which would also let us avoid producing the USR/Name/etc. for decls unless
  // we actually need it (once per Decl instead of once per occurrence).
  std::vector<IndexSymbol> symbolStack;

  std::function<void(SymbolTracker &)> onFinish;

public:
  IndexRecordingConsumer(std::function<void(SymbolTracker &)> onFinish)
      : onFinish(std::move(onFinish)) {}

  void failed(StringRef error) override {
    // FIXME: expose errors?
  }

  bool recordHash(StringRef hash, bool isKnown) override { return true; }
  bool startDependency(StringRef name, StringRef path, bool isClangModule,
                       bool isSystem, StringRef hash) override {
    return true;
  }
  bool finishDependency(bool isClangModule) override { return true; }
  Action startSourceEntity(const IndexSymbol &symbol) override {
    symbolStack.push_back(symbol);
    return Action::Continue;
  }
  bool finishSourceEntity(SymbolInfo sym, SymbolRoleSet roles) override {
    IndexSymbol symbol = std::move(symbolStack.back());
    symbolStack.pop_back();
    assert(!symbol.USR.empty());
    record.addOccurrence(symbol);
    return true;
  }

  void finish() override { onFinish(record); }
};

class StdlibGroupsIndexRecordingConsumer : public IndexDataConsumer {
  llvm::StringMap<std::unique_ptr<SymbolTracker>> TrackerByGroup;
  // Keep a USR map to uniquely identify Decls.
  // FIXME: if we just passed the original Decl * through we could use that,
  // which would also let us avoid producing the USR/Name/etc. for decls unless
  // we actually need it (once per Decl instead of once per occurrence).
  std::vector<IndexSymbol> symbolStack;

  std::function<bool(StringRef groupName, SymbolTracker &)> onFinish;

public:
  StdlibGroupsIndexRecordingConsumer(std::function<bool(StringRef groupName, SymbolTracker &)> onFinish)
      : onFinish(std::move(onFinish)) {}

  void failed(StringRef error) override {
    // FIXME: expose errors?
  }

  bool recordHash(StringRef hash, bool isKnown) override { return true; }
  bool startDependency(StringRef name, StringRef path, bool isClangModule,
                       bool isSystem, StringRef hash) override {
    return true;
  }
  bool finishDependency(bool isClangModule) override { return true; }
  Action startSourceEntity(const IndexSymbol &symbol) override {
    symbolStack.push_back(symbol);
    return Action::Continue;
  }
  bool finishSourceEntity(SymbolInfo sym, SymbolRoleSet roles) override {
    IndexSymbol symbol = std::move(symbolStack.back());
    symbolStack.pop_back();
    assert(!symbol.USR.empty());
    StringRef groupName = findGroupForSymbol(symbol);
    auto &tracker = TrackerByGroup[groupName];
    if (!tracker) {
      tracker = llvm::make_unique<SymbolTracker>();
    }
    tracker->addOccurrence(symbol);
    return true;
  }

  void finish() override {
    for (auto &pair : TrackerByGroup) {
      StringRef groupName = pair.first();
      SymbolTracker &tracker = *pair.second;
      bool cont = onFinish(groupName, tracker);
      if (!cont)
        break;
    }
  }

private:
  StringRef findGroupForSymbol(const IndexSymbol &sym);

};
} // end anonymous namespace

static StringRef findGroupNameForDecl(const Decl *D) {
  if (!D || isa<ModuleDecl>(D) || isa<TopLevelCodeDecl>(D))
    return StringRef();

  auto groupNameOpt = D->getGroupName();
  if (groupNameOpt)
    return *groupNameOpt;

  return findGroupNameForDecl(D->getDeclContext()->getInnermostDeclarationDeclContext());
}

StringRef StdlibGroupsIndexRecordingConsumer::findGroupForSymbol(const IndexSymbol &sym) {
  bool isDeclOrDef = sym.roles & ((SymbolRoleSet)SymbolRole::Declaration | (SymbolRoleSet)SymbolRole::Definition);
  if (isDeclOrDef) {
    if (!sym.group.empty())
      return sym.group;
    return findGroupNameForDecl(sym.decl);
  }

  for (auto &rel : sym.Relations) {
    if (!rel.group.empty())
      return rel.group;
    if (rel.decl)
      return findGroupNameForDecl(rel.decl);
  }
  llvm_unreachable("did not find group name for reference");
}

static bool writeRecord(SymbolTracker &record, std::string Filename,
                        std::string indexStorePath, DiagnosticEngine *diags,
                        std::string &outRecordFile) {
  if (record.getOccurrences().empty()) {
    outRecordFile = std::string();
    return false;
  }

  IndexRecordWriter recordWriter(indexStorePath);
  std::string error;
  auto result = recordWriter.beginRecord(
      Filename, record.hashRecord(), error, &outRecordFile);
  switch (result) {
  case IndexRecordWriter::Result::Failure:
    diags->diagnose(SourceLoc(), diag::error_write_index_record, error);
    return true;
  case IndexRecordWriter::Result::AlreadyExists:
    return false;
  case IndexRecordWriter::Result::Success:
    break;
  }

  for (auto &occurrence : record.getOccurrences()) {
    SmallVector<clang::index::writer::SymbolRelation, 3> relations;
    for(SymbolTracker::SymbolRelation symbolRelation: occurrence.related) {
      relations.push_back({record.getSymbol(symbolRelation.symbolIndex), symbolRelation.roles});
    }

    recordWriter.addOccurrence(
        record.getSymbol(occurrence.symbolIndex), occurrence.roles,
        occurrence.line, occurrence.column, relations);
  }

  result = recordWriter.endRecord(error,
      [&](clang::index::writer::OpaqueDecl opaqueSymbol,
          SmallVectorImpl<char> &scratch) {
    auto *symbol = static_cast<const SymbolTracker::Symbol *>(opaqueSymbol);
    clang::index::writer::Symbol result;
    result.SymInfo = symbol->symInfo;
    result.Name = symbol->name;
    result.USR = symbol->USR;
    result.CodeGenName = ""; // FIXME
    return result;
  });

  if (result == IndexRecordWriter::Result::Failure) {
    diags->diagnose(SourceLoc(), diag::error_write_index_record, error);
    return true;
  }

  return false;
}

static std::unique_ptr<IndexRecordingConsumer>
makeRecordingConsumer(std::string Filename, std::string indexStorePath,
                      DiagnosticEngine *diags,
                      std::string *outRecordFile,
                      bool *outFailed) {
  return llvm::make_unique<IndexRecordingConsumer>([=](SymbolTracker &record) {
    *outFailed = writeRecord(record, Filename, indexStorePath, diags,
                             *outRecordFile);
  });
}

static bool
recordSourceFile(SourceFile *SF, StringRef indexStorePath,
                 DiagnosticEngine &diags,
                 llvm::function_ref<void(StringRef, StringRef)> callback) {
  std::string recordFile;
  bool failed = false;
  auto consumer = makeRecordingConsumer(SF->getFilename(), indexStorePath,
                                        &diags, &recordFile, &failed);
  indexSourceFile(SF, /*Hash=*/"", *consumer);

  if (!failed && !recordFile.empty())
    callback(recordFile, SF->getFilename());
  return failed;
}

//===----------------------------------------------------------------------===//
// Index unit file writing
//===----------------------------------------------------------------------===//

// Used to get std::string pointers to pass as writer::OpaqueModule.
namespace {
class StringScratchSpace {
  std::vector<std::unique_ptr<std::string>> StrsCreated;

public:
  const std::string *createString(StringRef str) {
    StrsCreated.emplace_back(llvm::make_unique<std::string>(str));
    return StrsCreated.back().get();
  }
};
}

static clang::index::writer::ModuleInfo
getModuleInfoFromOpaqueModule(clang::index::writer::OpaqueModule mod,
                              SmallVectorImpl<char> &Scratch) {
  clang::index::writer::ModuleInfo info;
  info.Name = *static_cast<const std::string*>(mod);
  return info;
}

static bool
emitDataForSwiftSerializedModule(ModuleDecl *module,
                                 StringRef indexStorePath,
                                 bool indexSystemModules,
                                 StringRef targetTriple,
                                 const clang::CompilerInstance &clangCI,
                                 DiagnosticEngine &diags,
                                 IndexUnitWriter &parentUnitWriter);

static void addModuleDependencies(ArrayRef<ModuleDecl::ImportedModule> imports,
                                  StringRef indexStorePath,
                                  bool indexSystemModules,
                                  StringRef targetTriple,
                                  const clang::CompilerInstance &clangCI,
                                  DiagnosticEngine &diags,
                                  IndexUnitWriter &unitWriter,
                                  StringScratchSpace &moduleNameScratch) {
  auto &fileMgr = clangCI.getFileManager();

  for (auto &import : imports) {
    ModuleDecl *mod = import.second;
    if (mod->isOnoneSupportModule())
      continue; // ignore the Onone support library.
    if (mod->isSwiftShimsModule())
      continue;

    for (auto *FU : mod->getFiles()) {
      switch (FU->getKind()) {
      case FileUnitKind::Source:
      case FileUnitKind::Builtin:
        break;
      case FileUnitKind::SerializedAST:
      case FileUnitKind::ClangModule: {
        auto *LFU = cast<LoadedFile>(FU);
        if (auto *F = fileMgr.getFile(LFU->getFilename())) {
          std::string moduleName = mod->getNameStr();
          bool withoutUnitName = true;
          if (FU->getKind() == FileUnitKind::ClangModule) {
            withoutUnitName = false;
            auto clangModUnit = cast<ClangModuleUnit>(LFU);
            if (auto clangMod = clangModUnit->getUnderlyingClangModule()) {
              moduleName = clangMod->getTopLevelModuleName();
              // FIXME: clang's -Rremarks do not seem to go through Swift's
              // diagnostic emitter.
              clang::index::emitIndexDataForModuleFile(clangMod,
                                                       clangCI, unitWriter);
            }
          } else {
            // Serialized AST file.
            // Only index system modules (essentially stdlib and overlays).
            // We don't officially support binary swift modules, so normally
            // the index data for user modules would get generated while
            // building them.
            if (mod->isSystemModule() && indexSystemModules) {
              emitDataForSwiftSerializedModule(mod, indexStorePath,
                                               indexSystemModules,
                                               targetTriple, clangCI, diags,
                                               unitWriter);
              withoutUnitName = false;
            }
          }
          clang::index::writer::OpaqueModule opaqMod =
              moduleNameScratch.createString(moduleName);
          unitWriter.addASTFileDependency(F, mod->isSystemModule(), opaqMod,
                                          withoutUnitName);
        }
        break;
      }
      }
    }
  }
}

/// \returns true if an error occurred.
static bool
emitDataForSwiftSerializedModule(ModuleDecl *module,
                                 StringRef indexStorePath,
                                 bool indexSystemModules,
                                 StringRef targetTriple,
                                 const clang::CompilerInstance &clangCI,
                                 DiagnosticEngine &diags,
                                 IndexUnitWriter &parentUnitWriter) {
  StringRef filename = module->getModuleFilename();
  std::string moduleName = module->getNameStr();

  std::string error;
  auto isUptodateOpt = parentUnitWriter.isUnitUpToDateForOutputFile(/*FilePath=*/filename,
                                                                /*TimeCompareFilePath=*/filename, error);
  if (!isUptodateOpt.hasValue()) {
    diags.diagnose(SourceLoc(), diag::error_index_failed_status_check, error);
    return true;
  }
  if (*isUptodateOpt)
    return false;

  // FIXME: Would be useful for testing if swift had clang's -Rremark system so
  // we could output a remark here that we are going to create index data for
  // a module file.

  // Pairs of (recordFile, groupName).
  std::vector<std::pair<std::string, std::string>> records;

  if (!module->isStdlibModule()) {
    std::string recordFile;
    bool failed = false;
    auto consumer = makeRecordingConsumer(filename, indexStorePath,
                                          &diags, &recordFile, &failed);
    indexModule(module, /*Hash=*/"", *consumer);

    if (failed)
      return true;

    records.emplace_back(recordFile, moduleName);
  } else {
    // Record stdlib groups as if they were submodules.

    auto makeSubmoduleNameFromGroupName = [](StringRef groupName, SmallString<128> &buf) {
      buf += "Swift";
      if (groupName.empty())
        return;
      buf += '.';
      for (char ch : groupName) {
        if (ch == '/')
          buf += '.';
        else if (ch == ' ' || ch == '-')
          buf += '_';
        else
          buf += ch;
      }
    };
    auto appendGroupNameForFilename = [](StringRef groupName, SmallString<256> &buf) {
      if (groupName.empty())
        return;
      buf += '_';
      for (char ch : groupName) {
        if (ch == '/' || ch ==' ')
          buf += '_';
        else
          buf += ch;
      }
    };

    bool failed = false;
    StdlibGroupsIndexRecordingConsumer groupIndexConsumer([&](StringRef groupName, SymbolTracker &tracker) -> bool {
      SmallString<128> moduleName;
      makeSubmoduleNameFromGroupName(groupName, moduleName);
      SmallString<256> fileNameWithGroup = filename;
      appendGroupNameForFilename(groupName, fileNameWithGroup);

      std::string outRecordFile;
      failed = failed || writeRecord(tracker, fileNameWithGroup.str(), indexStorePath, &diags, outRecordFile);
      if (failed)
        return false;
      records.emplace_back(outRecordFile, moduleName.str());
      return true;
    });
    indexModule(module, /*Hash=*/"", groupIndexConsumer);
    if (failed)
      return true;
  }

  auto &fileMgr = clangCI.getFileManager();
  bool isSystem = module->isSystemModule();
  // FIXME: Get real values for the following.
  StringRef swiftVersion;
  StringRef sysrootPath = clangCI.getHeaderSearchOpts().Sysroot;
  std::string indexUnitToken = module->getModuleFilename();
  // For indexing serialized modules 'debug compilation' is irrelevant, so
  // set it to true by default.
  bool isDebugCompilation = true;

  IndexUnitWriter unitWriter(fileMgr, indexStorePath,
    "swift", swiftVersion, indexUnitToken, moduleName,
    /*MainFile=*/nullptr, isSystem, /*IsModuleUnit=*/true,
    isDebugCompilation, targetTriple, sysrootPath, getModuleInfoFromOpaqueModule);

  const clang::FileEntry *FE = fileMgr.getFile(filename);
  bool isSystemModule = module->isSystemModule();
  for (auto &pair : records) {
    std::string &recordFile = pair.first;
    std::string &groupName = pair.second;
    if (recordFile.empty())
      continue;
    clang::index::writer::OpaqueModule mod = &groupName;
    unitWriter.addRecordFile(recordFile, FE, isSystemModule, mod);
  }

  SmallVector<ModuleDecl::ImportedModule, 8> imports;
  module->getImportedModules(imports, ModuleDecl::ImportFilter::All);
  StringScratchSpace moduleNameScratch;
  addModuleDependencies(imports, indexStorePath, indexSystemModules,
                        targetTriple, clangCI, diags, unitWriter, moduleNameScratch);

  if (unitWriter.write(error)) {
    diags.diagnose(SourceLoc(), diag::error_write_index_unit, error);
    return true;
  }

  return false;
}

static bool
recordSourceFileUnit(SourceFile *primarySourceFile, StringRef indexUnitToken,
                     StringRef indexStorePath, bool indexSystemModules,
                     bool isDebugCompilation, StringRef targetTriple,
                     ArrayRef<const clang::FileEntry *> fileDependencies,
                     const clang::CompilerInstance &clangCI,
                     DiagnosticEngine &diags) {
  auto &fileMgr = clangCI.getFileManager();
  auto *module = primarySourceFile->getParentModule();
  bool isSystem = module->isSystemModule();
  auto *mainFile = fileMgr.getFile(primarySourceFile->getFilename());
  // FIXME: Get real values for the following.
  StringRef swiftVersion;
  StringRef sysrootPath = clangCI.getHeaderSearchOpts().Sysroot;

  IndexUnitWriter unitWriter(fileMgr, indexStorePath,
    "swift", swiftVersion, indexUnitToken, module->getNameStr(),
    mainFile, isSystem, /*isModuleUnit=*/false, isDebugCompilation,
    targetTriple, sysrootPath, getModuleInfoFromOpaqueModule);

  // Module dependencies.
  SmallVector<ModuleDecl::ImportedModule, 8> imports;
  primarySourceFile->getImportedModules(imports, ModuleDecl::ImportFilter::All);
  StringScratchSpace moduleNameScratch;
  addModuleDependencies(imports, indexStorePath, indexSystemModules,
                        targetTriple, clangCI, diags, unitWriter, moduleNameScratch);

  // File dependencies.
  for (auto *F : fileDependencies)
    unitWriter.addFileDependency(F, /*FIXME:isSystem=*/false, /*Module=*/nullptr);

  recordSourceFile(primarySourceFile, indexStorePath, diags,
                   [&](StringRef recordFile, StringRef filename) {
    unitWriter.addRecordFile(recordFile, fileMgr.getFile(filename),
                             module->isSystemModule(), /*Module=*/nullptr);
  });

  std::string error;
  if (unitWriter.write(error)) {
    diags.diagnose(SourceLoc(), diag::error_write_index_unit, error);
    return true;
  }

  return false;
}

// Not currently used, see related comments in the call sites.
#if 0
static void
collectFileDependencies(llvm::SetVector<const clang::FileEntry *> &result,
                        const DependencyTracker &dependencyTracker,
                        ModuleDecl *module, clang::FileManager &fileMgr) {
  for (auto *F : module->getFiles()) {
    if (auto *SF = dyn_cast<SourceFile>(F)) {
      if (auto *dep = fileMgr.getFile(SF->getFilename())) {
        result.insert(dep);
      }
    }
  }
  for (StringRef filename : dependencyTracker.getDependencies()) {
    if (auto *F = fileMgr.getFile(filename))
      result.insert(F);
  }
}
#endif

//===----------------------------------------------------------------------===//
// Indexing entry points
//===----------------------------------------------------------------------===//

bool index::indexAndRecord(SourceFile *primarySourceFile,
                           StringRef indexUnitToken,
                           StringRef indexStorePath,
                           bool indexSystemModules,
                           bool isDebugCompilation,
                           StringRef targetTriple,
                           const DependencyTracker &dependencyTracker) {
  auto &astContext = primarySourceFile->getASTContext();
  auto &clangCI = astContext.getClangModuleLoader()->getClangInstance();
  auto &diags = astContext.Diags;

  std::string error;
  if (IndexUnitWriter::initIndexDirectory(indexStorePath, error)) {
    diags.diagnose(SourceLoc(), diag::error_create_index_dir, error);
    return true;
  }

  llvm::SetVector<const clang::FileEntry *> fileDependencies;
  // FIXME: This is not desirable because:
  // 1. It picks shim header files as file dependencies
  // 2. Having all the other swift files of the module as file dependencies ends
  //   up making all of them associated with all the other files as main files.
  //   It's better to associate each swift file with the unit that recorded it
  //   as the main one.
  // Keeping the code in case we want to revisit.
#if 0
  auto *module = primarySourceFile->getParentModule();
  collectFileDependencies(fileDependencies, dependencyTracker, module, fileMgr);
#endif

  return recordSourceFileUnit(primarySourceFile, indexUnitToken,
                              indexStorePath, indexSystemModules,
                              isDebugCompilation, targetTriple,
                              fileDependencies.getArrayRef(),
                              clangCI, diags);
}

bool index::indexAndRecord(ModuleDecl *module,
                           ArrayRef<std::string> indexUnitTokens,
                           StringRef moduleUnitToken,
                           StringRef indexStorePath,
                           bool indexSystemModules,
                           bool isDebugCompilation,
                           StringRef targetTriple,
                           const DependencyTracker &dependencyTracker) {
  auto &astContext = module->getASTContext();
  auto &clangCI = astContext.getClangModuleLoader()->getClangInstance();
  auto &diags = astContext.Diags;

  std::string error;
  if (IndexUnitWriter::initIndexDirectory(indexStorePath, error)) {
    diags.diagnose(SourceLoc(), diag::error_create_index_dir, error);
    return true;
  }

  // Add the current module's source files to the dependencies.
  llvm::SetVector<const clang::FileEntry *> fileDependencies;
  // FIXME: This is not desirable because:
  // 1. It picks shim header files as file dependencies
  // 2. Having all the other swift files of the module as file dependencies ends
  //   up making all of them associated with all the other files as main files.
  //   It's better to associate each swift file with the unit that recorded it
  //   as the main one.
  // Keeping the code in case we want to revisit.
#if 0
  collectFileDependencies(fileDependencies, dependencyTracker, module, fileMgr);
#endif

  // Write a unit for each source file.
  unsigned unitIndex = 0;
  for (auto *F : module->getFiles()) {
    if (auto *SF = dyn_cast<SourceFile>(F)) {
      if (unitIndex == indexUnitTokens.size()) {
        diags.diagnose(SourceLoc(), diag::error_index_inputs_more_than_outputs);
        return true;
      }
      if (recordSourceFileUnit(SF, indexUnitTokens[unitIndex],
                               indexStorePath, indexSystemModules,
                               isDebugCompilation, targetTriple,
                               fileDependencies.getArrayRef(),
                               clangCI, diags))
        return true;
      unitIndex += 1;
    }
  }

  // In the case where inputs are swift modules, like in the merge-module step,
  // ignore the inputs; associated unit files for the modules' source inputs
  // should have been generated at swift module creation time.

  return false;
}
