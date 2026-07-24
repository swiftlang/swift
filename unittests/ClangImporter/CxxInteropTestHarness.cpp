//===--- CxxInteropTestHarness.cpp ------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "CxxInteropTestHarness.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/SILOptions.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/Basic/CASOptions.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Subsystems.h"
#include "swift/SymbolGraphGen/SymbolGraphOptions.h"
#include "clang/AST/DeclBase.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TargetParser/Host.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

namespace {

/// Absolute path of the in-memory C++ header the tests import as a bridging
/// header. It only ever exists in the in-memory overlay VFS.
constexpr const char *kHeaderPath = "/cxx-interop-test/TestModule.h";

/// Prints error diagnostics to stderr so import failures are visible in test
/// output.
class StderrDiagnosticConsumer : public DiagnosticConsumer {
public:
  void handleDiagnostic(SourceManager &SM,
                        const DiagnosticInfo &Info) override {
    if (Info.Kind != DiagnosticKind::Error)
      return;
    llvm::errs() << "[diag] ";
    DiagnosticEngine::formatDiagnosticText(llvm::errs(), Info.FormatString,
                                           Info.FormatArgs);
    llvm::errs() << "\n";
  }
};

} // end anonymous namespace

namespace swift::unittest {

/// Owns an ASTContext + ClangImporter that have imported a snippet of C++
/// source as a bridging header. The source lives entirely in an in-memory
/// overlay filesystem (no temp files); the Swift standard library is loaded so
/// that importing pointer-bearing C++ types succeeds.
class CxxInteropModule {
  // Declared before `diags` so it outlives the DiagnosticEngine that refers to
  // it (members are destroyed in reverse declaration order).
  StderrDiagnosticConsumer diagConsumer;

  LangOptions langOpts;
  TypeCheckerOptions typecheckOpts;
  SILOptions silOpts;
  SearchPathOptions searchPathOpts;
  ClangImporterOptions clangImporterOpts;
  symbolgraphgen::SymbolGraphOptions symbolGraphOpts;
  CASOptions casOpts;
  SerializationOptions serializationOpts;
  SourceManager sourceMgr;
  DiagnosticEngine diags;

  std::unique_ptr<ASTContext> context;
  ClangImporter *importer = nullptr;
  ModuleDecl *module = nullptr;

public:
  explicit CxxInteropModule(StringRef cxxSource) : diags(sourceMgr) {
    INITIALIZE_LLVM();

    // Put the C++ source in an in-memory overlay over the real filesystem, so
    // the header is never written to disk while the real stdlib remains
    // reachable. The ClangImporter reads through SourceManager's filesystem.
    auto inMemoryFS = llvm::makeIntrusiveRefCnt<llvm::vfs::InMemoryFileSystem>();
    inMemoryFS->addFile(kHeaderPath, /*ModificationTime=*/0,
                        llvm::MemoryBuffer::getMemBufferCopy(cxxSource,
                                                             kHeaderPath));
    auto overlayFS = llvm::makeIntrusiveRefCnt<llvm::vfs::OverlayFileSystem>(
        llvm::vfs::getRealFileSystem());
    overlayFS->pushOverlay(inMemoryFS);
    sourceMgr.setFileSystem(overlayFS);

    langOpts.Target = llvm::Triple(llvm::sys::getDefaultTargetTriple());
    langOpts.EnableCXXInterop = true;
    diags.addConsumer(diagConsumer);

    // Point at the just-built Swift standard library.
    SmallString<128> libDir(SWIFTLIB_DIR);
    llvm::sys::path::append(libDir, getPlatformNameForTriple(langOpts.Target));
    searchPathOpts.RuntimeResourcePath = SWIFTLIB_DIR;
    searchPathOpts.RuntimeLibraryPaths.push_back(std::string(libDir.str()));
    searchPathOpts.setRuntimeLibraryImportPaths({std::string(libDir.str())});

    context.reset(ASTContext::get(langOpts, typecheckOpts, silOpts,
                                  searchPathOpts, clangImporterOpts,
                                  symbolGraphOpts, casOpts, serializationOpts,
                                  sourceMgr, diags));

    registerParseRequestFunctions(context->evaluator);
    registerTypeCheckerRequestFunctions(context->evaluator);
    registerClangImporterRequestFunctions(context->evaluator);

    context->addModuleLoader(ImplicitSerializedModuleLoader::create(*context));
    auto importerOwner = ClangImporter::create(*context);
    importer = importerOwner.get();
    context->addModuleLoader(std::move(importerOwner), /*isClang=*/true);

    context->getStdlibModule(/*loadIfAbsent=*/true);

    // Import the C++ source as a bridging header. This parses the header
    // directly (no module cache / .pcm written to disk) and exposes its decls
    // through the imported-header module.
    auto *mainModule = ModuleDecl::createEmpty(
        context->getIdentifier("CxxInteropTestMain"), *context);
    bool failed = importer->importBridgingHeader(kHeaderPath, mainModule);
    EXPECT_FALSE(failed) << "failed to import C++ bridging header";
    if (!failed)
      module = importer->getImportedHeaderModule();
  }

  CxxInteropModule(const CxxInteropModule &) = delete;
  CxxInteropModule &operator=(const CxxInteropModule &) = delete;

  ASTContext &getASTContext() { return *context; }

  /// Look up the imported Swift decl named \p name.
  ValueDecl *lookupSwiftDecl(StringRef name) {
    if (!module)
      return nullptr;
    SmallVector<ValueDecl *, 4> results;
    module->lookupValue(context->getIdentifier(name),
                        NLKind::UnqualifiedLookup, results);
    if (results.size() != 1)
      return nullptr;
    return results.front();
  }
};

CxxInteropChecker::CxxInteropChecker(StringRef cxxSource)
    : impl(std::make_shared<CxxInteropModule>(cxxSource)) {}

CxxInteropChecker &
CxxInteropChecker::checkDecl(StringRef name,
                             llvm::function_ref<void(ValueDecl *)> check) {
  ValueDecl *decl = impl->lookupSwiftDecl(name);
  EXPECT_TRUE(decl) << "could not find imported Swift decl '" << name.str()
                    << "'";
  if (decl)
    check(decl);
  return *this;
}

CxxInteropChecker &CxxInteropChecker::checkCppDecl(
    StringRef name,
    llvm::function_ref<void(const clang::Decl *, ASTContext &)> check) {
  ValueDecl *decl = impl->lookupSwiftDecl(name);
  EXPECT_TRUE(decl) << "could not find imported Swift decl '" << name.str()
                    << "'";
  if (!decl)
    return *this;
  const clang::Decl *clangDecl = decl->getClangDecl();
  EXPECT_TRUE(clangDecl) << "imported decl '" << name.str()
                         << "' has no backing clang decl";
  if (clangDecl)
    check(clangDecl, impl->getASTContext());
  return *this;
}

CxxInteropChecker import(StringRef cxxSource) {
  return CxxInteropChecker(cxxSource);
}

// `compile` is a spelling of `import` that reads better in tests that inspect
// the clang-side decl rather than the imported Swift decl.
CxxInteropChecker compile(StringRef cxxSource) { return import(cxxSource); }

} // end namespace swift::unittest
