//===--- ClangDiagnosticConsumer.cpp - Handle Clang diagnostics -----------===//
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

#include "ClangDiagnosticConsumer.h"
#include "ImporterImpl.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "clang/AST/ASTContext.h"
#include "clang/Frontend/DiagnosticRenderer.h"
#include "clang/Frontend/FrontendDiagnostic.h"
#include "clang/Lex/LexDiagnostic.h"
#include "llvm/ADT/STLExtras.h"

using namespace swift;

namespace {
  class ClangDiagRenderer final : public clang::DiagnosticNoteRenderer {
    const llvm::function_ref<void(clang::FullSourceLoc,
                                  clang::DiagnosticsEngine::Level,
                                  StringRef)> callback;

  public:
    ClangDiagRenderer(const clang::LangOptions &langOpts,
                      clang::DiagnosticOptions *diagOpts,
                      decltype(callback) fn)
       : DiagnosticNoteRenderer(langOpts, diagOpts),
         callback(fn) {}

  private:
    /// Is this a diagnostic that doesn't do the user any good to show if it
    /// is located in one of Swift's synthetic buffers? If so, returns true to
    /// suppress it.
    static bool shouldSuppressDiagInSwiftBuffers(clang::DiagOrStoredDiag info) {
      if (info.isNull())
        return false;

      unsigned ID;
      if (auto *activeDiag = info.dyn_cast<const clang::Diagnostic *>())
        ID = activeDiag->getID();
      else
        ID = info.get<const clang::StoredDiagnostic *>()->getID();
      return ID == clang::diag::note_module_import_here ||
             ID == clang::diag::err_module_not_built;
    }

    /// Returns true if \p loc is inside one of Swift's synthetic buffers.
    static bool isInSwiftBuffers(clang::FullSourceLoc loc) {
      StringRef bufName = StringRef(loc.getManager().getBufferName(loc));
      return bufName == ClangImporter::Implementation::moduleImportBufferName ||
             bufName == ClangImporter::Implementation::bridgingHeaderBufferName;
    }

    void emitDiagnosticMessage(clang::FullSourceLoc Loc,
                               clang::PresumedLoc PLoc,
                               clang::DiagnosticsEngine::Level Level,
                               StringRef Message,
                               ArrayRef<clang::CharSourceRange> Ranges,
                               clang::DiagOrStoredDiag Info) override {
      if (shouldSuppressDiagInSwiftBuffers(Info) && isInSwiftBuffers(Loc))
        return;
      callback(Loc, Level, Message);
    }

    void emitDiagnosticLoc(clang::FullSourceLoc Loc, clang::PresumedLoc PLoc,
                           clang::DiagnosticsEngine::Level Level,
                           ArrayRef<clang::CharSourceRange> Ranges) override {}

    void emitCodeContext(clang::FullSourceLoc Loc,
                         clang::DiagnosticsEngine::Level Level,
                         SmallVectorImpl<clang::CharSourceRange>& Ranges,
                         ArrayRef<clang::FixItHint> Hints) override {}

    void emitNote(clang::FullSourceLoc Loc, StringRef Message) override {
      // We get invalid note locations when trying to describe where a module
      // is imported and the actual location is in Swift. We also want to ignore
      // things like "in module X imported from <swift-imported-modules>".
      if (Loc.isInvalid() || isInSwiftBuffers(Loc))
        return;
      emitDiagnosticMessage(Loc, {}, clang::DiagnosticsEngine::Note, Message,
                            {}, {});
    }
  };
} // end anonymous namespace

ClangDiagnosticConsumer::ClangDiagnosticConsumer(
    ClangImporter::Implementation &impl,
    clang::DiagnosticOptions &clangDiagOptions,
    bool dumpToStderr)
  : TextDiagnosticPrinter(llvm::errs(), &clangDiagOptions),
    ImporterImpl(impl), DumpToStderr(dumpToStderr) {}

static SourceLoc findEndOfLine(SourceManager &SM, SourceLoc loc,
                               unsigned bufferID) {
  CharSourceRange entireBuffer = SM.getRangeForBuffer(bufferID);
  CharSourceRange rangeFromLoc{SM, loc, entireBuffer.getEnd()};
  StringRef textFromLoc = SM.extractText(rangeFromLoc);
  size_t newlineOffset = textFromLoc.find_first_of({"\r\n\0", 3});
  if (newlineOffset == StringRef::npos)
    return entireBuffer.getEnd();
  return loc.getAdvancedLoc(newlineOffset);
}

SourceLoc ClangDiagnosticConsumer::resolveSourceLocation(
    const clang::SourceManager &clangSrcMgr,
    clang::SourceLocation clangLoc) {
  SourceManager &swiftSrcMgr = ImporterImpl.SwiftContext.SourceMgr;
  SourceLoc loc;

  clangLoc = clangSrcMgr.getFileLoc(clangLoc);
  auto decomposedLoc = clangSrcMgr.getDecomposedLoc(clangLoc);
  if (decomposedLoc.first.isInvalid())
    return loc;

  auto buffer = clangSrcMgr.getBuffer(decomposedLoc.first);
  unsigned mirrorID;

  auto mirrorIter = mirroredBuffers.find(buffer);
  if (mirrorIter != mirroredBuffers.end()) {
    mirrorID = mirrorIter->second;
  } else {
    std::unique_ptr<llvm::MemoryBuffer> mirrorBuffer{
      llvm::MemoryBuffer::getMemBuffer(buffer->getBuffer(),
                                       buffer->getBufferIdentifier(),
                                       /*RequiresNullTerminator=*/true)
    };
    mirrorID = swiftSrcMgr.addNewSourceBuffer(std::move(mirrorBuffer));
    mirroredBuffers[buffer] = mirrorID;
  }
  loc = swiftSrcMgr.getLocForOffset(mirrorID, decomposedLoc.second);

  auto presumedLoc = clangSrcMgr.getPresumedLoc(clangLoc);
  if (!presumedLoc.getFilename())
    return loc;
  if (presumedLoc.getLine() == 0)
    return SourceLoc();

  unsigned bufferLineNumber =
    clangSrcMgr.getLineNumber(decomposedLoc.first, decomposedLoc.second);

  StringRef presumedFile = presumedLoc.getFilename();
  SourceLoc startOfLine = loc.getAdvancedLoc(-presumedLoc.getColumn() + 1);
  bool isNewVirtualFile =
    swiftSrcMgr.openVirtualFile(startOfLine, presumedFile,
                                presumedLoc.getLine() - bufferLineNumber);
  if (isNewVirtualFile) {
    SourceLoc endOfLine = findEndOfLine(swiftSrcMgr, loc, mirrorID);
    swiftSrcMgr.closeVirtualFile(endOfLine);
  }

  using SourceManagerRef = llvm::IntrusiveRefCntPtr<const clang::SourceManager>;
  auto iter = std::lower_bound(sourceManagersWithDiagnostics.begin(),
                               sourceManagersWithDiagnostics.end(),
                               &clangSrcMgr,
                               [](const SourceManagerRef &inArray,
                                  const clang::SourceManager *toInsert) {
    return std::less<const clang::SourceManager *>()(inArray.get(), toInsert);
  });
  if (iter == sourceManagersWithDiagnostics.end() ||
      iter->get() != &clangSrcMgr) {
    sourceManagersWithDiagnostics.insert(iter, &clangSrcMgr);
  }

  return loc;
}


void ClangDiagnosticConsumer::HandleDiagnostic(
    clang::DiagnosticsEngine::Level clangDiagLevel,
    const clang::Diagnostic &clangDiag) {
  // Handle the module-not-found diagnostic specially if it's a top-level module
  // we're looking for.
  if (clangDiag.getID() == clang::diag::err_module_not_found &&
      CurrentImport && clangDiag.getArgStdStr(0) == CurrentImport->getName()) {
    return;
  }

  const ASTContext &ctx = ImporterImpl.SwiftContext;

  if (clangDiag.getID() == clang::diag::err_module_not_built &&
      CurrentImport && clangDiag.getArgStdStr(0) == CurrentImport->getName()) {
    SourceLoc loc = DiagLoc;
    if (clangDiag.getLocation().isValid())
      loc = resolveSourceLocation(clangDiag.getSourceManager(),
                                  clangDiag.getLocation());

    ctx.Diags.diagnose(loc, diag::clang_cannot_build_module,
                       ctx.LangOpts.EnableObjCInterop,
                       CurrentImport->getName());
    return;
  }

  // Satisfy the default implementation (bookkeeping).
  if (DumpToStderr)
    TextDiagnosticPrinter::HandleDiagnostic(clangDiagLevel, clangDiag);
  else
    DiagnosticConsumer::HandleDiagnostic(clangDiagLevel, clangDiag);

  // FIXME: Map over source ranges in the diagnostic.
  auto emitDiag = [&ctx, this](clang::FullSourceLoc clangNoteLoc,
                      clang::DiagnosticsEngine::Level clangDiagLevel,
                      StringRef message) {
    decltype(diag::error_from_clang) diagKind;
    switch (clangDiagLevel) {
    case clang::DiagnosticsEngine::Ignored:
      return;
    case clang::DiagnosticsEngine::Note:
      diagKind = diag::note_from_clang;
      break;
    case clang::DiagnosticsEngine::Remark:
      // FIXME: We don't handle remarks yet.
      return;
    case clang::DiagnosticsEngine::Warning:
      diagKind = diag::warning_from_clang;
      break;
    case clang::DiagnosticsEngine::Error:
    case clang::DiagnosticsEngine::Fatal:
      // FIXME: What happens after a fatal error in the importer?
      diagKind = diag::error_from_clang;
      break;
    }

    SourceLoc noteLoc;
    if (clangNoteLoc.isValid())
      noteLoc = resolveSourceLocation(clangNoteLoc.getManager(),
                                      clangNoteLoc);
    ctx.Diags.diagnose(noteLoc, diagKind, message);
  };

  llvm::SmallString<128> message;
  clangDiag.FormatDiagnostic(message);

  if (clangDiag.getLocation().isInvalid()) {
    // Diagnostic about the compiler arguments.
    emitDiag(clang::FullSourceLoc(), clangDiagLevel, message);

  } else {
    assert(clangDiag.hasSourceManager());
    auto clangCI = ImporterImpl.getClangInstance();
    ClangDiagRenderer renderer(clangCI->getLangOpts(),
                               &clangCI->getDiagnosticOpts(), emitDiag);
    clang::FullSourceLoc clangDiagLoc(clangDiag.getLocation(),
                                      clangDiag.getSourceManager());
    renderer.emitDiagnostic(clangDiagLoc, clangDiagLevel, message,
                            clangDiag.getRanges(), clangDiag.getFixItHints(),
                            &clangDiag);
  }
}
