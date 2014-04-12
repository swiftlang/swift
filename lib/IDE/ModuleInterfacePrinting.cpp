//===--- ModuleInterfacePrinting.cpp - Routines to print module interface -===//
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

#include "swift/IDE/ModuleInterfacePrinting.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Serialization/ModuleFile.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/Module.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/MacroInfo.h"
#include <utility>

using namespace swift;

namespace {
/// Prints regular comments from clang module headers.
class ClangCommentPrinter : public ASTPrinter {
public:
  ClangCommentPrinter(ASTPrinter &OtherPrinter, ClangModuleLoader &ClangLoader)
    : OtherPrinter(OtherPrinter),
      ClangLoader(ClangLoader) {}

private:
  void printDeclPre(const Decl *D) override;
  void printDeclPost(const Decl *D) override;

  // Forwarding implementations.

  void printText(StringRef Text) override {
    return OtherPrinter.printText(Text);
  }
  void printDeclLoc(const Decl *D) override {
    return OtherPrinter.printDeclLoc(D);
  }
  void printTypeRef(const TypeDecl *TD, StringRef Text) override {
    return OtherPrinter.printTypeRef(TD, Text);
  }
  void printModuleRef(const Module *Mod, StringRef Text) override {
    return OtherPrinter.printModuleRef(Mod, Text);
  }

  // Prints regular comments of the header the clang node comes from, until
  // the location of the node. Keeps track of the comments that were printed
  // from the file and resumes printing for the next node from the same file.
  // This expects to get passed clang nodes in source-order (at least within the
  // same header).
  void printCommentsUntil(ClangNode Node);

  bool isDocumentationComment(clang::SourceLocation CommentLoc,
                              ClangNode Node) const;

  unsigned getResumeOffset(clang::FileID FID) const {
    auto OffsI = ResumeOffsets.find(FID);
    if (OffsI != ResumeOffsets.end())
      return OffsI->second;
    return 0;
  }
  void setResumeOffset(clang::FileID FID, unsigned Offset) {
    ResumeOffsets[FID] = Offset;
  }

  ASTPrinter &OtherPrinter;
  ClangModuleLoader &ClangLoader;
  llvm::DenseMap<clang::FileID, unsigned> ResumeOffsets;
  SmallVector<StringRef, 2> PendingComments;
};
}

static const clang::Module *findTopLevelClangModule(const Module *M) {
  const ClangModuleUnit *CMU = nullptr;
  for (auto *FU : M->getFiles()) {
    if ((CMU = dyn_cast<ClangModuleUnit>(FU)))
      break;
    if (auto *AST = dyn_cast<SerializedASTFile>(FU)) {
      if (auto *ShadowedModule = AST->getFile().getShadowedModule())
        if (auto *Result = findTopLevelClangModule(ShadowedModule))
          return Result;
    }
  }
  if (!CMU)
    return nullptr;
  return CMU->getClangModule();
}

void swift::ide::printModuleInterface(Module *M,
                                      ModuleTraversalOptions TraversalOptions,
                                      ASTPrinter &Printer,
                                      const PrintOptions &Options) {
  printSubmoduleInterface(M, M->Name.str(), TraversalOptions, Printer, Options);
}

void swift::ide::printSubmoduleInterface(
       Module *M,
       ArrayRef<StringRef> FullModuleName,
       ModuleTraversalOptions TraversalOptions,
       ASTPrinter &Printer,
       const PrintOptions &Options) {
  auto AdjustedOptions = Options;

  // Don't print empty curly braces while printing the module interface.
  AdjustedOptions.FunctionDefinitions = false;

  AdjustedOptions.PrintGetSetOnRWProperties = false;

  // Print var declarations separately, one variable per decl.
  AdjustedOptions.ExplodePatternBindingDecls = true;
  AdjustedOptions.VarInitializers = false;

  AdjustedOptions.PrintDefaultParameterPlaceholder = true;

  SmallVector<Decl *, 1> Decls;
  M->getDisplayDecls(Decls);

  auto &SwiftContext = M->Ctx;
  auto &Importer =
      static_cast<ClangImporter &>(*SwiftContext.getClangModuleLoader());

  const clang::Module *InterestingClangModule = nullptr;

  SmallVector<ImportDecl *, 1> ImportDecls;
  SmallVector<Decl *, 1> SwiftDecls;
  llvm::DenseMap<const clang::Module *,
                 SmallVector<std::pair<Decl *, clang::SourceLocation>, 1>>
    ClangDecls;

  // Drop top-level module name.
  FullModuleName = FullModuleName.slice(1);

  InterestingClangModule = findTopLevelClangModule(M);
  if (InterestingClangModule) {
    for (StringRef Name : FullModuleName) {
      InterestingClangModule = InterestingClangModule->findSubmodule(Name);
      if (!InterestingClangModule)
        return;
    }
  } else {
    assert(FullModuleName.empty());
  }

  // If we're printing recursively, find all of the submodules to print.
  if (InterestingClangModule) {
    if (TraversalOptions) {
      SmallVector<const clang::Module *, 8> Worklist;
      SmallPtrSet<const clang::Module *, 8> Visited;
      Worklist.push_back(InterestingClangModule);
      Visited.insert(InterestingClangModule);
      while (!Worklist.empty()) {
        const clang::Module *CM = Worklist.pop_back_val();
        if (!(TraversalOptions & ModuleTraversal::VisitHidden) &&
            !InterestingClangModule->isModuleVisible(CM))
          continue;

        ClangDecls.insert({ CM, {} });

        // If we're supposed to visit submodules, add them now.
        if (TraversalOptions & ModuleTraversal::VisitSubmodules) {
          for (auto Sub = CM->submodule_begin(), SubEnd = CM->submodule_end();
               Sub != SubEnd; ++Sub) {
            if (Visited.insert(*Sub))
              Worklist.push_back(*Sub);
          }
        }
      }
    } else {
      ClangDecls.insert({ InterestingClangModule, {} });
    }
  }

  // Separate the declarations that we are going to print into different
  // buckets.
  for (Decl *D : Decls) {
    // If requested, skip unavailable declarations.
    if (Options.SkipUnavailable && D->getAttrs().isUnavailable())
      continue;

    if (auto ID = dyn_cast<ImportDecl>(D)) {
      ImportDecls.push_back(ID);
      continue;
    }
    if (auto CN = D->getClangNode()) {
      clang::SourceLocation Loc = CN.getLocation();

      auto *OwningModule = Importer.getClangOwningModule(CN);
      auto I = ClangDecls.find(OwningModule);
      if (I != ClangDecls.end()) {
        I->second.push_back({ D, Loc });
      }
      continue;
    }
    if (FullModuleName.empty()) {
      // Add Swift decls if we are printing the top-level module.
      SwiftDecls.push_back(D);
    }
  }

  auto &ClangSourceManager = Importer.getClangASTContext().getSourceManager();

  // Sort imported declarations in source order *within a submodule*.
  for (auto &P : ClangDecls) {
    std::sort(P.second.begin(), P.second.end(),
              [&](std::pair<Decl *, clang::SourceLocation> LHS,
                  std::pair<Decl *, clang::SourceLocation> RHS) -> bool {
                return ClangSourceManager.isBeforeInTranslationUnit(LHS.second,
                                                                    RHS.second);
              });
  }

  // Sort Swift declarations so that we print them in a consistent order.
  std::sort(ImportDecls.begin(), ImportDecls.end(),
            [](ImportDecl *LHS, ImportDecl *RHS) -> bool {
    auto LHSPath = LHS->getFullAccessPath();
    auto RHSPath = RHS->getFullAccessPath();
    for (unsigned i = 0, e = std::min(LHSPath.size(), RHSPath.size()); i != e;
         i++) {
      if (int Ret = LHSPath[i].first.str().compare(RHSPath[i].first.str()))
        return Ret < 0;
    }
    return false;
  });

  std::sort(SwiftDecls.begin(), SwiftDecls.end(),
            [](Decl *LHS, Decl *RHS) -> bool {
    auto *LHSValue = dyn_cast<ValueDecl>(LHS);
    auto *RHSValue = dyn_cast<ValueDecl>(RHS);
    if (LHSValue && RHSValue) {
      StringRef LHSName = LHSValue->getName().str();
      StringRef RHSName = RHSValue->getName().str();
      if (int Ret = LHSName.compare(RHSName))
        return Ret < 0;
      // FIXME: this is not sufficient to establish a total order for overloaded
      // decls.
      return LHS->getKind() < RHS->getKind();
    }

    return LHS->getKind() < RHS->getKind();
  });

  ASTPrinter *PrinterToUse = &Printer;

  ClangCommentPrinter RegularCommentPrinter(Printer, Importer);
  if (Options.PrintRegularClangComments)
    PrinterToUse = &RegularCommentPrinter;

  auto PrintDecl = [&](Decl *D) {
    if (isa<ExtensionDecl>(D))
      return;

    ASTPrinter &Printer = *PrinterToUse;
    D->print(Printer, AdjustedOptions);
    Printer << "\n";
    if (auto NTD = dyn_cast<NominalTypeDecl>(D)) {
      for (auto Ext : NTD->getExtensions()) {
        Ext->print(Printer, AdjustedOptions);
        Printer << "\n";
      }
    }
  };

  for (auto *D : ImportDecls)
    PrintDecl(D);

  {
    using ModuleAndName = std::pair<const clang::Module *, std::string>;
    SmallVector<ModuleAndName, 8> ClangModules;
    for (auto P : ClangDecls) {
      ClangModules.push_back({ P.first, P.first->getFullModuleName() });
    }
    // Sort modules by name.
    std::sort(ClangModules.begin(), ClangModules.end(),
              [](const ModuleAndName &LHS, const ModuleAndName &RHS)
                -> bool {
                  return LHS.second < RHS.second;
              });

    for (auto CM : ClangModules) {
      for (auto DeclAndLoc : ClangDecls[CM.first])
        PrintDecl(DeclAndLoc.first);
    }
  }

  if (!(TraversalOptions & ModuleTraversal::SkipOverlay) ||
      !InterestingClangModule) {
    for (auto *D : SwiftDecls)
      PrintDecl(D);
  }
}

void ClangCommentPrinter::printDeclPre(const Decl *D) {
  if (auto ClangN = D->getClangNode())
    printCommentsUntil(ClangN);
  return OtherPrinter.printDeclPre(D);
}

void ClangCommentPrinter::printDeclPost(const Decl *D) {
  OtherPrinter.printDeclPost(D);
  for (auto CommentText : PendingComments) {
    *this << " " << CommentText;
  }
  PendingComments.clear();
}

void ClangCommentPrinter::printCommentsUntil(ClangNode Node) {
  const auto &Ctx = ClangLoader.getClangASTContext();
  const auto &SM = Ctx.getSourceManager();

  clang::SourceLocation NodeLoc = SM.getFileLoc(Node.getLocation());
  if (NodeLoc.isInvalid())
    return;
  unsigned NodeLineNo = SM.getSpellingLineNumber(NodeLoc);
  clang::FileID FID = SM.getFileID(NodeLoc);
  if (FID.isInvalid())
    return;
  clang::SourceLocation FileLoc = SM.getLocForStartOfFile(FID);
  StringRef Text = SM.getBufferData(FID);
  if (Text.empty())
    return;

  const char *BufStart = Text.data();
  const char *BufPtr = BufStart + getResumeOffset(FID);
  const char *BufEnd = BufStart + Text.size();
  assert(BufPtr <= BufEnd);
  if (BufPtr == BufEnd)
    return; // nothing left.

  clang::Lexer Lex(FileLoc, Ctx.getLangOpts(), BufStart, BufPtr, BufEnd);
  Lex.SetCommentRetentionState(true);

  unsigned LastPrintedCommentLineNo = 0;
  clang::Token Tok;
  do {
    BufPtr = Lex.getBufferLocation();
    Lex.LexFromRawLexer(Tok);
    if (Tok.is(clang::tok::eof))
      break;
    if (Tok.isNot(clang::tok::comment))
      continue;

    // Reached a comment.

    clang::SourceLocation CommentLoc = Tok.getLocation();
    if (isDocumentationComment(CommentLoc, Node))
      continue;

    std::pair<clang::FileID, unsigned> LocInfo =
      SM.getDecomposedSpellingLoc(CommentLoc);
    assert(LocInfo.first == FID);
    unsigned LineNo = SM.getLineNumber(LocInfo.first, LocInfo.second);
    if (LineNo > NodeLineNo)
      break; // Comment is past the clang node.

    // Print out the comment.

    StringRef CommentText(BufStart + LocInfo.second, Tok.getLength());

    // Check if comment is on same line but after the declaration.
    if (SM.isBeforeInTranslationUnit(NodeLoc, Tok.getLocation())) {
      PendingComments.push_back(CommentText);
      LastPrintedCommentLineNo = 0;
      continue;
    }

    if (LastPrintedCommentLineNo && LineNo - LastPrintedCommentLineNo > 1) {
      *this << "\n";
      printIndent();
    }
    *this << CommentText << "\n";
    printIndent();
    LastPrintedCommentLineNo = LineNo;

  } while (true);

  if (LastPrintedCommentLineNo && NodeLineNo - LastPrintedCommentLineNo > 1) {
    *this << "\n";
    printIndent();
  }

  // Resume printing comments from this point.
  setResumeOffset(FID, BufPtr - BufStart);
}

bool ClangCommentPrinter::isDocumentationComment(
      clang::SourceLocation CommentLoc, ClangNode Node) const {
  const clang::Decl *D = Node.getAsDecl();
  if (!D)
    return false;

  const auto &Ctx = ClangLoader.getClangASTContext();
  const auto &SM = Ctx.getSourceManager();
  const clang::RawComment *RC = Ctx.getRawCommentForAnyRedecl(D);
  if (!RC)
    return false;

  clang::SourceRange DocRange = RC->getSourceRange();
  if (SM.isBeforeInTranslationUnit(CommentLoc, DocRange.getBegin()) ||
      SM.isBeforeInTranslationUnit(DocRange.getEnd(), CommentLoc))
    return false;
  return true;
}
