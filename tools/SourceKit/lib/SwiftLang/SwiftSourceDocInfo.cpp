//===--- SwiftSourceDocInfo.cpp -------------------------------------------===//
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

#include "SwiftASTManager.h"
#include "SwiftLangSupport.h"
#include "SourceKit/Support/UIdent.h"
#include "SourceKit/Support/ImmutableTextBuffer.h"
#include "SourceKit/Support/Logging.h"

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/SourceEntityWalker.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDE/Utils.h"
#include "swift/Markup/XMLUtils.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/Module.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Lex/Lexer.h"

#include "llvm/Support/MemoryBuffer.h"

using namespace SourceKit;
using namespace swift;
using namespace swift::ide;

class AnnotatedDeclarationPrinter : public XMLEscapingPrinter {
public:
  AnnotatedDeclarationPrinter(raw_ostream &OS)
    :XMLEscapingPrinter(OS) { }

private:
  void printTypeRef(const TypeDecl *TD, Identifier Name) override {
    printXML("<Type usr=\"");
    SwiftLangSupport::printUSR(TD, OS);
    printXML("\">");
    StreamPrinter::printTypeRef(TD, Name);
    printXML("</Type>");
  }
};

static void printAnnotatedDeclaration(const ValueDecl *VD, raw_ostream &OS) {
  AnnotatedDeclarationPrinter Printer(OS);
  PrintOptions PO = PrintOptions::printQuickHelpDeclaration();

  // If it's implicit, try to find an overridden ValueDecl that's not implicit.
  // This will ensure we can properly annotate TypeRepr with a usr
  // in AnnotatedDeclarationPrinter.
  while (VD->isImplicit() && VD->getOverriddenDecl())
    VD = VD->getOverriddenDecl();

  // Wrap this up in XML, as that's what we'll use for documentation comments.
  OS<<"<Declaration>";
  VD->print(Printer, PO);
  OS<<"</Declaration>";
}

template <typename FnTy>
void walkRelatedDecls(const ValueDecl *VD, const FnTy &Fn) {
  llvm::SmallDenseMap<DeclName, unsigned, 16> NamesSeen;
  ++NamesSeen[VD->getFullName()];
  SmallVector<ValueDecl *, 8> RelatedDecls;

  // FIXME: Extract useful related declarations, overloaded functions,
  // if VD is an initializer, we should extract other initializers etc.
  // For now we use UnqualifiedLookup to fetch other declarations with the same
  // base name.
  auto TypeResolver = VD->getASTContext().getLazyResolver();
  UnqualifiedLookup Lookup(VD->getName(), VD->getDeclContext(), TypeResolver);
  for (auto result : Lookup.Results) {
    ValueDecl *RelatedVD = result.getValueDecl();
    if (RelatedVD->getAttrs().isUnavailable(VD->getASTContext()))
      continue;

    if (RelatedVD != VD) {
      ++NamesSeen[RelatedVD->getFullName()];
      RelatedDecls.push_back(RelatedVD);
    }
  }

  // Now provide the results along with whether the name is duplicate or not.
  for (auto RelatedVD : RelatedDecls) {
    Fn(RelatedVD, NamesSeen[RelatedVD->getFullName()] > 1);
  }
}

//===----------------------------------------------------------------------===//
// SwiftLangSupport::getCursorInfo
//===----------------------------------------------------------------------===//

static StringRef getSourceToken(unsigned Offset,
                                ImmutableTextSnapshotRef Snap) {
  auto MemBuf = Snap->getBuffer()->getInternalBuffer();
  SourceManager SM;
  auto MemBufRef = llvm::MemoryBuffer::getMemBuffer(MemBuf->getBuffer(),
                                                 MemBuf->getBufferIdentifier());
  auto BufId = SM.addNewSourceBuffer(std::move(MemBufRef));
  SourceLoc Loc = SM.getLocForOffset(BufId, Offset);

  // Use fake language options; language options only affect validity
  // and the exact token produced.
  LangOptions FakeLangOpts;
  Lexer L(FakeLangOpts, SM, BufId, nullptr, /*InSILMode=*/ false,
          CommentRetentionMode::ReturnAsTokens);
  return L.getTokenAt(Loc).getText();
}

static llvm::Optional<unsigned> 
mapOffsetToOlderSnapshot(unsigned Offset,
                         ImmutableTextSnapshotRef NewSnap,
                         ImmutableTextSnapshotRef OldSnap) {
  SmallVector<ReplaceImmutableTextUpdateRef, 16> Updates;
  OldSnap->foreachReplaceUntil(NewSnap,
    [&](ReplaceImmutableTextUpdateRef Upd)->bool {
      Updates.push_back(Upd);
      return true;
    });

  // Walk the updates backwards and "undo" them.
  for (auto I = Updates.rbegin(), E = Updates.rend(); I != E; ++I) {
    auto Upd = *I;
    if (Upd->getByteOffset() <= Offset &&
        Offset < Upd->getByteOffset() + Upd->getText().size())
      return None; // Offset is part of newly inserted text.

    if (Upd->getByteOffset() <= Offset) {
      Offset += Upd->getLength(); // "bring back" what was removed.
      Offset -= Upd->getText().size(); // "remove" what was added.
    }
  }
  return Offset;
}

static llvm::Optional<unsigned> 
mapOffsetToNewerSnapshot(unsigned Offset,
                         ImmutableTextSnapshotRef OldSnap,
                         ImmutableTextSnapshotRef NewSnap) {
  bool Completed = OldSnap->foreachReplaceUntil(NewSnap,
    [&](ReplaceImmutableTextUpdateRef Upd)->bool {
      if (Upd->getByteOffset() <= Offset &&
          Offset < Upd->getByteOffset() + Upd->getLength())
        return false; // Offset is part of removed text.

      if (Upd->getByteOffset() <= Offset) {
        Offset += Upd->getText().size();
        Offset -= Upd->getLength();
      }
      return true;
    });

  if (Completed)
    return Offset;
  return None;
}

/// Tries to remap the location from a previous snapshot to the latest one.
static llvm::Optional<std::pair<unsigned, unsigned>>
tryRemappingLocToLatestSnapshot(SwiftLangSupport &Lang,
                                std::pair<unsigned, unsigned> Range,
                                StringRef Filename,
                         ArrayRef<ImmutableTextSnapshotRef> PreviousASTSnaps) {
  ImmutableTextSnapshotRef LatestSnap;
  if (auto EditorDoc = Lang.getEditorDocuments().findByPath(Filename))
    LatestSnap = EditorDoc->getLatestSnapshot();
  if (!LatestSnap)
    return Range;

  for (auto &PrevSnap : PreviousASTSnaps) {
    if (PrevSnap->isFromSameBuffer(LatestSnap)) {
      if (PrevSnap->getStamp() == LatestSnap->getStamp())
        return Range;

      auto OptBegin = mapOffsetToNewerSnapshot(Range.first,
                                               PrevSnap, LatestSnap);
      if (!OptBegin.hasValue())
        return None;

      auto OptEnd = mapOffsetToNewerSnapshot(Range.first+Range.second,
                                             PrevSnap, LatestSnap);
      if (!OptEnd.hasValue())
        return None;

      return std::make_pair(*OptBegin, *OptEnd-*OptBegin);
    }
  }

  return Range;
}


/// Returns true for error.
static bool passCursorInfoForModule(ModuleEntity Mod,
                                    SwiftInterfaceGenMap &IFaceGenContexts,
                                    const CompilerInvocation &Invok,
                             std::function<void(const CursorInfo &)> Receiver) {
  std::string Name = Mod.getName();
  std::string FullName = Mod.getFullName();
  CursorInfo Info;
  Info.Kind = SwiftLangSupport::getUIDForModuleRef();
  Info.Name = Name;
  Info.ModuleName = FullName;
  if (auto IFaceGenRef = IFaceGenContexts.find(Info.ModuleName, Invok))
    Info.ModuleInterfaceName = IFaceGenRef->getDocumentName();
  Info.IsSystem = Mod.isSystemModule();
  Receiver(Info);
  return false;
}

/// Returns true for failure to resolve.
static bool passCursorInfoForDecl(const ValueDecl *VD,
                                  const Module *MainModule,
                                  const Type Ty,
                                  bool IsRef,
                                  Optional<unsigned> OrigBufferID,
                                  SwiftLangSupport &Lang,
                                  const CompilerInvocation &Invok,
                            ArrayRef<ImmutableTextSnapshotRef> PreviousASTSnaps,
                             std::function<void(const CursorInfo &)> Receiver) {
  if (AvailableAttr::isUnavailable(VD))
    return true;

  SmallString<64> SS;

  unsigned NameBegin = SS.size();
  {
    llvm::raw_svector_ostream OS(SS);
    SwiftLangSupport::printDisplayName(VD, OS);
  }
  unsigned NameEnd = SS.size();

  unsigned USRBegin = SS.size();
  {
    llvm::raw_svector_ostream OS(SS);
    SwiftLangSupport::printUSR(VD, OS);
  }
  unsigned USREnd = SS.size();

  unsigned TypenameBegin = SS.size();
  if (VD->hasType()) {
    llvm::raw_svector_ostream OS(SS);
    VD->getType().print(OS);
  }
  unsigned TypenameEnd = SS.size();

  unsigned DocCommentBegin = SS.size();
  {
    llvm::raw_svector_ostream OS(SS);
    ide::getDocumentationCommentAsXML(VD, OS);
  }
  unsigned DocCommentEnd = SS.size();

  unsigned DeclBegin = SS.size();
  {
    llvm::raw_svector_ostream OS(SS);
    printAnnotatedDeclaration(VD, OS);
  }
  unsigned DeclEnd = SS.size();

  SmallVector<std::pair<unsigned, unsigned>, 4> OverUSROffs;

  ide::walkOverriddenDecls(VD,
    [&](llvm::PointerUnion<const ValueDecl*, const clang::NamedDecl*> D) {
      unsigned OverUSRBegin = SS.size();
      {
        llvm::raw_svector_ostream OS(SS);
        if (auto VD = D.dyn_cast<const ValueDecl*>()) {
          if (SwiftLangSupport::printUSR(VD, OS))
            return;
        } else {
          llvm::SmallString<128> Buf;
          if (clang::index::generateUSRForDecl(
              D.get<const clang::NamedDecl*>(), Buf))
            return;
          OS << Buf.str();
        }
      }
      unsigned OverUSREnd = SS.size();
      OverUSROffs.push_back(std::make_pair(OverUSRBegin, OverUSREnd));
  });

  SmallVector<std::pair<unsigned, unsigned>, 4> RelDeclOffs;
  walkRelatedDecls(VD, [&](const ValueDecl *RelatedDecl, bool DuplicateName) {
    unsigned RelatedDeclBegin = SS.size();
    {
      llvm::raw_svector_ostream OS(SS);
      OS<<"<RelatedName usr=\"";
      SwiftLangSupport::printUSR(RelatedDecl, OS);
      OS<<"\">";
      if (isa<AbstractFunctionDecl>(RelatedDecl) && DuplicateName) {
        // Related decls are generally overloads, so print parameter types to
        // differentiate them.
        PrintOptions PO;
        PO.SkipAttributes = true;
        PO.SkipIntroducerKeywords = true;
        PO.ArgAndParamPrinting = PrintOptions::ArgAndParamPrintingMode::ArgumentOnly;
        XMLEscapingPrinter Printer(OS);
        RelatedDecl->print(Printer, PO);
      } else {
        llvm::SmallString<128> Buf;
        {
          llvm::raw_svector_ostream OSBuf(Buf);
          SwiftLangSupport::printDisplayName(RelatedDecl, OSBuf);
        }
        llvm::markup::appendWithXMLEscaping(OS, Buf);
      }
      OS<<"</RelatedName>";
    }
    unsigned RelatedDeclEnd = SS.size();
    RelDeclOffs.push_back(std::make_pair(RelatedDeclBegin, RelatedDeclEnd));
  });

  ASTContext &Ctx = VD->getASTContext();

  ClangImporter *Importer = static_cast<ClangImporter*>(
      Ctx.getClangModuleLoader());
  std::string ModuleName;
  auto ClangNode = VD->getClangNode();
  if (ClangNode) {
    auto ClangMod = Importer->getClangOwningModule(ClangNode);
    ModuleName = ClangMod->getFullModuleName();
  } else if (VD->getLoc().isInvalid() && VD->getModuleContext() != MainModule) {
    ModuleName = VD->getModuleContext()->getName().str();
  }
  StringRef ModuleInterfaceName;
  if (auto IFaceGenRef = Lang.getIFaceGenContexts().find(ModuleName, Invok))
    ModuleInterfaceName = IFaceGenRef->getDocumentName();

  UIdent Kind = SwiftLangSupport::getUIDForDecl(VD, IsRef);
  StringRef Name = StringRef(SS.begin()+NameBegin, NameEnd-NameBegin);
  StringRef USR = StringRef(SS.begin()+USRBegin, USREnd-USRBegin);
  StringRef TypeName = StringRef(SS.begin()+TypenameBegin,
                                 TypenameEnd-TypenameBegin);
  StringRef DocComment = StringRef(SS.begin()+DocCommentBegin,
                                   DocCommentEnd-DocCommentBegin);
  StringRef AnnotatedDecl = StringRef(SS.begin()+DeclBegin,
                                      DeclEnd-DeclBegin);

  llvm::Optional<std::pair<unsigned, unsigned>> DeclarationLoc;
  StringRef Filename;
  getLocationInfo(VD, DeclarationLoc, Filename);
  if (DeclarationLoc.hasValue()) {
    DeclarationLoc = tryRemappingLocToLatestSnapshot(Lang,
                                                     *DeclarationLoc,
                                                     Filename,
                                                     PreviousASTSnaps);
    if (!DeclarationLoc.hasValue())
      return true; // failed to remap.
  }

  SmallVector<StringRef, 4> OverUSRs;
  for (auto Offs : OverUSROffs) {
    OverUSRs.push_back(StringRef(SS.begin()+Offs.first,
                                 Offs.second-Offs.first));
  }

  SmallVector<StringRef, 4> AnnotatedRelatedDecls;
  for (auto Offs : RelDeclOffs) {
    AnnotatedRelatedDecls.push_back(StringRef(SS.begin() + Offs.first,
                                              Offs.second - Offs.first));
  }

  bool IsSystem = VD->getModuleContext()->isSystemModule();
  std::string TypeInterface;

  CursorInfo Info;
  Info.Kind = Kind;
  Info.Name = Name;
  Info.USR = USR;
  Info.TypeName = TypeName;
  Info.DocComment = DocComment;
  Info.AnnotatedDeclaration = AnnotatedDecl;
  Info.ModuleName = ModuleName;
  Info.ModuleInterfaceName = ModuleInterfaceName;
  Info.DeclarationLoc = DeclarationLoc;
  Info.Filename = Filename;
  Info.OverrideUSRs = OverUSRs;
  Info.AnnotatedRelatedDeclarations = AnnotatedRelatedDecls;
  Info.IsSystem = IsSystem;
  Info.TypeInterface = ASTPrinter::printTypeInterface(Ty, VD->getDeclContext(),
                                                      TypeInterface) ?
    StringRef(TypeInterface) : StringRef();
  Receiver(Info);
  return false;
}

static void resolveCursor(SwiftLangSupport &Lang,
                          StringRef InputFile, unsigned Offset,
                          SwiftInvocationRef Invok,
                          bool TryExistingAST,
                          std::function<void(const CursorInfo &)> Receiver) {
  assert(Invok);

  class CursorInfoConsumer : public SwiftASTConsumer {
    std::string InputFile;
    unsigned Offset;
    SwiftLangSupport &Lang;
    SwiftInvocationRef ASTInvok;
    const bool TryExistingAST;
    std::function<void(const CursorInfo &)> Receiver;
    SmallVector<ImmutableTextSnapshotRef, 4> PreviousASTSnaps;

  public:
    CursorInfoConsumer(StringRef InputFile, unsigned Offset,
                       SwiftLangSupport &Lang,
                       SwiftInvocationRef ASTInvok,
                       bool TryExistingAST,
                       std::function<void(const CursorInfo &)> Receiver)
      : InputFile(InputFile), Offset(Offset),
        Lang(Lang),
        ASTInvok(std::move(ASTInvok)),
        TryExistingAST(TryExistingAST),
        Receiver(std::move(Receiver)) { }

    bool canUseASTWithSnapshots(
        ArrayRef<ImmutableTextSnapshotRef> Snapshots) override {
      if (!TryExistingAST) {
        LOG_INFO_FUNC(High, "will resolve using up-to-date AST");
        return false;
      }

      // If there is an existing AST and the offset can be mapped back to the
      // document snapshot that was used to create it, then use that AST.
      // The downside is that we may return stale information, but we get the
      // benefit of increased responsiveness, since the request will not be
      // blocked waiting on the AST to be fully typechecked.

      ImmutableTextSnapshotRef InputSnap;
      if (auto EditorDoc = Lang.getEditorDocuments().findByPath(InputFile))
        InputSnap = EditorDoc->getLatestSnapshot();
      if (!InputSnap)
        return false;

      auto mappedBackOffset = [&]()->llvm::Optional<unsigned> {
        for (auto &Snap : Snapshots) {
          if (Snap->isFromSameBuffer(InputSnap)) {
            if (Snap->getStamp() == InputSnap->getStamp())
              return Offset;

            auto OptOffset = mapOffsetToOlderSnapshot(Offset, InputSnap, Snap);
            if (!OptOffset.hasValue())
              return None;

            // Check that the new and old offset still point to the same token.
            StringRef NewTok = getSourceToken(Offset, InputSnap);
            if (NewTok.empty())
              return None;
            if (NewTok == getSourceToken(OptOffset.getValue(), Snap))
              return OptOffset;

            return None;
          }
        }
        return None;
      };

      auto OldOffsetOpt = mappedBackOffset();
      if (OldOffsetOpt.hasValue()) {
        Offset = *OldOffsetOpt;
        PreviousASTSnaps.append(Snapshots.begin(), Snapshots.end());
        LOG_INFO_FUNC(High, "will try existing AST");
        return true;
      }

      LOG_INFO_FUNC(High, "will resolve using up-to-date AST");
      return false;
    }

    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      auto &CompIns = AstUnit->getCompilerInstance();
      Module *MainModule = CompIns.getMainModule();

      unsigned BufferID = AstUnit->getPrimarySourceFile().getBufferID().getValue();
      SourceLoc Loc =
        Lexer::getLocForStartOfToken(CompIns.getSourceMgr(), BufferID, Offset);
      if (Loc.isInvalid()) {
        Receiver({});
        return;
      }

      trace::TracedOperation TracedOp;
      if (trace::enabled()) {
        trace::SwiftInvocation SwiftArgs;
        ASTInvok->raw(SwiftArgs.Args.Args, SwiftArgs.Args.PrimaryFile);
        trace::initTraceFiles(SwiftArgs, CompIns);
        TracedOp.start(trace::OperationKind::CursorInfoForSource, SwiftArgs,
                       {std::make_pair("Offset", std::to_string(Offset))});
      }

      SemaLocResolver Resolver(AstUnit->getPrimarySourceFile());
      SemaToken SemaTok = Resolver.resolve(Loc);
      if (SemaTok.isInvalid()) {
        Receiver({});
        return;
      }

      CompilerInvocation CompInvok;
      ASTInvok->applyTo(CompInvok);

      if (SemaTok.Mod) {
        passCursorInfoForModule(SemaTok.Mod, Lang.getIFaceGenContexts(),
                                CompInvok, Receiver);
      } else {
        ValueDecl *VD = SemaTok.CtorTyRef ? SemaTok.CtorTyRef : SemaTok.ValueD;
        bool Failed = passCursorInfoForDecl(VD, MainModule, SemaTok.Ty,
                                            SemaTok.IsRef, BufferID, Lang,
                                            CompInvok, PreviousASTSnaps,
                                            Receiver);
        if (Failed) {
          if (!PreviousASTSnaps.empty()) {
            // Attempt again using the up-to-date AST.
            resolveCursor(Lang, InputFile, Offset, ASTInvok,
                          /*TryExistingAST=*/false, Receiver);
          } else {
            Receiver({});
          }
        }
      }
    }

    void cancelled() override {
      CursorInfo Info;
      Info.IsCancelled = true;
      Receiver(Info);
    }

    void failed(StringRef Error) override {
      LOG_WARN_FUNC("cursor info failed: " << Error);
      Receiver({});
    }
  };

  auto Consumer = std::make_shared<CursorInfoConsumer>(
      InputFile, Offset, Lang, Invok, TryExistingAST, Receiver);
  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  Lang.getASTManager().processASTAsync(Invok, std::move(Consumer), &OncePerASTToken);
}

void SwiftLangSupport::getCursorInfo(
    StringRef InputFile, unsigned Offset,
    ArrayRef<const char *> Args,
    std::function<void(const CursorInfo &)> Receiver) {

  if (auto IFaceGenRef = IFaceGenContexts.get(InputFile)) {
    trace::TracedOperation TracedOp;
    if (trace::enabled()) {
      trace::SwiftInvocation SwiftArgs;
      trace::initTraceInfo(SwiftArgs, InputFile, Args);
      // Do we need to record any files? If yes -- which ones?
      trace::StringPairs OpArgs {
        std::make_pair("DocumentName", IFaceGenRef->getDocumentName()),
        std::make_pair("ModuleOrHeaderName", IFaceGenRef->getModuleOrHeaderName()),
        std::make_pair("Offset", std::to_string(Offset))};
      TracedOp.start(trace::OperationKind::CursorInfoForIFaceGen,
                     SwiftArgs, OpArgs);
    }

    SwiftInterfaceGenContext::ResolvedEntity Entity;
    Entity = IFaceGenRef->resolveEntityForOffset(Offset);
    if (Entity.isResolved()) {
      CompilerInvocation Invok;
      IFaceGenRef->applyTo(Invok);
      if (Entity.Mod) {
        passCursorInfoForModule(Entity.Mod, IFaceGenContexts, Invok, Receiver);
      } else {
        // FIXME: Should pass the main module for the interface but currently
        // it's not necessary.
        passCursorInfoForDecl(Entity.Dcl, /*MainModule*/nullptr, Type(),
                              Entity.IsRef,
                              /*OrigBufferID=*/None, *this, Invok,
                              {}, Receiver);
      }
    } else {
      Receiver({});
    }
    return;
  }

  std::string Error;
  SwiftInvocationRef Invok = ASTMgr->getInvocation(Args, InputFile, Error);
  if (!Invok) {
    // FIXME: Report it as failed request.
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << Error);
    Receiver({});
    return;
  }

  resolveCursor(*this, InputFile, Offset, Invok, /*TryExistingAST=*/true,
                Receiver);
}

//===----------------------------------------------------------------------===//
// SwiftLangSupport::findUSRRange
//===----------------------------------------------------------------------===//

llvm::Optional<std::pair<unsigned, unsigned>>
SwiftLangSupport::findUSRRange(StringRef DocumentName, StringRef USR) {
  if (auto IFaceGenRef = IFaceGenContexts.get(DocumentName))
    return IFaceGenRef->findUSRRange(USR);

  // Only works for a module interface document currently.
  // FIXME: Report it as failed request.
  return None;
}

//===----------------------------------------------------------------------===//
// SwiftLangSupport::findRelatedIdentifiersInFile
//===----------------------------------------------------------------------===//

namespace {
class RelatedIdScanner : public ide::SourceEntityWalker {
  ValueDecl *Dcl;
  llvm::SmallVectorImpl<std::pair<unsigned, unsigned>> &Ranges;
  SourceManager &SourceMgr;
  unsigned BufferID = -1;
  bool Cancelled = false;

public:
  explicit RelatedIdScanner(SourceFile &SrcFile, unsigned BufferID,
                            ValueDecl *D,
      llvm::SmallVectorImpl<std::pair<unsigned, unsigned>> &Ranges)
    : Dcl(D), Ranges(Ranges),
      SourceMgr(SrcFile.getASTContext().SourceMgr),
      BufferID(BufferID) {
  }

private:
  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    if (Cancelled)
      return false;
    if (D == Dcl)
      return passId(Range);
    return true;
  }
  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, Type T) override {
    if (Cancelled)
      return false;
    if (CtorTyRef)
      D = CtorTyRef;
    if (D == Dcl)
      return passId(Range);
    return true;
  }

  bool passId(CharSourceRange Range) {
    unsigned Offset = SourceMgr.getLocOffsetInBuffer(Range.getStart(),BufferID);
    Ranges.push_back({ Offset, Range.getByteLength() });
    return !Cancelled;
  }
};

}

void SwiftLangSupport::findRelatedIdentifiersInFile(
    StringRef InputFile, unsigned Offset,
    ArrayRef<const char *> Args,
    std::function<void(const RelatedIdentsInfo &)> Receiver) {

  std::string Error;
  SwiftInvocationRef Invok = ASTMgr->getInvocation(Args, InputFile, Error);
  if (!Invok) {
    // FIXME: Report it as failed request.
    LOG_WARN_FUNC("failed to create an ASTInvocation: " << Error);
    Receiver({});
    return;
  }

  class RelatedIdConsumer : public SwiftASTConsumer {
    unsigned Offset;
    std::function<void(const RelatedIdentsInfo &)> Receiver;
    SwiftInvocationRef Invok;

  public:
    RelatedIdConsumer(unsigned Offset,
                      std::function<void(const RelatedIdentsInfo &)> Receiver,
                      SwiftInvocationRef Invok)
      : Offset(Offset), Receiver(std::move(Receiver)), Invok(Invok) { }

    void handlePrimaryAST(ASTUnitRef AstUnit) override {
      auto &CompInst = AstUnit->getCompilerInstance();
      auto &SrcFile = AstUnit->getPrimarySourceFile();

      trace::TracedOperation TracedOp;

      SmallVector<std::pair<unsigned, unsigned>, 8> Ranges;

      auto Action = [&]() {
        if (trace::enabled()) {
          trace::SwiftInvocation SwiftArgs;
          Invok->raw(SwiftArgs.Args.Args, SwiftArgs.Args.PrimaryFile);
          trace::initTraceFiles(SwiftArgs, CompInst);
          TracedOp.start(trace::OperationKind::RelatedIdents, SwiftArgs,
                        {std::make_pair("Offset", std::to_string(Offset))});
        }

        unsigned BufferID = SrcFile.getBufferID().getValue();
        SourceLoc Loc =
          Lexer::getLocForStartOfToken(CompInst.getSourceMgr(), BufferID, Offset);
        if (Loc.isInvalid())
          return;

        SemaLocResolver Resolver(SrcFile);
        SemaToken SemaTok = Resolver.resolve(Loc);
        if (SemaTok.isInvalid())
          return;
        if (SemaTok.IsKeywordArgument)
          return;

        ValueDecl *VD = SemaTok.CtorTyRef ? SemaTok.CtorTyRef : SemaTok.ValueD;
        if (!VD)
          return; // This was a module reference.

        // Only accept pointing to an identifier.
        if (!SemaTok.IsRef &&
            (isa<ConstructorDecl>(VD) ||
             isa<DestructorDecl>(VD) ||
             isa<SubscriptDecl>(VD)))
          return;
        if (VD->getName().isOperator())
          return;

        RelatedIdScanner Scanner(SrcFile, BufferID, VD, Ranges);
        if (DeclContext *LocalDC = VD->getDeclContext()->getLocalContext()) {
          Scanner.walk(LocalDC);
        } else {
          Scanner.walk(SrcFile);
        }
      };
      Action();

      RelatedIdentsInfo Info;
      Info.Ranges = Ranges;
      Receiver(Info);
    }

    void cancelled() override {
      RelatedIdentsInfo Info;
      Info.IsCancelled = true;
      Receiver(Info);
    }

    void failed(StringRef Error) override {
      LOG_WARN_FUNC("related idents failed: " << Error);
      Receiver({});
    }
  };

  auto Consumer = std::make_shared<RelatedIdConsumer>(Offset, Receiver, Invok);
  /// FIXME: When request cancellation is implemented and Xcode adopts it,
  /// don't use 'OncePerASTToken'.
  static const char OncePerASTToken = 0;
  ASTMgr->processASTAsync(Invok, std::move(Consumer), &OncePerASTToken);
}
