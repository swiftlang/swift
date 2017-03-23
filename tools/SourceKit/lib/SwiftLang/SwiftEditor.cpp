//===--- SwiftEditor.cpp --------------------------------------------------===//
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

#include "SwiftASTManager.h"
#include "SwiftEditorDiagConsumer.h"
#include "SwiftLangSupport.h"
#include "SourceKit/Core/Context.h"
#include "SourceKit/Core/NotificationCenter.h"
#include "SourceKit/Support/ImmutableTextBuffer.h"
#include "SourceKit/Support/Logging.h"
#include "SourceKit/Support/Tracing.h"
#include "SourceKit/Support/UIdent.h"

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/SourceEntityWalker.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CodeCompletion.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDE/Formatting.h"
#include "swift/IDE/SyntaxModel.h"
#include "swift/Subsystems.h"

#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Mutex.h"

using namespace SourceKit;
using namespace swift;
using namespace ide;

void EditorDiagConsumer::handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                                          DiagnosticKind Kind, StringRef Text,
                                          const DiagnosticInfo &Info) {
  if (Kind == DiagnosticKind::Error) {
    HadAnyError = true;
  }

  // Filter out lexer errors for placeholders.
  if (Info.ID == diag::lex_editor_placeholder.ID)
    return;

  if (Loc.isInvalid()) {
    if (Kind == DiagnosticKind::Error)
      HadInvalidLocError = true;
    clearLastDiag();
    return;
  }
  bool IsNote = (Kind == DiagnosticKind::Note);

  if (IsNote && !haveLastDiag())
    // Is this possible?
    return;

  DiagnosticEntryInfo SKInfo;

  SKInfo.Description = Text;

  unsigned BufferID = SM.findBufferContainingLoc(Loc);

  if (!isInputBufferID(BufferID)) {
    if (Info.ID == diag::error_from_clang.ID ||
        Info.ID == diag::warning_from_clang.ID ||
        Info.ID == diag::note_from_clang.ID) {
      // Handle it as other diagnostics.
    } else {
      if (!IsNote) {
        LOG_WARN_FUNC("got swift diagnostic not pointing at input file, "
                      "buffer name: " << SM.getIdentifierForBuffer(BufferID));
        return;
      }

      // FIXME: This is a note pointing to a synthesized declaration buffer for
      // a declaration coming from a module.
      // We should include the Decl* in the DiagnosticInfo and have a way for
      // Xcode to handle this "points-at-a-decl-from-module" location.
      //
      // For now instead of ignoring it, pick up the declaration name from the
      // buffer identifier and append it to the diagnostic message.
      auto &LastDiag = getLastDiag();
      SKInfo.Description += " (";
      SKInfo.Description += SM.getIdentifierForBuffer(BufferID);
      SKInfo.Description += ")";
      SKInfo.Offset = LastDiag.Offset;
      SKInfo.Line = LastDiag.Line;
      SKInfo.Column = LastDiag.Column;
      SKInfo.Filename = LastDiag.Filename;
      LastDiag.Notes.push_back(std::move(SKInfo));
      return;
    }
  }

  SKInfo.Offset = SM.getLocOffsetInBuffer(Loc, BufferID);
  std::tie(SKInfo.Line, SKInfo.Column) = SM.getLineAndColumn(Loc, BufferID);
  SKInfo.Filename = SM.getIdentifierForBuffer(BufferID);

  for (auto R : Info.Ranges) {
    if (R.isInvalid() || SM.findBufferContainingLoc(R.getStart()) != BufferID)
      continue;
    unsigned Offset = SM.getLocOffsetInBuffer(R.getStart(), BufferID);
    unsigned Length = R.getByteLength();
    SKInfo.Ranges.push_back({ Offset, Length });
  }

  for (auto F : Info.FixIts) {
    if (F.getRange().isInvalid() ||
        SM.findBufferContainingLoc(F.getRange().getStart()) != BufferID)
      continue;
    unsigned Offset = SM.getLocOffsetInBuffer(F.getRange().getStart(),
                                              BufferID);
    unsigned Length = F.getRange().getByteLength();
    SKInfo.Fixits.push_back({ Offset, Length, F.getText() });
  }

  if (IsNote) {
    getLastDiag().Notes.push_back(std::move(SKInfo));
    return;
  }

  DiagnosticsTy &Diagnostics = BufferDiagnostics[BufferID];

  switch (Kind) {
    case DiagnosticKind::Error:
      SKInfo.Severity = DiagnosticSeverityKind::Error;
      break;
    case DiagnosticKind::Warning:
      SKInfo.Severity = DiagnosticSeverityKind::Warning;
      break;
    case DiagnosticKind::Note:
      llvm_unreachable("already covered");
  }

  if (Diagnostics.empty() || Diagnostics.back().Offset <= SKInfo.Offset) {
    Diagnostics.push_back(std::move(SKInfo));
    LastDiagBufferID = BufferID;
    LastDiagIndex = Diagnostics.size() - 1;
    return;
  }

  // Keep the diagnostics array in source order.
  auto Pos = std::lower_bound(Diagnostics.begin(), Diagnostics.end(), SKInfo.Offset,
    [&](const DiagnosticEntryInfo &LHS, unsigned Offset) -> bool {
      return LHS.Offset < Offset;
    });
  LastDiagBufferID = BufferID;
  LastDiagIndex = Pos - Diagnostics.begin();
  Diagnostics.insert(Pos, std::move(SKInfo));
}

SwiftEditorDocumentRef
SwiftEditorDocumentFileMap::getByUnresolvedName(StringRef FilePath) {
  SwiftEditorDocumentRef EditorDoc;

  Queue.dispatchSync([&]{
    auto It = Docs.find(FilePath);
    if (It != Docs.end())
      EditorDoc = It->second.DocRef;
   });

  return EditorDoc;
}

SwiftEditorDocumentRef
SwiftEditorDocumentFileMap::findByPath(StringRef FilePath) {
  SwiftEditorDocumentRef EditorDoc;

  std::string ResolvedPath = SwiftLangSupport::resolvePathSymlinks(FilePath);
  Queue.dispatchSync([&]{
    for (auto &Entry : Docs) {
      if (Entry.getKey() == FilePath ||
          Entry.getValue().ResolvedPath == ResolvedPath) {
        EditorDoc = Entry.getValue().DocRef;
        break;
      }
    }
  });

  return EditorDoc;
}

bool SwiftEditorDocumentFileMap::getOrUpdate(
    StringRef FilePath, SwiftLangSupport &LangSupport,
    SwiftEditorDocumentRef &EditorDoc) {

  bool found = false;

  std::string ResolvedPath = SwiftLangSupport::resolvePathSymlinks(FilePath);
  Queue.dispatchBarrierSync([&]{
    DocInfo &Doc = Docs[FilePath];
    if (!Doc.DocRef) {
      Doc.DocRef = EditorDoc;
      Doc.ResolvedPath = ResolvedPath;
    } else {
      EditorDoc = Doc.DocRef;
      found = true;
    }
  });

  return found;
}

SwiftEditorDocumentRef SwiftEditorDocumentFileMap::remove(StringRef FilePath) {
  SwiftEditorDocumentRef Removed;
  Queue.dispatchBarrierSync([&]{
    auto I = Docs.find(FilePath);
    if (I != Docs.end()) {
      Removed = I->second.DocRef;
      Docs.erase(I);
    }
  });
  return Removed;
}

namespace {

/// Merges two overlapping ranges and splits the first range into two
/// ranges before and after the overlapping range.
void mergeSplitRanges(unsigned Off1, unsigned Len1, unsigned Off2, unsigned Len2,
                      std::function<void(unsigned BeforeOff, unsigned BeforeLen,
                                         unsigned AfterOff,
                                         unsigned AfterLen)> applier) {
  unsigned End1 = Off1 + Len1;
  unsigned End2 = Off2 + Len2;
  if (End1 > Off2) {
    // Overlapping. Split into before and after ranges.
    unsigned BeforeOff = Off1;
    unsigned BeforeLen = Off2 > Off1 ? Off2 - Off1 : 0;
    unsigned AfterOff = End2;
    unsigned AfterLen = End1 > End2 ? End1 - End2 : 0;
    applier(BeforeOff, BeforeLen, AfterOff, AfterLen);
  }
  else {
    // Not overlapping.
    applier(Off1, Len1, 0, 0);
  }
}


struct SwiftSyntaxToken {
  unsigned Column;
  unsigned Length:24;
  SyntaxNodeKind Kind:8;

  SwiftSyntaxToken(unsigned Column, unsigned Length,
                   SyntaxNodeKind Kind)
    :Column(Column), Length(Length), Kind(Kind) { }
};

class SwiftSyntaxMap {
  typedef std::vector<SwiftSyntaxToken> SwiftSyntaxLineMap;
  std::vector<SwiftSyntaxLineMap> Lines;

public:
  bool matchesFirstTokenOnLine(unsigned Line,
                               const SwiftSyntaxToken &Token) const {
    assert(Line > 0);
    if (Lines.size() < Line)
      return false;

    unsigned LineOffset = Line - 1;
    const SwiftSyntaxLineMap &LineMap = Lines[LineOffset];
    if (LineMap.empty())
      return false;

    const SwiftSyntaxToken &Tok = LineMap.front();
    if (Tok.Column == Token.Column && Tok.Length == Token.Length
        && Tok.Kind == Token.Kind) {
      return true;
    }

    return false;
  }

  void addTokenForLine(unsigned Line, const SwiftSyntaxToken &Token) {
    assert(Line > 0);
    if (Lines.size() < Line) {
      Lines.resize(Line);
    }
    unsigned LineOffset = Line - 1;
    SwiftSyntaxLineMap &LineMap = Lines[LineOffset];
    // FIXME: Assert this token is after the last one
    LineMap.push_back(Token);
  }

  void mergeTokenForLine(unsigned Line, const SwiftSyntaxToken &Token) {
    assert(Line > 0);
    if (Lines.size() < Line) {
      Lines.resize(Line);
    }
    unsigned LineOffset = Line - 1;
    SwiftSyntaxLineMap &LineMap = Lines[LineOffset];
    if (!LineMap.empty()) {
      auto &LastTok = LineMap.back();
      mergeSplitRanges(LastTok.Column, LastTok.Length,
                       Token.Column, Token.Length,
                       [&](unsigned BeforeOff, unsigned BeforeLen,
                           unsigned AfterOff, unsigned AfterLen) {
        auto LastKind = LastTok.Kind;
        LineMap.pop_back();
        if (BeforeLen)
          LineMap.emplace_back(BeforeOff, BeforeLen, LastKind);
        LineMap.push_back(Token);
        if (AfterLen)
          LineMap.emplace_back(AfterOff, AfterLen, LastKind);
      });
    }
    else {
      // Not overlapping, just add the new token to the end
      LineMap.push_back(Token);
    }
  }

  void clearLineRange(unsigned StartLine, unsigned Length) {
    assert(StartLine > 0);
    unsigned LineOffset = StartLine - 1;
    for (unsigned Line = LineOffset; Line < LineOffset + Length
                                    && Line < Lines.size(); ++Line) {
      Lines[Line].clear();
    }
  }

  void removeLineRange(unsigned StartLine, unsigned Length) {
    assert(StartLine > 0 && Length > 0);

    if (StartLine < Lines.size()) {
      unsigned EndLine = StartLine + Length - 1;
      // Delete all syntax map data from start line through end line
      Lines.erase(Lines.begin() + StartLine - 1,
                  EndLine >= Lines.size() ? Lines.end()
                                          : Lines.begin() + EndLine);
    }
  }

  void insertLineRange(unsigned StartLine, unsigned Length) {
    Lines.insert(StartLine <= Lines.size() ? Lines.begin() + StartLine - 1
                                           : Lines.end(),
                 Length, SwiftSyntaxLineMap());
  }

  void reset() {
    Lines.clear();
  }
};

struct EditorConsumerSyntaxMapEntry {
  unsigned Offset;
  unsigned Length;
  UIdent Kind;
  EditorConsumerSyntaxMapEntry(unsigned Offset, unsigned Length, UIdent Kind)
    :Offset(Offset), Length(Length), Kind(Kind) { }
};

typedef std::pair<unsigned, unsigned> SwiftEditorCharRange;

struct SwiftSemanticToken {
  unsigned ByteOffset;
  unsigned Length : 24;
  // The code-completion kinds are a good match for the semantic kinds we want.
  // FIXME: Maybe rename CodeCompletionDeclKind to a more general concept ?
  CodeCompletionDeclKind Kind : 6;
  unsigned IsRef : 1;
  unsigned IsSystem : 1;

  SwiftSemanticToken(CodeCompletionDeclKind Kind,
                     unsigned ByteOffset, unsigned Length,
                     bool IsRef, bool IsSystem)
    : ByteOffset(ByteOffset), Length(Length), Kind(Kind),
      IsRef(IsRef), IsSystem(IsSystem) { }

  bool getIsRef() const { return static_cast<bool>(IsRef); }

  bool getIsSystem() const { return static_cast<bool>(IsSystem); }

  UIdent getUIdentForKind() const {
    return SwiftLangSupport::getUIDForCodeCompletionDeclKind(Kind, getIsRef());
  }
};
static_assert(sizeof(SwiftSemanticToken) == 8, "Too big");

class SwiftDocumentSemanticInfo :
    public ThreadSafeRefCountedBase<SwiftDocumentSemanticInfo> {

  const std::string Filename;
  SwiftASTManager &ASTMgr;
  NotificationCenter &NotificationCtr;
  ThreadSafeRefCntPtr<SwiftInvocation> InvokRef;
  std::string CompilerArgsError;

  uint64_t ASTGeneration = 0;
  ImmutableTextSnapshotRef TokSnapshot;
  std::vector<SwiftSemanticToken> SemaToks;

  ImmutableTextSnapshotRef DiagSnapshot;
  std::vector<DiagnosticEntryInfo> SemaDiags;

  mutable llvm::sys::Mutex Mtx;

public:
  SwiftDocumentSemanticInfo(StringRef Filename, SwiftLangSupport &LangSupport)
    : Filename(Filename),
      ASTMgr(LangSupport.getASTManager()),
      NotificationCtr(LangSupport.getContext().getNotificationCenter()) {}

  SwiftInvocationRef getInvocation() const {
    return InvokRef;
  }

  uint64_t getASTGeneration() const;

  void setCompilerArgs(ArrayRef<const char *> Args) {
    InvokRef = ASTMgr.getInvocation(Args, Filename, CompilerArgsError);
  }

  void readSemanticInfo(ImmutableTextSnapshotRef NewSnapshot,
                        std::vector<SwiftSemanticToken> &Tokens,
                        std::vector<DiagnosticEntryInfo> &Diags,
                        ArrayRef<DiagnosticEntryInfo> ParserDiags);

  void processLatestSnapshotAsync(EditableTextBufferRef EditableBuffer);

  void updateSemanticInfo(std::vector<SwiftSemanticToken> Toks,
                          std::vector<DiagnosticEntryInfo> Diags,
                          ImmutableTextSnapshotRef Snapshot,
                          uint64_t ASTGeneration);
  void removeCachedAST() {
    if (InvokRef)
      ASTMgr.removeCachedAST(InvokRef);
  }

private:
  std::vector<SwiftSemanticToken> takeSemanticTokens(
      ImmutableTextSnapshotRef NewSnapshot);

  std::vector<DiagnosticEntryInfo> getSemanticDiagnostics(
      ImmutableTextSnapshotRef NewSnapshot,
      ArrayRef<DiagnosticEntryInfo> ParserDiags);
};

class SwiftDocumentSyntaxInfo {
  SourceManager SM;
  EditorDiagConsumer DiagConsumer;
  std::unique_ptr<ParserUnit> Parser;
  unsigned BufferID;
  std::vector<std::string> Args;
  std::string PrimaryFile;

public:
  SwiftDocumentSyntaxInfo(const CompilerInvocation &CompInv,
                          ImmutableTextSnapshotRef Snapshot,
                          std::vector<std::string> &Args,
                          StringRef FilePath)
        : Args(Args), PrimaryFile(FilePath) {

    std::unique_ptr<llvm::MemoryBuffer> BufCopy =
      llvm::MemoryBuffer::getMemBufferCopy(
        Snapshot->getBuffer()->getText(), FilePath);

    BufferID = SM.addNewSourceBuffer(std::move(BufCopy));
    SM.setHashbangBufferID(BufferID);
    DiagConsumer.setInputBufferIDs(BufferID);

    Parser.reset(
      new ParserUnit(SM, BufferID,
                     CompInv.getLangOptions(),
                     CompInv.getModuleName())
    );

    Parser->getDiagnosticEngine().addConsumer(DiagConsumer);
  }

  void initArgsAndPrimaryFile(trace::SwiftInvocation &Info) {
    Info.Args.PrimaryFile = PrimaryFile;
    Info.Args.Args = Args;
  }

  void parse() {
    auto &P = Parser->getParser();

    trace::TracedOperation TracedOp;
    if (trace::enabled()) {
      trace::SwiftInvocation Info;
      initArgsAndPrimaryFile(Info);
      auto Text = SM.getLLVMSourceMgr().getMemoryBuffer(BufferID)->getBuffer();
      Info.Files.push_back(std::make_pair(PrimaryFile, Text));
      TracedOp.start(trace::OperationKind::SimpleParse, Info);
    }

    bool Done = false;
    while (!Done) {
      P.parseTopLevel();
      Done = P.Tok.is(tok::eof);
    }
  }

  SourceFile &getSourceFile() {
    return Parser->getSourceFile();
  }

  unsigned getBufferID() {
    return BufferID;
  }

  const LangOptions &getLangOptions() {
    return Parser->getLangOptions();
  }

  SourceManager &getSourceManager() {
    return SM;
  }

  ArrayRef<DiagnosticEntryInfo> getDiagnostics() {
    return DiagConsumer.getDiagnosticsForBuffer(BufferID);
  }
};

} // anonymous namespace

uint64_t SwiftDocumentSemanticInfo::getASTGeneration() const {
  llvm::sys::ScopedLock L(Mtx);
  return ASTGeneration;
}

void SwiftDocumentSemanticInfo::readSemanticInfo(
    ImmutableTextSnapshotRef NewSnapshot,
    std::vector<SwiftSemanticToken> &Tokens,
    std::vector<DiagnosticEntryInfo> &Diags,
    ArrayRef<DiagnosticEntryInfo> ParserDiags) {

  llvm::sys::ScopedLock L(Mtx);

  Tokens = takeSemanticTokens(NewSnapshot);
  Diags = getSemanticDiagnostics(NewSnapshot, ParserDiags);
}

std::vector<SwiftSemanticToken>
SwiftDocumentSemanticInfo::takeSemanticTokens(
    ImmutableTextSnapshotRef NewSnapshot) {

  llvm::sys::ScopedLock L(Mtx);

  if (SemaToks.empty())
    return {};

  // Adjust the position of the tokens.
  TokSnapshot->foreachReplaceUntil(NewSnapshot,
    [&](ReplaceImmutableTextUpdateRef Upd) -> bool {
      if (SemaToks.empty())
        return false;

      auto ReplaceBegin = std::lower_bound(SemaToks.begin(), SemaToks.end(),
          Upd->getByteOffset(),
          [&](const SwiftSemanticToken &Tok, unsigned StartOffset) -> bool {
            return Tok.ByteOffset+Tok.Length < StartOffset;
          });

      std::vector<SwiftSemanticToken>::iterator ReplaceEnd;
      if (Upd->getLength() == 0) {
        ReplaceEnd = ReplaceBegin;
      } else {
        ReplaceEnd = std::upper_bound(ReplaceBegin, SemaToks.end(),
            Upd->getByteOffset() + Upd->getLength(),
            [&](unsigned EndOffset, const SwiftSemanticToken &Tok) -> bool {
              return EndOffset < Tok.ByteOffset;
            });
      }

      unsigned InsertLen = Upd->getText().size();
      int Delta = InsertLen - Upd->getLength();
      if (Delta != 0) {
        for (std::vector<SwiftSemanticToken>::iterator
               I = ReplaceEnd, E = SemaToks.end(); I != E; ++I)
          I->ByteOffset += Delta;
      }
      SemaToks.erase(ReplaceBegin, ReplaceEnd);
      return true;
    });

  return std::move(SemaToks);
}

static bool
adjustDiagnosticRanges(SmallVectorImpl<std::pair<unsigned, unsigned>> &Ranges,
                       unsigned ByteOffset, unsigned RemoveLen, int Delta) {
  for (auto &Range : Ranges) {
    unsigned RangeBegin = Range.first;
    unsigned RangeEnd = Range.first +  Range.second;
    unsigned RemoveEnd = ByteOffset + RemoveLen;
    // If it intersects with the remove range, ignore the whole diagnostic.
    if (!(RangeEnd < ByteOffset || RangeBegin > RemoveEnd))
      return true; // Ignore.
    if (RangeBegin > RemoveEnd)
      Range.first += Delta;
  }
  return false;
}

static bool
adjustDiagnosticFixits(SmallVectorImpl<DiagnosticEntryInfo::Fixit> &Fixits,
                       unsigned ByteOffset, unsigned RemoveLen, int Delta) {
  for (auto &Fixit : Fixits) {
    unsigned FixitBegin = Fixit.Offset;
    unsigned FixitEnd = Fixit.Offset +  Fixit.Length;
    unsigned RemoveEnd = ByteOffset + RemoveLen;
    // If it intersects with the remove range, ignore the whole diagnostic.
    if (!(FixitEnd < ByteOffset || FixitBegin > RemoveEnd))
      return true; // Ignore.
    if (FixitBegin > RemoveEnd)
      Fixit.Offset += Delta;
  }
  return false;
}

static bool
adjustDiagnosticBase(DiagnosticEntryInfoBase &Diag,
                     unsigned ByteOffset, unsigned RemoveLen, int Delta) {
  if (Diag.Offset >= ByteOffset && Diag.Offset < ByteOffset+RemoveLen)
    return true; // Ignore.
  bool Ignore = adjustDiagnosticRanges(Diag.Ranges, ByteOffset, RemoveLen, Delta);
  if (Ignore)
    return true;
  Ignore = adjustDiagnosticFixits(Diag.Fixits, ByteOffset, RemoveLen, Delta);
  if (Ignore)
    return true;
  if (Diag.Offset > ByteOffset)
    Diag.Offset += Delta;
  return false;
}

static bool
adjustDiagnostic(DiagnosticEntryInfo &Diag, StringRef Filename,
                 unsigned ByteOffset, unsigned RemoveLen, int Delta) {
  for (auto &Note : Diag.Notes) {
    if (Filename != Note.Filename)
      continue;
    bool Ignore = adjustDiagnosticBase(Note, ByteOffset, RemoveLen, Delta);
    if (Ignore)
      return true;
  }
  return adjustDiagnosticBase(Diag, ByteOffset, RemoveLen, Delta);
}

static std::vector<DiagnosticEntryInfo>
adjustDiagnostics(std::vector<DiagnosticEntryInfo> Diags, StringRef Filename,
                  unsigned ByteOffset, unsigned RemoveLen, int Delta) {
  std::vector<DiagnosticEntryInfo> NewDiags;
  NewDiags.reserve(Diags.size());

  for (auto &Diag : Diags) {
    bool Ignore = adjustDiagnostic(Diag, Filename, ByteOffset, RemoveLen, Delta);
    if (!Ignore) {
      NewDiags.push_back(std::move(Diag));
    }
  }

  return NewDiags;
}

std::vector<DiagnosticEntryInfo>
SwiftDocumentSemanticInfo::getSemanticDiagnostics(
    ImmutableTextSnapshotRef NewSnapshot,
    ArrayRef<DiagnosticEntryInfo> ParserDiags) {

  llvm::sys::ScopedLock L(Mtx);

  if (SemaDiags.empty())
    return SemaDiags;

  assert(DiagSnapshot && "If we have diagnostics, we must have snapshot!");

  if (!DiagSnapshot->precedesOrSame(NewSnapshot)) {
    // It may happen that other thread has already updated the diagnostics to
    // the version *after* NewSnapshot. This can happen in at least two cases:
    //   (a) two or more editor.open or editor.replacetext queries are being
    //       processed concurrently (not valid, but possible call pattern)
    //   (b) while editor.replacetext processing is running, a concurrent
    //       thread executes getBuffer()/getBufferForSnapshot() on the same
    //       Snapshot/EditableBuffer (thus creating a new ImmutableTextBuffer)
    //       and updates DiagSnapshot/SemaDiags
    assert(NewSnapshot->precedesOrSame(DiagSnapshot));

    // Since we cannot "adjust back" diagnostics, we just return an empty set.
    // FIXME: add handling of the case#b above
    return {};
  }

  SmallVector<unsigned, 16> ParserDiagLines;
  for (auto Diag : ParserDiags)
    ParserDiagLines.push_back(Diag.Line);
  std::sort(ParserDiagLines.begin(), ParserDiagLines.end());

  auto hasParserDiagAtLine = [&](unsigned Line) {
    return std::binary_search(ParserDiagLines.begin(), ParserDiagLines.end(),
                              Line);
  };

  // Adjust the position of the diagnostics.
  DiagSnapshot->foreachReplaceUntil(NewSnapshot,
    [&](ReplaceImmutableTextUpdateRef Upd) -> bool {
      if (SemaDiags.empty())
        return false;

      unsigned ByteOffset = Upd->getByteOffset();
      unsigned RemoveLen = Upd->getLength();
      unsigned InsertLen = Upd->getText().size();
      int Delta = InsertLen - RemoveLen;
      SemaDiags = adjustDiagnostics(std::move(SemaDiags), Filename,
                                    ByteOffset, RemoveLen, Delta);
      return true;
    });

  if (!SemaDiags.empty()) {
    auto ImmBuf = NewSnapshot->getBuffer();
    for (auto &Diag : SemaDiags) {
      std::tie(Diag.Line, Diag.Column) = ImmBuf->getLineAndColumn(Diag.Offset);
    }

    // If there is a parser diagnostic in a line, ignore diagnostics in the same
    // line that we got from the semantic pass.
    // Note that the semantic pass also includes parser diagnostics so this
    // avoids duplicates.
    SemaDiags.erase(std::remove_if(SemaDiags.begin(), SemaDiags.end(),
                                   [&](const DiagnosticEntryInfo &Diag) -> bool {
                                     return hasParserDiagAtLine(Diag.Line);
                                   }),
                    SemaDiags.end());
  }

  DiagSnapshot = NewSnapshot;
  return SemaDiags;
}

void SwiftDocumentSemanticInfo::updateSemanticInfo(
    std::vector<SwiftSemanticToken> Toks,
    std::vector<DiagnosticEntryInfo> Diags,
    ImmutableTextSnapshotRef Snapshot,
    uint64_t ASTGeneration) {

  {
    llvm::sys::ScopedLock L(Mtx);
    if (ASTGeneration > this->ASTGeneration) {
      SemaToks = std::move(Toks);
      SemaDiags = std::move(Diags);
      TokSnapshot = DiagSnapshot = std::move(Snapshot);
      this->ASTGeneration = ASTGeneration;
    }
  }

  LOG_INFO_FUNC(High, "posted document update notification for: " << Filename);
  NotificationCtr.postDocumentUpdateNotification(Filename);
}

namespace {

class SemanticAnnotator : public SourceEntityWalker {
  SourceManager &SM;
  unsigned BufferID;
public:

  std::vector<SwiftSemanticToken> SemaToks;

  SemanticAnnotator(SourceManager &SM, unsigned BufferID)
    : SM(SM), BufferID(BufferID) {}

  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                          ReferenceMetaData Data) override {
    if (isa<VarDecl>(D) && D->hasName() && D->getName().str() == "self")
      return true;

    // Do not annotate references to unavailable decls.
    if (AvailableAttr::isUnavailable(D))
      return true;

    if (CtorTyRef)
      D = CtorTyRef;
    annotate(D, /*IsRef=*/true, Range);
    return true;
  }

  bool visitSubscriptReference(ValueDecl *D, CharSourceRange Range,
                               bool IsOpenBracket) override {
    // We should treat both open and close brackets equally
    return visitDeclReference(D, Range, nullptr, nullptr, Type(),
                              SemaReferenceKind::SubscriptRef);
  }

  void annotate(const Decl *D, bool IsRef, CharSourceRange Range) {
    unsigned ByteOffset = SM.getLocOffsetInBuffer(Range.getStart(), BufferID);
    unsigned Length = Range.getByteLength();
    auto Kind = CodeCompletionResult::getCodeCompletionDeclKind(D);
    bool IsSystem = D->getModuleContext()->isSystemModule();
    SemaToks.emplace_back(Kind, ByteOffset, Length, IsRef, IsSystem);
  }
};

} // anonymous namespace

namespace {

class AnnotAndDiagASTConsumer : public SwiftASTConsumer {
  EditableTextBufferRef EditableBuffer;
  RefPtr<SwiftDocumentSemanticInfo> SemaInfoRef;

public:
  std::vector<SwiftSemanticToken> SemaToks;

  AnnotAndDiagASTConsumer(EditableTextBufferRef EditableBuffer,
                          RefPtr<SwiftDocumentSemanticInfo> SemaInfoRef)
    : EditableBuffer(std::move(EditableBuffer)),
      SemaInfoRef(std::move(SemaInfoRef)) { }

  void failed(StringRef Error) override {
    LOG_WARN_FUNC("sema annotations failed: " << Error);
  }

  void handlePrimaryAST(ASTUnitRef AstUnit) override {
    auto Generation = AstUnit->getGeneration();
    auto &CompIns = AstUnit->getCompilerInstance();
    auto &Consumer = AstUnit->getEditorDiagConsumer();
    assert(Generation);

    if (Generation < SemaInfoRef->getASTGeneration()) {
      // It may happen that this request was waiting in async queue for
      // too long so another thread has already updated this sema with
      // ast generation bigger than ASTGeneration
      return;
    }

    ImmutableTextSnapshotRef DocSnapshot;
    for (auto &Snap : AstUnit->getSnapshots()) {
      if (Snap->getEditableBuffer() == EditableBuffer) {
        DocSnapshot = Snap;
        break;
      }
    }

    if (!DocSnapshot) {
      LOG_WARN_FUNC("did not find document snapshot when handling the AST");
      return;
    }

    if (Generation == SemaInfoRef->getASTGeneration()) {
      // Save time if we already know we processed this AST version.
      if (DocSnapshot->getStamp() != EditableBuffer->getSnapshot()->getStamp()){
        // Handle edits that occurred after we processed the AST.
        SemaInfoRef->processLatestSnapshotAsync(EditableBuffer);
      }
      return;
    }

    if (!AstUnit->getPrimarySourceFile().getBufferID().hasValue()) {
      LOG_WARN_FUNC("Primary SourceFile is expected to have a BufferID");
      return;
    }
    unsigned BufferID = AstUnit->getPrimarySourceFile().getBufferID().getValue();

    trace::TracedOperation TracedOp;
    if (trace::enabled()) {
      trace::SwiftInvocation SwiftArgs;
      SemaInfoRef->getInvocation()->raw(SwiftArgs.Args.Args,
                                        SwiftArgs.Args.PrimaryFile);
      trace::initTraceFiles(SwiftArgs, CompIns);
      TracedOp.start(trace::OperationKind::AnnotAndDiag, SwiftArgs);
    }

    SemanticAnnotator Annotator(CompIns.getSourceMgr(), BufferID);
    Annotator.walk(AstUnit->getPrimarySourceFile());
    SemaToks = std::move(Annotator.SemaToks);

    TracedOp.finish();

    SemaInfoRef->
      updateSemanticInfo(std::move(SemaToks),
                     std::move(Consumer.getDiagnosticsForBuffer(BufferID)),
                         DocSnapshot,
                         Generation);

    if (DocSnapshot->getStamp() != EditableBuffer->getSnapshot()->getStamp()) {
      // Handle edits that occurred after we processed the AST.
      SemaInfoRef->processLatestSnapshotAsync(EditableBuffer);
    }
  }
};

} // anonymous namespace

void SwiftDocumentSemanticInfo::processLatestSnapshotAsync(
    EditableTextBufferRef EditableBuffer) {

  SwiftInvocationRef Invok = InvokRef;
  if (!Invok)
    return;

  RefPtr<SwiftDocumentSemanticInfo> SemaInfoRef = this;
  auto Consumer = std::make_shared<AnnotAndDiagASTConsumer>(EditableBuffer,
                                                            SemaInfoRef);

  // Semantic annotation queries for a particular document should cancel
  // previously queued queries for the same document. Each document has a
  // SwiftDocumentSemanticInfo pointer so use that for the token.
  const void *OncePerASTToken = SemaInfoRef.get();
  ASTMgr.processASTAsync(Invok, std::move(Consumer), OncePerASTToken);
}

struct SwiftEditorDocument::Implementation {
  SwiftLangSupport &LangSupport;
  const std::string FilePath;
  EditableTextBufferRef EditableBuffer;

  SwiftSyntaxMap SyntaxMap;
  LineRange EditedLineRange;
  SwiftEditorCharRange AffectedRange;

  std::vector<DiagnosticEntryInfo> ParserDiagnostics;
  RefPtr<SwiftDocumentSemanticInfo> SemanticInfo;
  CodeFormatOptions FormatOptions;

  std::shared_ptr<SwiftDocumentSyntaxInfo> SyntaxInfo;

  std::shared_ptr<SwiftDocumentSyntaxInfo> getSyntaxInfo() {
    llvm::sys::ScopedLock L(AccessMtx);
    return SyntaxInfo;
  }

  llvm::sys::Mutex AccessMtx;

  Implementation(StringRef FilePath, SwiftLangSupport &LangSupport,
                 CodeFormatOptions options)
    : LangSupport(LangSupport), FilePath(FilePath), FormatOptions(options) {
      SemanticInfo = new SwiftDocumentSemanticInfo(FilePath, LangSupport);
  }

  void buildSwiftInv(trace::SwiftInvocation &Inv);
};

void SwiftEditorDocument::Implementation::buildSwiftInv(
                                                  trace::SwiftInvocation &Inv) {
  if (SemanticInfo->getInvocation()) {
    std::string PrimaryFile; // Ignored, FilePath will be used
    SemanticInfo->getInvocation()->raw(Inv.Args.Args, PrimaryFile);
  }
  Inv.Args.PrimaryFile = FilePath;
  auto &SM = SyntaxInfo->getSourceManager();
  auto ID = SyntaxInfo->getBufferID();
  auto Text = SM.getLLVMSourceMgr().getMemoryBuffer(ID)->getBuffer();
  Inv.Files.push_back(std::make_pair(FilePath, Text));
}

namespace  {

static UIdent getAccessibilityUID(Accessibility Access) {
  static UIdent AccessOpen("source.lang.swift.accessibility.open");
  static UIdent AccessPublic("source.lang.swift.accessibility.public");
  static UIdent AccessInternal("source.lang.swift.accessibility.internal");
  static UIdent AccessFilePrivate("source.lang.swift.accessibility.fileprivate");
  static UIdent AccessPrivate("source.lang.swift.accessibility.private");

  switch (Access) {
  case Accessibility::Private:
    return AccessPrivate;
  case Accessibility::FilePrivate:
    return AccessFilePrivate;
  case Accessibility::Internal:
    return AccessInternal;
  case Accessibility::Public:
    return AccessPublic;
  case Accessibility::Open:
    return AccessOpen;
  }

  llvm_unreachable("Unhandled Accessibility in switch.");
}

static Accessibility inferDefaultAccessibility(const ExtensionDecl *ED) {
  if (ED->hasDefaultAccessibility())
    return ED->getDefaultAccessibility();

  if (auto *AA = ED->getAttrs().getAttribute<AccessibilityAttr>())
    return AA->getAccess();

  // Assume "internal", which is the most common thing anyway.
  return Accessibility::Internal;
}

/// If typechecking was performed we use the computed accessibility, otherwise
/// we fallback to inferring accessibility syntactically. This may not be as
/// accurate but it's only until we have typechecked the AST.
static Accessibility inferAccessibility(const ValueDecl *D) {
  assert(D);
  if (D->hasAccessibility())
    return D->getFormalAccess();

  // Check if the decl has an explicit accessibility attribute.
  if (auto *AA = D->getAttrs().getAttribute<AccessibilityAttr>())
    return AA->getAccess();

  DeclContext *DC = D->getDeclContext();
  switch (DC->getContextKind()) {
  case DeclContextKind::SerializedLocal:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::Initializer:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::SubscriptDecl:
    return Accessibility::Private;
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
    return Accessibility::Internal;
  case DeclContextKind::GenericTypeDecl: {
    auto Nominal = cast<GenericTypeDecl>(DC);
    Accessibility Access = inferAccessibility(Nominal);
    if (!isa<ProtocolDecl>(Nominal))
      Access = std::min(Access, Accessibility::Internal);
    return Access;
  }
  case DeclContextKind::ExtensionDecl:
    return inferDefaultAccessibility(cast<ExtensionDecl>(DC));
  }

  llvm_unreachable("Unhandled DeclContextKind in switch.");
}

static Optional<Accessibility>
inferSetterAccessibility(const AbstractStorageDecl *D) {
  if (auto *VD = dyn_cast<VarDecl>(D)) {
    if (VD->isLet())
      return None;
  }
  if (D->getGetter() && !D->getSetter())
    return None;

  // FIXME: Have the parser detect as read-only the syntactic form of generated
  // interfaces, which is "var foo : Int { get }"

  if (auto *AA = D->getAttrs().getAttribute<SetterAccessibilityAttr>())
    return AA->getAccess();
  else
    return inferAccessibility(D);
}

class SwiftDocumentStructureWalker: public ide::SyntaxModelWalker {
  SourceManager &SrcManager;
  EditorConsumer &Consumer;
  unsigned BufferID;

public:
  SwiftDocumentStructureWalker(SourceManager &SrcManager,
                               unsigned BufferID,
                               EditorConsumer &Consumer)
    : SrcManager(SrcManager), Consumer(Consumer), BufferID(BufferID) { }

  bool walkToSubStructurePre(SyntaxStructureNode Node) override {
    unsigned StartOffset = SrcManager.getLocOffsetInBuffer(Node.Range.getStart(),
                                                           BufferID);
    unsigned EndOffset = SrcManager.getLocOffsetInBuffer(Node.Range.getEnd(),
                                                         BufferID);
    unsigned NameStart;
    unsigned NameEnd;
    if (Node.NameRange.isValid()) {
      NameStart = SrcManager.getLocOffsetInBuffer(Node.NameRange.getStart(),
                                                  BufferID);
      NameEnd = SrcManager.getLocOffsetInBuffer(Node.NameRange.getEnd(),
                                                BufferID);
    }
    else {
      NameStart = NameEnd = 0;
    }

    unsigned BodyOffset;
    unsigned BodyEnd;
    if (Node.BodyRange.isValid()) {
      BodyOffset = SrcManager.getLocOffsetInBuffer(Node.BodyRange.getStart(),
                                                   BufferID);
      BodyEnd = SrcManager.getLocOffsetInBuffer(Node.BodyRange.getEnd(),
                                                BufferID);
    }
    else {
      BodyOffset = BodyEnd = 0;
    }

    UIdent Kind = SwiftLangSupport::getUIDForSyntaxStructureKind(Node.Kind);
    UIdent AccessLevel;
    UIdent SetterAccessLevel;
    if (Node.Kind != SyntaxStructureKind::Parameter) {
      if (auto *VD = dyn_cast_or_null<ValueDecl>(Node.Dcl)) {
        AccessLevel = getAccessibilityUID(inferAccessibility(VD));
      }
      if (auto *ASD = dyn_cast_or_null<AbstractStorageDecl>(Node.Dcl)) {
        Optional<Accessibility> SetAccess = inferSetterAccessibility(ASD);
        if (SetAccess.hasValue()) {
          SetterAccessLevel = getAccessibilityUID(SetAccess.getValue());
        }
      }
    }

    SmallVector<StringRef, 4> InheritedNames;
    if (!Node.InheritedTypeRanges.empty()) {
      for (auto &TR : Node.InheritedTypeRanges) {
        InheritedNames.push_back(SrcManager.extractText(TR));
      }
    }

    StringRef TypeName;
    if (Node.TypeRange.isValid()) {
      TypeName = SrcManager.extractText(Node.TypeRange);
    }

    SmallString<64> DisplayNameBuf;
    StringRef DisplayName;
    if (auto ValueD = dyn_cast_or_null<ValueDecl>(Node.Dcl)) {
      llvm::raw_svector_ostream OS(DisplayNameBuf);
      if (!SwiftLangSupport::printDisplayName(ValueD, OS))
        DisplayName = OS.str();
    }
    else if (Node.NameRange.isValid()) {
      DisplayName = SrcManager.extractText(Node.NameRange);
    }

    SmallString<64> RuntimeNameBuf;
    StringRef RuntimeName = getObjCRuntimeName(Node.Dcl, RuntimeNameBuf);

    SmallString<64> SelectorNameBuf;
    StringRef SelectorName = getObjCSelectorName(Node.Dcl, SelectorNameBuf);

    std::vector<UIdent> Attrs = SwiftLangSupport::UIDsFromDeclAttributes(Node.Attrs);

    Consumer.beginDocumentSubStructure(StartOffset, EndOffset - StartOffset,
                                       Kind, AccessLevel, SetterAccessLevel,
                                       NameStart, NameEnd - NameStart,
                                       BodyOffset, BodyEnd - BodyOffset,
                                       DisplayName,
                                       TypeName, RuntimeName,
                                       SelectorName,
                                       InheritedNames, Attrs);

    for (const auto &Elem : Node.Elements) {
      if (Elem.Range.isInvalid())
        continue;

      UIdent Kind = SwiftLangSupport::getUIDForSyntaxStructureElementKind(Elem.Kind);
      unsigned Offset = SrcManager.getLocOffsetInBuffer(Elem.Range.getStart(),
                                                        BufferID);
      unsigned Length = Elem.Range.getByteLength();
      Consumer.handleDocumentSubStructureElement(Kind, Offset, Length);
    }

    return true;
  }

  StringRef getObjCRuntimeName(const Decl *D, SmallString<64> &Buf) {
    if (!D)
      return StringRef();
    if (!isa<ClassDecl>(D) && !isa<ProtocolDecl>(D))
      return StringRef();
    // We don't support getting the runtime name for nested classes.
    // This would require typechecking or at least name lookup, if the nested
    // class is in an extension.
    if (!D->getDeclContext()->isModuleScopeContext())
      return StringRef();

    if (auto ClassD = dyn_cast<ClassDecl>(D)) {
      // We don't vend the runtime name for generic classes for now.
      if (ClassD->getGenericParams())
        return StringRef();
      return ClassD->getObjCRuntimeName(Buf);
    }
    return cast<ProtocolDecl>(D)->getObjCRuntimeName(Buf);
  }

  StringRef getObjCSelectorName(const Decl *D, SmallString<64> &Buf) {
    if (auto FuncD = dyn_cast_or_null<AbstractFunctionDecl>(D)) {
      // We only vend the selector name for @IBAction methods.
      if (FuncD->getAttrs().hasAttribute<IBActionAttr>())
        return FuncD->getObjCSelector().getString(Buf);
    }
    return StringRef();
  }

  bool walkToSubStructurePost(SyntaxStructureNode Node) override {
    Consumer.endDocumentSubStructure();
    return true;
  }

  bool walkToNodePre(SyntaxNode Node) override {
    if (Node.Kind != SyntaxNodeKind::CommentMarker)
      return false;

    unsigned StartOffset = SrcManager.getLocOffsetInBuffer(Node.Range.getStart(),
                                                           BufferID);
    unsigned EndOffset = SrcManager.getLocOffsetInBuffer(Node.Range.getEnd(),
                                                         BufferID);
    UIdent Kind = SwiftLangSupport::getUIDForSyntaxNodeKind(Node.Kind);
    Consumer.beginDocumentSubStructure(StartOffset, EndOffset - StartOffset,
                                       Kind, UIdent(), UIdent(), 0, 0,
                                       0, 0,
                                       StringRef(),
                                       StringRef(), StringRef(),
                                       StringRef(),
                                       {}, {});
    return true;
  }

  bool walkToNodePost(SyntaxNode Node) override {
    if (Node.Kind != SyntaxNodeKind::CommentMarker)
      return true;

    Consumer.endDocumentSubStructure();
    return true;
  }
};

class SwiftEditorSyntaxWalker: public ide::SyntaxModelWalker {
  SwiftSyntaxMap &SyntaxMap;
  LineRange EditedLineRange;
  SwiftEditorCharRange &AffectedRange;
  SourceManager &SrcManager;
  EditorConsumer &Consumer;
  unsigned BufferID;
  SwiftDocumentStructureWalker DocStructureWalker;
  std::vector<EditorConsumerSyntaxMapEntry> ConsumerSyntaxMap;
  unsigned NestingLevel = 0;
public:
  SwiftEditorSyntaxWalker(SwiftSyntaxMap &SyntaxMap,
                          LineRange EditedLineRange,
                          SwiftEditorCharRange &AffectedRange,
                          SourceManager &SrcManager, EditorConsumer &Consumer,
                          unsigned BufferID)
    : SyntaxMap(SyntaxMap), EditedLineRange(EditedLineRange),
      AffectedRange(AffectedRange), SrcManager(SrcManager), Consumer(Consumer),
      BufferID(BufferID),
      DocStructureWalker(SrcManager, BufferID, Consumer) { }

  bool walkToNodePre(SyntaxNode Node) override {
    if (Node.Kind == SyntaxNodeKind::CommentMarker)
      return DocStructureWalker.walkToNodePre(Node);

    ++NestingLevel;
    SourceLoc StartLoc = Node.Range.getStart();
    auto StartLineAndColumn = SrcManager.getLineAndColumn(StartLoc);
    auto EndLineAndColumn = SrcManager.getLineAndColumn(Node.Range.getEnd());
    unsigned StartLine = StartLineAndColumn.first;
    unsigned EndLine = EndLineAndColumn.second > 1 ? EndLineAndColumn.first
                                                   : EndLineAndColumn.first - 1;
    unsigned Offset = SrcManager.getByteDistance(
                           SrcManager.getLocForBufferStart(BufferID), StartLoc);
    // Note that the length can span multiple lines.
    unsigned Length = Node.Range.getByteLength();

    SwiftSyntaxToken Token(StartLineAndColumn.second, Length,
                           Node.Kind);
    if (EditedLineRange.isValid()) {
      if (StartLine < EditedLineRange.startLine()) {
        if (EndLine < EditedLineRange.startLine()) {
          // We're entirely before the edited range, no update needed.
          return true;
        }

        // This token starts before the edited range, but doesn't end before it,
        // we need to adjust edited line range and clear the affected syntax map
        // line range.
        unsigned AdjLineCount = EditedLineRange.startLine() - StartLine;
        EditedLineRange.setRange(StartLine, AdjLineCount
                                            + EditedLineRange.lineCount());
        SyntaxMap.clearLineRange(StartLine, AdjLineCount);

        // Also adjust the affected char range accordingly.
        unsigned AdjCharCount = AffectedRange.first - Offset;
        AffectedRange.first -= AdjCharCount;
        AffectedRange.second += AdjCharCount;
      }
      else if (Offset > AffectedRange.first + AffectedRange.second) {
        // We're passed the affected range and already synced up, just return.
        return true;
      }
      else if (StartLine > EditedLineRange.endLine()) {
        // We're after the edited line range, let's test if we're synced up.
        if (SyntaxMap.matchesFirstTokenOnLine(StartLine, Token)) {
          // We're synced up, mark the affected range and return.
          AffectedRange.second =
                 Offset - (StartLineAndColumn.second - 1) - AffectedRange.first;
          return true;
        }

        // We're not synced up, continue replacing syntax map data on this line.
        SyntaxMap.clearLineRange(StartLine, 1);
        EditedLineRange.extendToIncludeLine(StartLine);
      }

      if (EndLine > StartLine) {
        // The token spans multiple lines, make sure to replace syntax map data
        // for affected lines.
        EditedLineRange.extendToIncludeLine(EndLine);

        unsigned LineCount = EndLine - StartLine + 1;
        SyntaxMap.clearLineRange(StartLine, LineCount);
      }

    }

    // Add the syntax map token.
    if (NestingLevel > 1)
      SyntaxMap.mergeTokenForLine(StartLine, Token);
    else
      SyntaxMap.addTokenForLine(StartLine, Token);

    // Add consumer entry.
    unsigned ByteOffset = SrcManager.getLocOffsetInBuffer(Node.Range.getStart(),
                                                          BufferID);
    UIdent Kind = SwiftLangSupport::getUIDForSyntaxNodeKind(Node.Kind);
    if (NestingLevel > 1) {
      assert(!ConsumerSyntaxMap.empty());
      auto &Last = ConsumerSyntaxMap.back();
      mergeSplitRanges(Last.Offset, Last.Length, ByteOffset, Length,
                       [&](unsigned BeforeOff, unsigned BeforeLen,
                           unsigned AfterOff, unsigned AfterLen) {
        auto LastKind = Last.Kind;
        ConsumerSyntaxMap.pop_back();
        if (BeforeLen)
          ConsumerSyntaxMap.emplace_back(BeforeOff, BeforeLen, LastKind);
        ConsumerSyntaxMap.emplace_back(ByteOffset, Length, Kind);
        if (AfterLen)
          ConsumerSyntaxMap.emplace_back(AfterOff, AfterLen, LastKind);
      });
    }
    else
      ConsumerSyntaxMap.emplace_back(ByteOffset, Length, Kind);

    return true;
  }

  bool walkToNodePost(SyntaxNode Node) override {
    if (Node.Kind == SyntaxNodeKind::CommentMarker)
      return DocStructureWalker.walkToNodePost(Node);

    if (--NestingLevel == 0) {
      // We've unwound to the top level, so inform the consumer and drain
      // the consumer syntax map queue.
      for (auto &Entry: ConsumerSyntaxMap)
        Consumer.handleSyntaxMap(Entry.Offset, Entry.Length, Entry.Kind);
      ConsumerSyntaxMap.clear();
    }

    return true;
  }

  bool walkToSubStructurePre(SyntaxStructureNode Node) override {
    return DocStructureWalker.walkToSubStructurePre(Node);
  }

  bool walkToSubStructurePost(SyntaxStructureNode Node) override {
    return DocStructureWalker.walkToSubStructurePost(Node);
  }

};

class PlaceholderExpansionScanner {

public:
  struct Param {
    CharSourceRange NameRange;
    CharSourceRange TypeRange;
    Param(CharSourceRange NameRange, CharSourceRange TypeRange)
      :NameRange(NameRange), TypeRange(TypeRange) { }
  };

private:

  struct ClosureInfo {
    std::vector<Param> Params;
    CharSourceRange ReturnTypeRange;
  };

  SourceManager &SM;
  ClosureInfo TargetClosureInfo;
  EditorPlaceholderExpr *PHE = nullptr;

  class PlaceholderFinder: public ASTWalker {
    SourceLoc PlaceholderLoc;
    EditorPlaceholderExpr *&Found;

  public:
    PlaceholderFinder(SourceLoc PlaceholderLoc,
                      EditorPlaceholderExpr *&Found)
    : PlaceholderLoc(PlaceholderLoc), Found(Found) {
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (isa<EditorPlaceholderExpr>(E) && E->getStartLoc() == PlaceholderLoc) {
        Found = cast<EditorPlaceholderExpr>(E);
        return { false, nullptr };
      }
      return { true, E };
    }
  };

  class ClosureTypeWalker: public ASTWalker {
    SourceManager &SM;
    ClosureInfo &Info;
  public:
    bool FoundFunctionTypeRepr = false;
    explicit ClosureTypeWalker(SourceManager &SM, ClosureInfo &Info) : SM(SM),
      Info(Info) { }

    bool walkToTypeReprPre(TypeRepr *T) override {
      if (auto *FTR = dyn_cast<FunctionTypeRepr>(T)) {
        FoundFunctionTypeRepr = true;
        if (auto *TTR = dyn_cast_or_null<TupleTypeRepr>(FTR->getArgsTypeRepr())) {
          for (unsigned i = 0, end = TTR->getNumElements(); i != end; ++i) {
            auto *ArgTR = TTR->getElement(i);
            CharSourceRange NR;
            CharSourceRange TR;
            auto name = TTR->getElementName(i);
            if (!name.empty()) {
              NR = CharSourceRange(TTR->getElementNameLoc(i),
                                   name.getLength());
            }
            SourceLoc SRE = Lexer::getLocForEndOfToken(SM,
                                                       ArgTR->getEndLoc());
            TR = CharSourceRange(SM, ArgTR->getStartLoc(), SRE);
            Info.Params.emplace_back(NR, TR);
          }
        } else if (FTR->getArgsTypeRepr()) {
          CharSourceRange TR;
          TR = CharSourceRange(SM, FTR->getArgsTypeRepr()->getStartLoc(),
                               Lexer::getLocForEndOfToken(SM,
                                        FTR->getArgsTypeRepr()->getEndLoc()));
          Info.Params.emplace_back(CharSourceRange(), TR);
        }
        if (auto *RTR = FTR->getResultTypeRepr()) {
          SourceLoc SRE = Lexer::getLocForEndOfToken(SM, RTR->getEndLoc());
          Info.ReturnTypeRange = CharSourceRange(SM, RTR->getStartLoc(), SRE);
        }
      }
      return !FoundFunctionTypeRepr;
    }

    bool walkToTypeReprPost(TypeRepr *T) override {
      // If we just visited the FunctionTypeRepr, end traversal.
      return !FoundFunctionTypeRepr;
    }

  };

  bool containClosure(Expr *E) {
    if (E->getStartLoc().isInvalid())
      return false;
    EditorPlaceholderExpr *Found = nullptr;
    ClosureInfo Info;
    ClosureTypeWalker ClosureWalker(SM, Info);
    PlaceholderFinder Finder(E->getStartLoc(), Found);
    E->walk(Finder);
    if (Found) {
      if (auto TR = Found->getTypeLoc().getTypeRepr()) {
        TR->walk(ClosureWalker);
        return ClosureWalker.FoundFunctionTypeRepr;
      }
    }
    E->walk(ClosureWalker);
    return ClosureWalker.FoundFunctionTypeRepr;
  }

  bool scanClosureType(SourceFile &SF, SourceLoc PlaceholderLoc) {
    TargetClosureInfo.Params.clear();
    TargetClosureInfo.ReturnTypeRange = CharSourceRange();
    PlaceholderFinder Finder(PlaceholderLoc, PHE);
    SF.walk(Finder);
    if (!PHE || !PHE->getTypeForExpansion())
      return false;
    ClosureTypeWalker PW(SM, TargetClosureInfo);
    PHE->getTypeForExpansion()->walk(PW);
    return PW.FoundFunctionTypeRepr;
  }

  /// Finds the enclosing CallExpr, and indicates whether it should be further
  /// considered a candidate for application of trailing closure.
  /// For example, if the CallExpr is enclosed in another expression or statement
  /// such as "outer(inner(<#closure#>))", or "if inner(<#closure#>)", then trailing
  /// closure should not be applied to the inner call.
  std::pair<CallExpr *, bool> enclosingCallExpr(SourceFile &SF, SourceLoc SL) {

    class CallExprFinder : public SourceEntityWalker {
    public:
      const SourceManager &SM;
      SourceLoc TargetLoc;
      CallExpr *EnclosingCall;
      Expr *OuterExpr;
      Stmt *OuterStmt;
      explicit CallExprFinder(const SourceManager &SM)
        :SM(SM) { }

      bool walkToExprPre(Expr *E) override {
        auto SR = E->getSourceRange();
        if (SR.isValid() && SM.rangeContainsTokenLoc(SR, TargetLoc)) {
          if (auto *CE = dyn_cast<CallExpr>(E)) {
            if (EnclosingCall)
              OuterExpr = EnclosingCall;
            EnclosingCall = CE;
          }
          else if (!EnclosingCall)
            OuterExpr = E;
        }
        return true;
      }

      bool walkToExprPost(Expr *E) override {
        if (E->getStartLoc() == TargetLoc)
          return false; // found what we needed to find, stop walking.
        return true;
      }

      bool walkToStmtPre(Stmt *S) override {
        auto SR = S->getSourceRange();
        if (SR.isValid() && SM.rangeContainsTokenLoc(SR, TargetLoc)) {
          if (!EnclosingCall && !isa<BraceStmt>(S))
            OuterStmt = S;
        }
        return true;
      }

      CallExpr *findEnclosingCall(SourceFile &SF, SourceLoc SL) {
        EnclosingCall = nullptr;
        OuterExpr = nullptr;
        OuterStmt = nullptr;
        TargetLoc = SL;
        walk(SF);
        return EnclosingCall;
      }
    };

    CallExprFinder CEFinder(SM);
    auto *CE = CEFinder.findEnclosingCall(SF, SL);

    if (!CE)
      return std::make_pair(CE, false);
    if (CEFinder.OuterExpr)
      return std::make_pair(CE, false);
    if (CEFinder.OuterStmt)
      return std::make_pair(CE, false);

    return std::make_pair(CE, true);
  }

  bool shouldUseTrailingClosureInTuple(TupleExpr *TE,
                                       SourceLoc PlaceHolderStartLoc) {
    if (!TE->getElements().empty()) {
      for (unsigned I = 0, N = TE->getNumElements(); I < N; ++ I) {
        bool IsLast = I == N - 1;
        Expr *E = TE->getElement(I);
        if (IsLast) {
          return E->getStartLoc() == PlaceHolderStartLoc;
        } else if (containClosure(E)) {
          return false;
        }
      }
    }
    return false;
  }

public:
  explicit PlaceholderExpansionScanner(SourceManager &SM) : SM(SM) { }

  /// Retrieves the parameter list, return type and context info for
  /// a typed completion placeholder in a function call.
  /// For example: foo.bar(aaa, <#T##(Int, Int) -> Bool#>).
  bool scan(SourceFile &SF, unsigned BufID, unsigned Offset,
             unsigned Length, std::function<void(Expr *Args,
                                                 bool UseTrailingClosure,
                                                 ArrayRef<Param>,
                                                 CharSourceRange)> Callback,
            std::function<bool(EditorPlaceholderExpr*)> NonClosureCallback) {

    SourceLoc PlaceholderStartLoc = SM.getLocForOffset(BufID, Offset);

    // See if the placeholder is encapsulated with an EditorPlaceholderExpr
    // and retrieve parameter and return type ranges.
    if (!scanClosureType(SF, PlaceholderStartLoc)) {
      return NonClosureCallback(PHE);
    }

    // Now we need to see if we can suggest trailing closure expansion,
    // and if the call parens can be removed in that case.
    // We'll first find the enclosing CallExpr, and then do further analysis.
    bool UseTrailingClosure = false;
    std::pair<CallExpr*, bool> ECE = enclosingCallExpr(SF, PlaceholderStartLoc);
    Expr *Args = ECE.first ? ECE.first->getArg() : nullptr;
    if (Args && ECE.second) {
      if (isa<ParenExpr>(Args)) {
        UseTrailingClosure = true;
      } else if (auto *TE = dyn_cast<TupleExpr>(Args)) {
        UseTrailingClosure = shouldUseTrailingClosureInTuple(TE,
                                                          PlaceholderStartLoc);
      }
    }

    Callback(Args, UseTrailingClosure, TargetClosureInfo.Params,
             TargetClosureInfo.ReturnTypeRange);
    return true;
  }

};

} // anonymous namespace

SwiftEditorDocument::SwiftEditorDocument(StringRef FilePath,
    SwiftLangSupport &LangSupport, CodeFormatOptions Options)
  :Impl(*new Implementation(FilePath, LangSupport, Options)) { }

SwiftEditorDocument::~SwiftEditorDocument()
{
  delete &Impl;
}

ImmutableTextSnapshotRef SwiftEditorDocument::initializeText(
    llvm::MemoryBuffer *Buf, ArrayRef<const char *> Args) {

  llvm::sys::ScopedLock L(Impl.AccessMtx);

  Impl.EditableBuffer =
      new EditableTextBuffer(Impl.FilePath, Buf->getBuffer());
  Impl.SyntaxMap.reset();
  Impl.EditedLineRange.setRange(0,0);
  Impl.AffectedRange = std::make_pair(0, Buf->getBufferSize());
  Impl.SemanticInfo =
      new SwiftDocumentSemanticInfo(Impl.FilePath, Impl.LangSupport);
  Impl.SemanticInfo->setCompilerArgs(Args);
  return Impl.EditableBuffer->getSnapshot();
}

ImmutableTextSnapshotRef SwiftEditorDocument::replaceText(
    unsigned Offset, unsigned Length, llvm::MemoryBuffer *Buf,
    bool ProvideSemanticInfo) {

  llvm::sys::ScopedLock L(Impl.AccessMtx);

  llvm::StringRef Str = Buf->getBuffer();
  ImmutableTextSnapshotRef Snapshot =
      Impl.EditableBuffer->replace(Offset, Length, Str);

  if (ProvideSemanticInfo) {
    // If this is not a no-op, update semantic info.
    if (Length != 0 || Buf->getBufferSize() != 0) {
      updateSemaInfo();

      if (auto Invok = Impl.SemanticInfo->getInvocation()) {
        // Update semantic info for open editor documents of the same module.
        // FIXME: Detect edits that don't affect other files, e.g. whitespace,
        // comments, inside a function body, etc.
        CompilerInvocation CI;
        Invok->applyTo(CI);
        auto &EditorDocs = Impl.LangSupport.getEditorDocuments();
        for (auto &Input : CI.getInputFilenames()) {
          if (auto EditorDoc = EditorDocs.findByPath(Input)) {
            if (EditorDoc.get() != this)
              EditorDoc->updateSemaInfo();
          }
        }
      }
    }
  }

  SourceManager &SrcManager = Impl.SyntaxInfo->getSourceManager();
  unsigned BufID = Impl.SyntaxInfo->getBufferID();
  SourceLoc StartLoc = SrcManager.getLocForBufferStart(BufID).getAdvancedLoc(
                                                                        Offset);
  unsigned StartLine = SrcManager.getLineAndColumn(StartLoc).first;
  unsigned EndLine = SrcManager.getLineAndColumn(
                                         StartLoc.getAdvancedLoc(Length)).first;

  // Delete all syntax map data from start line through end line.
  unsigned OldLineCount = EndLine - StartLine + 1;
  Impl.SyntaxMap.removeLineRange(StartLine, OldLineCount);

  // Insert empty syntax map data for replaced lines.
  unsigned NewLineCount = Str.count('\n') + 1;
  Impl.SyntaxMap.insertLineRange(StartLine, NewLineCount);

  // Update the edited line range.
  Impl.EditedLineRange.setRange(StartLine, NewLineCount);

  ImmutableTextBufferRef ImmBuf = Snapshot->getBuffer();

  // The affected range starts from the previous newline.
  if (Offset > 0) {
    auto AffectedRangeOffset = ImmBuf->getText().rfind('\n', Offset);
    Impl.AffectedRange.first =
      AffectedRangeOffset != StringRef::npos ? AffectedRangeOffset + 1 : 0;
  }
  else
    Impl.AffectedRange.first = 0;

  Impl.AffectedRange.second = ImmBuf->getText().size() - Impl.AffectedRange.first;

  return Snapshot;
}

void SwiftEditorDocument::updateSemaInfo() {
  if (Impl.SemanticInfo) {
    Impl.SemanticInfo->processLatestSnapshotAsync(Impl.EditableBuffer);
  }
}

void SwiftEditorDocument::parse(ImmutableTextSnapshotRef Snapshot,
                                SwiftLangSupport &Lang) {
  llvm::sys::ScopedLock L(Impl.AccessMtx);

  assert(Impl.SemanticInfo && "Impl.SemanticInfo must be set");

  std::vector<std::string> Args;
  std::string PrimaryFile; // Ignored, Impl.FilePath will be used

  CompilerInvocation CompInv;
  if (Impl.SemanticInfo->getInvocation()) {
    Impl.SemanticInfo->getInvocation()->applyTo(CompInv);
    Impl.SemanticInfo->getInvocation()->raw(Args, PrimaryFile);
  } else {
    ArrayRef<const char *> Args;
    std::string Error;
    // Ignore possible error(s)
    Lang.getASTManager().
      initCompilerInvocation(CompInv, Args, StringRef(), Error);
  }

  // Access to Impl.SyntaxInfo is guarded by Impl.AccessMtx
  Impl.SyntaxInfo.reset(
    new SwiftDocumentSyntaxInfo(CompInv, Snapshot, Args, Impl.FilePath));

  Impl.SyntaxInfo->parse();
}

void SwiftEditorDocument::readSyntaxInfo(EditorConsumer &Consumer) {
  llvm::sys::ScopedLock L(Impl.AccessMtx);

  trace::TracedOperation TracedOp;
  if (trace::enabled()) {
    trace::SwiftInvocation Info;
    Impl.buildSwiftInv(Info);
    TracedOp.start(trace::OperationKind::ReadSyntaxInfo, Info);
  }

  Impl.ParserDiagnostics = Impl.SyntaxInfo->getDiagnostics();

  ide::SyntaxModelContext ModelContext(Impl.SyntaxInfo->getSourceFile());

  SwiftEditorSyntaxWalker SyntaxWalker(Impl.SyntaxMap,
                                       Impl.EditedLineRange,
                                       Impl.AffectedRange,
                                       Impl.SyntaxInfo->getSourceManager(),
                                       Consumer,
                                       Impl.SyntaxInfo->getBufferID());

  ModelContext.walk(SyntaxWalker);

  Consumer.recordAffectedRange(Impl.AffectedRange.first,
                               Impl.AffectedRange.second);
}

void SwiftEditorDocument::readSemanticInfo(ImmutableTextSnapshotRef Snapshot,
                                           EditorConsumer& Consumer) {
  trace::TracedOperation TracedOp;
  if (trace::enabled()) {
    trace::SwiftInvocation Info;
    Impl.buildSwiftInv(Info);
    TracedOp.start(trace::OperationKind::ReadSemanticInfo, Info);
  }

  std::vector<SwiftSemanticToken> SemaToks;
  std::vector<DiagnosticEntryInfo> SemaDiags;

  // FIXME: Parser diagnostics should be filtered out of the semantic ones,
  // Then just merge the semantic ones with the current parse ones.
  Impl.SemanticInfo->readSemanticInfo(Snapshot, SemaToks, SemaDiags,
                                      Impl.ParserDiagnostics);

  for (auto SemaTok : SemaToks) {
    unsigned Offset = SemaTok.ByteOffset;
    unsigned Length = SemaTok.Length;
    UIdent Kind = SemaTok.getUIdentForKind();
    bool IsSystem = SemaTok.getIsSystem();
    if (Kind.isValid())
      if (!Consumer.handleSemanticAnnotation(Offset, Length, Kind, IsSystem))
        break;
  }

  static UIdent SemaDiagStage("source.diagnostic.stage.swift.sema");
  static UIdent ParseDiagStage("source.diagnostic.stage.swift.parse");

  if (!SemaDiags.empty() || !SemaToks.empty()) {
    Consumer.setDiagnosticStage(SemaDiagStage);
  } else {
    Consumer.setDiagnosticStage(ParseDiagStage);
  }

  for (auto &Diag : Impl.ParserDiagnostics)
    Consumer.handleDiagnostic(Diag, ParseDiagStage);
  for (auto &Diag : SemaDiags)
    Consumer.handleDiagnostic(Diag, SemaDiagStage);
}

void SwiftEditorDocument::removeCachedAST() {
  Impl.SemanticInfo->removeCachedAST();
}

void SwiftEditorDocument::applyFormatOptions(OptionsDictionary &FmtOptions) {
  static UIdent KeyUseTabs("key.editor.format.usetabs");
  static UIdent KeyIndentWidth("key.editor.format.indentwidth");
  static UIdent KeyTabWidth("key.editor.format.tabwidth");

  FmtOptions.valueForOption(KeyUseTabs, Impl.FormatOptions.UseTabs);
  FmtOptions.valueForOption(KeyIndentWidth, Impl.FormatOptions.IndentWidth);
  FmtOptions.valueForOption(KeyTabWidth, Impl.FormatOptions.TabWidth);
}

const CodeFormatOptions &SwiftEditorDocument::getFormatOptions() {
  return Impl.FormatOptions;
}

void SwiftEditorDocument::formatText(unsigned Line, unsigned Length,
                                     EditorConsumer &Consumer) {
  auto SyntaxInfo = Impl.getSyntaxInfo();
  SourceFile &SF = SyntaxInfo->getSourceFile();
  SourceManager &SM = SyntaxInfo->getSourceManager();
  unsigned BufID = SyntaxInfo->getBufferID();

  trace::TracedOperation TracedOp;
  if (trace::enabled()) {
    trace::SwiftInvocation SwiftArgs;
    // Compiler arguments do not matter
    auto Buf = SM.getLLVMSourceMgr().getMemoryBuffer(BufID);
    SwiftArgs.Args.PrimaryFile = Buf->getBufferIdentifier();
    SwiftArgs.addFile(SwiftArgs.Args.PrimaryFile, Buf->getBuffer());
    trace::StringPairs OpArgs = {
      std::make_pair("Line", std::to_string(Line)),
      std::make_pair("Length", std::to_string(Length)),
      std::make_pair("IndentWidth",
                     std::to_string(Impl.FormatOptions.IndentWidth)),
      std::make_pair("TabWidth",
                     std::to_string(Impl.FormatOptions.TabWidth)),
      std::make_pair("UseTabs",
                     std::to_string(Impl.FormatOptions.UseTabs))};
    TracedOp.start(trace::OperationKind::FormatText, SwiftArgs, OpArgs);
  }

  LineRange inputRange = LineRange(Line, Length);
  CodeFormatOptions Options = getFormatOptions();
  auto indented = reformat(inputRange, Options, SM, SF);

  LineRange LineRange = indented.first;
  StringRef ModifiedText = indented.second;
  Consumer.recordFormattedText(ModifiedText);
  Consumer.recordAffectedLineRange(LineRange.startLine(), LineRange.lineCount());
}

bool isReturningVoid(SourceManager &SM, CharSourceRange Range) {
  if (Range.isInvalid())
    return false;
  StringRef Text = SM.extractText(Range);
  return "()" == Text || "Void" == Text;
}

void SwiftEditorDocument::expandPlaceholder(unsigned Offset, unsigned Length,
                                            EditorConsumer &Consumer) {
  auto SyntaxInfo = Impl.getSyntaxInfo();
  SourceManager &SM = SyntaxInfo->getSourceManager();
  unsigned BufID = SyntaxInfo->getBufferID();

  const unsigned PlaceholderStartLen = 2;
  const unsigned PlaceholderEndLen = 2;

  if (Length < (PlaceholderStartLen + PlaceholderEndLen)) {
    Consumer.handleRequestError("Invalid Length parameter");
    return;
  }

  trace::TracedOperation TracedOp;
  if (trace::enabled()) {
    trace::SwiftInvocation SwiftArgs;
    SyntaxInfo->initArgsAndPrimaryFile(SwiftArgs);
    auto Buf = SM.getLLVMSourceMgr().getMemoryBuffer(BufID);
    SwiftArgs.addFile(Buf->getBufferIdentifier(), Buf->getBuffer());
    trace::StringPairs OpArgs = {
      std::make_pair("Offset", std::to_string(Offset)),
      std::make_pair("Length", std::to_string(Length))};
    TracedOp.start(trace::OperationKind::ExpandPlaceholder, SwiftArgs, OpArgs);
  }

  PlaceholderExpansionScanner Scanner(SM);
  SourceFile &SF = SyntaxInfo->getSourceFile();

  Scanner.scan(SF, BufID, Offset, Length,
          [&](Expr *Args,
              bool UseTrailingClosure,
              ArrayRef<PlaceholderExpansionScanner::Param> ClosureParams,
              CharSourceRange ClosureReturnTypeRange) {

      unsigned EffectiveOffset = Offset;
      unsigned EffectiveLength = Length;
      llvm::SmallString<128> ExpansionStr;
      {
        llvm::raw_svector_ostream OS(ExpansionStr);
        if (UseTrailingClosure) {
          assert(Args);

          if (isa<ParenExpr>(Args)) {
            // There appears to be no other parameters in this call, so we'll
            // expand replacement for trailing closure and cover call parens.
            // For example:
            // foo.bar(<#closure#>) turns into foo.bar <#closure#>.
            EffectiveOffset = SM.getLocOffsetInBuffer(Args->getStartLoc(), BufID);
            OS << " ";
          } else {
            auto *TupleE = cast<TupleExpr>(Args);
            auto Elems = TupleE->getElements();
            assert(!Elems.empty());
            if (Elems.size() == 1) {
              EffectiveOffset = SM.getLocOffsetInBuffer(Args->getStartLoc(), BufID);
              OS << " ";
            } else {
              // Expand replacement range for trailing closure.
              // For example:
              // foo.bar(a, <#closure#>) turns into foo.bar(a) <#closure#>.

              // If the preceding token in the call is the leading parameter
              // separator, we'll expand replacement to cover that.
              assert(Elems.size() > 1);
              SourceLoc BeforeLoc = Lexer::getLocForEndOfToken(SM,
                                              Elems[Elems.size()-2]->getEndLoc());
              EffectiveOffset = SM.getLocOffsetInBuffer(BeforeLoc, BufID);
              OS << ") ";
            }
          }

          unsigned End = SM.getLocOffsetInBuffer(Args->getEndLoc(), BufID);
          EffectiveLength = (End + 1) - EffectiveOffset;
        }

        OS << "{ ";

        bool ReturningVoid = isReturningVoid(SM, ClosureReturnTypeRange);

        bool HasSignature = !ClosureParams.empty() ||
                            (ClosureReturnTypeRange.isValid() && !ReturningVoid);
        bool FirstParam = true;
        if (HasSignature)
          OS << "(";
        for (auto &Param: ClosureParams) {
          if (!FirstParam)
            OS << ", ";
          FirstParam = false;
          if (Param.NameRange.isValid()) {
            // If we have a parameter name, just output the name as is and skip
            // the type. For example:
            // <#(arg1: Int, arg2: Int)#> turns into (arg1, arg2).
            OS << SM.extractText(Param.NameRange);
          }
          else {
            // If we only have the parameter type, output the type as a
            // placeholder. For example:
            // <#(Int, Int)#> turns into (<#Int#>, <#Int#>).
            OS << "<#";
            OS << SM.extractText(Param.TypeRange);
            OS << "#>";
          }
        }
        if (HasSignature)
          OS << ") ";
        if (ClosureReturnTypeRange.isValid()) {
          auto ReturnTypeText = SM.extractText(ClosureReturnTypeRange);

          // We need return type if it is not Void.
          if (!ReturningVoid) {
            OS << "-> ";
            OS << ReturnTypeText << " ";
          }
        }
        if (HasSignature)
          OS << "in";
        OS << "\n" << getCodePlaceholder() << "\n";
        OS << "}";
      }
      Consumer.handleSourceText(ExpansionStr);
      Consumer.recordAffectedRange(EffectiveOffset, EffectiveLength);
    }, [&](EditorPlaceholderExpr *PHE) {
      if (!PHE)
        return false;
      if (auto Ty = PHE->getTypeForExpansion()) {
        std::string S;
        llvm::raw_string_ostream OS(S);
        Ty->print(OS);
        Consumer.handleSourceText(OS.str());
        Consumer.recordAffectedRange(Offset, Length);
        return true;
      }
      return false;
    });
}

ImmutableTextSnapshotRef SwiftEditorDocument::getLatestSnapshot() const {
  return Impl.EditableBuffer->getSnapshot();
}

void SwiftEditorDocument::reportDocumentStructure(SourceFile &SrcFile,
                                                  EditorConsumer &Consumer) {
  ide::SyntaxModelContext ModelContext(SrcFile);
  SwiftDocumentStructureWalker Walker(SrcFile.getASTContext().SourceMgr,
                                      *SrcFile.getBufferID(),
                                      Consumer);
  ModelContext.walk(Walker);
}

//===----------------------------------------------------------------------===//
// EditorOpen
//===----------------------------------------------------------------------===//

void SwiftLangSupport::editorOpen(StringRef Name, llvm::MemoryBuffer *Buf,
                                  bool EnableSyntaxMap,
                                  EditorConsumer &Consumer,
                                  ArrayRef<const char *> Args) {

  ImmutableTextSnapshotRef Snapshot = nullptr;

  auto EditorDoc = EditorDocuments.getByUnresolvedName(Name);
  if (!EditorDoc) {
    EditorDoc = new SwiftEditorDocument(Name, *this);
    Snapshot = EditorDoc->initializeText(Buf, Args);
    EditorDoc->parse(Snapshot, *this);
    if (EditorDocuments.getOrUpdate(Name, *this, EditorDoc)) {
      // Document already exists, re-initialize it. This should only happen
      // if we get OPEN request while the previous document is not closed.
      LOG_WARN_FUNC("Document already exists in editorOpen(..): " << Name);
      Snapshot = nullptr;
    }
  }

  if (!Snapshot) {
    Snapshot = EditorDoc->initializeText(Buf, Args);
    EditorDoc->parse(Snapshot, *this);
  }

  if (Consumer.needsSemanticInfo()) {
    EditorDoc->updateSemaInfo();
  }

  EditorDoc->readSyntaxInfo(Consumer);
  EditorDoc->readSemanticInfo(Snapshot, Consumer);
}


//===----------------------------------------------------------------------===//
// EditorClose
//===----------------------------------------------------------------------===//

void SwiftLangSupport::editorClose(StringRef Name, bool RemoveCache) {
  auto Removed = EditorDocuments.remove(Name);
  if (!Removed)
    IFaceGenContexts.remove(Name);
  if (Removed && RemoveCache)
    Removed->removeCachedAST();
  // FIXME: Report error if Name did not apply to anything ?
}


//===----------------------------------------------------------------------===//
// EditorReplaceText
//===----------------------------------------------------------------------===//

void SwiftLangSupport::editorReplaceText(StringRef Name, llvm::MemoryBuffer *Buf,
                                         unsigned Offset, unsigned Length,
                                         EditorConsumer &Consumer) {
  auto EditorDoc = EditorDocuments.getByUnresolvedName(Name);
  if (!EditorDoc) {
    Consumer.handleRequestError("No associated Editor Document");
    return;
  }

  ImmutableTextSnapshotRef Snapshot;
  if (Length != 0 || Buf->getBufferSize() != 0) {
    Snapshot = EditorDoc->replaceText(Offset, Length, Buf,
                                      Consumer.needsSemanticInfo());
    assert(Snapshot);
    EditorDoc->parse(Snapshot, *this);
    EditorDoc->readSyntaxInfo(Consumer);
  } else {
    Snapshot = EditorDoc->getLatestSnapshot();
  }

  EditorDoc->readSemanticInfo(Snapshot, Consumer);
}


//===----------------------------------------------------------------------===//
// EditorFormatText
//===----------------------------------------------------------------------===//
void SwiftLangSupport::editorApplyFormatOptions(StringRef Name,
                                                OptionsDictionary &FmtOptions) {
  auto EditorDoc = EditorDocuments.getByUnresolvedName(Name);
  if (EditorDoc)
    EditorDoc->applyFormatOptions(FmtOptions);
}

void SwiftLangSupport::editorFormatText(StringRef Name, unsigned Line,
                                        unsigned Length,
                                        EditorConsumer &Consumer) {
  auto EditorDoc = EditorDocuments.getByUnresolvedName(Name);
  if (!EditorDoc) {
    Consumer.handleRequestError("No associated Editor Document");
    return;
  }

  EditorDoc->formatText(Line, Length, Consumer);
}

void SwiftLangSupport::editorExtractTextFromComment(StringRef Source,
                                                    EditorConsumer &Consumer) {
  Consumer.handleSourceText(extractPlainTextFromComment(Source));
}

void SwiftLangSupport::editorConvertMarkupToXML(StringRef Source,
                                                EditorConsumer &Consumer) {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  if (convertMarkupToXML(Source, OS)) {
    Consumer.handleRequestError("Conversion failed.");
    return;
  }
  Consumer.handleSourceText(Result);
}

//===----------------------------------------------------------------------===//
// EditorExpandPlaceholder
//===----------------------------------------------------------------------===//
void SwiftLangSupport::editorExpandPlaceholder(StringRef Name, unsigned Offset,
                                               unsigned Length,
                                               EditorConsumer &Consumer) {
  auto EditorDoc = EditorDocuments.getByUnresolvedName(Name);
  if (!EditorDoc) {
    Consumer.handleRequestError("No associated Editor Document");
    return;
  }

  EditorDoc->expandPlaceholder(Offset, Length, Consumer);
}
