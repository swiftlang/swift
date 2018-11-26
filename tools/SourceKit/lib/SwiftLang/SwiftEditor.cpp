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
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Demangling/ManglingUtils.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CodeCompletion.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDE/Formatting.h"
#include "swift/IDE/SourceEntityWalker.h"
#include "swift/IDE/SyntaxModel.h"
#include "swift/Subsystems.h"
#include "swift/Syntax/Serialization/SyntaxSerialization.h"
#include "swift/Syntax/SyntaxNodes.h"

#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Mutex.h"

using namespace SourceKit;
using namespace swift;
using namespace ide;

static std::vector<unsigned> getSortedBufferIDs(
    const llvm::DenseMap<unsigned, std::vector<DiagnosticEntryInfo>> &Map) {
  std::vector<unsigned> bufferIDs;
  bufferIDs.reserve(Map.size());
  for (auto I = Map.begin(), E = Map.end(); I != E; ++I) {
    bufferIDs.push_back(I->getFirst());
  }
  llvm::array_pod_sort(bufferIDs.begin(), bufferIDs.end());
  return bufferIDs;
}

void EditorDiagConsumer::getAllDiagnostics(
    SmallVectorImpl<DiagnosticEntryInfo> &Result) {

  Result.append(InvalidLocDiagnostics.begin(), InvalidLocDiagnostics.end());

  // Note: we cannot reuse InputBufIds because there may be diagnostics outside
  // the inputs.  Instead, sort the extant buffers.
  auto bufferIDs = getSortedBufferIDs(BufferDiagnostics);
  for (unsigned bufferID : bufferIDs) {
    const auto &diags = BufferDiagnostics[bufferID];
    Result.append(diags.begin(), diags.end());
  }
}

void EditorDiagConsumer::handleDiagnostic(
    SourceManager &SM, SourceLoc Loc, DiagnosticKind Kind,
    StringRef FormatString, ArrayRef<DiagnosticArgument> FormatArgs,
    const DiagnosticInfo &Info) {

  if (Kind == DiagnosticKind::Error) {
    HadAnyError = true;
  }

  // Filter out benign diagnostics for editing.
  if (Info.ID == diag::lex_editor_placeholder.ID ||
      Info.ID == diag::error_doing_code_completion.ID)
    return;

  bool IsNote = (Kind == DiagnosticKind::Note);

  if (IsNote && !haveLastDiag())
    // Is this possible?
    return;

  if (Kind == DiagnosticKind::Remark) {
    // FIXME: we may want to handle optimization remarks in sourcekitd.
    LOG_WARN_FUNC("unhandled optimization remark");
    return;
  }

  DiagnosticEntryInfo SKInfo;

  // Actually substitute the diagnostic arguments into the diagnostic text.
  llvm::SmallString<256> Text;
  {
    llvm::raw_svector_ostream Out(Text);
    DiagnosticEngine::formatDiagnosticText(Out, FormatString, FormatArgs);
  }
  SKInfo.Description = Text.str();

  Optional<unsigned> BufferIDOpt;
  if (Loc.isValid()) {
    BufferIDOpt =  SM.findBufferContainingLoc(Loc);
  }

  if (BufferIDOpt && !isInputBufferID(*BufferIDOpt)) {
    if (Info.ID == diag::error_from_clang.ID ||
        Info.ID == diag::warning_from_clang.ID ||
        Info.ID == diag::note_from_clang.ID ||
        !IsNote) {
      // Handle it as other diagnostics.
    } else {
      // FIXME: This is a note pointing to a synthesized declaration buffer for
      // a declaration coming from a module.
      // We should include the Decl* in the DiagnosticInfo and have a way for
      // Xcode to handle this "points-at-a-decl-from-module" location.
      //
      // For now instead of ignoring it, pick up the declaration name from the
      // buffer identifier and append it to the diagnostic message.
      auto &LastDiag = getLastDiag();
      SKInfo.Description += " (";
      SKInfo.Description += SM.getIdentifierForBuffer(*BufferIDOpt);
      SKInfo.Description += ")";
      SKInfo.Offset = LastDiag.Offset;
      SKInfo.Line = LastDiag.Line;
      SKInfo.Column = LastDiag.Column;
      SKInfo.Filename = LastDiag.Filename;
      LastDiag.Notes.push_back(std::move(SKInfo));
      return;
    }
  }

  if (BufferIDOpt.hasValue()) {
    unsigned BufferID = *BufferIDOpt;

    SKInfo.Offset = SM.getLocOffsetInBuffer(Loc, BufferID);
    std::tie(SKInfo.Line, SKInfo.Column) = SM.getLineAndColumn(Loc, BufferID);
    SKInfo.Filename = SM.getDisplayNameForLoc(Loc);

    for (auto R : Info.Ranges) {
      if (R.isInvalid() || SM.findBufferContainingLoc(R.getStart()) != BufferID)
        continue;
      unsigned Offset = SM.getLocOffsetInBuffer(R.getStart(), BufferID);
      unsigned Length = R.getByteLength();
      SKInfo.Ranges.push_back({Offset, Length});
    }

    for (auto F : Info.FixIts) {
      if (F.getRange().isInvalid() ||
          SM.findBufferContainingLoc(F.getRange().getStart()) != BufferID)
        continue;
      unsigned Offset =
          SM.getLocOffsetInBuffer(F.getRange().getStart(), BufferID);
      unsigned Length = F.getRange().getByteLength();
      SKInfo.Fixits.push_back({Offset, Length, F.getText()});
    }
  } else {
    SKInfo.Filename = "<unknown>";
  }

  if (IsNote) {
    getLastDiag().Notes.push_back(std::move(SKInfo));
    return;
  }

  switch (Kind) {
    case DiagnosticKind::Error:
      SKInfo.Severity = DiagnosticSeverityKind::Error;
      break;
    case DiagnosticKind::Warning:
      SKInfo.Severity = DiagnosticSeverityKind::Warning;
      break;
    case DiagnosticKind::Note:
    case DiagnosticKind::Remark:
      llvm_unreachable("already covered");
  }

  if (!BufferIDOpt) {
    InvalidLocDiagnostics.push_back(std::move(SKInfo));
    clearLastDiag();
    return;
  }

  unsigned BufferID = *BufferIDOpt;
  DiagnosticsTy &Diagnostics = BufferDiagnostics[BufferID];

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
  unsigned Offset;
  unsigned Length:24;
  SyntaxNodeKind Kind:8;

  static SwiftSyntaxToken createInvalid() {
    return {0, 0, SyntaxNodeKind::AttributeBuiltin};
  }

  SwiftSyntaxToken(unsigned Offset, unsigned Length, SyntaxNodeKind Kind)
    : Offset(Offset), Length(Length), Kind(Kind) {}

  unsigned endOffset() const { return Offset + Length; }

  bool isInvalid() const { return Length == 0; }

  bool operator==(const SwiftSyntaxToken &Other) const {
    return Offset == Other.Offset && Length == Other.Length &&
      Kind == Other.Kind;
  }

  bool operator!=(const SwiftSyntaxToken &Other) const {
    return Offset != Other.Offset || Length != Other.Length ||
      Kind != Other.Kind;
  }
};

struct SwiftEditorCharRange {
  unsigned Offset;
  unsigned EndOffset;

  SwiftEditorCharRange(unsigned Offset, unsigned EndOffset) :
    Offset(Offset), EndOffset(EndOffset) {}

  SwiftEditorCharRange(SwiftSyntaxToken Token) :
    Offset(Token.Offset), EndOffset(Token.endOffset()) {}

  size_t length() const { return EndOffset - Offset; }
  bool isEmpty() const { return Offset == EndOffset; }
  bool intersects(const SwiftSyntaxToken &Token) const {
    return this->Offset < (Token.endOffset()) && this->EndOffset > Token.Offset;
  }
  void extendToInclude(const SwiftEditorCharRange &Range) {
    if (Range.Offset < Offset)
      Offset = Range.Offset;
    if (Range.EndOffset > EndOffset)
      EndOffset = Range.EndOffset;
  }
  void extendToInclude(unsigned OtherOffset) {
    extendToInclude({OtherOffset, OtherOffset});
  }
};

/// Finds and represents the first mismatching tokens in two syntax maps,
/// ignoring invalidated tokens.
template <class Iter>
struct TokenMismatch {
  /// The begin and end iterators of the previous syntax map
  Iter PrevTok, PrevEnd;
  /// The begin and end iterators of the current syntax map
  Iter CurrTok, CurrEnd;

  TokenMismatch(Iter CurrTok, Iter CurrEnd, Iter PrevTok, Iter PrevEnd) :
  PrevTok(PrevTok), PrevEnd(PrevEnd), CurrTok(CurrTok), CurrEnd(CurrEnd) {
    skipInvalid();
    while(advance());
  }

  /// Returns true if a mismatch was found
  bool foundMismatch() const {
    return CurrTok != CurrEnd || PrevTok != PrevEnd;
  }

  /// Returns the smallest start offset of the mismatched token ranges
  unsigned mismatchStart() const {
    assert(foundMismatch());
    if (CurrTok != CurrEnd) {
      if (PrevTok != PrevEnd)
        return std::min(CurrTok->Offset, PrevTok->Offset);
      return CurrTok->Offset;
    }
    return PrevTok->Offset;
  }

  /// Returns the largest end offset of the mismatched token ranges
  unsigned mismatchEnd() const {
    assert(foundMismatch());
    if (CurrTok != CurrEnd) {
      if (PrevTok != PrevEnd)
        return std::max(CurrTok->endOffset(), PrevTok->endOffset());
      return CurrTok->endOffset();
    }
    return PrevTok->endOffset();
  }

private:
  void skipInvalid() {
    while (PrevTok != PrevEnd && PrevTok->isInvalid())
      ++PrevTok;
  }

  bool advance() {
    if (CurrTok == CurrEnd || PrevTok == PrevEnd || *CurrTok != *PrevTok)
      return false;
    ++CurrTok;
    ++PrevTok;
    skipInvalid();
    return true;
  }
};

/// Represents a the syntax highlighted token ranges in a source file
struct SwiftSyntaxMap {
  std::vector<SwiftSyntaxToken> Tokens;

  explicit SwiftSyntaxMap(unsigned Capacity = 0) {
    if (Capacity)
      Tokens.reserve(Capacity);
  }

  void addToken(const SwiftSyntaxToken &Token) {
    assert(Tokens.empty() || Token.Offset >= Tokens.back().Offset);
    Tokens.push_back(Token);
  }

  /// Merge this nested token into the last token that was added
  void mergeToken(const SwiftSyntaxToken &Token) {
    if (Tokens.empty()) {
      Tokens.push_back(Token);
      return;
    }
    auto &LastTok = Tokens.back();
    assert(LastTok.Offset <= Token.Offset);
    mergeSplitRanges(LastTok.Offset, LastTok.Length, Token.Offset, Token.Length,
                     [&](unsigned BeforeOff, unsigned BeforeLen,
                         unsigned AfterOff, unsigned AfterLen) {
                       auto LastKind = LastTok.Kind;
                       Tokens.pop_back();
                       if (BeforeLen)
                         Tokens.emplace_back(BeforeOff, BeforeLen, LastKind);
                       Tokens.push_back(Token);
                       if (AfterLen)
                         Tokens.emplace_back(AfterOff, AfterLen, LastKind);
                     });
  }

  /// Adjusts the token offsets and lengths in this syntax map to account for
  /// replacing \p Len bytes at the given \p Offset with \p NewLen bytes. Tokens
  /// before the replacement stay the same, tokens after it are shifted, and
  /// tokens that intersect it are 'removed' (really just marked invalid).
  /// Clients are expected to match this behavior.
  ///
  /// Returns the union of the replaced range and the token ranges it
  /// intersected, or nothing if no tokens were intersected.
  llvm::Optional<SwiftEditorCharRange>
  adjustForReplacement(unsigned Offset, unsigned Len, unsigned NewLen) {
    unsigned ReplacedStart = Offset;
    unsigned ReplacedEnd = Offset + Len;
    bool TokenIntersected = false;
    SwiftEditorCharRange Affected = { /*Offset=*/ReplacedStart,
                                      /*EndOffset=*/ReplacedEnd};
    // Adjust the tokens
    auto Token = Tokens.begin();
    while (Token != Tokens.end() && Token->endOffset() <= ReplacedStart) {
      // Completely before the replaced range – no change needed
      ++Token;
    }

    while (Token != Tokens.end() && Token->Offset < ReplacedEnd) {
      // Intersecting the replaced range – extend Affected and invalidate
      TokenIntersected = true;
      Affected.extendToInclude(*Token);
      *Token = SwiftSyntaxToken::createInvalid();
      ++Token;
    }

    while (Token != Tokens.end()) {
      // Completely after the replaced range - shift to account for NewLen
      if (NewLen >= Len)
        Token->Offset += NewLen - Len;
      else
        Token->Offset -= Len - NewLen;
      ++Token;
    }

    // If the replaced range didn't intersect with any existing tokens, there's
    // no need to report an affected range
    if (!TokenIntersected)
      return None;

    // Update the end of the affected range to account for NewLen
    if (NewLen >= Len) {
      Affected.EndOffset += NewLen - Len;
    } else {
      Affected.EndOffset -= Len - NewLen;
    }

    return Affected;
  }

  /// Passes each token in this SwiftSyntaxMap to the given \p Consumer
  void forEach(EditorConsumer &Consumer) {
    for (auto &Token: Tokens) {
      auto Kind = SwiftLangSupport::getUIDForSyntaxNodeKind(Token.Kind);
      Consumer.handleSyntaxMap(Token.Offset, Token.Length, Kind);
    }
  }

  /// Finds the delta between the given SwiftSyntaxMap, \p Prev, and this one.
  /// It passes each token not in \p Prev to the given \p Consumer and, if
  /// needed, also expands or sets the given \p Affected range to cover all
  /// non-matching tokens in the two lists.
  ///
  /// Returns true if this SwiftSyntaxMap is different to \p Prev.
  bool forEachChanged(const SwiftSyntaxMap &Prev,
                      llvm::Optional<SwiftEditorCharRange> &Affected,
                      EditorConsumer &Consumer) const {
    typedef std::vector<SwiftSyntaxToken>::const_iterator ForwardIt;
    typedef std::vector<SwiftSyntaxToken>::const_reverse_iterator ReverseIt;

    // Find the first pair of tokens that don't match
    TokenMismatch<ForwardIt>
    Forward(Tokens.begin(), Tokens.end(), Prev.Tokens.begin(), Prev.Tokens.end());

    // Exit early if there was no mismatch
    if (!Forward.foundMismatch())
      return Affected && !Affected->isEmpty();

    // Find the last pair of tokens that don't match
    TokenMismatch<ReverseIt>
    Backward(Tokens.rbegin(), Tokens.rend(), Prev.Tokens.rbegin(), Prev.Tokens.rend());
    assert(Backward.foundMismatch());

    // Set or extend the affected range to include the  mismatched range
    SwiftEditorCharRange
    MismatchRange = {Forward.mismatchStart(),Backward.mismatchEnd()};
    if (!Affected) {
      Affected = MismatchRange;
    } else {
      Affected->extendToInclude(MismatchRange);
    }

    // Report all tokens in the affected range to the EditorConsumer
    auto From = Forward.CurrTok;
    auto To = Backward.CurrTok;
    while (From != Tokens.begin() && (From-1)->Offset >= Affected->Offset)
      --From;
    while (To != Tokens.rbegin() && (To-1)->endOffset() <= Affected->EndOffset)
      --To;
    for (; From < To.base(); ++From) {
      auto Kind = SwiftLangSupport::getUIDForSyntaxNodeKind(From->Kind);
      Consumer.handleSyntaxMap(From->Offset, From->Length, Kind);
    }

    return true;
  }
};

struct EditorConsumerSyntaxMapEntry {
  unsigned Offset;
  unsigned Length;
  UIdent Kind;
  EditorConsumerSyntaxMapEntry(unsigned Offset, unsigned Length, UIdent Kind)
    :Offset(Offset), Length(Length), Kind(Kind) { }
};

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
                        Optional<std::vector<DiagnosticEntryInfo>> &Diags,
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

  Optional<std::vector<DiagnosticEntryInfo>> getSemanticDiagnostics(
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
  /// Whether or not the AST stored in the source file is up-to-date or just an
  /// artifact of incremental syntax parsing
  bool HasUpToDateAST;

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
    DiagConsumer.setInputBufferIDs(BufferID);

    Parser.reset(
                 new ParserUnit(SM, SourceFileKind::Main, BufferID,
                     CompInv.getLangOptions(),
                     CompInv.getModuleName(),
                     CompInv.getMainFileSyntaxParsingCache())
    );

    Parser->getDiagnosticEngine().addConsumer(DiagConsumer);

    // If there is a syntax parsing cache, incremental syntax parsing is
    // performed and thus the generated AST may not be up-to-date.
    HasUpToDateAST = CompInv.getMainFileSyntaxParsingCache() == nullptr;
  }

  void parse() {
    auto &P = Parser->getParser();
    bool Done = false;
    while (!Done) {
      P.parseTopLevel();
      Done = P.Tok.is(tok::eof);
    }
    P.finalizeSyntaxTree();
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

  bool hasUpToDateAST() { return HasUpToDateAST; }

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
    Optional<std::vector<DiagnosticEntryInfo>> &Diags,
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

Optional<std::vector<DiagnosticEntryInfo>>
SwiftDocumentSemanticInfo::getSemanticDiagnostics(
    ImmutableTextSnapshotRef NewSnapshot,
    ArrayRef<DiagnosticEntryInfo> ParserDiags) {

  std::vector<DiagnosticEntryInfo> curSemaDiags;
  {
    llvm::sys::ScopedLock L(Mtx);

    if (!DiagSnapshot || DiagSnapshot->getStamp() != NewSnapshot->getStamp()) {
      // The semantic diagnostics are out-of-date, ignore them.
      return llvm::None;
    }

    curSemaDiags = SemaDiags;
  }

  // Diagnostics from the AST and diagnostics from the parser are based on the
  // same source text snapshot. But diagnostics from the AST may have excluded
  // the parser diagnostics due to a fatal error, e.g. if the source has a
  // 'so such module' error, which will suppress other diagnostics.
  // We don't want to turn off the suppression to avoid a flood of diagnostics
  // when a module import fails, but we also don't want to lose the parser
  // diagnostics in such a case, so merge the parser diagnostics with the sema
  // ones.

  auto orderDiagnosticEntryInfos = [](const DiagnosticEntryInfo &LHS,
                                      const DiagnosticEntryInfo &RHS) -> bool {
    if (LHS.Filename != RHS.Filename)
      return LHS.Filename < RHS.Filename;
    if (LHS.Offset != RHS.Offset)
      return LHS.Offset < RHS.Offset;
    return LHS.Description < RHS.Description;
  };

  std::vector<DiagnosticEntryInfo> sortedParserDiags;
  sortedParserDiags.reserve(ParserDiags.size());
  sortedParserDiags.insert(sortedParserDiags.end(), ParserDiags.begin(),
                           ParserDiags.end());
  std::stable_sort(sortedParserDiags.begin(), sortedParserDiags.end(),
                   orderDiagnosticEntryInfos);

  std::vector<DiagnosticEntryInfo> finalDiags;
  finalDiags.reserve(sortedParserDiags.size()+curSemaDiags.size());

  // Add sema diagnostics unless it is an existing parser diagnostic.
  // Note that we want to merge and eliminate diagnostics from the 'sema' set
  // that also show up in the 'parser' set, but we don't want to remove
  // duplicate diagnostics from within the same set (e.g. duplicates existing in
  // the 'sema' set). We want to report the diagnostics as the compiler reported
  // them, even if there's some duplicate one. This is why we don't just do a
  // simple append/sort/keep-uniques step.
  for (const auto &curDE : curSemaDiags) {
    bool existsAsParserDiag = std::binary_search(sortedParserDiags.begin(),
                                                 sortedParserDiags.end(),
                                             curDE, orderDiagnosticEntryInfos);
    if (!existsAsParserDiag) {
      finalDiags.push_back(curDE);
    }
  }

  finalDiags.insert(finalDiags.end(),
                    sortedParserDiags.begin(), sortedParserDiags.end());
  std::stable_sort(finalDiags.begin(), finalDiags.end(),
                   orderDiagnosticEntryInfos);

  return finalDiags;
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
      if (isa<VarDecl>(D) && D->hasName() &&
          D->getFullName() == D->getASTContext().Id_self)
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
                               Optional<AccessKind> AccKind,
                               bool IsOpenBracket) override {
    // We should treat both open and close brackets equally
    return visitDeclReference(D, Range, nullptr, nullptr, Type(),
                      ReferenceMetaData(SemaReferenceKind::SubscriptRef, AccKind));
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

    SemanticAnnotator Annotator(CompIns.getSourceMgr(), BufferID);
    Annotator.walk(AstUnit->getPrimarySourceFile());
    SemaToks = std::move(Annotator.SemaToks);

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

  /// The list of syntax highlighted token offsets and ranges in the document
  SwiftSyntaxMap SyntaxMap;
  /// The minimal range of syntax highlighted tokens affected by the last edit
  llvm::Optional<SwiftEditorCharRange> AffectedRange;
  /// Whether the last operation was an edit rather than a document open
  bool Edited;
  /// The syntax tree of the document
  llvm::Optional<SourceFileSyntax> SyntaxTree;

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
};

namespace  {

static UIdent getAccessLevelUID(AccessLevel Access) {
  static UIdent AccessOpen("source.lang.swift.accessibility.open");
  static UIdent AccessPublic("source.lang.swift.accessibility.public");
  static UIdent AccessInternal("source.lang.swift.accessibility.internal");
  static UIdent AccessFilePrivate("source.lang.swift.accessibility.fileprivate");
  static UIdent AccessPrivate("source.lang.swift.accessibility.private");

  switch (Access) {
  case AccessLevel::Private:
    return AccessPrivate;
  case AccessLevel::FilePrivate:
    return AccessFilePrivate;
  case AccessLevel::Internal:
    return AccessInternal;
  case AccessLevel::Public:
    return AccessPublic;
  case AccessLevel::Open:
    return AccessOpen;
  }

  llvm_unreachable("Unhandled access level in switch.");
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

    unsigned DocOffset = 0;
    unsigned DocEnd = 0;
    if (Node.DocRange.isValid()) {
      DocOffset = SrcManager.getLocOffsetInBuffer(Node.DocRange.getStart(),
                                                  BufferID);
      DocEnd = SrcManager.getLocOffsetInBuffer(Node.DocRange.getEnd(),
                                               BufferID);
    }

    UIdent Kind = SwiftLangSupport::getUIDForSyntaxStructureKind(Node.Kind);
    UIdent AccessLevel;
    UIdent SetterAccessLevel;
    if (Node.Kind != SyntaxStructureKind::Parameter &&
        Node.Kind != SyntaxStructureKind::LocalVariable &&
        Node.Kind != SyntaxStructureKind::GenericTypeParam) {
      if (auto *VD = dyn_cast_or_null<ValueDecl>(Node.Dcl)) {
        AccessLevel = getAccessLevelUID(VD->getFormalAccess());
      } else if (auto *ED = dyn_cast_or_null<ExtensionDecl>(Node.Dcl)) {
        auto StrictAccess = ED->getDefaultAccessLevel();
        AccessLevel = getAccessLevelUID(StrictAccess);
      }
      if (auto *ASD = dyn_cast_or_null<AbstractStorageDecl>(Node.Dcl)) {
        if (ASD->isSettable(/*UseDC=*/nullptr)) {
          swift::AccessLevel SetAccess = ASD->getSetterFormalAccess();
          SetterAccessLevel = getAccessLevelUID(SetAccess);
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

    std::vector<std::tuple<UIdent, unsigned, unsigned>> Attrs;

    for (auto Attr : Node.Attrs) {
      if (auto AttrUID = SwiftLangSupport::getUIDForDeclAttribute(Attr)) {
        unsigned AttrOffset = 0;
        unsigned AttrEnd = 0;
        auto AttrRange = Attr->getRangeWithAt();
        if (AttrRange.isValid()) {
          auto CharRange = Lexer::getCharSourceRangeFromSourceRange(SrcManager,
                                                                    AttrRange);
          AttrOffset = SrcManager.getLocOffsetInBuffer(CharRange.getStart(),
                                                       BufferID);
          AttrEnd = SrcManager.getLocOffsetInBuffer(CharRange.getEnd(),
                                                    BufferID);
        }

        auto AttrTuple = std::make_tuple(AttrUID.getValue(), AttrOffset,
                                         AttrEnd - AttrOffset);
        Attrs.push_back(AttrTuple);
      }
    }

    Consumer.beginDocumentSubStructure(StartOffset, EndOffset - StartOffset,
                                       Kind, AccessLevel, SetterAccessLevel,
                                       NameStart, NameEnd - NameStart,
                                       BodyOffset, BodyEnd - BodyOffset,
                                       DocOffset, DocEnd - DocOffset,
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
    if (!D || D->isInvalid())
      return StringRef();
    if (!isa<ClassDecl>(D) && !isa<ProtocolDecl>(D))
      return StringRef();
    auto *VD = cast<ValueDecl>(D);
    if (!VD->hasName())
      return StringRef();
    auto ident = VD->getBaseName().getIdentifier().str();
    if (ident.empty() || Mangle::isDigit(ident.front()))
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
                                       0, 0, 0, 0,
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

/// Walks the syntax model to populate a given SwiftSyntaxMap with the token
/// ranges to highlight and pass document structure information to the given
/// EditorConsumer.
class SwiftEditorSyntaxWalker: public ide::SyntaxModelWalker {
  /// The syntax map to populate
  SwiftSyntaxMap &SyntaxMap;
  SourceManager &SrcManager;
  unsigned BufferID;
  SwiftDocumentStructureWalker DocStructureWalker;
  /// The current token nesting level (e.g. for a field in a doc comment)
  unsigned NestingLevel = 0;
public:
  SwiftEditorSyntaxWalker(SwiftSyntaxMap &SyntaxMap,
                          SourceManager &SrcManager, EditorConsumer &Consumer,
                          unsigned BufferID)
    : SyntaxMap(SyntaxMap), SrcManager(SrcManager), BufferID(BufferID),
      DocStructureWalker(SrcManager, BufferID, Consumer) { }

  bool walkToNodePre(SyntaxNode Node) override {
    if (Node.Kind == SyntaxNodeKind::CommentMarker)
      return DocStructureWalker.walkToNodePre(Node);
    ++NestingLevel;

    auto End = SrcManager.getLocOffsetInBuffer(Node.Range.getEnd(), BufferID),
      Start = SrcManager.getLocOffsetInBuffer(Node.Range.getStart(), BufferID);

    if (NestingLevel > 1) {
      // We're nested inside the previously reported token - merge
      SyntaxMap.mergeToken({Start, End - Start, Node.Kind});
    } else {
      // We're a top-level token, add it after the previous one
      SyntaxMap.addToken({Start, End - Start, Node.Kind});
    }

    return true;
  }

  bool walkToNodePost(SyntaxNode Node) override {
    if (Node.Kind == SyntaxNodeKind::CommentMarker)
      return DocStructureWalker.walkToNodePost(Node);
    --NestingLevel;

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
        for (auto &ArgElt : FTR->getArgsTypeRepr()->getElements()) {
          CharSourceRange NR;
          CharSourceRange TR;
          auto name = ArgElt.Name;
          if (!name.empty()) {
            NR = CharSourceRange(ArgElt.NameLoc,
                                 name.getLength());
          }
          SourceLoc SRE = Lexer::getLocForEndOfToken(SM,
                                                  ArgElt.Type->getEndLoc());
          TR = CharSourceRange(SM, ArgElt.Type->getStartLoc(), SRE);
          Info.Params.emplace_back(NR, TR);
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
  std::pair<Expr*, bool> enclosingCallExprArg(SourceFile &SF, SourceLoc SL) {

    class CallExprFinder : public SourceEntityWalker {
    public:
      const SourceManager &SM;
      SourceLoc TargetLoc;
      std::pair<Expr *, Expr*> EnclosingCallAndArg;
      Expr *OuterExpr;
      Stmt *OuterStmt;
      explicit CallExprFinder(const SourceManager &SM)
        :SM(SM) { }

      bool checkCallExpr(Expr *E) {
        Expr* Arg = nullptr;
        if (auto *CE = dyn_cast<CallExpr>(E)) {
          // Call expression can have argument.
          Arg = CE->getArg();
        } else if (auto UME = dyn_cast<UnresolvedMemberExpr>(E)) {
          // Unresolved member can have argument too.
          Arg = UME->getArgument();
        }
        if (!Arg)
          return false;
        if (EnclosingCallAndArg.first)
          OuterExpr = EnclosingCallAndArg.first;
        EnclosingCallAndArg = {E, Arg};
        return true;
      }

      bool walkToExprPre(Expr *E) override {
        auto SR = E->getSourceRange();
        if (SR.isValid() && SM.rangeContainsTokenLoc(SR, TargetLoc)) {
          if (!checkCallExpr(E) && !EnclosingCallAndArg.first)
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
          if (!EnclosingCallAndArg.first) {
            if (isa<BraceStmt>(S))
              // In case OuterStmt is already set, we should clear it to nullptr.
              OuterStmt = nullptr;
            else
              OuterStmt = S;
          }
        }
        return true;
      }

      Expr *findEnclosingCallArg(SourceFile &SF, SourceLoc SL) {
        EnclosingCallAndArg = {nullptr, nullptr};
        OuterExpr = nullptr;
        OuterStmt = nullptr;
        TargetLoc = SL;
        walk(SF);
        return EnclosingCallAndArg.second;
      }
    };

    CallExprFinder CEFinder(SM);
    auto *CE = CEFinder.findEnclosingCallArg(SF, SL);

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
    auto ECE = enclosingCallExprArg(SF, PlaceholderStartLoc);
    Expr *Args = ECE.first;
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

  Impl.Edited = false;
  Impl.EditableBuffer =
      new EditableTextBuffer(Impl.FilePath, Buf->getBuffer());

  // Reset the syntax map data and affected range
  Impl.SyntaxMap.Tokens.clear();
  Impl.AffectedRange = {0, static_cast<unsigned>(Buf->getBufferSize())};

  Impl.SemanticInfo =
      new SwiftDocumentSemanticInfo(Impl.FilePath, Impl.LangSupport);
  Impl.SemanticInfo->setCompilerArgs(Args);
  return Impl.EditableBuffer->getSnapshot();
}

ImmutableTextSnapshotRef SwiftEditorDocument::replaceText(
    unsigned Offset, unsigned Length, llvm::MemoryBuffer *Buf,
    bool ProvideSemanticInfo, std::string &error) {

  llvm::sys::ScopedLock L(Impl.AccessMtx);

  // Validate offset and length.
  if ((Offset + Length) > Impl.EditableBuffer->getSize()) {
    error = "'offset' + 'length' is out of range";
    return nullptr;
  }

  Impl.Edited = true;
  llvm::StringRef Str = Buf->getBuffer();

  // Update the buffer itself
  ImmutableTextSnapshotRef Snapshot =
      Impl.EditableBuffer->replace(Offset, Length, Str);

  if (ProvideSemanticInfo) {
    // If this is not a no-op, update semantic info.
    if (Length != 0 || Buf->getBufferSize() != 0) {
      updateSemaInfo();

      // FIXME: we should also update any "interesting" ASTs that depend on this
      // document here, e.g. any ASTs for files visible in an editor. However,
      // because our API conflates this with any file with unsaved changes we do
      // not update all open documents, since there could be too many of them.
    }
  }

  // Update the old syntax map offsets to account for the replaced range.
  // Also set the initial AffectedRange to cover any tokens that
  // the replaced range intersected. This allows for clients that split
  // multi-line tokens at line boundaries, and ensure all parts of these tokens
  // will be cleared.
  Impl.AffectedRange = Impl.SyntaxMap.adjustForReplacement(Offset, Length, Str.size());

  return Snapshot;
}

void SwiftEditorDocument::updateSemaInfo() {
  if (Impl.SemanticInfo) {
    Impl.SemanticInfo->processLatestSnapshotAsync(Impl.EditableBuffer);
  }
}

void SwiftEditorDocument::parse(ImmutableTextSnapshotRef Snapshot,
                                SwiftLangSupport &Lang, bool BuildSyntaxTree,
                                SyntaxParsingCache *SyntaxCache) {
  llvm::sys::ScopedLock L(Impl.AccessMtx);

  assert(Impl.SemanticInfo && "Impl.SemanticInfo must be set");

  std::vector<std::string> Args;
  std::string PrimaryFile; // Ignored, Impl.FilePath will be used

  CompilerInvocation CompInv;
  if (Impl.SemanticInfo->getInvocation()) {
    Impl.SemanticInfo->getInvocation()->applyTo(CompInv);
    Impl.SemanticInfo->getInvocation()->raw(Args, PrimaryFile);
  } else {
    // Use stdin as a .swift input to satisfy the driver. Note that we don't
    // use Impl.FilePath here because it may be invalid filename for driver
    // like "" or "-foobar".
    SmallVector<const char *, 1> Args;
    Args.push_back("-");
    std::string Error;
    // Ignore possible error(s)
    Lang.getASTManager().
      initCompilerInvocation(CompInv, Args, StringRef(), Error);
  }
  CompInv.getLangOptions().BuildSyntaxTree = BuildSyntaxTree;
  CompInv.setMainFileSyntaxParsingCache(SyntaxCache);
  // When reuse parts of the syntax tree from a SyntaxParsingCache, not
  // all tokens are visited and thus token collection is invalid
  CompInv.getLangOptions().CollectParsedToken = (SyntaxCache == nullptr);
  // Access to Impl.SyntaxInfo is guarded by Impl.AccessMtx
  Impl.SyntaxInfo.reset(
    new SwiftDocumentSyntaxInfo(CompInv, Snapshot, Args, Impl.FilePath));

  Impl.SyntaxInfo->parse();
}

void SwiftEditorDocument::readSyntaxInfo(EditorConsumer &Consumer) {
  llvm::sys::ScopedLock L(Impl.AccessMtx);

  Impl.ParserDiagnostics = Impl.SyntaxInfo->getDiagnostics();

  SwiftSyntaxMap NewMap = SwiftSyntaxMap(Impl.SyntaxMap.Tokens.size() + 16);

  if (Consumer.syntaxTreeEnabled()) {
    auto SyntaxTree = Impl.SyntaxInfo->getSourceFile().getSyntaxRoot();
    Impl.SyntaxTree.emplace(SyntaxTree);
    if (Consumer.syntaxMapEnabled()) {
      Consumer.handleRequestError(
          "Retrieving both a syntax map and a syntax tree at the same time is "
          "not supported. Use the SyntaxClassifier in swiftSyntax to generate "
          "the syntax map on the Swift side.");
    }
    if (Consumer.documentStructureEnabled()) {
      Consumer.handleRequestError(
          "Retrieving both the document structure and a syntax tree at the "
          "same time is not supported. Use the syntax tree to compute the "
          "document structure.");
    }
  } else {
    ide::SyntaxModelContext ModelContext(Impl.SyntaxInfo->getSourceFile());

    SwiftEditorSyntaxWalker SyntaxWalker(
        NewMap, Impl.SyntaxInfo->getSourceManager(), Consumer,
        Impl.SyntaxInfo->getBufferID());
    ModelContext.walk(SyntaxWalker);

    bool SawChanges = true;
    if (Impl.Edited) {
      // We're ansering an edit request. Report all highlighted token ranges not
      // in the previous syntax map to the Consumer and extend the AffectedRange
      // to contain all added/removed token ranges.
      SawChanges =
          NewMap.forEachChanged(Impl.SyntaxMap, Impl.AffectedRange, Consumer);
    } else {
      // The is an open/initialise. Report all highlighted token ranges to the
      // Consumer.
      NewMap.forEach(Consumer);
    }
    Impl.SyntaxMap = std::move(NewMap);

    // Recording an affected length of 0 still results in the client updating
    // its copy of the syntax map (by clearning all tokens on the line of the
    // affected offset). We need to not record it at all to signal a no-op.
    if (SawChanges)
      Consumer.recordAffectedRange(Impl.AffectedRange->Offset,
                                   Impl.AffectedRange->length());
  }
}

void SwiftEditorDocument::readSemanticInfo(ImmutableTextSnapshotRef Snapshot,
                                           EditorConsumer& Consumer) {
  std::vector<SwiftSemanticToken> SemaToks;
  Optional<std::vector<DiagnosticEntryInfo>> SemaDiags;
  Impl.SemanticInfo->readSemanticInfo(Snapshot, SemaToks, SemaDiags,
                                      Impl.ParserDiagnostics);

  for (auto SemaTok : SemaToks) {
    unsigned Offset = SemaTok.ByteOffset;
    unsigned Length = SemaTok.Length;
    UIdent Kind = SemaTok.getUIdentForKind();
    bool IsSystem = SemaTok.getIsSystem();
    if (Kind.isValid())
      Consumer.handleSemanticAnnotation(Offset, Length, Kind, IsSystem);
  }

  static UIdent SemaDiagStage("source.diagnostic.stage.swift.sema");
  static UIdent ParseDiagStage("source.diagnostic.stage.swift.parse");

  // If there's no value returned for diagnostics it means they are out-of-date
  // (based on a different snapshot).
  if (SemaDiags.hasValue()) {
    Consumer.setDiagnosticStage(SemaDiagStage);
    for (auto &Diag : SemaDiags.getValue())
      Consumer.handleDiagnostic(Diag, SemaDiagStage);
  } else {
    Consumer.setDiagnosticStage(ParseDiagStage);
    for (auto &Diag : Impl.ParserDiagnostics)
      Consumer.handleDiagnostic(Diag, ParseDiagStage);
  }
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

const llvm::Optional<swift::SourceFileSyntax> &
SwiftEditorDocument::getSyntaxTree() const {
  return Impl.SyntaxTree;
}

std::string SwiftEditorDocument::getFilePath() const { return Impl.FilePath; }

bool SwiftEditorDocument::hasUpToDateAST() const {
  return Impl.SyntaxInfo->hasUpToDateAST();
}

void SwiftEditorDocument::formatText(unsigned Line, unsigned Length,
                                     EditorConsumer &Consumer) {
  auto SyntaxInfo = Impl.getSyntaxInfo();
  SourceFile &SF = SyntaxInfo->getSourceFile();
  SourceManager &SM = SyntaxInfo->getSourceManager();

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
                                  EditorConsumer &Consumer,
                                  ArrayRef<const char *> Args) {

  ImmutableTextSnapshotRef Snapshot = nullptr;
  auto EditorDoc = EditorDocuments.getByUnresolvedName(Name);
  if (!EditorDoc) {
    EditorDoc = new SwiftEditorDocument(Name, *this);
    Snapshot = EditorDoc->initializeText(Buf, Args);
    EditorDoc->parse(Snapshot, *this, Consumer.syntaxTreeEnabled());
    if (EditorDocuments.getOrUpdate(Name, *this, EditorDoc)) {
      // Document already exists, re-initialize it. This should only happen
      // if we get OPEN request while the previous document is not closed.
      LOG_WARN_FUNC("Document already exists in editorOpen(..): " << Name);
      Snapshot = nullptr;
    }
    auto numOpen = ++Stats.numOpenDocs;
    Stats.maxOpenDocs.updateMax(numOpen);
  }

  if (!Snapshot) {
    Snapshot = EditorDoc->initializeText(Buf, Args);
    EditorDoc->parse(Snapshot, *this, Consumer.syntaxTreeEnabled());
  }

  if (Consumer.needsSemanticInfo()) {
    EditorDoc->updateSemaInfo();
  }

  EditorDoc->readSyntaxInfo(Consumer);
  EditorDoc->readSemanticInfo(Snapshot, Consumer);

  if (Consumer.syntaxTreeEnabled()) {
    assert(EditorDoc->getSyntaxTree().hasValue());
    std::unordered_set<unsigned> ReusedNodeIds;
    Consumer.handleSyntaxTree(EditorDoc->getSyntaxTree().getValue(), 
                              ReusedNodeIds);
  }
}


//===----------------------------------------------------------------------===//
// EditorClose
//===----------------------------------------------------------------------===//

void SwiftLangSupport::editorClose(StringRef Name, bool RemoveCache) {
  auto Removed = EditorDocuments.remove(Name);
  if (Removed) {
    --Stats.numOpenDocs;
  } else {
    IFaceGenContexts.remove(Name);
  }

  if (Removed && RemoveCache)
    Removed->removeCachedAST();
  // FIXME: Report error if Name did not apply to anything ?
}


//===----------------------------------------------------------------------===//
// EditorReplaceText
//===----------------------------------------------------------------------===//

void verifyIncrementalParse(SwiftEditorDocumentRef EditorDoc,
                            unsigned EditOffset, unsigned EditLength,
                            StringRef PreEditText, StringRef ReplaceText) {
  swift::json::Output::UserInfoMap JsonUserInfo;
  JsonUserInfo[swift::json::DontSerializeNodeIdsUserInfoKey] =
      reinterpret_cast<void *>(true);

  // Dump the incremental syntax tree
  std::string IncrTreeString;
  llvm::raw_string_ostream IncrTreeStream(IncrTreeString);
  swift::json::Output IncrTreeOutput(IncrTreeStream, JsonUserInfo);
  IncrTreeOutput << *EditorDoc->getSyntaxTree()->getRaw();

  // Reparse the file from scratch
  CompilerInvocation Invocation;
  Invocation.getLangOptions().BuildSyntaxTree = true;
  std::vector<std::string> Args;
  SwiftDocumentSyntaxInfo ScratchSyntaxInfo(Invocation,
                                            EditorDoc->getLatestSnapshot(),
                                            Args, EditorDoc->getFilePath());
  ScratchSyntaxInfo.parse();

  // Dump the from-scratch syntax tree
  std::string FromScratchTreeString;
  llvm::raw_string_ostream ScratchTreeStream(FromScratchTreeString);
  swift::json::Output ScratchTreeOutput(ScratchTreeStream, JsonUserInfo);
  auto SyntaxRoot = ScratchSyntaxInfo.getSourceFile().getSyntaxRoot();
  ScratchTreeOutput << *SyntaxRoot.getRaw();

  // If the serialized format of the two trees doesn't match incremental parsing
  // we have found an error.
  if (IncrTreeStream.str().compare(ScratchTreeStream.str())) {
    LOG_SECTION("Incremental Parsing", Warning) {
      Log->getOS() << "Incremental parsing different to from scratch parsing\n";
      Log->getOS() << "Edit was " << EditOffset << "-"
                   << (EditOffset + EditLength) << "='" << ReplaceText << "'"
                   << " pre-edit-text: '" << PreEditText << "'\n";

      SmallString<32> DirectoryName;
      if (llvm::sys::fs::createUniqueDirectory(
              "SourceKit-IncrementalParsing-Inconsistency", DirectoryName)) {
        Log->getOS() << "Failed to create log directory\n";
      }

      std::error_code ErrorCode;

      // Write the incremental syntax tree
      auto IncrTreeFilename = DirectoryName + "/incrementalTree.json";
      llvm::raw_fd_ostream IncrementalFilestream(
          IncrTreeFilename.str(), ErrorCode,
          llvm::sys::fs::FA_Read | llvm::sys::fs::FA_Write);
      IncrementalFilestream << IncrTreeStream.str();
      if (ErrorCode) {
        Log->getOS() << "Failed to write incremental syntax tree to "
                     << IncrTreeFilename << "(error code " << ErrorCode.value()
                     << ": " << ErrorCode.message() << ")\n";
      } else {
        Log->getOS() << "Incremental syntax tree written to "
                     << IncrTreeFilename << '\n';
      }

      // Write from-scratch syntax tree
      auto ScratchTreeFilename = DirectoryName + "/fromScratchTree.json";
      llvm::raw_fd_ostream ScratchTreeFilestream(
          ScratchTreeFilename.str(), ErrorCode,
          llvm::sys::fs::FA_Read | llvm::sys::fs::FA_Write);
      ScratchTreeFilestream << ScratchTreeStream.str();
      if (ErrorCode) {
        Log->getOS() << "Failed to write from-scratch syntax tree to "
                     << ScratchTreeFilename << "(error code "
                     << ErrorCode.value() << ": " << ErrorCode.message()
                     << ")\n";
      } else {
        Log->getOS() << "From-scratch syntax tree written to "
                     << ScratchTreeFilename << '\n';
      }

      // Write source file
      auto SourceFilename = DirectoryName + "/postEditSource.swift";
      llvm::raw_fd_ostream SourceFilestream(SourceFilename.str(), ErrorCode,
                              llvm::sys::fs::FA_Read | llvm::sys::fs::FA_Write);
      auto FileBuffer = EditorDoc->getLatestSnapshot()->getBuffer();
      SourceFilestream << FileBuffer->getText();
    }
  }
}

void SwiftLangSupport::editorReplaceText(StringRef Name,
                                         llvm::MemoryBuffer *Buf,
                                         unsigned Offset, unsigned Length,
                                         EditorConsumer &Consumer) {
  bool LogReuseRegions = ::getenv("SOURCEKIT_LOG_INCREMENTAL_REUSE_REGIONS");
  bool ValidateSyntaxTree = ::getenv("SOURCEKIT_INCREMENTAL_PARSE_VALIDATION");

  auto EditorDoc = EditorDocuments.getByUnresolvedName(Name);
  if (!EditorDoc) {
    Consumer.handleRequestError("No associated Editor Document");
    return;
  }

  ImmutableTextSnapshotRef Snapshot;
  if (Length != 0 || Buf->getBufferSize() != 0) {
    std::string PreEditText;
    if (ValidateSyntaxTree) {
      auto CurBuffer = EditorDoc->getLatestSnapshot()->getBuffer();
      auto BufferStart = CurBuffer->getInternalBuffer()->getBufferStart();
      StringRef PreEditTextRef(BufferStart + Offset, Length);
      PreEditText = PreEditTextRef.str();
    }
    std::string error;
    Snapshot = EditorDoc->replaceText(Offset, Length, Buf,
                                      Consumer.needsSemanticInfo(), error);
    if (!Snapshot) {
      assert(error.size());
      Consumer.handleRequestError(error.c_str());
      return;
    }

    llvm::Optional<SyntaxParsingCache> SyntaxCache = llvm::None;
    if (EditorDoc->getSyntaxTree().hasValue()) {
      SyntaxCache.emplace(EditorDoc->getSyntaxTree().getValue());
      SyntaxCache->addEdit(Offset, Offset + Length, Buf->getBufferSize());
    }

    SyntaxParsingCache *SyntaxCachePtr = nullptr;
    if (SyntaxCache.hasValue()) {
      SyntaxCachePtr = SyntaxCache.getPointer();
    }
    EditorDoc->parse(Snapshot, *this, Consumer.syntaxTreeEnabled(),
                     SyntaxCachePtr);
    EditorDoc->readSyntaxInfo(Consumer);

    // Log reuse information
    if (SyntaxCache.hasValue() && LogReuseRegions) {
      auto &SyntaxTree = EditorDoc->getSyntaxTree();
      auto ReuseRegions = SyntaxCache->getReusedRegions(*SyntaxTree);
      LOG_SECTION("SyntaxCache", InfoHighPrio) {
        Log->getOS() << "Reused ";

        bool FirstIteration = true;
        for (auto ReuseRegion : ReuseRegions) {
          if (!FirstIteration) {
            Log->getOS() << ", ";
          } else {
            FirstIteration = false;
          }

          Log->getOS() << ReuseRegion.Start << " - " << ReuseRegion.End;
        }
      }
    }

    if (Consumer.syntaxTreeEnabled()) {
      std::unordered_set<unsigned> ReusedNodeIds;
      if (SyntaxCache.hasValue()) {
        auto &ReusedVector = SyntaxCache->getReusedNodeIds();
        ReusedNodeIds = std::unordered_set<unsigned>(ReusedVector.begin(),
                                                     ReusedVector.end());
      }
      Consumer.handleSyntaxTree(EditorDoc->getSyntaxTree().getValue(),
                                ReusedNodeIds);
    }

    if (ValidateSyntaxTree) {
      verifyIncrementalParse(EditorDoc, Offset, Length, PreEditText,
                             Buf->getBuffer());
    }
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

  if (!EditorDoc->hasUpToDateAST()) {
    // An up-to-date AST is needed for formatting. If it does not exist, fall
    // back to a full reparse of the file
    EditorDoc->parse(EditorDoc->getLatestSnapshot(), *this,
                     /*BuildSyntaxTree=*/true);
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
