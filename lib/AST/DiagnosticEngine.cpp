//===--- DiagnosticEngine.cpp - Diagnostic Display Engine -----------------===//
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
//
//  This file defines the DiagnosticEngine class, which manages any diagnostics
//  emitted by Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticSuppression.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Config.h"
#include "swift/Parse/Lexer.h" // bad dependency
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace {
enum class DiagnosticOptions {
  /// No options.
  none,

  /// The location of this diagnostic points to the beginning of the first
  /// token that the parser considers invalid.  If this token is located at the
  /// beginning of the line, then the location is adjusted to point to the end
  /// of the previous token.
  ///
  /// This behavior improves experience for "expected token X" diagnostics.
  PointsToFirstBadToken,

  /// After a fatal error subsequent diagnostics are suppressed.
  Fatal,
};
struct StoredDiagnosticInfo {
  DiagnosticKind kind : 2;
  bool pointsToFirstBadToken : 1;
  bool isFatal : 1;

  constexpr StoredDiagnosticInfo(DiagnosticKind k, bool firstBadToken,
                                 bool fatal)
      : kind(k), pointsToFirstBadToken(firstBadToken), isFatal(fatal) {}
  constexpr StoredDiagnosticInfo(DiagnosticKind k, DiagnosticOptions opts)
      : StoredDiagnosticInfo(k,
                             opts == DiagnosticOptions::PointsToFirstBadToken,
                             opts == DiagnosticOptions::Fatal) {}
};

// Reproduce the DiagIDs, as we want both the size and access to the raw ids
// themselves.
enum LocalDiagID : uint32_t {
#define DIAG(KIND, ID, Options, Text, Signature) ID,
#include "swift/AST/DiagnosticsAll.def"
  NumDiags
};
} // end anonymous namespace

// TODO: categorization
static const constexpr StoredDiagnosticInfo storedDiagnosticInfos[] = {
#define ERROR(ID, Options, Text, Signature)                                    \
  StoredDiagnosticInfo(DiagnosticKind::Error, DiagnosticOptions::Options),
#define WARNING(ID, Options, Text, Signature)                                  \
  StoredDiagnosticInfo(DiagnosticKind::Warning, DiagnosticOptions::Options),
#define NOTE(ID, Options, Text, Signature)                                     \
  StoredDiagnosticInfo(DiagnosticKind::Note, DiagnosticOptions::Options),
#define REMARK(ID, Options, Text, Signature)                                   \
  StoredDiagnosticInfo(DiagnosticKind::Remark, DiagnosticOptions::Options),
#include "swift/AST/DiagnosticsAll.def"
};
static_assert(sizeof(storedDiagnosticInfos) / sizeof(StoredDiagnosticInfo) ==
                  LocalDiagID::NumDiags,
              "array size mismatch");

static constexpr const char * const diagnosticStrings[] = {
#define ERROR(ID, Options, Text, Signature) Text,
#define WARNING(ID, Options, Text, Signature) Text,
#define NOTE(ID, Options, Text, Signature) Text,
#define REMARK(ID, Options, Text, Signature) Text,
#include "swift/AST/DiagnosticsAll.def"
    "<not a diagnostic>",
};

static constexpr const char *const debugDiagnosticStrings[] = {
#define ERROR(ID, Options, Text, Signature) Text " [" #ID "]",
#define WARNING(ID, Options, Text, Signature) Text " [" #ID "]",
#define NOTE(ID, Options, Text, Signature) Text " [" #ID "]",
#define REMARK(ID, Options, Text, Signature) Text " [" #ID "]",
#include "swift/AST/DiagnosticsAll.def"
    "<not a diagnostic>",
};

static constexpr const char *const fixItStrings[] = {
#define FIXIT(ID, Text, Signature) Text,
#include "swift/AST/FixIts.def"
    "<not a fix-it>",
};

DiagnosticState::DiagnosticState() {
  // Initialize our per-diagnostic state to default
  perDiagnosticBehavior.resize(LocalDiagID::NumDiags, Behavior::Unspecified);
}

static CharSourceRange toCharSourceRange(SourceManager &SM, SourceRange SR) {
  return CharSourceRange(SM, SR.Start, Lexer::getLocForEndOfToken(SM, SR.End));
}

static CharSourceRange toCharSourceRange(SourceManager &SM, SourceLoc Start,
                                         SourceLoc End) {
  return CharSourceRange(SM, Start, End);
}

/// Extract a character at \p Loc. If \p Loc is the end of the buffer,
/// return '\f'.
static char extractCharAfter(SourceManager &SM, SourceLoc Loc) {
  auto chars = SM.extractText({Loc, 1});
  return chars.empty() ? '\f' : chars[0];
}

/// Extract a character immediately before \p Loc. If \p Loc is the
/// start of the buffer, return '\f'.
static char extractCharBefore(SourceManager &SM, SourceLoc Loc) {
  // We have to be careful not to go off the front of the buffer.
  auto bufferID = SM.findBufferContainingLoc(Loc);
  auto bufferRange = SM.getRangeForBuffer(bufferID);
  if (bufferRange.getStart() == Loc)
    return '\f';
  auto chars = SM.extractText({Loc.getAdvancedLoc(-1), 1}, bufferID);
  assert(!chars.empty() && "Couldn't extractText with valid range");
  return chars[0];
}

InFlightDiagnostic &InFlightDiagnostic::highlight(SourceRange R) {
  assert(IsActive && "Cannot modify an inactive diagnostic");
  if (Engine && R.isValid())
    Engine->getActiveDiagnostic()
        .addRange(toCharSourceRange(Engine->SourceMgr, R));
  return *this;
}

InFlightDiagnostic &InFlightDiagnostic::highlightChars(SourceLoc Start,
                                                       SourceLoc End) {
  assert(IsActive && "Cannot modify an inactive diagnostic");
  if (Engine && Start.isValid())
    Engine->getActiveDiagnostic()
        .addRange(toCharSourceRange(Engine->SourceMgr, Start, End));
  return *this;
}

/// Add an insertion fix-it to the currently-active diagnostic.  The
/// text is inserted immediately *after* the token specified.
///
InFlightDiagnostic &
InFlightDiagnostic::fixItInsertAfter(SourceLoc L, StringRef Str,
                                     ArrayRef<DiagnosticArgument> Args) {
  L = Lexer::getLocForEndOfToken(Engine->SourceMgr, L);
  return fixItInsert(L, Str, std::move(Args));
}

/// Add a token-based removal fix-it to the currently-active
/// diagnostic.
InFlightDiagnostic &InFlightDiagnostic::fixItRemove(SourceRange R) {
  assert(IsActive && "Cannot modify an inactive diagnostic");
  if (R.isInvalid() || !Engine) return *this;

  // Convert from a token range to a CharSourceRange, which points to the end of
  // the token we want to remove.
  auto &SM = Engine->SourceMgr;
  auto charRange = toCharSourceRange(SM, R);

  // If we're removing something (e.g. a keyword), do a bit of extra work to
  // make sure that we leave the code in a good place, without extraneous white
  // space around its hole.  Specifically, check to see there is whitespace
  // before and after the end of range.  If so, nuke the space afterward to keep
  // things consistent.
  if (extractCharAfter(SM, charRange.getEnd()) == ' ' &&
      isspace(extractCharBefore(SM, charRange.getStart()))) {
    charRange = CharSourceRange(charRange.getStart(),
                                charRange.getByteLength()+1);
  }
  Engine->getActiveDiagnostic().addFixIt(Diagnostic::FixIt(charRange, {}, {}));
  return *this;
}

InFlightDiagnostic &
InFlightDiagnostic::fixItReplace(SourceRange R, StringRef Str,
                                 ArrayRef<DiagnosticArgument> Args) {
  if (Str.empty())
    return fixItRemove(R);

  assert(IsActive && "Cannot modify an inactive diagnostic");
  if (R.isInvalid() || !Engine) return *this;

  auto &SM = Engine->SourceMgr;
  auto charRange = toCharSourceRange(SM, R);

  // If we're replacing with something that wants spaces around it, do a bit of
  // extra work so that we don't suggest extra spaces.
  if (Str.back() == ' ') {
    if (isspace(extractCharAfter(SM, charRange.getEnd())))
      Str = Str.drop_back();
  }
  if (!Str.empty() && Str.front() == ' ') {
    if (isspace(extractCharBefore(SM, charRange.getStart())))
      Str = Str.drop_front();
  }

  Engine->getActiveDiagnostic().addFixIt(
      Diagnostic::FixIt(charRange, Str, std::move(Args)));
  return *this;
}

InFlightDiagnostic &
InFlightDiagnostic::fixItReplaceChars(SourceLoc Start, SourceLoc End,
                                      StringRef Str,
                                      ArrayRef<DiagnosticArgument> Args) {
  assert(IsActive && "Cannot modify an inactive diagnostic");
  if (Engine && Start.isValid())
    Engine->getActiveDiagnostic().addFixIt(
        Diagnostic::FixIt(toCharSourceRange(Engine->SourceMgr, Start, End), Str,
                          std::move(Args)));
  return *this;
}

InFlightDiagnostic &InFlightDiagnostic::fixItExchange(SourceRange R1,
                                                      SourceRange R2) {
  assert(IsActive && "Cannot modify an inactive diagnostic");

  auto &SM = Engine->SourceMgr;
  // Convert from a token range to a CharSourceRange
  auto charRange1 = toCharSourceRange(SM, R1);
  auto charRange2 = toCharSourceRange(SM, R2);
  // Extract source text.
  auto text1 = SM.extractText(charRange1);
  auto text2 = SM.extractText(charRange2);

  Engine->getActiveDiagnostic().addFixIt(
      Diagnostic::FixIt(charRange1, text2, {}));
  Engine->getActiveDiagnostic().addFixIt(
      Diagnostic::FixIt(charRange2, text1, {}));
  return *this;
}

void InFlightDiagnostic::flush() {
  if (!IsActive)
    return;
  
  IsActive = false;
  if (Engine)
    Engine->flushActiveDiagnostic();
}

bool DiagnosticEngine::isDiagnosticPointsToFirstBadToken(DiagID ID) const {
  return storedDiagnosticInfos[(unsigned) ID].pointsToFirstBadToken;
}

bool DiagnosticEngine::finishProcessing() {
  bool hadError = false;
  for (auto &Consumer : Consumers) {
    hadError |= Consumer->finishProcessing();
  }
  return hadError;
}

static DiagnosticKind toDiagnosticKind(DiagnosticState::Behavior behavior) {
  switch (behavior) {
  case DiagnosticState::Behavior::Unspecified:
    llvm_unreachable("unspecified behavior");
  case DiagnosticState::Behavior::Ignore:
    llvm_unreachable("trying to map an ignored diagnostic");
  case DiagnosticState::Behavior::Error:
  case DiagnosticState::Behavior::Fatal:
    return DiagnosticKind::Error;
  case DiagnosticState::Behavior::Note:
    return DiagnosticKind::Note;
  case DiagnosticState::Behavior::Warning:
    return DiagnosticKind::Warning;
  case DiagnosticState::Behavior::Remark:
    return DiagnosticKind::Remark;
  }

  llvm_unreachable("Unhandled DiagnosticKind in switch.");
}

// A special option only for compiler writers that causes Diagnostics to assert
// when a failure diagnostic is emitted. Intended for use in the debugger.
llvm::cl::opt<bool> AssertOnError("swift-diagnostics-assert-on-error",
                                  llvm::cl::init(false));
// A special option only for compiler writers that causes Diagnostics to assert
// when a warning diagnostic is emitted. Intended for use in the debugger.
llvm::cl::opt<bool> AssertOnWarning("swift-diagnostics-assert-on-warning",
                                    llvm::cl::init(false));

DiagnosticState::Behavior DiagnosticState::determineBehavior(DiagID id) {
  auto set = [this](DiagnosticState::Behavior lvl) {
    if (lvl == Behavior::Fatal) {
      fatalErrorOccurred = true;
      anyErrorOccurred = true;
    } else if (lvl == Behavior::Error) {
      anyErrorOccurred = true;
    }

    assert((!AssertOnError || !anyErrorOccurred) && "We emitted an error?!");
    assert((!AssertOnWarning || (lvl != Behavior::Warning)) &&
           "We emitted a warning?!");
    previousBehavior = lvl;
    return lvl;
  };

  // We determine how to handle a diagnostic based on the following rules
  //   1) If current state dictates a certain behavior, follow that
  //   2) If the user provided a behavior for this specific diagnostic, follow
  //      that
  //   3) If the user provided a behavior for this diagnostic's kind, follow
  //      that
  //   4) Otherwise remap the diagnostic kind

  auto diagInfo = storedDiagnosticInfos[(unsigned)id];
  bool isNote = diagInfo.kind == DiagnosticKind::Note;

  //   1) If current state dictates a certain behavior, follow that

  // Notes relating to ignored diagnostics should also be ignored
  if (previousBehavior == Behavior::Ignore && isNote)
    return set(Behavior::Ignore);

  // Suppress diagnostics when in a fatal state, except for follow-on notes
  if (fatalErrorOccurred)
    if (!showDiagnosticsAfterFatalError && !isNote)
      return set(Behavior::Ignore);

  //   2) If the user provided a behavior for this specific diagnostic, follow
  //      that

  if (perDiagnosticBehavior[(unsigned)id] != Behavior::Unspecified)
    return set(perDiagnosticBehavior[(unsigned)id]);

  //   3) If the user provided a behavior for this diagnostic's kind, follow
  //      that
  if (diagInfo.kind == DiagnosticKind::Warning) {
    if (suppressWarnings)
      return set(Behavior::Ignore);
    if (warningsAsErrors)
      return set(Behavior::Error);
  }

  //   4) Otherwise remap the diagnostic kind
  switch (diagInfo.kind) {
  case DiagnosticKind::Note:
    return set(Behavior::Note);
  case DiagnosticKind::Error:
    return set(diagInfo.isFatal ? Behavior::Fatal : Behavior::Error);
  case DiagnosticKind::Warning:
    return set(Behavior::Warning);
  case DiagnosticKind::Remark:
    return set(Behavior::Remark);
  }

  llvm_unreachable("Unhandled DiagnosticKind in switch.");
}

void DiagnosticEngine::flushActiveDiagnostic() {
  assert(ActiveDiagnostic && "No active diagnostic to flush");
  if (TransactionCount == 0) {
    emitDiagnostic(*ActiveDiagnostic);
  } else {
    onTentativeDiagnosticFlush(*ActiveDiagnostic);
    TentativeDiagnostics.emplace_back(std::move(*ActiveDiagnostic));
  }
  ActiveDiagnostic.reset();
}

void DiagnosticEngine::emitTentativeDiagnostics() {
  for (auto &diag : TentativeDiagnostics) {
    emitDiagnostic(diag);
  }
  TentativeDiagnostics.clear();
}

void DiagnosticEngine::emitDiagnostic(const Diagnostic &diagnostic) {
  auto behavior = state.determineBehavior(diagnostic.getID());
  if (behavior == DiagnosticState::Behavior::Ignore)
    return;

  // Figure out the source location.
  SourceLoc loc = diagnostic.getLoc();
  if (loc.isInvalid() && diagnostic.getDecl()) {
    const Decl *decl = diagnostic.getDecl();
    // If a declaration was provided instead of a location, and that declaration
    // has a location we can point to, use that location.
    loc = decl->getLoc();

    if (loc.isInvalid()) {
      // There is no location we can point to. Pretty-print the declaration
      // so we can point to it.
      SourceLoc ppLoc = PrettyPrintedDeclarations[decl];
      if (ppLoc.isInvalid()) {
        class TrackingPrinter : public StreamPrinter {
          SmallVectorImpl<std::pair<const Decl *, uint64_t>> &Entries;

        public:
          TrackingPrinter(
              SmallVectorImpl<std::pair<const Decl *, uint64_t>> &Entries,
              raw_ostream &OS) :
            StreamPrinter(OS), Entries(Entries) {}

          void printDeclLoc(const Decl *D) override {
            Entries.push_back({ D, OS.tell() });
          }
        };
        SmallVector<std::pair<const Decl *, uint64_t>, 8> entries;
        llvm::SmallString<128> buffer;
        llvm::SmallString<128> bufferName;
        {
          // Figure out which declaration to print. It's the top-most
          // declaration (not a module).
          const Decl *ppDecl = decl;
          auto dc = decl->getDeclContext();

          // FIXME: Horrible, horrible hackaround. We're not getting a
          // DeclContext everywhere we should.
          if (!dc) {
            return;
          }

          while (!dc->isModuleContext()) {
            switch (dc->getContextKind()) {
            case DeclContextKind::Module:
              llvm_unreachable("Not in a module context!");
              break;

            case DeclContextKind::FileUnit:
            case DeclContextKind::TopLevelCodeDecl:
              break;

            case DeclContextKind::ExtensionDecl:
              ppDecl = cast<ExtensionDecl>(dc);
              break;

            case DeclContextKind::GenericTypeDecl:
              ppDecl = cast<GenericTypeDecl>(dc);
              break;

            case DeclContextKind::SerializedLocal:
            case DeclContextKind::Initializer:
            case DeclContextKind::AbstractClosureExpr:
            case DeclContextKind::AbstractFunctionDecl:
            case DeclContextKind::SubscriptDecl:
            case DeclContextKind::EnumElementDecl:
              break;
            }

            dc = dc->getParent();
          }

          // Build the module name path (in reverse), which we use to
          // build the name of the buffer.
          SmallVector<StringRef, 4> nameComponents;
          while (dc) {
            nameComponents.push_back(cast<ModuleDecl>(dc)->getName().str());
            dc = dc->getParent();
          }

          for (unsigned i = nameComponents.size(); i; --i) {
            bufferName += nameComponents[i-1];
            bufferName += '.';
          }

          if (auto value = dyn_cast<ValueDecl>(ppDecl)) {
            bufferName += value->getBaseName().userFacingName();
          } else if (auto ext = dyn_cast<ExtensionDecl>(ppDecl)) {
            bufferName += ext->getExtendedType().getString();
          }

          // Pretty-print the declaration we've picked.
          llvm::raw_svector_ostream out(buffer);
          TrackingPrinter printer(entries, out);
          ppDecl->print(printer, PrintOptions::printForDiagnostics());
        }

        // Build a buffer with the pretty-printed declaration.
        auto bufferID = SourceMgr.addMemBufferCopy(buffer, bufferName);
        auto memBufferStartLoc = SourceMgr.getLocForBufferStart(bufferID);

        // Go through all of the pretty-printed entries and record their
        // locations.
        for (auto entry : entries) {
          PrettyPrintedDeclarations[entry.first] =
              memBufferStartLoc.getAdvancedLoc(entry.second);
        }

        // Grab the pretty-printed location.
        ppLoc = PrettyPrintedDeclarations[decl];
      }

      loc = ppLoc;
    }
  }

  // Pass the diagnostic off to the consumer.
  DiagnosticInfo Info;
  Info.ID = diagnostic.getID();
  Info.Ranges = diagnostic.getRanges();
  Info.FixIts = diagnostic.getFixIts();
  for (auto &Consumer : Consumers) {
    Consumer->handleDiagnostic(
        SourceMgr, loc, toDiagnosticKind(behavior),
        diagnosticStringFor(Info.ID, getPrintDiagnosticNames()),
        diagnostic.getArgs(), Info, getDefaultDiagnosticLoc());
  }
}

const char *DiagnosticEngine::diagnosticStringFor(const DiagID id,
                                                  bool printDiagnosticName) {
  if (printDiagnosticName) {
    return debugDiagnosticStrings[(unsigned)id];
  }
  return diagnosticStrings[(unsigned)id];
}

const char *InFlightDiagnostic::fixItStringFor(const FixItID id) {
  return fixItStrings[(unsigned)id];
}

void DiagnosticEngine::setBufferIndirectlyCausingDiagnosticToInput(
    SourceLoc loc) {
  // If in the future, nested BufferIndirectlyCausingDiagnosticRAII need be
  // supported, the compiler will need a stack for
  // bufferIndirectlyCausingDiagnostic.
  assert(bufferIndirectlyCausingDiagnostic.isInvalid() &&
         "Buffer should not already be set.");
  bufferIndirectlyCausingDiagnostic = loc;
  assert(bufferIndirectlyCausingDiagnostic.isValid() &&
         "Buffer must be valid for previous assertion to work.");
}

void DiagnosticEngine::resetBufferIndirectlyCausingDiagnostic() {
  bufferIndirectlyCausingDiagnostic = SourceLoc();
}

DiagnosticSuppression::DiagnosticSuppression(DiagnosticEngine &diags)
  : diags(diags)
{
  consumers = diags.takeConsumers();
}

DiagnosticSuppression::~DiagnosticSuppression() {
  for (auto consumer : consumers)
    diags.addConsumer(*consumer);
}

BufferIndirectlyCausingDiagnosticRAII::BufferIndirectlyCausingDiagnosticRAII(
    const SourceFile &SF)
    : Diags(SF.getASTContext().Diags) {
  auto id = SF.getBufferID();
  if (!id)
    return;
  auto loc = SF.getASTContext().SourceMgr.getLocForBufferStart(*id);
  if (loc.isValid())
    Diags.setBufferIndirectlyCausingDiagnosticToInput(loc);
}

void DiagnosticEngine::onTentativeDiagnosticFlush(Diagnostic &diagnostic) {
  for (auto &argument : diagnostic.Args) {
    if (argument.getKind() != DiagnosticArgumentKind::String)
      continue;

    auto content = argument.getAsString();
    if (content.empty())
      continue;

    auto I = TransactionStrings.insert(std::make_pair(content, char())).first;
    argument = DiagnosticArgument(StringRef(I->getKeyData()));
  }
}
