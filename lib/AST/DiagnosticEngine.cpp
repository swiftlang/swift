//===--- DiagnosticEngine.cpp - Diagnostic Display Engine -----------------===//
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
//
//  This file defines the DiagnosticEngine class, which manages any diagnostics
//  emitted by Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticGroups.h"
#include "swift/AST/DiagnosticList.h"
#include "swift/AST/DiagnosticSuppression.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Config.h"
#include "swift/Localization/LocalizationFormat.h"
#include "swift/Parse/Lexer.h" // bad dependency
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/AST/Type.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

static_assert(IsTriviallyDestructible<ZeroArgDiagnostic>::value,
              "ZeroArgDiagnostic is meant to be trivially destructable");

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

  /// An API or ABI breakage diagnostic emitted by the API digester.
  APIDigesterBreakage,

  /// A deprecation warning or error.
  Deprecation,

  /// A diagnostic warning about an unused element.
  NoUsage,

  /// The diagnostic should be ignored by default, but will be re-enabled
  /// by various warning options (-Wwarning, -Werror). This only makes sense
  /// for warnings.
  DefaultIgnore,
};
struct StoredDiagnosticInfo {
  DiagnosticKind kind : 2;
  bool pointsToFirstBadToken : 1;
  bool isFatal : 1;
  bool isAPIDigesterBreakage : 1;
  bool isDeprecation : 1;
  bool isNoUsage : 1;
  bool defaultIgnore : 1;
  DiagGroupID groupID;

  constexpr StoredDiagnosticInfo(DiagnosticKind k, bool firstBadToken,
                                 bool fatal, bool isAPIDigesterBreakage,
                                 bool deprecation, bool noUsage,
                                 bool defaultIgnore, DiagGroupID groupID)
      : kind(k), pointsToFirstBadToken(firstBadToken), isFatal(fatal),
        isAPIDigesterBreakage(isAPIDigesterBreakage),
        isDeprecation(deprecation), isNoUsage(noUsage),
        defaultIgnore(defaultIgnore), groupID(groupID) {}
  constexpr StoredDiagnosticInfo(DiagnosticKind k, DiagnosticOptions opts,
                                 DiagGroupID groupID)
      : StoredDiagnosticInfo(k,
                             opts == DiagnosticOptions::PointsToFirstBadToken,
                             opts == DiagnosticOptions::Fatal,
                             opts == DiagnosticOptions::APIDigesterBreakage,
                             opts == DiagnosticOptions::Deprecation,
                             opts == DiagnosticOptions::NoUsage,
                             opts == DiagnosticOptions::DefaultIgnore,
                             groupID) {}
};
} // end anonymous namespace

// TODO: categorization
static const constexpr StoredDiagnosticInfo storedDiagnosticInfos[] = {
#define GROUPED_ERROR(ID, Group, Options, Text, Signature)                     \
  StoredDiagnosticInfo(DiagnosticKind::Error, DiagnosticOptions::Options,      \
                       DiagGroupID::Group),
#define GROUPED_WARNING(ID, Group, Options, Text, Signature)                   \
  StoredDiagnosticInfo(DiagnosticKind::Warning, DiagnosticOptions::Options,    \
                       DiagGroupID::Group),
#define NOTE(ID, Options, Text, Signature)                                     \
  StoredDiagnosticInfo(DiagnosticKind::Note, DiagnosticOptions::Options,       \
                       DiagGroupID::no_group),
#define REMARK(ID, Options, Text, Signature)                                   \
  StoredDiagnosticInfo(DiagnosticKind::Remark, DiagnosticOptions::Options,     \
                       DiagGroupID::no_group),
#include "swift/AST/DiagnosticsAll.def"
};
static_assert(sizeof(storedDiagnosticInfos) / sizeof(StoredDiagnosticInfo) ==
                  NumDiagIDs,
              "array size mismatch");

static constexpr const char * const diagnosticStrings[] = {
#define DIAG(KIND, ID, Group, Options, Text, Signature) Text,
#include "swift/AST/DiagnosticsAll.def"
    "<not a diagnostic>",
};

static constexpr const char *const diagnosticIDStrings[] = {
#define DIAG(KIND, ID, Group, Options, Text, Signature) #ID,
#include "swift/AST/DiagnosticsAll.def"
    "<not a diagnostic>",
};

static constexpr const char *const fixItStrings[] = {
#define DIAG(KIND, ID, Group, Options, Text, Signature)
#define FIXIT(ID, Text, Signature) Text,
#include "swift/AST/DiagnosticsAll.def"
    "<not a fix-it>",
};

DiagnosticState::DiagnosticState() {
  // Initialize our ignored diagnostics to defaults
  ignoredDiagnostics.reserve(NumDiagIDs);
  for (const auto &info : storedDiagnosticInfos) {
    ignoredDiagnostics.push_back(info.defaultIgnore);
  }

  // Initialize warningsAsErrors to default
  warningsAsErrors.resize(DiagGroupsCount);
}

Diagnostic::Diagnostic(DiagID ID)
    : Diagnostic(ID, storedDiagnosticInfos[(unsigned)ID].groupID) {}

std::optional<const DiagnosticInfo *> Diagnostic::getWrappedDiagnostic() const {
  for (const auto &arg : getArgs()) {
    if (arg.getKind() == DiagnosticArgumentKind::Diagnostic) {
      return arg.getAsDiagnostic();
    }
  }

  return std::nullopt;
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

InFlightDiagnostic &InFlightDiagnostic::highlightChars(CharSourceRange Range) {
  assert(IsActive && "Cannot modify an inactive diagnostic");
  if (Engine && Range.getStart().isValid())
    Engine->getActiveDiagnostic().addRange(Range);
  return *this;
}

/// Add an insertion fix-it to the currently-active diagnostic.  The
/// text is inserted immediately *after* the token specified.
///
InFlightDiagnostic &
InFlightDiagnostic::fixItInsertAfter(SourceLoc L, StringRef FormatString,
                                     ArrayRef<DiagnosticArgument> Args) {
  L = Lexer::getLocForEndOfToken(Engine->SourceMgr, L);
  return fixItInsert(L, FormatString, Args);
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
InFlightDiagnostic::fixItReplace(SourceRange R, StringRef FormatString,
                                 ArrayRef<DiagnosticArgument> Args) {
  auto &SM = Engine->SourceMgr;
  auto charRange = toCharSourceRange(SM, R);

  Engine->getActiveDiagnostic().addFixIt(
      Diagnostic::FixIt(charRange, FormatString, Args));
  return *this;
}

InFlightDiagnostic &InFlightDiagnostic::fixItReplace(SourceRange R,
                                                     StringRef Str) {
  if (Str.empty())
    return fixItRemove(R);

  assert(IsActive && "Cannot modify an inactive diagnostic");
  if (R.isInvalid() || !Engine) return *this;

  auto &SM = Engine->SourceMgr;
  auto charRange = toCharSourceRange(SM, R);

  // If we're replacing with something that wants spaces around it, do a bit of
  // extra work so that we don't suggest extra spaces.
  // FIXME: This could probably be applied to structured fix-its as well.
  if (Str.back() == ' ') {
    if (isspace(extractCharAfter(SM, charRange.getEnd())))
      Str = Str.drop_back();
  }
  if (!Str.empty() && Str.front() == ' ') {
    if (isspace(extractCharBefore(SM, charRange.getStart())))
      Str = Str.drop_front();
  }
  
  return fixItReplace(R, "%0", {Str});
}

InFlightDiagnostic &
InFlightDiagnostic::fixItReplaceChars(SourceLoc Start, SourceLoc End,
                                      StringRef FormatString,
                                      ArrayRef<DiagnosticArgument> Args) {
  assert(IsActive && "Cannot modify an inactive diagnostic");
  if (Engine && Start.isValid())
    Engine->getActiveDiagnostic().addFixIt(
        Diagnostic::FixIt(toCharSourceRange(Engine->SourceMgr, Start, End),
                          FormatString, Args));
  return *this;
}

SourceLoc DiagnosticEngine::getBestAddImportFixItLoc(SourceFile *SF) const {
  auto &SM = SourceMgr;

  SourceLoc bestLoc;
  if (!SF)
    return bestLoc;

  for (auto item : SF->getTopLevelItems()) {
    // If we found an import declaration, we want to insert after it.
    if (auto importDecl =
            dyn_cast_or_null<ImportDecl>(item.dyn_cast<Decl *>())) {
      SourceLoc loc = importDecl->getEndLoc();
      if (loc.isValid()) {
        bestLoc = Lexer::getLocForEndOfLine(SM, loc);
      }

      // Keep looking for more import declarations.
      continue;
    }

    // If we got a location based on import declarations, we're done.
    if (bestLoc.isValid())
      break;

    // For any other item, we want to insert before it.
    SourceLoc loc = item.getStartLoc();
    if (loc.isValid()) {
      bestLoc = Lexer::getLocForStartOfLine(SM, loc);
      break;
    }
  }

  return bestLoc;
}

InFlightDiagnostic &InFlightDiagnostic::fixItAddImport(StringRef ModuleName) {
  assert(IsActive && "Cannot modify an inactive diagnostic");
  auto decl = Engine->ActiveDiagnostic->getDecl();
  if (!decl)
    return *this;

  auto bestLoc = Engine->getBestAddImportFixItLoc(
      decl->getDeclContext()->getOutermostParentSourceFile());
  if (!bestLoc.isValid())
    return *this;

  llvm::SmallString<64> importText;
  importText += "import ";
  importText += ModuleName;
  importText += "\n";

  return fixItInsert(bestLoc, importText);
}

InFlightDiagnostic &
InFlightDiagnostic::fixItInsertAttribute(SourceLoc L,
                                         const DeclAttribute *Attr) {
  return fixItInsert(L, "%0 ", {Attr});
}

InFlightDiagnostic &
InFlightDiagnostic::fixItAddAttribute(const DeclAttribute *Attr,
                                      const ClosureExpr *E) {
  ASSERT(!E->isImplicit());

  SourceLoc insertionLoc = E->getBracketRange().Start;

  if (insertionLoc.isInvalid()) {
    if (auto *paramList = E->getParameters()) {
      // HACK: Don't set insertion loc to param list start loc if it's equal to
      // closure start loc (meaning it's implicit).
      // FIXME: Don't set the start loc of an implicit param list, or put an
      // isImplicit bit on ParameterList.
      if (paramList->getStartLoc() != E->getStartLoc()) {
        insertionLoc = paramList->getStartLoc();
      }
    }
  }

  if (insertionLoc.isInvalid()) {
    insertionLoc = E->getInLoc();
  }

  if (insertionLoc.isValid()) {
    return fixItInsert(insertionLoc, "%0 ", {Attr});
  } else {
    auto *body = E->getBody();

    insertionLoc = body->getLBraceLoc();
    ASSERT(insertionLoc.isValid());

    StringRef fixIt = " %0 in";
    // If the first token in the body literally begins with the next char after
    // '{', play it safe with a trailing space.
    if (body->getContentStartLoc() ==
        insertionLoc.getAdvancedLoc(/*ByteOffset=*/1)) {
      fixIt = " %0 in ";
    }

    return fixItInsertAfter(insertionLoc, fixIt, {Attr});
  }
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
      Diagnostic::FixIt(charRange1, "%0", {text2}));
  Engine->getActiveDiagnostic().addFixIt(
      Diagnostic::FixIt(charRange2, "%0", {text1}));
  return *this;
}

InFlightDiagnostic &
InFlightDiagnostic::limitBehavior(DiagnosticBehavior limit) {
  Engine->getActiveDiagnostic().setBehaviorLimit(limit);
  return *this;
}

InFlightDiagnostic &
InFlightDiagnostic::limitBehaviorUntilSwiftVersion(
    DiagnosticBehavior limit, unsigned majorVersion) {
  if (!Engine->languageVersion.isVersionAtLeast(majorVersion)) {
    // If the behavior limit is a warning or less, wrap the diagnostic
    // in a message that this will become an error in a later Swift
    // version. We do this before limiting the behavior, because
    // wrapIn will result in the behavior of the wrapping diagnostic.
    if (limit >= DiagnosticBehavior::Warning) {
      if (majorVersion >= version::Version::getFutureMajorLanguageVersion()) {
        wrapIn(diag::error_in_a_future_swift_lang_mode);
      } else {
        wrapIn(diag::error_in_swift_lang_mode, majorVersion);
      }
    }

    limitBehavior(limit);
  }

  // Record all of the diagnostics that are going to be emitted.
  if (majorVersion == 6 && limit != DiagnosticBehavior::Ignore) {
    if (auto stats = Engine->statsReporter) {
      ++stats->getFrontendCounters().NumSwift6Errors;
    }
  }

  return *this;
}

InFlightDiagnostic &InFlightDiagnostic::warnUntilFutureSwiftVersion() {
  using namespace version;
  return warnUntilSwiftVersion(Version::getFutureMajorLanguageVersion());
}

InFlightDiagnostic &
InFlightDiagnostic::warnUntilSwiftVersion(unsigned majorVersion) {
  return limitBehaviorUntilSwiftVersion(DiagnosticBehavior::Warning,
                                        majorVersion);
}

InFlightDiagnostic &
InFlightDiagnostic::warnInSwiftInterface(const DeclContext *context) {
  if (context->isInSwiftinterface()) {
    return limitBehavior(DiagnosticBehavior::Warning);
  }

  return *this;
}

InFlightDiagnostic &
InFlightDiagnostic::wrapIn(const Diagnostic &wrapper) {
  // Save current active diagnostic into WrappedDiagnostics, ignoring state
  // so we don't get a None return or influence future diagnostics.
  DiagnosticState tempState;
  Engine->state.swap(tempState);
  llvm::SaveAndRestore<DiagnosticBehavior>
      limit(Engine->getActiveDiagnostic().BehaviorLimit,
            DiagnosticBehavior::Unspecified);

  Engine->WrappedDiagnostics.push_back(*Engine->diagnosticInfoForDiagnostic(
      Engine->getActiveDiagnostic(), /* includeDiagnosticName= */ false));

  Engine->state.swap(tempState);

  auto &wrapped = Engine->WrappedDiagnostics.back();

  // Copy and update its arg list.
  Engine->WrappedDiagnosticArgs.emplace_back(wrapped.FormatArgs);
  wrapped.FormatArgs = Engine->WrappedDiagnosticArgs.back();

  // Overwrite the ID and arguments with those from the wrapper.
  Engine->getActiveDiagnostic().ID = wrapper.ID;
  Engine->getActiveDiagnostic().Args = wrapper.Args;
  // Intentionally keeping the original GroupID here

  // Set the argument to the diagnostic being wrapped.
  ASSERT(wrapper.getArgs().front().getKind() ==
         DiagnosticArgumentKind::Diagnostic);
  Engine->getActiveDiagnostic().Args.front() = &wrapped;

  return *this;
}

void InFlightDiagnostic::flush() {
  if (!IsActive)
    return;
  
  IsActive = false;
  if (Engine)
    Engine->flushActiveDiagnostic();
}

void Diagnostic::addChildNote(Diagnostic &&D) {
  assert(storedDiagnosticInfos[(unsigned)D.ID].kind == DiagnosticKind::Note &&
         "Only notes can have a parent.");
  assert(storedDiagnosticInfos[(unsigned)ID].kind != DiagnosticKind::Note &&
         "Notes can't have children.");
  ChildNotes.push_back(std::move(D));
}

bool DiagnosticEngine::isDiagnosticPointsToFirstBadToken(DiagID ID) const {
  return storedDiagnosticInfos[(unsigned) ID].pointsToFirstBadToken;
}

bool DiagnosticEngine::isAPIDigesterBreakageDiagnostic(DiagID ID) const {
  return storedDiagnosticInfos[(unsigned)ID].isAPIDigesterBreakage;
}

bool DiagnosticEngine::isDeprecationDiagnostic(DiagID ID) const {
  return storedDiagnosticInfos[(unsigned)ID].isDeprecation;
}

bool DiagnosticEngine::isNoUsageDiagnostic(DiagID ID) const {
  return storedDiagnosticInfos[(unsigned)ID].isNoUsage;
}

bool DiagnosticEngine::finishProcessing() {
  bool hadError = false;
  for (auto &Consumer : Consumers) {
    hadError |= Consumer->finishProcessing();
  }
  return hadError;
}

void DiagnosticEngine::setWarningsAsErrorsRules(
    const std::vector<WarningAsErrorRule> &rules) {
  std::vector<std::string> unknownGroups;
  for (const auto &rule : rules) {
    bool isEnabled = [&] {
      switch (rule.getAction()) {
      case WarningAsErrorRule::Action::Enable:
        return true;
      case WarningAsErrorRule::Action::Disable:
        return false;
      }
    }();
    auto target = rule.getTarget();
    if (auto group = std::get_if<WarningAsErrorRule::TargetGroup>(&target)) {
      auto name = std::string_view(group->name);
      // Validate the group name and set the new behavior for each diagnostic
      // associated with the group and all its subgroups.
      if (auto groupID = getDiagGroupIDByName(name);
          groupID && *groupID != DiagGroupID::no_group) {
        getDiagGroupInfoByID(*groupID).traverseDepthFirst([&](auto group) {
          state.setWarningsAsErrorsForDiagGroupID(*groupID, isEnabled);
          for (DiagID diagID : group.diagnostics) {
            state.setIgnoredDiagnostic(diagID, false);
          }
        });
      } else {
        unknownGroups.push_back(std::string(name));
      }
    } else if (std::holds_alternative<WarningAsErrorRule::TargetAll>(target)) {
      state.setAllWarningsAsErrors(isEnabled);
    } else {
      llvm_unreachable("unhandled WarningAsErrorRule::Target");
    }
  }
  for (const auto &unknownGroup : unknownGroups) {
    diagnose(SourceLoc(), diag::unknown_warning_group, unknownGroup);
  }
}

/// Skip forward to one of the given delimiters.
///
/// \param Text The text to search through, which will be updated to point
/// just after the delimiter.
///
/// \param Delim The first character delimiter to search for.
///
/// \param FoundDelim On return, true if the delimiter was found, or false
/// if the end of the string was reached.
///
/// \returns The string leading up to the delimiter, or the empty string
/// if no delimiter is found.
static StringRef 
skipToDelimiter(StringRef &Text, char Delim, bool *FoundDelim = nullptr) {
  unsigned Depth = 0;
  if (FoundDelim)
    *FoundDelim = false;

  unsigned I = 0;
  for (unsigned N = Text.size(); I != N; ++I) {
    if (Text[I] == '{') {
      ++Depth;
      continue;
    }
    if (Depth > 0) {
      if (Text[I] == '}')
        --Depth;
      continue;
    }
    
    if (Text[I] == Delim) {
      if (FoundDelim)
        *FoundDelim = true;
      break;
    }
  }

  assert(Depth == 0 && "Unbalanced {} set in diagnostic text");
  StringRef Result = Text.substr(0, I);
  Text = Text.substr(I + 1);
  return Result;
}

/// Handle the integer 'select' modifier.  This is used like this:
/// %select{foo|bar|baz}2.  This means that the integer argument "%2" has a
/// value from 0-2.  If the value is 0, the diagnostic prints 'foo'.
/// If the value is 1, it prints 'bar'.  If it has the value 2, it prints 'baz'.
/// This is very useful for certain classes of variant diagnostics.
static void formatSelectionArgument(StringRef ModifierArguments,
                                    ArrayRef<DiagnosticArgument> Args,
                                    unsigned SelectedIndex,
                                    DiagnosticFormatOptions FormatOpts,
                                    llvm::raw_ostream &Out) {
  bool foundPipe = false;
  do {
    assert((!ModifierArguments.empty() || foundPipe) &&
           "Index beyond bounds in %select modifier");
    StringRef Text = skipToDelimiter(ModifierArguments, '|', &foundPipe);
    if (SelectedIndex == 0) {
      DiagnosticEngine::formatDiagnosticText(Out, Text, Args, FormatOpts);
      break;
    }
    --SelectedIndex;
  } while (true);
  
}

static bool isInterestingTypealias(Type type) {
  // Dig out the typealias declaration, if there is one.
  TypeAliasDecl *aliasDecl = nullptr;
  if (auto aliasTy = dyn_cast<TypeAliasType>(type.getPointer()))
    aliasDecl = aliasTy->getDecl();
  else
    return false;

  if (type->isVoid())
    return false;

  // The 'Swift.AnyObject' typealias is not 'interesting'.
  if (aliasDecl->getName() ==
        aliasDecl->getASTContext().getIdentifier("AnyObject") &&
      (aliasDecl->getParentModule()->isStdlibModule() ||
       aliasDecl->getParentModule()->isBuiltinModule())) {
    return false;
  }

  // Compatibility aliases are only interesting insofar as their underlying
  // types are interesting.
  if (aliasDecl->isCompatibilityAlias()) {
    auto underlyingTy = aliasDecl->getUnderlyingType();
    return isInterestingTypealias(underlyingTy);
  }

  // Builtin types are never interesting typealiases.
  if (type->is<BuiltinType>()) return false;

  return true;
}

/// Walks the type recursively desugaring  types to display, but skipping
/// `GenericTypeParamType` because we would lose association with its original
/// declaration and end up presenting the parameter in τ_0_0 format on
/// diagnostic.
static Type getAkaTypeForDisplay(Type type) {
  return type.transformRec([&](TypeBase *visitTy) -> std::optional<Type> {
    if (isa<SugarType>(visitTy) &&
        !isa<GenericTypeParamType>(visitTy))
      return getAkaTypeForDisplay(visitTy->getDesugaredType());
    return std::nullopt;
  });
}

/// Decide whether to show the desugared type or not.  We filter out some
/// cases to avoid too much noise.
static bool shouldShowAKA(Type type, StringRef typeName) {
  // Canonical types are already desugared.
  if (type->isCanonical())
    return false;

  // Only show 'aka' if there's a typealias involved; other kinds of sugar
  // are easy enough for people to read on their own.
  if (!type.findIf(isInterestingTypealias))
    return false;

  // If they are textually the same, don't show them.  This can happen when
  // they are actually different types, because they exist in different scopes
  // (e.g. everyone names their type parameters 'T').
  if (typeName == getAkaTypeForDisplay(type).getString())
    return false;

  return true;
}

/// If a type is part of an argument list which includes another, distinct type
/// with the same string representation, it should be qualified during
/// formatting.
static bool typeSpellingIsAmbiguous(Type type,
                                    ArrayRef<DiagnosticArgument> Args,
                                    PrintOptions &PO) {
  for (auto arg : Args) {
    if (arg.getKind() == DiagnosticArgumentKind::Type) {
      auto argType = arg.getAsType();
      if (argType && argType.getPointer() != type.getPointer() &&
          argType.getString(PO) == type.getString(PO)) {
        // Currently, existential types are spelled the same way
        // as protocols and compositions. We can remove this once
        // existenials are printed with 'any'.
        if (type->is<ExistentialType>() || argType->isExistentialType()) {
          auto constraint = type;
          if (auto existential = type->getAs<ExistentialType>())
            constraint = existential->getConstraintType();

          auto argConstraint = argType;
          if (auto existential = argType->getAs<ExistentialType>())
            argConstraint = existential->getConstraintType();

          if (constraint.getPointer() != argConstraint.getPointer())
            return true;

          continue;
        }

        return true;
      }
    }
  }
  return false;
}

void swift::printClangDeclName(const clang::NamedDecl *ND,
                               llvm::raw_ostream &os) {
  ND->getNameForDiagnostic(os, ND->getASTContext().getPrintingPolicy(), false);
}

void swift::printClangTypeName(const clang::Type *Ty, llvm::raw_ostream &os) {
  clang::QualType::print(Ty, clang::Qualifiers(), os,
                         clang::PrintingPolicy{clang::LangOptions()}, "");
}

/// Format a single diagnostic argument and write it to the given
/// stream.
static void formatDiagnosticArgument(StringRef Modifier,
                                     StringRef ModifierArguments,
                                     ArrayRef<DiagnosticArgument> Args,
                                     unsigned ArgIndex,
                                     DiagnosticFormatOptions FormatOpts,
                                     llvm::raw_ostream &Out) {
  const DiagnosticArgument &Arg = Args[ArgIndex];
  switch (Arg.getKind()) {
  case DiagnosticArgumentKind::Integer:
    if (Modifier == "select") {
      assert(Arg.getAsInteger() >= 0 && "Negative selection index");
      formatSelectionArgument(ModifierArguments, Args, Arg.getAsInteger(),
                              FormatOpts, Out);
    } else if (Modifier == "s") {
      if (Arg.getAsInteger() != 1)
        Out << 's';
    } else {
      assert(Modifier.empty() && "Improper modifier for integer argument");
      Out << Arg.getAsInteger();
    }
    break;

  case DiagnosticArgumentKind::Unsigned:
    if (Modifier == "select") {
      formatSelectionArgument(ModifierArguments, Args, Arg.getAsUnsigned(),
                              FormatOpts, Out);
    } else if (Modifier == "s") {
      if (Arg.getAsUnsigned() != 1)
        Out << 's';
    } else {
      assert(Modifier.empty() && "Improper modifier for unsigned argument");
      Out << Arg.getAsUnsigned();
    }
    break;

  case DiagnosticArgumentKind::String:
    if (Modifier == "select") {
      formatSelectionArgument(ModifierArguments, Args,
                              Arg.getAsString().empty() ? 0 : 1, FormatOpts,
                              Out);
    } else {
      assert(Modifier.empty() && "Improper modifier for string argument");
      Out << Arg.getAsString();
    }
    break;

  case DiagnosticArgumentKind::Identifier:
    if (Modifier == "select") {
      formatSelectionArgument(ModifierArguments, Args,
                              Arg.getAsIdentifier() ? 1 : 0, FormatOpts,
                              Out);
    } else {
      assert(Modifier.empty() && "Improper modifier for identifier argument");
      Out << FormatOpts.OpeningQuotationMark;
      Arg.getAsIdentifier().printPretty(Out);
      Out << FormatOpts.ClosingQuotationMark;
    }
    break;

  case DiagnosticArgumentKind::ObjCSelector:
    assert(Modifier.empty() && "Improper modifier for selector argument");
    Out << FormatOpts.OpeningQuotationMark << Arg.getAsObjCSelector()
        << FormatOpts.ClosingQuotationMark;
    break;

  case DiagnosticArgumentKind::Decl: {
    auto D = Arg.getAsDecl();

    if (Modifier == "select") {
      formatSelectionArgument(ModifierArguments, Args, D ? 1 : 0, FormatOpts,
                              Out);
      break;
    }

    // Parse info out of modifier
    bool includeKind = false;
    bool includeName = true;
    bool baseNameOnly = false;

    if (Modifier == "kind") {
      includeKind = true;
    } else if (Modifier == "base") {
      baseNameOnly = true;
    } else if (Modifier == "kindbase") {
      includeKind = true;
      baseNameOnly = true;
    } else if (Modifier == "kindonly") {
      includeName = false;
    } else {
      assert(Modifier.empty() && "Improper modifier for ValueDecl argument");
    }

    if (includeName) {
      if (auto accessor = dyn_cast<AccessorDecl>(D)) {
        // If it's an accessor, describe that and then switch to discussing its
        // storage declaration.
        Out << Decl::getDescriptiveKindName(D->getDescriptiveKind()) << " for ";
        D = accessor->getStorage();
      } else if (auto ext = dyn_cast<ExtensionDecl>(D)) {
        // If it's an extension with a valid bound type declaration, describe
        // the extension itself then switch to discussing the bound
        // declaration.
        if (auto *nominal = ext->getSelfNominalTypeDecl()) {
          Out << Decl::getDescriptiveKindName(D->getDescriptiveKind())
              << " of ";
          D = nominal;
        }
      }
    }

    // Figure out the name we want to print.
    DeclName name;
    if (includeName) {
      if (auto MD = dyn_cast<ModuleDecl>(D))
        name = MD->getPublicModuleName(/*onlyIfImported=*/true);
      else if (auto VD = dyn_cast<ValueDecl>(D))
        name = VD->getName();
      else if (auto PGD = dyn_cast<PrecedenceGroupDecl>(D))
        name = PGD->getName();
      else if (auto OD = dyn_cast<OperatorDecl>(D))
        name = OD->getName();
      else if (auto MMD = dyn_cast<MissingMemberDecl>(D))
        name = MMD->getName();

      if (baseNameOnly && name)
        name = name.getBaseName();
    }

    // If the declaration is anonymous or we asked for a descriptive kind, print
    // it.
    if (!name || includeKind) {
      Out << Decl::getDescriptiveKindName(D->getDescriptiveKind());
      if (name)
        Out << " ";
    }

    // Print the name.
    if (name) {
      Out << FormatOpts.OpeningQuotationMark;
      name.printPretty(Out);
      Out << FormatOpts.ClosingQuotationMark;
    }
    break;
  }

  case DiagnosticArgumentKind::FullyQualifiedType:
  case DiagnosticArgumentKind::Type:
  case DiagnosticArgumentKind::WitnessType: {
    std::optional<DiagnosticFormatOptions> TypeFormatOpts;
    if (Modifier == "noformat") {
      TypeFormatOpts.emplace(DiagnosticFormatOptions::formatForFixIts());
    } else {
      assert(Modifier.empty() && "Improper modifier for Type argument");
      TypeFormatOpts.emplace(FormatOpts);
    }
    
    // Strip extraneous parentheses; they add no value.
    Type type;
    bool needsQualification = false;

    // Compute the appropriate print options for this argument.
    auto printOptions = PrintOptions::forDiagnosticArguments();
    if (Arg.getKind() == DiagnosticArgumentKind::Type) {
      type = Arg.getAsType();
      if (type->getASTContext().TypeCheckerOpts.PrintFullConvention)
        printOptions.PrintFunctionRepresentationAttrs =
            PrintOptions::FunctionRepresentationMode::Full;
      needsQualification = typeSpellingIsAmbiguous(type, Args, printOptions);
    } else if (Arg.getKind() == DiagnosticArgumentKind::FullyQualifiedType) {
      type = Arg.getAsFullyQualifiedType().getType();
      if (type->getASTContext().TypeCheckerOpts.PrintFullConvention)
        printOptions.PrintFunctionRepresentationAttrs =
            PrintOptions::FunctionRepresentationMode::Full;
      needsQualification = true;
    } else {
      assert(Arg.getKind() == DiagnosticArgumentKind::WitnessType);
      type = Arg.getAsWitnessType().getType();
      printOptions.PrintGenericRequirements = false;
      printOptions.PrintInverseRequirements = false;
      needsQualification = typeSpellingIsAmbiguous(type, Args, printOptions);
    }

    // If a type has an unresolved type, print it with syntax sugar removed for
    // clarity. For example, print `Array<_>` instead of `[_]`.
    if (type->hasUnresolvedType()) {
      type = type->getWithoutSyntaxSugar();
    }

    if (needsQualification &&
        isa<OpaqueTypeArchetypeType>(type.getPointer()) &&
        cast<ArchetypeType>(type.getPointer())->isRoot()) {
      auto opaqueTypeDecl = type->castTo<OpaqueTypeArchetypeType>()->getDecl();

      llvm::SmallString<256> NamingDeclText;
      llvm::raw_svector_ostream OutNaming(NamingDeclText);
      auto namingDecl = opaqueTypeDecl->getNamingDecl();
      if (namingDecl->getDeclContext()->isTypeContext()) {
        auto selfTy = namingDecl->getDeclContext()->getSelfInterfaceType();
        selfTy->print(OutNaming);
        OutNaming << '.';
      }
      namingDecl->getName().printPretty(OutNaming);

      auto descriptiveKind = opaqueTypeDecl->getDescriptiveKind();

      Out << llvm::format(TypeFormatOpts->OpaqueResultFormatString.c_str(),
                          type->getString(printOptions).c_str(),
                          Decl::getDescriptiveKindName(descriptiveKind).data(),
                          NamingDeclText.c_str());

    } else {
      printOptions.FullyQualifiedTypes = needsQualification;
      std::string typeName = type->getString(printOptions);

      if (shouldShowAKA(type, typeName)) {
        llvm::SmallString<256> AkaText;
        llvm::raw_svector_ostream OutAka(AkaText);

        getAkaTypeForDisplay(type)->print(OutAka, printOptions);
        Out << llvm::format(TypeFormatOpts->AKAFormatString.c_str(),
                            typeName.c_str(), AkaText.c_str());
      } else {
        Out << TypeFormatOpts->OpeningQuotationMark << typeName
            << TypeFormatOpts->ClosingQuotationMark;
      }
    }
    break;
  }

  case DiagnosticArgumentKind::TypeRepr:
    assert(Modifier.empty() && "Improper modifier for TypeRepr argument");
    assert(Arg.getAsTypeRepr() && "TypeRepr argument is null");
    Out << FormatOpts.OpeningQuotationMark << Arg.getAsTypeRepr()
        << FormatOpts.ClosingQuotationMark;
    break;

  case DiagnosticArgumentKind::DescriptivePatternKind:
    assert(Modifier.empty() &&
           "Improper modifier for DescriptivePatternKind argument");
    Out << Pattern::getDescriptivePatternKindName(
        Arg.getAsDescriptivePatternKind());
    break;

  case DiagnosticArgumentKind::SelfAccessKind:
    if (Modifier == "select") {
      formatSelectionArgument(ModifierArguments, Args,
                              unsigned(Arg.getAsSelfAccessKind()),
                              FormatOpts, Out);
    } else {
      assert(Modifier.empty() &&
             "Improper modifier for SelfAccessKind argument");
      Out << Arg.getAsSelfAccessKind();
    }
    break;

  case DiagnosticArgumentKind::ReferenceOwnership:
    if (Modifier == "select") {
      formatSelectionArgument(ModifierArguments, Args,
                              unsigned(Arg.getAsReferenceOwnership()),
                              FormatOpts, Out);
    } else {
      assert(Modifier.empty() &&
             "Improper modifier for ReferenceOwnership argument");
      Out << Arg.getAsReferenceOwnership();
    }
    break;

  case DiagnosticArgumentKind::StaticSpellingKind:
    if (Modifier == "select") {
      formatSelectionArgument(ModifierArguments, Args,
                              unsigned(Arg.getAsStaticSpellingKind()),
                              FormatOpts, Out);
    } else {
      assert(Modifier.empty() &&
             "Improper modifier for StaticSpellingKind argument");
      Out << Arg.getAsStaticSpellingKind();
    }
    break;

  case DiagnosticArgumentKind::DescriptiveDeclKind:
    assert(Modifier.empty() &&
           "Improper modifier for DescriptiveDeclKind argument");
    Out << Decl::getDescriptiveKindName(Arg.getAsDescriptiveDeclKind());
    break;

  case DiagnosticArgumentKind::DescriptiveStmtKind:
    assert(Modifier.empty() && "Improper modifier for StmtKind argument");
    Out << Stmt::getDescriptiveKindName(Arg.getAsDescriptiveStmtKind());
    break;

  case DiagnosticArgumentKind::DeclAttribute: {
    auto *const attr = Arg.getAsDeclAttribute();
    const auto printAttrName = [&] {
      if (auto *custom = dyn_cast<CustomAttr>(attr)) {
        custom->getTypeRepr()->print(Out);
      } else {
        Out << attr->getAttrName();
      }
    };

    assert(Modifier.empty() &&
           "Improper modifier for DeclAttribute argument");
    if (Arg.getAsDeclAttribute()->isDeclModifier()) {
      Out << FormatOpts.OpeningQuotationMark;
      printAttrName();
      Out << FormatOpts.ClosingQuotationMark;
    } else {
      Out << '@';
      printAttrName();
    }
    break;
  }
  case DiagnosticArgumentKind::TypeAttribute: {
    bool useAtStyle = true;
    if (Modifier == "kind") {
      useAtStyle = false;
    } else {
      ASSERT(Modifier.empty() &&
             "Improper modifier for TypeAttribute argument");
    }

    if (!useAtStyle) {
      Out << "attribute ";
    }
    Out << FormatOpts.OpeningQuotationMark;
    if (useAtStyle) {
      Out << '@';
    }
    Out << Arg.getAsTypeAttribute()->getAttrName();
    Out << FormatOpts.ClosingQuotationMark;
    break;
  }
  case DiagnosticArgumentKind::AvailabilityDomain:
    assert(Modifier.empty() &&
           "Improper modifier for AvailabilityDomain argument");
    Out << Arg.getAsAvailabilityDomain().getNameForDiagnostics();
    break;
  case DiagnosticArgumentKind::AvailabilityRange:
    assert(Modifier.empty() &&
           "Improper modifier for AvailabilityRange argument");
    Out << Arg.getAsAvailabilityRange().getRawMinimumVersion().getAsString();
    break;
  case DiagnosticArgumentKind::VersionTuple:
    assert(Modifier.empty() &&
           "Improper modifier for VersionTuple argument");
    Out << Arg.getAsVersionTuple().getAsString();
    break;
  case DiagnosticArgumentKind::LayoutConstraint:
    assert(Modifier.empty() && "Improper modifier for LayoutConstraint argument");
    Out << FormatOpts.OpeningQuotationMark << Arg.getAsLayoutConstraint()
        << FormatOpts.ClosingQuotationMark;
    break;
  case DiagnosticArgumentKind::ActorIsolation: {
    assert((Modifier.empty() || Modifier == "noun") &&
           "Improper modifier for ActorIsolation argument");
    auto isolation = Arg.getAsActorIsolation();
    isolation.printForDiagnostics(Out, FormatOpts.OpeningQuotationMark,
                                  /*asNoun*/ Modifier == "noun");
    break;
  }
  case DiagnosticArgumentKind::IsolationSource: {
    assert(Modifier.empty() && "Improper modifier for IsolationSource argument");
    auto isolation = Arg.getAsIsolationSource();
    isolation.printForDiagnostics(Out, FormatOpts.OpeningQuotationMark);
    break;
  }
  case DiagnosticArgumentKind::Diagnostic: {
    assert(Modifier.empty() && "Improper modifier for Diagnostic argument");
    auto diagArg = Arg.getAsDiagnostic();
    DiagnosticEngine::formatDiagnosticText(Out, diagArg->FormatString,
                                           diagArg->FormatArgs);
    break;
  }

  case DiagnosticArgumentKind::ClangDecl:
    assert(Modifier.empty() && "Improper modifier for ClangDecl argument");
    Out << FormatOpts.OpeningQuotationMark;
    printClangDeclName(Arg.getAsClangDecl(), Out);
    Out << FormatOpts.ClosingQuotationMark;
    break;

  case DiagnosticArgumentKind::ClangType:
    assert(Modifier.empty() && "Improper modifier for ClangDecl argument");
    Out << FormatOpts.OpeningQuotationMark;
    printClangTypeName(Arg.getAsClangType(), Out);
    Out << FormatOpts.ClosingQuotationMark;
    break;
  }
}

/// Format the given diagnostic text and place the result in the given
/// buffer.
void DiagnosticEngine::formatDiagnosticText(
    llvm::raw_ostream &Out, StringRef InText, ArrayRef<DiagnosticArgument> Args,
    DiagnosticFormatOptions FormatOpts) {
  while (!InText.empty()) {
    size_t Percent = InText.find('%');
    if (Percent == StringRef::npos) {
      // Write the rest of the string; we're done.
      Out.write(InText.data(), InText.size());
      break;
    }
    
    // Write the string up to (but not including) the %, then drop that text
    // (including the %).
    Out.write(InText.data(), Percent);
    InText = InText.substr(Percent + 1);
    
    // '%%' -> '%'.
    if (InText[0] == '%') {
      Out.write('%');
      InText = InText.substr(1);
      continue;
    }

    // Parse an optional modifier.
    StringRef Modifier;
    {
      size_t Length = InText.find_if_not(isalpha);
      Modifier = InText.substr(0, Length);
      InText = InText.substr(Length);
    }
    
    if (Modifier == "error") {
      Out << StringRef("<<INTERNAL ERROR: encountered %error in diagnostic text>>");
      continue;
    }

    // Parse the optional argument list for a modifier, which is brace-enclosed.
    StringRef ModifierArguments;
    if (InText[0] == '{') {
      InText = InText.substr(1);
      ModifierArguments = skipToDelimiter(InText, '}');
    }
    
    // Find the digit sequence, and parse it into an argument index.
    size_t Length = InText.find_if_not(isdigit);
    unsigned ArgIndex;      
    bool IndexParseFailed = InText.substr(0, Length).getAsInteger(10, ArgIndex);

    if (IndexParseFailed) {
      Out << StringRef("<<INTERNAL ERROR: unparseable argument index in diagnostic text>>");
      continue;
    }

    InText = InText.substr(Length);

    if (ArgIndex >= Args.size()) {
      Out << StringRef("<<INTERNAL ERROR: out-of-range argument index in diagnostic text>>");
      continue;
    }

    // Convert the argument to a string.
    formatDiagnosticArgument(Modifier, ModifierArguments, Args, ArgIndex,
                             FormatOpts, Out);
  }
}

static DiagnosticKind toDiagnosticKind(DiagnosticBehavior behavior) {
  switch (behavior) {
  case DiagnosticBehavior::Unspecified:
    llvm_unreachable("unspecified behavior");
  case DiagnosticBehavior::Ignore:
    llvm_unreachable("trying to map an ignored diagnostic");
  case DiagnosticBehavior::Error:
  case DiagnosticBehavior::Fatal:
    return DiagnosticKind::Error;
  case DiagnosticBehavior::Note:
    return DiagnosticKind::Note;
  case DiagnosticBehavior::Warning:
    return DiagnosticKind::Warning;
  case DiagnosticBehavior::Remark:
    return DiagnosticKind::Remark;
  }

  llvm_unreachable("Unhandled DiagnosticKind in switch.");
}

static
DiagnosticBehavior toDiagnosticBehavior(DiagnosticKind kind, bool isFatal) {
  switch (kind) {
  case DiagnosticKind::Note:
    return DiagnosticBehavior::Note;
  case DiagnosticKind::Error:
    return isFatal ? DiagnosticBehavior::Fatal : DiagnosticBehavior::Error;
  case DiagnosticKind::Warning:
    return DiagnosticBehavior::Warning;
  case DiagnosticKind::Remark:
    return DiagnosticBehavior::Remark;
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

DiagnosticBehavior
DiagnosticState::determineBehavior(const Diagnostic &diag) const {
  // We determine how to handle a diagnostic based on the following rules
  //   1) Map the diagnostic to its "intended" behavior, applying the behavior
  //      limit for this particular emission
  //   2) If current state dictates a certain behavior, follow that
  //   3) If the user ignored this specific diagnostic, follow that
  //   4) If the user substituted a different behavior for this behavior, apply
  //      that change
  //   5) Update current state for use during the next diagnostic

  //   1) Map the diagnostic to its "intended" behavior, applying the behavior
  //      limit for this particular emission
  auto diagInfo = storedDiagnosticInfos[(unsigned)diag.getID()];
  DiagnosticBehavior lvl =
      std::max(toDiagnosticBehavior(diagInfo.kind, diagInfo.isFatal),
               diag.getBehaviorLimit());
  assert(lvl != DiagnosticBehavior::Unspecified);

  //   2) If current state dictates a certain behavior, follow that

  // Notes relating to ignored diagnostics should also be ignored
  if (previousBehavior == DiagnosticBehavior::Ignore
      && lvl == DiagnosticBehavior::Note)
    lvl = DiagnosticBehavior::Ignore;

  // Suppress diagnostics when in a fatal state, except for follow-on notes
  if (fatalErrorOccurred)
    if (!showDiagnosticsAfterFatalError && lvl != DiagnosticBehavior::Note)
      lvl = DiagnosticBehavior::Ignore;

  //   3) If the user ignored this specific diagnostic, follow that
  if (ignoredDiagnostics[(unsigned)diag.getID()])
    lvl = DiagnosticBehavior::Ignore;

  //   4) If the user substituted a different behavior for this behavior, apply
  //      that change
  if (lvl == DiagnosticBehavior::Warning) {
    if (getWarningsAsErrorsForDiagGroupID(diag.getGroupID()))
      lvl = DiagnosticBehavior::Error;
    if (suppressWarnings)
      lvl = DiagnosticBehavior::Ignore;
  }
  
  if (lvl == DiagnosticBehavior::Remark) {
    if (suppressRemarks)
      lvl = DiagnosticBehavior::Ignore;
  }
  return lvl;
}

void DiagnosticState::updateFor(DiagnosticBehavior behavior) {
  // Update current state for use during the next diagnostic
  if (behavior == DiagnosticBehavior::Fatal) {
    fatalErrorOccurred = true;
    anyErrorOccurred = true;
  } else if (behavior == DiagnosticBehavior::Error) {
    anyErrorOccurred = true;
  }

  ASSERT((!AssertOnError || !anyErrorOccurred) && "We emitted an error?!");
  ASSERT((!AssertOnWarning || (behavior != DiagnosticBehavior::Warning)) &&
         "We emitted a warning?!");

  previousBehavior = behavior;
}

void DiagnosticEngine::flushActiveDiagnostic() {
  assert(ActiveDiagnostic && "No active diagnostic to flush");
  handleDiagnostic(std::move(*ActiveDiagnostic));
  ActiveDiagnostic.reset();
}

void DiagnosticEngine::handleDiagnostic(Diagnostic &&diag) {
  if (TransactionCount == 0) {
    emitDiagnostic(diag);
    WrappedDiagnostics.clear();
    WrappedDiagnosticArgs.clear();
  } else {
    onTentativeDiagnosticFlush(diag);
    TentativeDiagnostics.emplace_back(std::move(diag));
  }
}

void DiagnosticEngine::clearTentativeDiagnostics() {
  TentativeDiagnostics.clear();
  WrappedDiagnostics.clear();
  WrappedDiagnosticArgs.clear();
}

void DiagnosticEngine::emitTentativeDiagnostics() {
  for (auto &diag : TentativeDiagnostics) {
    emitDiagnostic(diag);
  }
  clearTentativeDiagnostics();
}

void DiagnosticEngine::forwardTentativeDiagnosticsTo(
    DiagnosticEngine &targetEngine) {
  for (auto &diag : TentativeDiagnostics) {
    targetEngine.handleDiagnostic(std::move(diag));
  }
  clearTentativeDiagnostics();
}

std::optional<DiagnosticInfo>
DiagnosticEngine::diagnosticInfoForDiagnostic(const Diagnostic &diagnostic,
                                              bool includeDiagnosticName) {
  auto behavior = state.determineBehavior(diagnostic);
  state.updateFor(behavior);

  if (behavior == DiagnosticBehavior::Ignore)
    return std::nullopt;

  // Figure out the source location.
  SourceLoc loc = diagnostic.getLoc();
  if (loc.isInvalid() && diagnostic.getDecl()) {
    const Decl *decl = diagnostic.getDecl();
    // If a declaration was provided instead of a location, and that declaration
    // has a location we can point to, use that location.
    loc = decl->getLoc();

    // If the location of the decl is invalid still, try to pretty-print the
    // declaration into a buffer and capture the source location there.
    if (loc.isInvalid()) {
      loc = evaluateOrDefault(
          decl->getASTContext().evaluator, PrettyPrintDeclRequest{decl},
          SourceLoc());
    }
  }

  auto groupID = diagnostic.getGroupID();
  StringRef Category;
  if (auto wrapped = diagnostic.getWrappedDiagnostic())
    Category = wrapped.value()->Category;
  else if (isAPIDigesterBreakageDiagnostic(diagnostic.getID()))
    Category = "api-digester-breaking-change";
  else if (isNoUsageDiagnostic(diagnostic.getID()))
    Category = "no-usage";
  else if (groupID != DiagGroupID::no_group)
    Category = getDiagGroupInfoByID(groupID).name;
  else if (isDeprecationDiagnostic(diagnostic.getID()))
    Category = "deprecation";

  auto fixIts = diagnostic.getFixIts();
  if (loc.isValid()) {
    // If the diagnostic is being emitted in a generated buffer, drop the
    // fix-its, as the user will have no way of applying them.
    auto bufferID = SourceMgr.findBufferContainingLoc(loc);
    if (auto generatedInfo = SourceMgr.getGeneratedSourceInfo(bufferID)) {
      switch (generatedInfo->kind) {
#define MACRO_ROLE(Name, Description)  \
      case GeneratedSourceInfo::Name##MacroExpansion:
#include "swift/Basic/MacroRoles.def"
      case GeneratedSourceInfo::PrettyPrinted:
      case GeneratedSourceInfo::DefaultArgument:
      case GeneratedSourceInfo::AttributeFromClang:
        fixIts = {};
        break;
      case GeneratedSourceInfo::ReplacedFunctionBody:
        // A replaced function body is for user-written code, so fix-its are
        // still valid.
        break;
      }
    }
  }

  auto formatString =
      getFormatStringForDiagnostic(diagnostic, includeDiagnosticName);

  return DiagnosticInfo(diagnostic.getID(), loc, toDiagnosticKind(behavior),
                        formatString, diagnostic.getArgs(), Category,
                        getDefaultDiagnosticLoc(),
                        /*child note info*/ {}, diagnostic.getRanges(), fixIts,
                        diagnostic.isChildNote());
}

static DeclName
getGeneratedSourceInfoMacroName(const GeneratedSourceInfo &info) {
  ASTNode expansionNode = ASTNode::getFromOpaqueValue(info.astNode);
  switch (info.kind) {
#define MACRO_ROLE(Name, Description)                                          \
  case GeneratedSourceInfo::Name##MacroExpansion:
#include "swift/Basic/MacroRoles.def"
    {
      if (auto customAttr = info.attachedMacroCustomAttr) {
        // FIXME: How will we handle deserialized custom attributes like this?
        auto declRefType = cast<DeclRefTypeRepr>(customAttr->getTypeRepr());
        return declRefType->getNameRef().getFullName();
      }

      if (auto expansionExpr = dyn_cast_or_null<MacroExpansionExpr>(
              expansionNode.dyn_cast<Expr *>())) {
        return expansionExpr->getMacroName().getFullName();
      }

      auto expansionDecl =
          cast<MacroExpansionDecl>(cast<Decl *>(expansionNode));
      return expansionDecl->getMacroName().getFullName();
    }

  case GeneratedSourceInfo::PrettyPrinted:
  case GeneratedSourceInfo::ReplacedFunctionBody:
  case GeneratedSourceInfo::DefaultArgument:
  case GeneratedSourceInfo::AttributeFromClang:
    return DeclName();
  }
}

std::vector<Diagnostic>
DiagnosticEngine::getGeneratedSourceBufferNotes(SourceLoc loc) {
  // The set of child notes we're building up.
  std::vector<Diagnostic> childNotes;

  // If the location is invalid, there's nothing to do.
  if (loc.isInvalid())
    return childNotes;

  // If we already emitted these notes for a prior part of the diagnostic,
  // don't do so again.
  auto currentBufferID = SourceMgr.findBufferContainingLoc(loc);
  SourceLoc currentLoc = loc;
  do {
    auto generatedInfo = SourceMgr.getGeneratedSourceInfo(currentBufferID);
    if (!generatedInfo)
      return childNotes;

    ASTNode expansionNode =
        ASTNode::getFromOpaqueValue(generatedInfo->astNode);

    switch (generatedInfo->kind) {
#define MACRO_ROLE(Name, Description)  \
    case GeneratedSourceInfo::Name##MacroExpansion:
#include "swift/Basic/MacroRoles.def"
    {
      DeclName macroName = getGeneratedSourceInfoMacroName(*generatedInfo);

      // If it was an expansion of an attached macro, increase the range to
      // include the decl's attributes. Also add the name of the decl the macro
      // is attached to.
      CustomAttr *attachedAttr = generatedInfo->attachedMacroCustomAttr;
      Decl *attachedDecl =
          attachedAttr ? expansionNode.dyn_cast<Decl *>() : nullptr;
      SourceRange origRange = attachedDecl
                                  ? attachedDecl->getSourceRangeIncludingAttrs()
                                  : expansionNode.getSourceRange();

      Diagnostic expansionNote(diag::in_macro_expansion, macroName,
                               attachedDecl);
      if (attachedAttr) {
        expansionNote.setLoc(attachedAttr->getLocation());
      } else {
        expansionNote.setLoc(origRange.Start);
      }
      expansionNote.addRange(
          Lexer::getCharSourceRangeFromSourceRange(SourceMgr, origRange));
      expansionNote.setIsChildNote(true);
      childNotes.push_back(std::move(expansionNote));
      break;
    }

    case GeneratedSourceInfo::PrettyPrinted:
      break;

    case GeneratedSourceInfo::DefaultArgument:
    case GeneratedSourceInfo::ReplacedFunctionBody:
    case GeneratedSourceInfo::AttributeFromClang:
      return childNotes;
    }

    // Walk up the stack.
    currentLoc = expansionNode.getStartLoc();
    if (currentLoc.isInvalid())
      return childNotes;

    currentBufferID = SourceMgr.findBufferContainingLoc(currentLoc);
  } while (true);
}

void DiagnosticEngine::emitDiagnostic(const Diagnostic &diagnostic) {

  ArrayRef<Diagnostic> childNotes = diagnostic.getChildNotes();
  std::vector<Diagnostic> extendedChildNotes;

  if (auto info =
          diagnosticInfoForDiagnostic(diagnostic,
                                      /* includeDiagnosticName= */ true)) {
    // If the diagnostic location is within a buffer containing generated
    // source code, add child notes showing where the generation occurred.
    // We need to avoid doing this if this is itself a child note, as otherwise
    // we'd end up doubling up on notes.
    if (!info->IsChildNote) {
      extendedChildNotes = getGeneratedSourceBufferNotes(info->Loc);
    }
    if (!extendedChildNotes.empty()) {
      extendedChildNotes.insert(extendedChildNotes.end(),
                                childNotes.begin(), childNotes.end());
      childNotes = extendedChildNotes;
    }

    SmallVector<DiagnosticInfo, 1> childInfo;
    for (unsigned i : indices(childNotes)) {
      auto child =
          diagnosticInfoForDiagnostic(childNotes[i],
                                      /* includeDiagnosticName= */ true);
      assert(child);
      assert(child->Kind == DiagnosticKind::Note &&
             "Expected child diagnostics to all be notes?!");
      childInfo.push_back(*child);
    }
    TinyPtrVector<DiagnosticInfo *> childInfoPtrs;
    for (unsigned i : indices(childInfo)) {
      childInfoPtrs.push_back(&childInfo[i]);
    }
    info->ChildDiagnosticInfo = childInfoPtrs;

    // Capture information about the diagnostic group and its documentation
    // URL.
    auto groupID = diagnostic.getGroupID();
    if (groupID != DiagGroupID::no_group) {
      const auto &diagGroup = getDiagGroupInfoByID(groupID);
      
      std::string docURL(getDiagnosticDocumentationPath());
      if (!docURL.empty() && docURL.back() != '/')
        docURL += "/";
      docURL += diagGroup.documentationFile;
      info->CategoryDocumentationURL = std::move(docURL);
    }

    for (auto &consumer : Consumers) {
      consumer->handleDiagnostic(SourceMgr, *info);
    }
  }

  // For compatibility with DiagnosticConsumers which don't know about child
  // notes. These can be ignored by consumers which do take advantage of the
  // grouping.
  for (auto &childNote : childNotes)
    emitDiagnostic(childNote);
}

DiagnosticKind DiagnosticEngine::declaredDiagnosticKindFor(const DiagID id) {
  return storedDiagnosticInfos[(unsigned)id].kind;
}

llvm::StringRef DiagnosticEngine::getFormatStringForDiagnostic(DiagID id) {
  llvm::StringRef message = diagnosticStrings[(unsigned)id];
  if (auto localizationProducer = localization.get()) {
    message = localizationProducer->getMessageOr(id, message);
  }
  return message;
}

llvm::StringRef
DiagnosticEngine::getFormatStringForDiagnostic(const Diagnostic &diagnostic,
                                               bool includeDiagnosticName) {
  auto diagID = diagnostic.getID();
  auto message = getFormatStringForDiagnostic(diagID);

  if (!includeDiagnosticName) {
    return message;
  }

  auto formatMessageWithName = [&](StringRef message, StringRef name) {
    const int additionalCharsLength = 3; // ' ', '[', ']'
    std::string messageWithName;
    messageWithName.reserve(message.size() + name.size() +
                            additionalCharsLength);
    messageWithName += message;
    messageWithName += " [";
    messageWithName += name;
    messageWithName += "]";
    return DiagnosticStringsSaver.save(messageWithName);
  };
  switch (printDiagnosticNamesMode) {
  case PrintDiagnosticNamesMode::None:
  case PrintDiagnosticNamesMode::Group:
    break;
  case PrintDiagnosticNamesMode::Identifier: {
    auto userFacingID = diagID;
    // If this diagnostic wraps another one, use the ID of the wrapped
    // diagnostic.
    if (auto wrapped = diagnostic.getWrappedDiagnostic()) {
      userFacingID = wrapped.value()->ID;
    }

    message =
        formatMessageWithName(message, diagnosticIDStringFor(userFacingID));
    break;
  }
  }

  return message;
}

llvm::StringRef
DiagnosticEngine::diagnosticIDStringFor(const DiagID id) {
  return diagnosticIDStrings[(unsigned)id];
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

bool DiagnosticSuppression::isEnabled(const DiagnosticEngine &diags) {
  return diags.getConsumers().empty();
}

BufferIndirectlyCausingDiagnosticRAII::BufferIndirectlyCausingDiagnosticRAII(
    const SourceFile &SF)
    : Diags(SF.getASTContext().Diags) {
  auto id = SF.getBufferID();
  auto loc = SF.getASTContext().SourceMgr.getLocForBufferStart(id);
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

    auto I = TransactionStrings.insert(content).first;
    argument = DiagnosticArgument(StringRef(I->getKeyData()));
  }
}

EncodedDiagnosticMessage::EncodedDiagnosticMessage(StringRef S)
    : Message(Lexer::getEncodedStringSegment(S, Buf, /*IsFirstSegment=*/true,
                                             /*IsLastSegment=*/true,
                                             /*IndentToStrip=*/~0U)) {}
