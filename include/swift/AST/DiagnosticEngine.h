//===--- DiagnosticEngine.h - Diagnostic Display Engine ---------*- C++ -*-===//
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
//  This file declares the DiagnosticEngine class, which manages any diagnostics
//  emitted by Swift.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_DIAGNOSTICENGINE_H
#define SWIFT_BASIC_DIAGNOSTICENGINE_H

#include "swift/AST/DeclNameLoc.h"
#include "swift/AST/DiagnosticArgument.h"
#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/PrintDiagnosticNamesMode.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/Version.h"
#include "swift/Basic/WarningAsErrorRule.h"
#include "swift/Localization/LocalizationFormat.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SaveAndRestore.h"

namespace clang {
class NamedDecl;
class Type;
}

namespace swift {
  class ClosureExpr;
  class Decl;
  class DeclAttribute;
  class DiagnosticEngine;
  class FuncDecl;
  class SourceManager;
  class SourceFile;

  /// Enumeration describing all of possible diagnostics.
  ///
  /// Each of the diagnostics described in Diagnostics.def has an entry in
  /// this enumeration type that uniquely identifies it.
  enum class DiagID : uint32_t;

  enum class DiagGroupID : uint16_t;

  /// Describes a diagnostic along with its argument types.
  ///
  /// The diagnostics header introduces instances of this type for each 
  /// diagnostic, which provide both the set of argument types (used to
  /// check/convert the arguments at each call site) and the diagnostic ID
  /// (for other information about the diagnostic).
  template<typename ...ArgTypes>
  struct Diag {
    /// The diagnostic ID corresponding to this diagnostic.
    DiagID ID;
  };

  namespace detail {
    /// Describes how to pass a diagnostic argument of the given type.
    ///
    /// By default, diagnostic arguments are passed by value, because they
    /// tend to be small. Larger diagnostic arguments
    /// need to specialize this class template to pass by reference.
    template<typename T>
    struct PassArgument {
      typedef T type;
    };
  }

  template <class... ArgTypes>
  using DiagArgTuple =
    std::tuple<typename detail::PassArgument<ArgTypes>::type...>;

  namespace diag {
    enum class RequirementKind : uint8_t;
  }

  /// Describes the current behavior to take with a diagnostic.
  /// Ordered from most severe to least.
  struct DiagnosticBehavior {
    enum Kind : uint8_t {
      Unspecified = 0,
      Fatal,
      Error,
      Warning,
      Remark,
      Note,
      Ignore,
    };

    Kind kind;

    DiagnosticBehavior() : kind(Unspecified) {}
    DiagnosticBehavior(Kind kind) : kind(kind) {}
    operator Kind() const { return kind; }

    /// Move up the lattice returning the max value.
    DiagnosticBehavior merge(DiagnosticBehavior other) const {
      auto value = std::max(std::underlying_type<Kind>::type(*this),
                            std::underlying_type<Kind>::type(other));
      return Kind(value);
    }
  };

  struct DiagnosticFormatOptions {
    const std::string OpeningQuotationMark;
    const std::string ClosingQuotationMark;
    const std::string AKAFormatString;
    const std::string OpaqueResultFormatString;

    DiagnosticFormatOptions(std::string OpeningQuotationMark,
                            std::string ClosingQuotationMark,
                            std::string AKAFormatString,
                            std::string OpaqueResultFormatString)
        : OpeningQuotationMark(OpeningQuotationMark),
          ClosingQuotationMark(ClosingQuotationMark),
          AKAFormatString(AKAFormatString),
          OpaqueResultFormatString(OpaqueResultFormatString) {}

    DiagnosticFormatOptions()
        : OpeningQuotationMark("'"), ClosingQuotationMark("'"),
          AKAFormatString("'%s' (aka '%s')"),
          OpaqueResultFormatString("'%s' (%s of '%s')") {}

    /// When formatting fix-it arguments, don't include quotes or other
    /// additions which would result in invalid code.
    static DiagnosticFormatOptions formatForFixIts() {
      return DiagnosticFormatOptions("", "", "%s", "%s");
    }
  };

  enum class FixItID : uint32_t;

  /// Represents a fix-it defined  with a format string and optional
  /// DiagnosticArguments. The template parameters allow the
  /// fixIt... methods on InFlightDiagnostic to infer their own
  /// template params.
  template <typename... ArgTypes> struct StructuredFixIt { FixItID ID; };

  /// Diagnostic - This is a specific instance of a diagnostic along with all of
  /// the DiagnosticArguments that it requires. 
  class Diagnostic {
  public:
    typedef DiagnosticInfo::FixIt FixIt;

  private:
    DiagID ID;
    DiagGroupID GroupID;
    SmallVector<DiagnosticArgument, 3> Args;
    SmallVector<CharSourceRange, 2> Ranges;
    SmallVector<FixIt, 2> FixIts;
    std::vector<Diagnostic> ChildNotes;
    SourceLoc Loc;
    bool IsChildNote = false;
    const swift::Decl *Decl = nullptr;
    DiagnosticBehavior BehaviorLimit = DiagnosticBehavior::Unspecified;

    friend DiagnosticEngine;
    friend class InFlightDiagnostic;

    /// Constructs a Diagnostic with DiagGroupID infered from DiagID.
    Diagnostic(DiagID ID);

  protected:
    /// Only use this constructor privately in this class or in unit tests by
    /// subclassing.
    /// In unit tests, it is used as a means for associating diagnostics with
    /// groups and, thus, circumventing the need to otherwise define mock
    /// diagnostics, which is not accounted for in the current design.
    Diagnostic(DiagID ID, DiagGroupID GroupID) : ID(ID), GroupID(GroupID) {}

  public:
    // All constructors are intentionally implicit.
    template<typename ...ArgTypes>
    Diagnostic(Diag<ArgTypes...> ID,
               typename detail::PassArgument<ArgTypes>::type... VArgs)
        : Diagnostic(ID.ID) {
      Args.reserve(sizeof...(ArgTypes));
      gatherArgs(VArgs...);
    }

    Diagnostic(DiagID ID, ArrayRef<DiagnosticArgument> Args) : Diagnostic(ID) {
      this->Args.append(Args.begin(), Args.end());
    }

    template <class... ArgTypes>
    static Diagnostic fromTuple(Diag<ArgTypes...> id,
                                const DiagArgTuple<ArgTypes...> &tuple) {
      Diagnostic result(id.ID);
      result.gatherArgsFromTuple<DiagArgTuple<ArgTypes...>, 0, ArgTypes...>(tuple);
      return result;
    }
    
    // Accessors.
    DiagID getID() const { return ID; }
    DiagGroupID getGroupID() const { return GroupID; }
    ArrayRef<DiagnosticArgument> getArgs() const { return Args; }
    ArrayRef<CharSourceRange> getRanges() const { return Ranges; }
    ArrayRef<FixIt> getFixIts() const { return FixIts; }
    ArrayRef<Diagnostic> getChildNotes() const { return ChildNotes; }
    bool isChildNote() const { return IsChildNote; }
    SourceLoc getLoc() const { return Loc; }
    const class Decl *getDecl() const { return Decl; }
    DiagnosticBehavior getBehaviorLimit() const { return BehaviorLimit; }

    void setLoc(SourceLoc loc) { Loc = loc; }
    void setIsChildNote(bool isChildNote) { IsChildNote = isChildNote; }
    void setDecl(const class Decl *decl) { Decl = decl; }
    void setBehaviorLimit(DiagnosticBehavior limit){ BehaviorLimit = limit; }

    /// Returns the wrapped diagnostic, if any.
    std::optional<const DiagnosticInfo *> getWrappedDiagnostic() const;

    /// Returns true if this object represents a particular diagnostic.
    ///
    /// \code
    /// someDiag.is(diag::invalid_diagnostic)
    /// \endcode
    template<typename ...OtherArgTypes>
    bool is(Diag<OtherArgTypes...> Other) const {
      return ID == Other.ID;
    }

    void addRange(CharSourceRange R) {
      Ranges.push_back(R);
    }

    // Avoid copying the fix-it text more than necessary.
    void addFixIt(FixIt &&F) {
      FixIts.push_back(std::move(F));
    }

    void addChildNote(Diagnostic &&D);
    void insertChildNote(unsigned beforeIndex, Diagnostic &&D);

  private:
    // gatherArgs could just be `Args.emplace_back(args)...;` if C++
    // allowed pack expansions in statement context.

    // Base case.
    void gatherArgs() {}

    // Pull one off the pack.
    template <class ArgType, class... RemainingArgTypes>
    void gatherArgs(ArgType arg, RemainingArgTypes... remainingArgs) {
      Args.emplace_back(arg);
      gatherArgs(remainingArgs...);
    }

    // gatherArgsFromTuple could just be
    // `Args.emplace_back(std::get<packIndexOf<ArgTypes>>(tuple))...;`
    // in a better world.

    // Base case.
    template <class Tuple, size_t Index>
    void gatherArgsFromTuple(const Tuple &tuple) {}

    // Pull one off the pack.
    template <class Tuple, size_t Index,
              class ArgType, class... RemainingArgTypes>
    void gatherArgsFromTuple(const Tuple &tuple) {
      Args.emplace_back(std::move(std::get<Index>(tuple)));
      gatherArgsFromTuple<Tuple, Index + 1, RemainingArgTypes...>(
        std::move(tuple));
    }
  };

  /// A diagnostic that has no input arguments, so it is trivially-destructable.
  using ZeroArgDiagnostic = Diag<>;
  
  /// Describes an in-flight diagnostic, which is currently active
  /// within the diagnostic engine and can be augmented within additional
  /// information (source ranges, Fix-Its, etc.).
  ///
  /// Only a single in-flight diagnostic can be active at one time, and all
  /// additional information must be emitted through the active in-flight
  /// diagnostic.
  class InFlightDiagnostic {
    friend class DiagnosticEngine;
    
    DiagnosticEngine *Engine;
    bool IsActive;
    
    /// Create a new in-flight diagnostic. 
    ///
    /// This constructor is only available to the DiagnosticEngine.
    InFlightDiagnostic(DiagnosticEngine &Engine)
      : Engine(&Engine), IsActive(true) { }
    
    InFlightDiagnostic(const InFlightDiagnostic &) = delete;
    InFlightDiagnostic &operator=(const InFlightDiagnostic &) = delete;
    InFlightDiagnostic &operator=(InFlightDiagnostic &&) = delete;

  public:
    /// Create an active but unattached in-flight diagnostic.
    /// 
    /// The resulting diagnostic can be used as a dummy, accepting the
    /// syntax to add additional information to a diagnostic without
    /// actually emitting a diagnostic.
    InFlightDiagnostic() : Engine(0), IsActive(true) { }
    
    /// Transfer an in-flight diagnostic to a new object, which is
    /// typically used when returning in-flight diagnostics.
    InFlightDiagnostic(InFlightDiagnostic &&Other)
      : Engine(Other.Engine), IsActive(Other.IsActive) {
      Other.IsActive = false;
    }
    
    ~InFlightDiagnostic() {
      if (IsActive)
        flush();
    }
    
    /// Flush the active diagnostic to the diagnostic output engine.
    void flush();

    /// Returns the \c SourceManager associated with \c SourceLoc s for this
    /// diagnostic.
    SourceManager &getSourceManager();

    /// Prevent the diagnostic from behaving more severely than \p limit. For
    /// instance, if \c DiagnosticBehavior::Warning is passed, an error will be
    /// emitted as a warning, but a note will still be emitted as a note.
    InFlightDiagnostic &limitBehavior(DiagnosticBehavior limit);

    /// Conditionally prevent the diagnostic from behaving more severely than \p
    /// limit. If the condition is false, no limit is imposed.
    InFlightDiagnostic &limitBehaviorIf(bool shouldLimit,
                                        DiagnosticBehavior limit) {
      if (!shouldLimit) {
        return *this;
      }
      return limitBehavior(limit);
    }

    /// Conditionally limit the diagnostic behavior if the given \c limit
    /// is not \c None.
    InFlightDiagnostic &
    limitBehaviorIf(std::optional<DiagnosticBehavior> limit) {
      if (!limit) {
        return *this;
      }

      return limitBehavior(*limit);
    }

    /// Limit the diagnostic behavior to \c limit until the specified
    /// version.
    ///
    /// This helps stage in fixes for stricter diagnostics as warnings
    /// until the next major language version.
    InFlightDiagnostic &limitBehaviorUntilSwiftVersion(
        DiagnosticBehavior limit, unsigned majorVersion);

    /// Limits the diagnostic behavior to \c limit accordingly if
    /// preconcurrency applies. Otherwise, the behavior limit only applies
    /// prior to the given language mode.
    ///
    /// `@preconcurrency` applied to a nominal declaration or an import
    /// statement will limit concurrency diagnostic behavior based on the
    /// strict concurrency checking level. Under minimal checking,
    /// preconcurrency will suppress the diagnostics. With strict concurrency
    /// checking, including when building with the Swift 6 language mode,
    /// preconcurrency errors are downgraded to warnings. This allows libraries
    /// to stage in concurrency annotations even after their clients have
    /// migrated to Swift 6 using `@preconcurrency` alongside the newly added
    /// `@Sendable` or `@MainActor` annotations.
    InFlightDiagnostic
    &limitBehaviorWithPreconcurrency(DiagnosticBehavior limit,
                                     bool preconcurrency,
                                     unsigned languageMode = 6) {
      if (preconcurrency) {
        return limitBehavior(limit);
      }

      return limitBehaviorUntilSwiftVersion(limit, languageMode);
    }

    /// Limit the diagnostic behavior to warning until the next future
    /// language mode.
    ///
    /// This should be preferred over passing the next major version to
    /// `warnUntilSwiftVersion` to make it easier to find and update clients
    /// when a new language mode is introduced.
    ///
    /// This helps stage in fixes for stricter diagnostics as warnings
    /// until the next major language version.
    InFlightDiagnostic &warnUntilFutureSwiftVersion();

    InFlightDiagnostic &warnUntilFutureSwiftVersionIf(bool shouldLimit) {
      if (!shouldLimit)
        return *this;
      return warnUntilFutureSwiftVersion();
    }

    /// Limit the diagnostic behavior to warning until the specified version.
    ///
    /// This helps stage in fixes for stricter diagnostics as warnings
    /// until the next major language version.
    InFlightDiagnostic &warnUntilSwiftVersion(unsigned majorVersion);

    /// Limit the diagnostic behavior to warning if the context is a
    /// swiftinterface.
    ///
    /// This is useful for diagnostics for restrictions that may be lifted by a
    /// future version of the compiler. In such cases, it may be helpful to
    /// avoid failing to build a module from its interface if the interface was
    /// emitted using a compiler that no longer has the restriction.
    InFlightDiagnostic &warnInSwiftInterface(const DeclContext *context);

    /// Conditionally limit the diagnostic behavior to warning until
    /// the specified version.  If the condition is false, no limit is
    /// imposed, meaning (presumably) it is treated as an error.
    ///
    /// This helps stage in fixes for stricter diagnostics as warnings
    /// until the next major language version.
    InFlightDiagnostic &warnUntilSwiftVersionIf(bool shouldLimit,
                                                unsigned majorVersion) {
      if (!shouldLimit) return *this;
      return warnUntilSwiftVersion(majorVersion);
    }

    /// Wraps this diagnostic in another diagnostic. That is, \p wrapper will be
    /// emitted in place of the diagnostic that otherwise would have been
    /// emitted.
    ///
    /// The first argument of \p wrapper must be of type 'Diagnostic *'.
    ///
    /// The emitted diagnostic will have:
    ///
    /// \li The ID, message, and behavior of \c wrapper.
    /// \li The arguments of \c wrapper, with the last argument replaced by the
    ///     diagnostic currently in \c *this.
    /// \li The location, ranges, decl, fix-its, and behavior limit of the
    ///     diagnostic currently in \c *this.
    InFlightDiagnostic &wrapIn(const Diagnostic &wrapper);

    /// Wraps this diagnostic in another diagnostic. That is, \p ID and
    /// \p VArgs will be emitted in place of the diagnostic that otherwise would
    /// have been emitted.
    ///
    /// The first argument of \p ID must be of type 'Diagnostic *'.
    ///
    /// The emitted diagnostic will have:
    ///
    /// \li The ID, message, and behavior of \c ID.
    /// \li The arguments of \c VArgs, with an argument appended for the
    ///     diagnostic currently in \c *this.
    /// \li The location, ranges, decl, fix-its, and behavior limit of the
    ///     diagnostic currently in \c *this.
    template<typename ...ArgTypes>
    InFlightDiagnostic &
    wrapIn(Diag<DiagnosticInfo *, ArgTypes...> ID,
           typename detail::PassArgument<ArgTypes>::type... VArgs) {
      Diagnostic wrapper{ID, nullptr, std::move(VArgs)...};
      return wrapIn(wrapper);
    }

    /// Add a token-based range to the currently-active diagnostic.
    InFlightDiagnostic &highlight(SourceRange R);

    /// Add a character-based range to the currently-active diagnostic.
    InFlightDiagnostic &highlightChars(SourceLoc Start, SourceLoc End);

    /// Add a character-based range to the currently-active diagnostic.
    InFlightDiagnostic &highlightChars(CharSourceRange Range);

    static const char *fixItStringFor(const FixItID id);

    /// Add a token-based replacement fix-it to the currently-active
    /// diagnostic.
    template <typename... ArgTypes>
    InFlightDiagnostic &
    fixItReplace(SourceRange R, StructuredFixIt<ArgTypes...> fixIt,
                 typename detail::PassArgument<ArgTypes>::type... VArgs) {
      DiagnosticArgument DiagArgs[] = { std::move(VArgs)... };
      return fixItReplace(R, fixItStringFor(fixIt.ID), DiagArgs);
    }

    /// Add a character-based replacement fix-it to the currently-active
    /// diagnostic.
    template <typename... ArgTypes>
    InFlightDiagnostic &
    fixItReplaceChars(SourceLoc Start, SourceLoc End,
                      StructuredFixIt<ArgTypes...> fixIt,
                      typename detail::PassArgument<ArgTypes>::type... VArgs) {
      DiagnosticArgument DiagArgs[] = { std::move(VArgs)... };
      return fixItReplaceChars(Start, End, fixItStringFor(fixIt.ID), DiagArgs);
    }

    /// Add an insertion fix-it to the currently-active diagnostic.
    template <typename... ArgTypes>
    InFlightDiagnostic &
    fixItInsert(SourceLoc L, StructuredFixIt<ArgTypes...> fixIt,
                typename detail::PassArgument<ArgTypes>::type... VArgs) {
      DiagnosticArgument DiagArgs[] = { std::move(VArgs)... };
      return fixItReplaceChars(L, L, fixItStringFor(fixIt.ID), DiagArgs);
    }

    /// Add an insertion fix-it to the currently-active diagnostic.  The
    /// text is inserted immediately *after* the token specified.
    template <typename... ArgTypes>
    InFlightDiagnostic &
    fixItInsertAfter(SourceLoc L, StructuredFixIt<ArgTypes...> fixIt,
                     typename detail::PassArgument<ArgTypes>::type... VArgs) {
      DiagnosticArgument DiagArgs[] = { std::move(VArgs)... };
      return fixItInsertAfter(L, fixItStringFor(fixIt.ID), DiagArgs);
    }

    /// Add a token-based replacement fix-it to the currently-active
    /// diagnostic.
    InFlightDiagnostic &fixItReplace(SourceRange R, StringRef Str);

    /// Add a character-based replacement fix-it to the currently-active
    /// diagnostic.
    InFlightDiagnostic &fixItReplaceChars(SourceLoc Start, SourceLoc End,
                                          StringRef Str) {
      return fixItReplaceChars(Start, End, "%0", {Str});
    }

    /// Add an insertion fix-it to the currently-active diagnostic.
    InFlightDiagnostic &fixItInsert(SourceLoc L, StringRef Str) {
      return fixItReplaceChars(L, L, "%0", {Str});
    }

    /// Add a fix-it suggesting to 'import' some module.
    InFlightDiagnostic &fixItAddImport(StringRef ModuleName);

    /// Add an insertion fix-it to the currently-active diagnostic.  The
    /// text is inserted immediately *after* the token specified.
    InFlightDiagnostic &fixItInsertAfter(SourceLoc L, StringRef Str) {
      return fixItInsertAfter(L, "%0", {Str});
    }

    /// Add a fix-it suggesting to insert the given attribute at the given
    /// location.
    InFlightDiagnostic &fixItInsertAttribute(SourceLoc L,
                                             const DeclAttribute *Attr);

    /// Add a fix-it suggesting to add the given attribute to the given
    /// closure.
    InFlightDiagnostic &fixItAddAttribute(const DeclAttribute *Attr,
                                          const ClosureExpr *E);

    /// Add a token-based removal fix-it to the currently-active
    /// diagnostic.
    InFlightDiagnostic &fixItRemove(SourceRange R);
    
    /// Add a character-based removal fix-it to the currently-active
    /// diagnostic.
    InFlightDiagnostic &fixItRemoveChars(SourceLoc Start, SourceLoc End) {
      return fixItReplaceChars(Start, End, {});
    }

    /// Add two replacement fix-it exchanging source ranges to the
    /// currently-active diagnostic.
    InFlightDiagnostic &fixItExchange(SourceRange R1, SourceRange R2);
    
  private:
    InFlightDiagnostic &fixItReplace(SourceRange R, StringRef FormatString,
                                     ArrayRef<DiagnosticArgument> Args);

    InFlightDiagnostic &fixItReplaceChars(SourceLoc Start, SourceLoc End,
                                          StringRef FormatString,
                                          ArrayRef<DiagnosticArgument> Args);

    InFlightDiagnostic &fixItInsert(SourceLoc L, StringRef FormatString,
                                    ArrayRef<DiagnosticArgument> Args) {
      return fixItReplaceChars(L, L, FormatString, Args);
    }

    InFlightDiagnostic &fixItInsertAfter(SourceLoc L, StringRef FormatString,
                                         ArrayRef<DiagnosticArgument> Args);
  };

  /// Class to track, map, and remap diagnostic severity and fatality
  ///
  class DiagnosticState {
    /// Whether we should continue to emit diagnostics, even after a
    /// fatal error
    bool showDiagnosticsAfterFatalError = false;

    /// Don't emit any warnings
    bool suppressWarnings = false;
    
    /// Don't emit any remarks
    bool suppressRemarks = false;

    /// A mapping from `DiagGroupID` identifiers to Boolean values indicating
    /// whether warnings belonging to the respective diagnostic groups should be
    /// escalated to errors.
    llvm::BitVector warningsAsErrors;

    /// Whether a fatal error has occurred
    bool fatalErrorOccurred = false;

    /// Whether any error diagnostics have been emitted.
    bool anyErrorOccurred = false;

    /// Track the previous emitted Behavior, useful for notes
    DiagnosticBehavior previousBehavior = DiagnosticBehavior::Unspecified;

    /// Track which diagnostics should be ignored.
    llvm::BitVector ignoredDiagnostics;

    friend class DiagnosticStateRAII;

  public:
    DiagnosticState();

    /// Figure out the Behavior for the given diagnostic, taking current
    /// state such as fatality into account.
    DiagnosticBehavior determineBehavior(const Diagnostic &diag) const;

    /// Updates the diagnostic state for a diagnostic to emit.
    void updateFor(DiagnosticBehavior behavior);

    bool hadAnyError() const { return anyErrorOccurred; }
    bool hasFatalErrorOccurred() const { return fatalErrorOccurred; }

    void setShowDiagnosticsAfterFatalError(bool val = true) {
      showDiagnosticsAfterFatalError = val;
    }
    bool getShowDiagnosticsAfterFatalError() {
      return showDiagnosticsAfterFatalError;
    }

    /// Whether to skip emitting warnings
    void setSuppressWarnings(bool val) { suppressWarnings = val; }
    bool getSuppressWarnings() const { return suppressWarnings; }
    
    /// Whether to skip emitting remarks
    void setSuppressRemarks(bool val) { suppressRemarks = val; }
    bool getSuppressRemarks() const { return suppressRemarks; }

    /// Sets whether warnings belonging to the diagnostic group identified by
    /// `id` should be escalated to errors.
    void setWarningsAsErrorsForDiagGroupID(DiagGroupID id, bool value) {
      warningsAsErrors[(unsigned)id] = value;
    }

    /// Returns a Boolean value indicating whether warnings belonging to the
    /// diagnostic group identified by `id` should be escalated to errors.
    bool getWarningsAsErrorsForDiagGroupID(DiagGroupID id) const {
      return warningsAsErrors[(unsigned)id];
    }

    /// Whether all warnings should be upgraded to errors or not.
    void setAllWarningsAsErrors(bool value) {
      // This works as intended because every diagnostic belongs to either a
      // custom group or the top-level `DiagGroupID::no_group`, which is also
      // a group.
      if (value) {
        warningsAsErrors.set();
      } else {
        warningsAsErrors.reset();
      }
    }

    void resetHadAnyError() {
      anyErrorOccurred = false;
      fatalErrorOccurred = false;
    }

    /// Set whether a diagnostic should be ignored.
    void setIgnoredDiagnostic(DiagID id, bool ignored) {
      ignoredDiagnostics[(unsigned)id] = ignored;
    }

    void swap(DiagnosticState &other) {
      std::swap(showDiagnosticsAfterFatalError, other.showDiagnosticsAfterFatalError);
      std::swap(suppressWarnings, other.suppressWarnings);
      std::swap(suppressRemarks, other.suppressRemarks);
      std::swap(warningsAsErrors, other.warningsAsErrors);
      std::swap(fatalErrorOccurred, other.fatalErrorOccurred);
      std::swap(anyErrorOccurred, other.anyErrorOccurred);
      std::swap(previousBehavior, other.previousBehavior);
      std::swap(ignoredDiagnostics, other.ignoredDiagnostics);
    }

  private:
    // Make the state movable only
    DiagnosticState(const DiagnosticState &) = delete;
    const DiagnosticState &operator=(const DiagnosticState &) = delete;

    DiagnosticState(DiagnosticState &&) = default;
    DiagnosticState &operator=(DiagnosticState &&) = default;
  };

  /// A lightweight reference to a diagnostic that's been fully applied to
  /// its arguments.  This allows a general routine (in the parser, say) to
  /// be customized to emit an arbitrary diagnostic without needing to
  /// eagerly construct a full Diagnostic.  Like ArrayRef and function_ref,
  /// this stores a reference to what's likely to be a temporary, so it
  /// should only be used as a function parameter.  If you need to persist
  /// the diagnostic, you'll have to call createDiagnostic().
  ///
  /// You can initialize a DiagRef parameter in one of two ways:
  /// - passing a Diag<> as the argument, e.g.
  ///      diag::circular_reference
  ///   or
  /// - constructing it with a Diag and its arguments, e.g.
  ///      {diag::circular_protocol_def, {proto->getName()}}
  ///
  /// It'd be nice to let people write `{diag::my_error, arg0, arg1}`
  /// instead of `{diag::my_error, {arg0, arg1}}`, but we can't: the
  /// temporary needs to be created in the calling context.
  class DiagRef {
    DiagID id;

    /// If this is null, then id is a Diag<> and there are no arguments.
    Diagnostic (*createFn)(DiagID id, const void *opaqueStorage);
    const void *opaqueStorage;

  public:
    /// Construct a diagnostic from a diagnostic ID that's known to not take
    /// arguments.
    DiagRef(Diag<> id)
      : id(id.ID), createFn(nullptr), opaqueStorage(nullptr) {}

    /// Construct a diagnostic from a diagnostic ID and its arguments.
    template <class... ArgTypes>
    DiagRef(Diag<ArgTypes...> id, const DiagArgTuple<ArgTypes...> &tuple)
      : id(id.ID),
        createFn(&createFromTuple<ArgTypes...>),
        opaqueStorage(&tuple) {}

    // A specialization of the general constructor above for diagnostics
    // with no arguments; this is a useful optimization when a DiagRef
    // is constructed generically.
    DiagRef(Diag<> id, const DiagArgTuple<> &tuple)
      : DiagRef(id) {}

    /// Return the diagnostic ID that this will emit.
    DiagID getID() const {
      return id;
    }

    /// Create a full Diagnostic.  It's safe to do this multiple times on
    /// a single DiagRef.
    Diagnostic createDiagnostic() {
      if (!createFn) {
        return Diagnostic(Diag<> {id});
      } else {
        return createFn(id, opaqueStorage);
      }
    }

  private:
    template <class... ArgTypes>
    static Diagnostic createFromTuple(DiagID id, const void *opaqueStorage) {
      auto tuple = static_cast<const DiagArgTuple<ArgTypes...> *>(opaqueStorage);
      return Diagnostic::fromTuple(Diag<ArgTypes...> {id}, *tuple);
    }
  };

  /// Class responsible for formatting diagnostics and presenting them
  /// to the user.
  class DiagnosticEngine {
  public:
    /// The source manager used to interpret source locations and
    /// display diagnostics.
    SourceManager &SourceMgr;

  private:
    /// The diagnostic consumer(s) that will be responsible for actually
    /// emitting diagnostics.
    SmallVector<DiagnosticConsumer *, 2> Consumers;

    /// Tracks diagnostic behaviors and state
    DiagnosticState state;

    /// The currently active diagnostic, if there is one.
    std::optional<Diagnostic> ActiveDiagnostic;

    /// Diagnostics wrapped by ActiveDiagnostic, if any.
    SmallVector<DiagnosticInfo, 2> WrappedDiagnostics;
    SmallVector<std::vector<DiagnosticArgument>, 4> WrappedDiagnosticArgs;

    /// All diagnostics that have are no longer active but have not yet
    /// been emitted due to an open transaction.
    SmallVector<Diagnostic, 4> TentativeDiagnostics;

    llvm::BumpPtrAllocator TransactionAllocator;
    /// A set of all strings involved in current transactional chain.
    /// This is required because diagnostics are not directly emitted
    /// but rather stored until all transactions complete.
    llvm::StringSet<llvm::BumpPtrAllocator &> TransactionStrings;

    /// Diagnostic producer to handle the logic behind retrieving a localized
    /// diagnostic message.
    std::unique_ptr<diag::LocalizationProducer> localization;

    /// This allocator will retain diagnostic strings containing the
    /// diagnostic's message and identifier as `message [id]` for the duration
    /// of compiler invocation. This will be used when the frontend flags
    /// `-debug-diagnostic-names` or `-print-diagnostic-groups` are used.
    llvm::BumpPtrAllocator DiagnosticStringsAllocator;
    llvm::StringSaver DiagnosticStringsSaver;

    /// The number of open diagnostic transactions. Diagnostics are only
    /// emitted once all transactions have closed.
    unsigned TransactionCount = 0;

    /// For batch mode, use this to know where to output a diagnostic from a
    /// non-primary file. It's any location in the buffer of the current primary
    /// input being compiled.
    /// May be invalid.
    SourceLoc bufferIndirectlyCausingDiagnostic;

    /// When printing diagnostics, include either the diagnostic name
    /// (diag::whatever) at the end or the associated diagnostic group.
    PrintDiagnosticNamesMode printDiagnosticNamesMode =
        PrintDiagnosticNamesMode::None;

    /// Path to diagnostic documentation directory.
    std::string diagnosticDocumentationPath = "";

    /// The Swift language version. This is used to limit diagnostic behavior
    /// until a specific language version, e.g. Swift 6.
    version::Version languageVersion;

    /// The stats reporter used to keep track of Swift 6 errors
    /// diagnosed via \c warnUntilSwiftVersion(6).
    UnifiedStatsReporter *statsReporter = nullptr;

    /// Whether we are actively pretty-printing a declaration as part of
    /// diagnostics.
    bool IsPrettyPrintingDecl = false;

    friend class InFlightDiagnostic;
    friend class DiagnosticTransaction;
    friend class CompoundDiagnosticTransaction;
    friend class DiagnosticStateRAII;
    friend class DiagnosticQueue;
    friend class PrettyPrintDeclRequest;

  public:
    explicit DiagnosticEngine(SourceManager &SourceMgr)
        : SourceMgr(SourceMgr), ActiveDiagnostic(),
          TransactionStrings(TransactionAllocator),
          DiagnosticStringsSaver(DiagnosticStringsAllocator) {}

    /// hadAnyError - return true if any *error* diagnostics have been emitted.
    bool hadAnyError() const { return state.hadAnyError(); }

    bool hasFatalErrorOccurred() const {
      return state.hasFatalErrorOccurred();
    }

    void setShowDiagnosticsAfterFatalError(bool val = true) {
      state.setShowDiagnosticsAfterFatalError(val);
    }
    bool getShowDiagnosticsAfterFatalError() {
      return state.getShowDiagnosticsAfterFatalError();
    }

    void flushConsumers() {
      for (auto consumer : Consumers)
        consumer->flush();
    }

    /// Whether to skip emitting warnings
    void setSuppressWarnings(bool val) { state.setSuppressWarnings(val); }
    bool getSuppressWarnings() const {
      return state.getSuppressWarnings();
    }

    /// Whether to skip emitting remarks
    void setSuppressRemarks(bool val) { state.setSuppressRemarks(val); }
    bool getSuppressRemarks() const {
      return state.getSuppressRemarks();
    }

    /// Apply rules specifing what warnings should or shouldn't be treated as
    /// errors. For group rules the string is a group name defined by
    /// DiagnosticGroups.def
    /// Rules are applied in order they appear in the vector.
    /// In case the vector contains rules affecting the same diagnostic ID
    /// the last rule wins.
    void setWarningsAsErrorsRules(const std::vector<WarningAsErrorRule> &rules);

    /// Whether to print diagnostic names after their messages
    void setPrintDiagnosticNamesMode(PrintDiagnosticNamesMode val) {
      printDiagnosticNamesMode = val;
    }
    PrintDiagnosticNamesMode getPrintDiagnosticNamesMode() const {
      return printDiagnosticNamesMode;
    }

    void setDiagnosticDocumentationPath(std::string path) {
      diagnosticDocumentationPath = path;
    }
    StringRef getDiagnosticDocumentationPath() {
      return diagnosticDocumentationPath;
    }

    bool isPrettyPrintingDecl() const { return IsPrettyPrintingDecl; }

    void setLanguageVersion(version::Version v) { languageVersion = v; }

    void setStatsReporter(UnifiedStatsReporter *stats) {
      statsReporter = stats;
    }

    void setLocalization(StringRef locale, StringRef path) {
      assert(!locale.empty());
      assert(!path.empty());
      localization = diag::LocalizationProducer::producerFor(locale, path);
    }

    void ignoreDiagnostic(DiagID id) {
      state.setIgnoredDiagnostic(id, true);
    }

    void resetHadAnyError() {
      state.resetHadAnyError();
    }

    /// Add an additional DiagnosticConsumer to receive diagnostics.
    void addConsumer(DiagnosticConsumer &Consumer) {
      Consumers.push_back(&Consumer);
    }

    /// Remove a specific DiagnosticConsumer.
    void removeConsumer(DiagnosticConsumer &Consumer) {
      Consumers.erase(
          std::remove(Consumers.begin(), Consumers.end(), &Consumer));
    }

    /// Remove and return all \c DiagnosticConsumers.
    std::vector<DiagnosticConsumer *> takeConsumers() {
      auto Result = std::vector<DiagnosticConsumer*>(Consumers.begin(),
                                                     Consumers.end());
      Consumers.clear();
      return Result;
    }

    /// Return all \c DiagnosticConsumers.
    ArrayRef<DiagnosticConsumer *> getConsumers() const {
      return Consumers;
    }

    /// Emit a diagnostic using a preformatted array of diagnostic
    /// arguments.
    ///
    /// \param Loc The location to which the diagnostic refers in the source
    /// code.
    ///
    /// \param ID The diagnostic ID.
    ///
    /// \param Args The preformatted set of diagnostic arguments. The caller
    /// must ensure that the diagnostic arguments have the appropriate type.
    ///
    /// \returns An in-flight diagnostic, to which additional information can
    /// be attached.
    InFlightDiagnostic diagnose(SourceLoc Loc, DiagID ID, 
                                ArrayRef<DiagnosticArgument> Args) {
      return diagnose(Loc, Diagnostic(ID, Args));
    }

    /// Emit a diagnostic using a preformatted array of diagnostic
    /// arguments.
    ///
    /// \param Loc The declaration name location to which the
    /// diagnostic refers in the source code.
    ///
    /// \param ID The diagnostic ID.
    ///
    /// \param Args The preformatted set of diagnostic arguments. The caller
    /// must ensure that the diagnostic arguments have the appropriate type.
    ///
    /// \returns An in-flight diagnostic, to which additional information can
    /// be attached.
    InFlightDiagnostic diagnose(DeclNameLoc Loc, DiagID ID, 
                                ArrayRef<DiagnosticArgument> Args) {
      return diagnose(Loc.getBaseNameLoc(), Diagnostic(ID, Args));
    }

    /// Emit an already-constructed diagnostic at the given location.
    ///
    /// \param Loc The location to which the diagnostic refers in the source
    /// code.
    ///
    /// \param D The diagnostic.
    ///
    /// \returns An in-flight diagnostic, to which additional information can
    /// be attached.
    InFlightDiagnostic diagnose(SourceLoc Loc, const Diagnostic &D) {
      assert(!ActiveDiagnostic && "Already have an active diagnostic");
      ActiveDiagnostic = D;
      ActiveDiagnostic->setLoc(Loc);
      return InFlightDiagnostic(*this);
    }
    
    /// Emit a diagnostic with the given set of diagnostic arguments.
    ///
    /// \param Loc The location to which the diagnostic refers in the source
    /// code.
    ///
    /// \param ID The diagnostic to be emitted.
    ///
    /// \param Args The diagnostic arguments, which will be converted to
    /// the types expected by the diagnostic \p ID.
    template<typename ...ArgTypes>
    InFlightDiagnostic 
    diagnose(SourceLoc Loc, Diag<ArgTypes...> ID,
             typename detail::PassArgument<ArgTypes>::type... Args) {
      return diagnose(Loc, Diagnostic(ID, std::move(Args)...));
    }

    /// Emit the given lazily-applied diagnostic at the specified
    /// source location.
    InFlightDiagnostic diagnose(SourceLoc loc, DiagRef diag) {
      return diagnose(loc, diag.createDiagnostic());
    }

    /// Delete an API that may lead clients to avoid specifying source location.
    template<typename ...ArgTypes>
    InFlightDiagnostic
    diagnose(Diag<ArgTypes...> ID,
             typename detail::PassArgument<ArgTypes>::type... Args) = delete;

    /// Emit a diagnostic with the given set of diagnostic arguments.
    ///
    /// \param Loc The declaration name location to which the
    /// diagnostic refers in the source code.
    ///
    /// \param ID The diagnostic to be emitted.
    ///
    /// \param Args The diagnostic arguments, which will be converted to
    /// the types expected by the diagnostic \p ID.
    template<typename ...ArgTypes>
    InFlightDiagnostic 
    diagnose(DeclNameLoc Loc, Diag<ArgTypes...> ID,
             typename detail::PassArgument<ArgTypes>::type... Args) {
      return diagnose(Loc.getBaseNameLoc(), Diagnostic(ID, std::move(Args)...));
    }

    /// Emit a diagnostic using a preformatted array of diagnostic
    /// arguments.
    ///
    /// \param decl The declaration to which this diagnostic refers, which
    /// may or may not have associated source-location information.
    ///
    /// \param id The diagnostic ID.
    ///
    /// \param args The preformatted set of diagnostic arguments. The caller
    /// must ensure that the diagnostic arguments have the appropriate type.
    ///
    /// \returns An in-flight diagnostic, to which additional information can
    /// be attached.
    InFlightDiagnostic diagnose(const Decl *decl, DiagID id,
                                ArrayRef<DiagnosticArgument> args) {
      return diagnose(decl, Diagnostic(id, args));
    }

    /// Emit an already-constructed diagnostic referencing the given
    /// declaration.
    ///
    /// \param decl The declaration to which this diagnostic refers, which
    /// may or may not have associated source-location information.
    ///
    /// \param diag The diagnostic.
    ///
    /// \returns An in-flight diagnostic, to which additional information can
    /// be attached.
    InFlightDiagnostic diagnose(const Decl *decl, const Diagnostic &diag) {
      assert(!ActiveDiagnostic && "Already have an active diagnostic");
      ActiveDiagnostic = diag;
      ActiveDiagnostic->setDecl(decl);
      return InFlightDiagnostic(*this);
    }

    /// Emit a diagnostic with the given set of diagnostic arguments.
    ///
    /// \param decl The declaration to which this diagnostic refers, which
    /// may or may not have associated source-location information.
    ///
    /// \param id The diagnostic to be emitted.
    ///
    /// \param args The diagnostic arguments, which will be converted to
    /// the types expected by the diagnostic \p ID.
    template<typename ...ArgTypes>
    InFlightDiagnostic
    diagnose(const Decl *decl, Diag<ArgTypes...> id,
             typename detail::PassArgument<ArgTypes>::type... args) {
      return diagnose(decl, Diagnostic(id, std::move(args)...));
    }

    /// Emit a parent diagnostic and attached notes.
    ///
    /// \param parentDiag An InFlightDiagnostic representing the parent diag.
    ///
    /// \param builder A closure which builds and emits notes to be attached to
    /// the parent diag.
    void diagnoseWithNotes(InFlightDiagnostic parentDiag,
                           llvm::function_ref<void(void)> builder);

    /// \returns true if diagnostic is marked with PointsToFirstBadToken
    /// option.
    bool isDiagnosticPointsToFirstBadToken(DiagID id) const;

    /// \returns true if the diagnostic is an API digester API or ABI breakage
    /// diagnostic.
    bool isAPIDigesterBreakageDiagnostic(DiagID id) const;

    /// \returns true if the diagnostic is marking a deprecation.
    bool isDeprecationDiagnostic(DiagID id) const;

    /// \returns true if the diagnostic is marking an unused element.
    bool isNoUsageDiagnostic(DiagID id) const;

    /// \returns true if any diagnostic consumer gave an error while invoking
    //// \c finishProcessing.
    bool finishProcessing();

    /// Format the given diagnostic text and place the result in the given
    /// buffer.
    static void formatDiagnosticText(
        llvm::raw_ostream &Out, StringRef InText,
        ArrayRef<DiagnosticArgument> FormatArgs,
        DiagnosticFormatOptions FormatOpts = DiagnosticFormatOptions());

  private:
    /// Called when tentative diagnostic is about to be flushed,
    /// to apply any required transformations e.g. copy string arguments
    /// to extend their lifetime.
    void onTentativeDiagnosticFlush(Diagnostic &diagnostic);

    /// Flush the active diagnostic.
    void flushActiveDiagnostic();
    
    /// Retrieve the active diagnostic.
    Diagnostic &getActiveDiagnostic() { return *ActiveDiagnostic; }

    /// Generate DiagnosticInfo for a Diagnostic to be passed to consumers.
    std::optional<DiagnosticInfo>
    diagnosticInfoForDiagnostic(const Diagnostic &diagnostic,
                                bool includeDiagnosticName);

    /// Send \c diag to all diagnostic consumers.
    void emitDiagnostic(const Diagnostic &diag);

    /// Retrieve the set of child notes that describe how the generated
    /// source buffer was derived, e.g., a macro expansion backtrace.
    std::vector<Diagnostic> getGeneratedSourceBufferNotes(SourceLoc loc);

    /// Handle a new diagnostic, which will either be emitted, or added to an
    /// active transaction.
    void handleDiagnostic(Diagnostic &&diag);

    /// Clear any tentative diagnostics.
    void clearTentativeDiagnostics();

    /// Send all tentative diagnostics to all diagnostic consumers and
    /// delete them.
    void emitTentativeDiagnostics();

    /// Forward all tentative diagnostics to a different diagnostic engine.
    void forwardTentativeDiagnosticsTo(DiagnosticEngine &targetEngine);

  public:
    DiagnosticKind declaredDiagnosticKindFor(const DiagID id);

    /// Get a localized format string for the given `DiagID`. If no localization
    /// is available, returns the default string.
    llvm::StringRef getFormatStringForDiagnostic(DiagID id);

    /// Get a localized format string for the given diagnostic. If no
    /// localization is available, returns the default string.
    ///
    /// \param includeDiagnosticName Whether to at all consider embedding the
    /// name of the diagnostic identifier or group, per the setting.
    llvm::StringRef getFormatStringForDiagnostic(const Diagnostic &diagnostic,
                                                 bool includeDiagnosticName);

    static llvm::StringRef diagnosticIDStringFor(const DiagID id);

    /// If there is no clear .dia file for a diagnostic, put it in the one
    /// corresponding to the SourceLoc given here.
    /// In particular, in batch mode when a diagnostic is located in
    /// a non-primary file, use this affordance to place it in the .dia
    /// file for the primary that is currently being worked on.
    void setBufferIndirectlyCausingDiagnosticToInput(SourceLoc);
    void resetBufferIndirectlyCausingDiagnostic();
    SourceLoc getDefaultDiagnosticLoc() const {
      return bufferIndirectlyCausingDiagnostic;
    }
    SourceLoc getBestAddImportFixItLoc(SourceFile *sf) const;
  };

  inline SourceManager &InFlightDiagnostic::getSourceManager() {
    return Engine->SourceMgr;
  }

  /// Remember details about the state of a diagnostic engine and restore them
  /// when the object is destroyed.
  ///
  /// Diagnostic engines contain state about the most recent diagnostic emitted
  /// which influences subsequent emissions; in particular, if you try to emit
  /// a note and the previous diagnostic was ignored, the note will be ignored
  /// too. This can be a problem in code structured like:
  ///
  ///     D->diagnose(diag::an_error);
  ///     if (conditionWhichMightEmitDiagnostics())
  ///        D->diagnose(diag::a_note); // might be affected by diagnostics from
  ///                                   // conditionWhichMightEmitDiagnostics()!
  ///
  /// To prevent this, functions which are called for their return values but
  /// may emit diagnostics as a side effect can use \c DiagnosticStateRAII to
  /// ensure that their changes to diagnostic engine state don't leak out and
  /// affect the caller's diagnostics.
  class DiagnosticStateRAII {
    llvm::SaveAndRestore<DiagnosticBehavior> previousBehavior;

  public:
    DiagnosticStateRAII(DiagnosticEngine &diags)
      : previousBehavior(diags.state.previousBehavior) {}

    ~DiagnosticStateRAII() {}
  };

  class BufferIndirectlyCausingDiagnosticRAII {
  private:
    DiagnosticEngine &Diags;
  public:
    BufferIndirectlyCausingDiagnosticRAII(const SourceFile &SF);
    ~BufferIndirectlyCausingDiagnosticRAII() {
      Diags.resetBufferIndirectlyCausingDiagnostic();
    }
  };

  /// Represents a diagnostic transaction. While a transaction is
  /// open, all recorded diagnostics are saved until the transaction commits,
  /// at which point they are emitted. If the transaction is instead aborted,
  /// the diagnostics are erased. Transactions may be nested but must be closed
  /// in LIFO order. An open transaction is implicitly committed upon
  /// destruction.
  class DiagnosticTransaction {
  protected:
    DiagnosticEngine &Engine;

    /// How many tentative diagnostics there were when the transaction
    /// was opened.
    unsigned PrevDiagnostics;

    /// How many other transactions were open when this transaction was
    /// opened.
    unsigned Depth;

    /// Whether this transaction is currently open.
    bool IsOpen = true;

  public:
    DiagnosticTransaction(const DiagnosticTransaction &) = delete;
    DiagnosticTransaction &operator=(const DiagnosticTransaction &) = delete;

    explicit DiagnosticTransaction(DiagnosticEngine &engine)
      : Engine(engine),
        PrevDiagnostics(Engine.TentativeDiagnostics.size()),
        Depth(Engine.TransactionCount),
        IsOpen(true)
    {
      Engine.TransactionCount++;
    }

    ~DiagnosticTransaction() {
      if (IsOpen) {
        commit();
      }

      if (Depth == 0) {
        Engine.TransactionStrings.clear();
        Engine.TransactionAllocator.Reset();
      }
    }

    bool hasErrors() const {
      ArrayRef<Diagnostic> diagnostics(Engine.TentativeDiagnostics.begin() +
                                           PrevDiagnostics,
                                       Engine.TentativeDiagnostics.end());

      for (auto &diagnostic : diagnostics) {
        auto behavior = Engine.state.determineBehavior(diagnostic);
        if (behavior == DiagnosticBehavior::Fatal ||
            behavior == DiagnosticBehavior::Error)
          return true;
      }

      return false;
    }

    /// Abort and close this transaction and erase all diagnostics
    /// record while it was open.
    void abort() {
      close();
      Engine.TentativeDiagnostics.erase(
        Engine.TentativeDiagnostics.begin() + PrevDiagnostics,
        Engine.TentativeDiagnostics.end());
    }

    /// Commit and close this transaction. If this is the top-level
    /// transaction, emit any diagnostics that were recorded while it was open.
    void commit() {
      close();
      if (Depth == 0) {
        assert(PrevDiagnostics == 0);
        Engine.emitTentativeDiagnostics();
      }
    }

  private:
    void close() {
      assert(IsOpen && "only open transactions may be closed");
      IsOpen = false;
      Engine.TransactionCount--;
      assert(Depth == Engine.TransactionCount &&
             "transactions must be closed LIFO");
    }
  };

  /// Represents a diagnostic transaction which constructs a compound diagnostic
  /// from any diagnostics emitted inside. A compound diagnostic consists of a
  /// parent error, warning, or remark followed by a variable number of child
  /// notes. The semantics are otherwise the same as a regular
  /// DiagnosticTransaction.
  class CompoundDiagnosticTransaction : public DiagnosticTransaction {
  public:
    explicit CompoundDiagnosticTransaction(DiagnosticEngine &engine)
        : DiagnosticTransaction(engine) {}

    ~CompoundDiagnosticTransaction() {
      if (IsOpen) {
        commit();
      }

      if (Depth == 0) {
        Engine.TransactionStrings.clear();
        Engine.TransactionAllocator.Reset();
      }
    }

    void commit() {
      assert(PrevDiagnostics < Engine.TentativeDiagnostics.size() &&
             "CompoundDiagnosticTransaction must contain at least one diag");

      // The first diagnostic is assumed to be the parent. If this is not an
      // error or warning, we'll assert later when trying to add children.
      Diagnostic &parent = Engine.TentativeDiagnostics[PrevDiagnostics];

      // Associate the children with the parent.
      for (auto diag =
               Engine.TentativeDiagnostics.begin() + PrevDiagnostics + 1;
           diag != Engine.TentativeDiagnostics.end(); ++diag) {
        diag->setIsChildNote(true);
        parent.addChildNote(std::move(*diag));
      }

      // Erase the children, they'll be emitted alongside their parent.
      Engine.TentativeDiagnostics.erase(Engine.TentativeDiagnostics.begin() +
                                            PrevDiagnostics + 1,
                                        Engine.TentativeDiagnostics.end());

      DiagnosticTransaction::commit();
    }
  };

  /// Represents a queue of diagnostics that have their emission delayed until
  /// the queue is destroyed. This is similar to DiagnosticTransaction, but
  /// with a few key differences:
  /// 
  /// - The queue maintains its own diagnostic engine (which may be accessed
  ///   through `getDiags()`), and diagnostics must be specifically emitted
  ///   using that engine to be enqueued.
  /// - It allows for non-LIFO transactions, as each queue operates
  ///   independently.
  /// - A queue can be drained multiple times without having to be recreated
  ///   (unlike DiagnosticTransaction, it has no concept of "closing").
  ///
  /// Note you may add DiagnosticTransactions to the queue's diagnostic engine,
  /// but they must be closed before attempting to clear or emit the diagnostics
  /// in the queue.
  ///
  class DiagnosticQueue final {
    /// The underlying diagnostic engine that the diagnostics will be emitted
    /// by.
    DiagnosticEngine &UnderlyingEngine;

    /// A temporary engine used to queue diagnostics.
    DiagnosticEngine QueueEngine;

    /// Whether the queued diagnostics should be emitted on the destruction of
    /// the queue, or whether they should be cleared.
    bool EmitOnDestruction;

  public:
    DiagnosticQueue(const DiagnosticQueue &) = delete;
    DiagnosticQueue &operator=(const DiagnosticQueue &) = delete;

    /// Create a new diagnostic queue with a given engine to forward the
    /// diagnostics to.
    explicit DiagnosticQueue(DiagnosticEngine &engine, bool emitOnDestruction)
        : UnderlyingEngine(engine), QueueEngine(engine.SourceMgr),
          EmitOnDestruction(emitOnDestruction) {
      // Open a transaction to avoid emitting any diagnostics for the temporary
      // engine.
      QueueEngine.TransactionCount++;
    }

    /// Retrieve the engine which may be used to enqueue diagnostics.
    DiagnosticEngine &getDiags() { return QueueEngine; }

    /// Retrieve the underlying engine which will receive the diagnostics.
    DiagnosticEngine &getUnderlyingDiags() const { return UnderlyingEngine; }

    /// Clear this queue and erase all diagnostics recorded.
    void clear() {
      assert(QueueEngine.TransactionCount == 1 &&
             "Must close outstanding DiagnosticTransactions before draining");
      QueueEngine.clearTentativeDiagnostics();
    }

    /// Emit all the diagnostics recorded by this queue.
    void emit() {
      assert(QueueEngine.TransactionCount == 1 &&
             "Must close outstanding DiagnosticTransactions before draining");
      QueueEngine.forwardTentativeDiagnosticsTo(UnderlyingEngine);
    }

    ~DiagnosticQueue() {
      if (EmitOnDestruction) {
        emit();
      } else {
        clear();
      }
      QueueEngine.TransactionCount--;
    }
  };

  /// A RAII object that adds and removes a diagnostic consumer from an engine.
  class DiagnosticConsumerRAII final {
    DiagnosticEngine &Diags;
    DiagnosticConsumer &Consumer;

  public:
    DiagnosticConsumerRAII(DiagnosticEngine &diags,
                           DiagnosticConsumer &consumer)
        : Diags(diags), Consumer(consumer) {
      Diags.addConsumer(Consumer);
    }
    ~DiagnosticConsumerRAII() {
      Diags.removeConsumer(Consumer);
    }
  };

  inline void
  DiagnosticEngine::diagnoseWithNotes(InFlightDiagnostic parentDiag,
                                      llvm::function_ref<void(void)> builder) {
    CompoundDiagnosticTransaction transaction(*this);
    parentDiag.flush();
    builder();
  }

  void printClangDeclName(const clang::NamedDecl *ND, llvm::raw_ostream &os);
  void printClangTypeName(const clang::Type *Ty, llvm::raw_ostream &os);

  /// Temporary on-stack storage and unescaping for encoded diagnostic
  /// messages.
  class EncodedDiagnosticMessage {
    llvm::SmallString<128> Buf;

  public:
    /// \param S A string with an encoded message
    EncodedDiagnosticMessage(StringRef S);

    /// The unescaped message to display to the user.
    const StringRef Message;
  };

} // end namespace swift

#endif
