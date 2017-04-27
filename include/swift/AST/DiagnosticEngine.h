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

#include "swift/AST/TypeLoc.h"
#include "swift/AST/DeclNameLoc.h"
#include "swift/Basic/DiagnosticConsumer.h"

namespace swift {
  class Decl;
  class DiagnosticEngine;
  class SourceManager;
  class ValueDecl;
  
  enum class PatternKind : uint8_t;
  enum class StaticSpellingKind : uint8_t;
  enum class DescriptiveDeclKind : uint8_t;
  enum DeclAttrKind : unsigned;

  /// \brief Enumeration describing all of possible diagnostics.
  ///
  /// Each of the diagnostics described in Diagnostics.def has an entry in
  /// this enumeration type that uniquely identifies it.
  enum class DiagID : uint32_t;

  /// \brief Describes a diagnostic along with its argument types.
  ///
  /// The diagnostics header introduces instances of this type for each 
  /// diagnostic, which provide both the set of argument types (used to
  /// check/convert the arguments at each call site) and the diagnostic ID
  /// (for other information about the diagnostic).
  template<typename ...ArgTypes>
  struct Diag {
    /// \brief The diagnostic ID corresponding to this diagnostic.
    DiagID ID;
  };

  namespace detail {
    /// \brief Describes how to pass a diagnostic argument of the given type.
    ///
    /// By default, diagnostic arguments are passed by value, because they
    /// tend to be small. Larger diagnostic arguments
    /// need to specialize this class template to pass by reference.
    template<typename T>
    struct PassArgument {
      typedef T type;
    };
  }
    
  /// \brief Describes the kind of diagnostic argument we're storing.
  ///
  enum class DiagnosticArgumentKind {
    String,
    Integer,
    Unsigned,
    Identifier,
    ObjCSelector,
    ValueDecl,
    Type,
    TypeRepr,
    PatternKind,
    StaticSpellingKind,
    DescriptiveDeclKind,
    DeclAttribute,
    VersionTuple,
    LayoutConstraint,
  };

  namespace diag {
    enum class RequirementKind : uint8_t;
  }

  /// \brief Variant type that holds a single diagnostic argument of a known
  /// type.
  ///
  /// All diagnostic arguments are converted to an instance of this class.
  class DiagnosticArgument {
    DiagnosticArgumentKind Kind;
    union {
      int IntegerVal;
      unsigned UnsignedVal;
      StringRef StringVal;
      DeclName IdentifierVal;
      ObjCSelector ObjCSelectorVal;
      ValueDecl *TheValueDecl;
      Type TypeVal;
      TypeRepr *TyR;
      PatternKind PatternKindVal;
      StaticSpellingKind StaticSpellingKindVal;
      DescriptiveDeclKind DescriptiveDeclKindVal;
      const DeclAttribute *DeclAttributeVal;
      clang::VersionTuple VersionVal;
      LayoutConstraint LayoutConstraintVal;
    };
    
  public:
    DiagnosticArgument(StringRef S)
      : Kind(DiagnosticArgumentKind::String), StringVal(S) {
    }

    DiagnosticArgument(int I) 
      : Kind(DiagnosticArgumentKind::Integer), IntegerVal(I) {
    }

    DiagnosticArgument(unsigned I) 
      : Kind(DiagnosticArgumentKind::Unsigned), UnsignedVal(I) {
    }

    DiagnosticArgument(DeclName I)
      : Kind(DiagnosticArgumentKind::Identifier), IdentifierVal(I) {
    }
    
    DiagnosticArgument(Identifier I)
      : Kind(DiagnosticArgumentKind::Identifier), IdentifierVal(I) {
    }

    DiagnosticArgument(ObjCSelector S)
      : Kind(DiagnosticArgumentKind::ObjCSelector), ObjCSelectorVal(S) {
    }

    DiagnosticArgument(ValueDecl *VD)
      : Kind(DiagnosticArgumentKind::ValueDecl), TheValueDecl(VD) {
    }

    DiagnosticArgument(Type T)
      : Kind(DiagnosticArgumentKind::Type), TypeVal(T) {
    }

    DiagnosticArgument(TypeRepr *T)
      : Kind(DiagnosticArgumentKind::TypeRepr), TyR(T) {
    }

    DiagnosticArgument(const TypeLoc &TL) {
      if (TypeRepr *tyR = TL.getTypeRepr()) {
        Kind = DiagnosticArgumentKind::TypeRepr;
        TyR = tyR;
      } else {
        Kind = DiagnosticArgumentKind::Type;
        TypeVal = TL.getType();
      }
    }

    DiagnosticArgument(PatternKind K)
        : Kind(DiagnosticArgumentKind::PatternKind), PatternKindVal(K) {}

    DiagnosticArgument(StaticSpellingKind SSK)
        : Kind(DiagnosticArgumentKind::StaticSpellingKind),
          StaticSpellingKindVal(SSK) {}

    DiagnosticArgument(DescriptiveDeclKind DDK)
        : Kind(DiagnosticArgumentKind::DescriptiveDeclKind),
          DescriptiveDeclKindVal(DDK) {}

    DiagnosticArgument(const DeclAttribute *attr)
        : Kind(DiagnosticArgumentKind::DeclAttribute),
          DeclAttributeVal(attr) {}

    DiagnosticArgument(clang::VersionTuple version)
      : Kind(DiagnosticArgumentKind::VersionTuple),
        VersionVal(version) { }

    DiagnosticArgument(LayoutConstraint L)
      : Kind(DiagnosticArgumentKind::LayoutConstraint), LayoutConstraintVal(L) {
    }
    /// Initializes a diagnostic argument using the underlying type of the
    /// given enum.
    template<
        typename EnumType,
        typename std::enable_if<std::is_enum<EnumType>::value>::type* = nullptr>
    DiagnosticArgument(EnumType value)
      : DiagnosticArgument(
          static_cast<typename std::underlying_type<EnumType>::type>(value)) {}

    DiagnosticArgumentKind getKind() const { return Kind; }

    StringRef getAsString() const {
      assert(Kind == DiagnosticArgumentKind::String);
      return StringVal;
    }

    int getAsInteger() const {
      assert(Kind == DiagnosticArgumentKind::Integer);
      return IntegerVal;
    }

    unsigned getAsUnsigned() const {
      assert(Kind == DiagnosticArgumentKind::Unsigned);
      return UnsignedVal;
    }

    DeclName getAsIdentifier() const {
      assert(Kind == DiagnosticArgumentKind::Identifier);
      return IdentifierVal;
    }

    ObjCSelector getAsObjCSelector() const {
      assert(Kind == DiagnosticArgumentKind::ObjCSelector);
      return ObjCSelectorVal;
    }

    ValueDecl *getAsValueDecl() const {
      assert(Kind == DiagnosticArgumentKind::ValueDecl);
      return TheValueDecl;
    }

    Type getAsType() const {
      assert(Kind == DiagnosticArgumentKind::Type);
      return TypeVal;
    }

    TypeRepr *getAsTypeRepr() const {
      assert(Kind == DiagnosticArgumentKind::TypeRepr);
      return TyR;
    }
    
    PatternKind getAsPatternKind() const {
      assert(Kind == DiagnosticArgumentKind::PatternKind);
      return PatternKindVal;
    }

    StaticSpellingKind getAsStaticSpellingKind() const {
      assert(Kind == DiagnosticArgumentKind::StaticSpellingKind);
      return StaticSpellingKindVal;
    }

    DescriptiveDeclKind getAsDescriptiveDeclKind() const {
      assert(Kind == DiagnosticArgumentKind::DescriptiveDeclKind);
      return DescriptiveDeclKindVal;
    }

    const DeclAttribute *getAsDeclAttribute() const {
      assert(Kind == DiagnosticArgumentKind::DeclAttribute);
      return DeclAttributeVal;
    }

    clang::VersionTuple getAsVersionTuple() const {
      assert(Kind == DiagnosticArgumentKind::VersionTuple);
      return VersionVal;
    }

    LayoutConstraint getAsLayoutConstraint() const {
      assert(Kind == DiagnosticArgumentKind::LayoutConstraint);
      return LayoutConstraintVal;
    }
  };
  
  /// Diagnostic - This is a specific instance of a diagnostic along with all of
  /// the DiagnosticArguments that it requires. 
  class Diagnostic {
  public:
    typedef DiagnosticInfo::FixIt FixIt;

  private:
    DiagID ID;
    SmallVector<DiagnosticArgument, 3> Args;
    SmallVector<CharSourceRange, 2> Ranges;
    SmallVector<FixIt, 2> FixIts;
    SourceLoc Loc;
    const Decl *Decl = nullptr;

  public:
    // All constructors are intentionally implicit.
    template<typename ...ArgTypes>
    Diagnostic(Diag<ArgTypes...> ID,
               typename detail::PassArgument<ArgTypes>::type... VArgs)
      : ID(ID.ID) {
      DiagnosticArgument DiagArgs[] = { 
        DiagnosticArgument(0), std::move(VArgs)... 
      };
      Args.append(DiagArgs + 1, DiagArgs + 1 + sizeof...(VArgs));
    }

    /*implicit*/Diagnostic(DiagID ID, ArrayRef<DiagnosticArgument> Args)
      : ID(ID), Args(Args.begin(), Args.end()) {}
    
    // Accessors.
    DiagID getID() const { return ID; }
    ArrayRef<DiagnosticArgument> getArgs() const { return Args; }
    ArrayRef<CharSourceRange> getRanges() const { return Ranges; }
    ArrayRef<FixIt> getFixIts() const { return FixIts; }
    SourceLoc getLoc() const { return Loc; }
    const class Decl *getDecl() const { return Decl; }

    void setLoc(SourceLoc loc) { Loc = loc; }
    void setDecl(const class Decl *decl) { Decl = decl; }

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
  };
  
  /// \brief Describes an in-flight diagnostic, which is currently active
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
    
    /// \brief Create a new in-flight diagnostic. 
    ///
    /// This constructor is only available to the DiagnosticEngine.
    InFlightDiagnostic(DiagnosticEngine &Engine)
      : Engine(&Engine), IsActive(true) { }
    
    InFlightDiagnostic(const InFlightDiagnostic &) = delete;
    InFlightDiagnostic &operator=(const InFlightDiagnostic &) = delete;
    InFlightDiagnostic &operator=(InFlightDiagnostic &&) = delete;

  public:
    /// \brief Create an active but unattached in-flight diagnostic.
    /// 
    /// The resulting diagnostic can be used as a dummy, accepting the
    /// syntax to add additional information to a diagnostic without
    /// actually emitting a diagnostic.
    InFlightDiagnostic() : Engine(0), IsActive(true) { }
    
    /// \brief Transfer an in-flight diagnostic to a new object, which is
    /// typically used when returning in-flight diagnostics.
    InFlightDiagnostic(InFlightDiagnostic &&Other)
      : Engine(Other.Engine), IsActive(Other.IsActive) {
      Other.IsActive = false;
    }
    
    ~InFlightDiagnostic() {
      if (IsActive)
        flush();
    }
    
    /// \brief Flush the active diagnostic to the diagnostic output engine.
    void flush();

    /// \brief Add a token-based range to the currently-active diagnostic.
    InFlightDiagnostic &highlight(SourceRange R);

    /// \brief Add a character-based range to the currently-active diagnostic.
    InFlightDiagnostic &highlightChars(SourceLoc Start, SourceLoc End);

    /// \brief Add a token-based replacement fix-it to the currently-active
    /// diagnostic.
    InFlightDiagnostic &fixItReplace(SourceRange R, StringRef Str);

    /// \brief Add a character-based replacement fix-it to the currently-active
    /// diagnostic.
    InFlightDiagnostic &fixItReplaceChars(SourceLoc Start, SourceLoc End,
                                          StringRef Str);

    /// \brief Add an insertion fix-it to the currently-active diagnostic.
    InFlightDiagnostic &fixItInsert(SourceLoc L, StringRef Str) {
      return fixItReplaceChars(L, L, Str);
    }

    /// \brief Add an insertion fix-it to the currently-active diagnostic.  The
    /// text is inserted immediately *after* the token specified.
    ///
    InFlightDiagnostic &fixItInsertAfter(SourceLoc L, StringRef);

    /// \brief Add a token-based removal fix-it to the currently-active
    /// diagnostic.
    InFlightDiagnostic &fixItRemove(SourceRange R);
    
    /// \brief Add a character-based removal fix-it to the currently-active
    /// diagnostic.
    InFlightDiagnostic &fixItRemoveChars(SourceLoc Start, SourceLoc End) {
      return fixItReplaceChars(Start, End, {});
    }

    /// \brief Add two replacement fix-it exchanging source ranges to the
    /// currently-active diagnostic.
    InFlightDiagnostic &fixItExchange(SourceRange R1, SourceRange R2);
  };

  /// \brief Class to track, map, and remap diagnostic severity and fatality
  ///
  class DiagnosticState {
  public:
    /// \brief Describes the current behavior to take with a diagnostic
    enum class Behavior : uint8_t {
      Unspecified,
      Ignore,
      Note,
      Warning,
      Error,
      Fatal,
    };

  private:
    /// \brief Whether we should continue to emit diagnostics, even after a
    /// fatal error
    bool showDiagnosticsAfterFatalError = false;

    /// \brief Don't emit any warnings
    bool suppressWarnings = false;

    /// \brief Emit all warnings as errors
    bool warningsAsErrors = false;

    /// \brief Whether a fatal error has occurred
    bool fatalErrorOccurred = false;

    /// \brief Whether any error diagnostics have been emitted.
    bool anyErrorOccurred = false;

    /// \brief Track the previous emitted Behavior, useful for notes
    Behavior previousBehavior = Behavior::Unspecified;

    /// \brief Track settable, per-diagnostic state that we store
    std::vector<Behavior> perDiagnosticBehavior;

  public:
    DiagnosticState();

    /// \brief Figure out the Behavior for the given diagnostic, taking current
    /// state such as fatality into account.
    Behavior determineBehavior(DiagID id);

    bool hadAnyError() const { return anyErrorOccurred; }
    bool hasFatalErrorOccurred() const { return fatalErrorOccurred; }

    void setShowDiagnosticsAfterFatalError(bool val = true) {
      showDiagnosticsAfterFatalError = val;
    }
    bool getShowDiagnosticsAfterFatalError() {
      return showDiagnosticsAfterFatalError;
    }

    /// \brief Whether to skip emitting warnings
    void setSuppressWarnings(bool val) { suppressWarnings = val; }
    bool getSuppressWarnings() const { return suppressWarnings; }

    /// \brief Whether to treat warnings as errors
    void setWarningsAsErrors(bool val) { warningsAsErrors = val; }
    bool getWarningsAsErrors() const { return warningsAsErrors; }

    void resetHadAnyError() {
      anyErrorOccurred = false;
      fatalErrorOccurred = false;
    }

    /// Set per-diagnostic behavior
    void setDiagnosticBehavior(DiagID id, Behavior behavior) {
      perDiagnosticBehavior[(unsigned)id] = behavior;
    }

  private:
    // Make the state movable only
    DiagnosticState(const DiagnosticState &) = delete;
    const DiagnosticState &operator=(const DiagnosticState &) = delete;

    DiagnosticState(DiagnosticState &&) = default;
    DiagnosticState &operator=(DiagnosticState &&) = default;
  };
    
  /// \brief Class responsible for formatting diagnostics and presenting them
  /// to the user.
  class DiagnosticEngine {
    /// \brief The source manager used to interpret source locations and
    /// display diagnostics.
    SourceManager &SourceMgr;

    /// \brief The diagnostic consumer(s) that will be responsible for actually
    /// emitting diagnostics.
    SmallVector<DiagnosticConsumer *, 2> Consumers;

    /// \brief Tracks diagnostic behaviors and state
    DiagnosticState state;

    /// \brief The currently active diagnostic, if there is one.
    Optional<Diagnostic> ActiveDiagnostic;

    /// \brief All diagnostics that have are no longer active but have not yet
    /// been emitted due to an open transaction.
    SmallVector<Diagnostic, 4> TentativeDiagnostics;

    /// \brief The set of declarations for which we have pretty-printed
    /// results that we can point to on the command line.
    llvm::DenseMap<const Decl *, SourceLoc> PrettyPrintedDeclarations;

    /// \brief The number of open diagnostic transactions. Diagnostics are only
    /// emitted once all transactions have closed.
    unsigned TransactionCount = 0;

    friend class InFlightDiagnostic;
    friend class DiagnosticTransaction;
    
  public:
    explicit DiagnosticEngine(SourceManager &SourceMgr)
      : SourceMgr(SourceMgr), ActiveDiagnostic() {
    }

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

    /// \brief Whether to skip emitting warnings
    void setSuppressWarnings(bool val) { state.setSuppressWarnings(val); }
    bool getSuppressWarnings() const {
      return state.getSuppressWarnings();
    }

    /// \brief Whether to treat warnings as errors
    void setWarningsAsErrors(bool val) { state.setWarningsAsErrors(val); }
    bool getWarningsAsErrors() const {
      return state.getWarningsAsErrors();
    }

    void ignoreDiagnostic(DiagID id) {
      state.setDiagnosticBehavior(id, DiagnosticState::Behavior::Ignore);
    }

    void resetHadAnyError() {
      state.resetHadAnyError();
    }

    /// \brief Add an additional DiagnosticConsumer to receive diagnostics.
    void addConsumer(DiagnosticConsumer &Consumer) {
      Consumers.push_back(&Consumer);
    }

    /// \brief Remove and return all \c DiagnosticConsumers.
    std::vector<DiagnosticConsumer *> takeConsumers() {
      auto Result = std::vector<DiagnosticConsumer*>(Consumers.begin(),
                                                     Consumers.end());
      Consumers.clear();
      return Result;
    }

    /// \brief Emit a diagnostic using a preformatted array of diagnostic
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
      assert(!ActiveDiagnostic && "Already have an active diagnostic");
      ActiveDiagnostic = Diagnostic(ID, Args);
      ActiveDiagnostic->setLoc(Loc);
      return InFlightDiagnostic(*this);
    }

    /// \brief Emit a diagnostic using a preformatted array of diagnostic
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
      return diagnose(Loc.getBaseNameLoc(), ID, Args);
    }

    /// \brief Emit an already-constructed diagnostic at the given location.
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
    
    /// \brief Emit a diagnostic with the given set of diagnostic arguments.
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
      assert(!ActiveDiagnostic && "Already have an active diagnostic");
      ActiveDiagnostic = Diagnostic(ID, std::move(Args)...);
      ActiveDiagnostic->setLoc(Loc);
      return InFlightDiagnostic(*this);
    }

    /// \brief Emit a diagnostic with the given set of diagnostic arguments.
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
      assert(!ActiveDiagnostic && "Already have an active diagnostic");
      ActiveDiagnostic = Diagnostic(ID, std::move(Args)...);
      ActiveDiagnostic->setLoc(Loc.getBaseNameLoc());
      return InFlightDiagnostic(*this);
    }

    /// \brief Emit a diagnostic using a preformatted array of diagnostic
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
      assert(!ActiveDiagnostic && "Already have an active diagnostic");
      ActiveDiagnostic = Diagnostic(id, args);
      ActiveDiagnostic->setDecl(decl);
      return InFlightDiagnostic(*this);
    }

    /// \brief Emit an already-constructed diagnostic referencing the given
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

    /// \brief Emit a diagnostic with the given set of diagnostic arguments.
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
      ActiveDiagnostic = Diagnostic(id, std::move(args)...);
      ActiveDiagnostic->setDecl(decl);
      return InFlightDiagnostic(*this);
    }

    /// \returns true if diagnostic is marked with PointsToFirstBadToken
    /// option.
    bool isDiagnosticPointsToFirstBadToken(DiagID id) const;

  private:
    /// \brief Flush the active diagnostic.
    void flushActiveDiagnostic();
    
    /// \brief Retrieve the active diagnostic.
    Diagnostic &getActiveDiagnostic() { return *ActiveDiagnostic; }

    /// \brief Send \c diag to all diagnostic consumers.
    void emitDiagnostic(const Diagnostic &diag);

    /// \brief Send all tentative diagnostics to all diagnostic consumers and
    /// delete them.
    void emitTentativeDiagnostics();
  };

  /// \brief Represents a diagnostic transaction. While a transaction is
  /// open, all recorded diagnostics are saved until the transaction commits,
  /// at which point they are emitted. If the transaction is instead aborted,
  /// the diagnostics are erased. Transactions may be nested but must be closed
  /// in LIFO order. An open transaction is implicitly committed upon
  /// destruction.
  class DiagnosticTransaction {
    DiagnosticEngine &Engine;

    /// \brief How many tentative diagnostics there were when the transaction
    /// was opened.
    unsigned PrevDiagnostics;

    /// \brief How many other transactions were open when this transaction was
    /// opened.
    unsigned Depth;

    /// \brief Whether this transaction is currently open.
    bool IsOpen = true;

  public:
    explicit DiagnosticTransaction(DiagnosticEngine &engine)
      : Engine(engine),
        PrevDiagnostics(Engine.TentativeDiagnostics.size()),
        Depth(Engine.TransactionCount),
        IsOpen(true)
    {
      assert(!Engine.ActiveDiagnostic);
      Engine.TransactionCount++;
    }

    ~DiagnosticTransaction() {
      if (IsOpen) {
        commit();
      }
    }

    /// \brief Abort and close this transaction and erase all diagnostics
    /// record while it was open.
    void abort() {
      close();
      Engine.TentativeDiagnostics.erase(
        Engine.TentativeDiagnostics.begin() + PrevDiagnostics,
        Engine.TentativeDiagnostics.end());
    }

    /// \brief Commit and close this transaction. If this is the top-level
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
} // end namespace swift

#endif
