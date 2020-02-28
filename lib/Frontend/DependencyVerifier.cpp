//===--- DependencyVerifier.cpp - Dependency Verifier ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Implements a verifier for dependencies registered against the
//  ReferencedNameTracker in a SourceFile.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Parse/Lexer.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/Support/FormatVariadic.h"

using namespace swift;

namespace {

/// An \c Expectation represents a user-provided expectation for a particular
/// dependency entry. An expectation is usually written in-line in a comment
/// attached near the relevant declaration and takes one of the following forms:
///
/// // expected-provides {{ProvidedName}}
/// // expected-private-member {{some.User.member}}
///
/// An expectation contains additional information about the expectation
/// \c Kind, which matches one of the few kinds of dependency entry that are
/// currently representable in the dependency graph, and an expectation
/// \c Flavor which must either be \c private or \c cascading.
///
/// The supported set of expectations is given by the following matrix:
///
#define EXPECTATION_MATRIX                                                     \
  MATRIX_ENTRY("expected-no-dependency", None, Negative)                       \
  MATRIX_ENTRY("expected-provides", None, Provides)                            \
  MATRIX_ENTRY("expected-private-superclass", Private, Superclass)             \
  MATRIX_ENTRY("expected-cascading-superclass", Cascading, Superclass)         \
  MATRIX_ENTRY("expected-private-conformance", Private, Conformance)           \
  MATRIX_ENTRY("expected-cascading-conformance", Cascading, Conformance)       \
  MATRIX_ENTRY("expected-private-member", Private, Member)                     \
  MATRIX_ENTRY("expected-cascading-member", Cascading, Member)                 \
  MATRIX_ENTRY("expected-private-dynamic-member", Private, DynamicMember)      \
  MATRIX_ENTRY("expected-cascading-dynamic-member", Cascading, DynamicMember)
struct Expectation {
public:
  enum class Kind : uint8_t {
    Negative,
    Provides,
    Member,
    PotentialMember,
    Superclass = PotentialMember,
    Conformance = PotentialMember,
    DynamicMember,
  };

  enum class Flavor : uint8_t {
    None,
    Private,
    Cascading,
  };

  /// The full range of the "expected-foo {{}}".
  const char *ExpectedStart, *ExpectedEnd = nullptr;

  /// Additional information about the expectation.
  std::pair<Expectation::Kind, Expectation::Flavor> Info;

  /// The raw input buffer for the message text, the part in the {{...}}
  StringRef MessageRange;

public:
  Expectation(const char *ExpectedStart, Expectation::Kind k,
              Expectation::Flavor f)
      : ExpectedStart(ExpectedStart), Info{k, f} {}

  bool isCascading() const {
    return Info.second == Expectation::Flavor::Cascading;
  }
};

/// An \c Obligation represents a compiler-provided entry in the set of
/// dependencies for a given source file. Similar to an \c Expectation an \c
/// Obligation contains a name, information about its kind and flavor, and an
/// extra source location that can be used to guide where diagnostics are
/// emitted. Unlike an \c Obligation, it provides an extra piece of state that
/// represents the obligation's "fulfillment status".
///
/// All \c Obligations begin in the \c Active state. Once an obligation has been
/// paired with a matching \c Expectation, the obligation may then transition to
/// either \c Fulfilled if it has been satisfied completely, or \c Failed
/// otherwise. The verifier turns all unfulfilled obligations into errors.
struct Obligation {
  /// The state of an \c Obligation
  enum class State : uint8_t {
    /// The \c Obligation is active and has not been paired with a corresponding
    /// \c Expectation.
    Active,
    /// The \c Obligation is fulfilled.
    Fulfilled,
    /// The \c Obligation was matched against an \c Expectation, but that
    /// expectation could not
    /// fulfill the obligation because additional requirements did not pass.
    Failed,
  };

  /// A token returned when an \c Obligation is fulfilled or failed. An \c
  /// Obligation is the only type that may construct fulfillment tokens.
  struct FullfillmentToken {
    friend Obligation;

  private:
    FullfillmentToken() = default;
  };

  /// An \c Obligation::Key is a reduced set of the common data contained in an
  /// \c Obligation and an \c Expectation.
  ///
  /// This provides a way to use a value of either type to index into an  \c
  /// ObligationMap.
  struct Key {
    StringRef Name;
    Expectation::Kind Kind;

  public:
    Key() = delete;

  public:
    static Key forNegative(StringRef name) {
      return Key{name, Expectation::Kind::Negative};
    }

    static Key forProvides(StringRef name) {
      return Key{name, Expectation::Kind::Provides};
    }

    static Key forDynamicMember(StringRef name) {
      return Key{name, Expectation::Kind::DynamicMember};
    }

    static Key forPotentialMember(StringRef name) {
      return Key{name, Expectation::Kind::PotentialMember};
    }

    static Key forMember(StringRef name) {
      return Key{name, Expectation::Kind::Member};
    }

    static Key forExpectation(const Expectation &E) {
      switch (E.Info.first) {
      case Expectation::Kind::Negative:
        return Key::forNegative(E.MessageRange);
      case Expectation::Kind::Provides:
        return Key::forProvides(E.MessageRange);
      case Expectation::Kind::Member:
        return Key::forMember(E.MessageRange);
      case Expectation::Kind::PotentialMember:
        return Key::forPotentialMember(E.MessageRange);
      case Expectation::Kind::DynamicMember:
        return Key::forDynamicMember(E.MessageRange);
      }
    }

  public:
    struct Info {
      static inline Obligation::Key getEmptyKey() {
        return Obligation::Key{llvm::DenseMapInfo<StringRef>::getEmptyKey(),
                               static_cast<Expectation::Kind>(~0)};
      }
      static inline Obligation::Key getTombstoneKey() {
        return Obligation::Key{llvm::DenseMapInfo<StringRef>::getTombstoneKey(),
                               static_cast<Expectation::Kind>(~0U - 1)};
      }
      static unsigned getHashValue(const Obligation::Key &Val) {
        return llvm::hash_combine(Val.Name, Val.Kind);
      }
      static bool isEqual(const Obligation::Key &LHS,
                          const Obligation::Key &RHS) {
        return LHS.Name == RHS.Name && LHS.Kind == RHS.Kind;
      }
    };
  };

private:
  DeclBaseName name;
  std::pair<Expectation::Kind, Expectation::Flavor> info;
  SourceLoc contextLoc;
  State state;

public:
  Obligation(DeclBaseName name, Expectation::Kind k, Expectation::Flavor f,
             SourceLoc loc)
      : name(name), info{k, f}, contextLoc{loc}, state(State::Active) {
    assert(k != Expectation::Kind::Negative &&
           "Cannot form negative obligation!");
  }

  Expectation::Kind getKind() const { return info.first; }
  DeclBaseName getName() const { return name; }
  bool getCascades() const {
    return info.second == Expectation::Flavor::Cascading;
  }
  StringRef describeCascade() const {
    switch (info.second) {
    case Expectation::Flavor::None:
      llvm_unreachable("Cannot describe obligation with no cascade info");
    case Expectation::Flavor::Private:
      return "non-cascading";
    case Expectation::Flavor::Cascading:
      return "cascading";
    }
  }

public:
  SourceLoc getLocation() const { return contextLoc; }

public:
  bool isActive() const { return state == State::Active; }
  FullfillmentToken fullfill() {
    assert(state == State::Active &&
           "Cannot fulfill an obligation more than once!");
    state = State::Fulfilled;
    return FullfillmentToken{};
  }
  FullfillmentToken fail() {
    assert(state == State::Active &&
           "Cannot fail an obligation more than once!");
    state = State::Failed;
    return FullfillmentToken{};
  }
};

/// The \c DependencyVerifier implements routines to verify a set of \c
/// Expectations in a given source file meet and match a set of \c Obligations
/// in the referenced name trackers associated with that file.
class DependencyVerifier {
  SourceManager &SM;
  std::vector<llvm::SMDiagnostic> Errors = {};
  llvm::BumpPtrAllocator StringAllocator;

public:
  explicit DependencyVerifier(SourceManager &SM) : SM(SM) {}

  bool verifyFile(const SourceFile *SF);

public:
  using ObligationMap = llvm::MapVector<
      Obligation::Key, Obligation,
      llvm::DenseMap<Obligation::Key, unsigned, Obligation::Key::Info>>;
  using NegativeExpectationMap = llvm::StringMap<Expectation>;

private:
  bool parseExpectations(const SourceFile *SF,
                         std::vector<Expectation> &Expectations);
  bool constructObligations(const SourceFile *SF, ObligationMap &map);

  bool verifyObligations(const SourceFile *SF,
                         const std::vector<Expectation> &Exs,
                         ObligationMap &Obs,
                         NegativeExpectationMap &NegativeExpectations);

  bool verifyNegativeExpectations(ObligationMap &Obs,
                                  NegativeExpectationMap &Negs);

  bool diagnoseUnfulfilledObligations(const SourceFile *SF, ObligationMap &OM);

private:
  /// Given an \c ObligationMap and an \c Expectation, attempt to identify a
  /// corresponding active \c Obligation and verify it. If there is a matching
  /// obligation, the \p fulfill callback is given the obligation. Otherwise \p
  /// fail is called with the unmatched expectation value.
  void matchExpectationOrFail(
      ObligationMap &OM, const Expectation &expectation,
      llvm::function_ref<Obligation::FullfillmentToken(Obligation &)> fulfill,
      llvm::function_ref<void(const Expectation &)> fail) {
    auto entry = OM.find(Obligation::Key::forExpectation(expectation));
    if (entry == OM.end()) {
      return fail(expectation);
    }

    fulfill(entry->second);
  }

  /// For each active \c Obligation, call the provided callback with its
  /// relevant name data and the Obligation itself.
  void
  forEachActiveObligation(ObligationMap &OM,
                          llvm::function_ref<void(StringRef, Obligation &)> f) {
    for (auto &p : OM) {
      if (p.second.isActive())
        f(p.first.Name, p.second);
    }
  }

private:
  StringRef bumpAllocateData(const char *Data, size_t Length) {
    char *Ptr = StringAllocator.Allocate<char>(Length + 1);
    memcpy(Ptr, Data, Length);
    *(Ptr + Length) = 0;
    return StringRef(Ptr, Length);
  }

  StringRef allocateString(StringRef S) {
    return bumpAllocateData(S.data(), S.size());
  }

private:
  template <typename... Ts>
  inline auto addFormattedDiagnostic(const Expectation &dep, const char *Fmt,
                                     Ts &&... Vals) {
    return addFormattedDiagnostic(dep.MessageRange.begin(), Fmt,
                                  std::forward<Ts>(Vals)...);
  }

  template <typename... Ts>
  inline auto addFormattedDiagnostic(const char *Loc, const char *Fmt,
                                     Ts &&... Vals) {
    auto loc = SourceLoc(llvm::SMLoc::getFromPointer(Loc));
    auto diag =
        SM.GetMessage(loc, llvm::SourceMgr::DK_Error,
                      llvm::formatv(Fmt, std::forward<Ts>(Vals)...), {}, {});
    Errors.push_back(diag);
  }

  void addError(const char *Loc, const Twine &Msg,
                ArrayRef<llvm::SMFixIt> FixIts = {}) {
    auto loc = SourceLoc(llvm::SMLoc::getFromPointer(Loc));
    auto diag = SM.GetMessage(loc, llvm::SourceMgr::DK_Error, Msg, {}, FixIts);
    Errors.push_back(diag);
  };
};
} // end anonymous namespace

bool DependencyVerifier::parseExpectations(
    const SourceFile *SF, std::vector<Expectation> &Expectations) {
  const auto MaybeBufferID = SF->getBufferID();
  if (!MaybeBufferID) {
    llvm::errs() << "source file has no buffer: " << SF->getFilename();
    return true;
  }

  const auto BufferID = MaybeBufferID.getValue();
  CharSourceRange EntireRange = SM.getRangeForBuffer(BufferID);
  StringRef InputFile = SM.extractText(EntireRange);

  for (size_t Match = InputFile.find("expected-"); Match != StringRef::npos;
       Match = InputFile.find("expected-", Match + 1)) {
    StringRef MatchStart = InputFile.substr(Match);
    const char *DiagnosticLoc = MatchStart.data();

    Expectation::Kind ExpectedKind;
    Expectation::Flavor ExpectedFlavor;
    {
#define MATRIX_ENTRY(STRING, FLAVOR, KIND)                                     \
  .StartsWith(STRING, [&]() {                                                  \
    ExpectedKind = Expectation::Kind::KIND;                                    \
    ExpectedFlavor = Expectation::Flavor::FLAVOR;                              \
    MatchStart = MatchStart.substr(strlen(STRING));                            \
  })

      // clang-format off
        llvm::StringSwitch<llvm::function_ref<void(void)>>{MatchStart}
          EXPECTATION_MATRIX
          .Default([]() {})();
      // clang-format on
#undef MATRIX_ENTRY
    }

    // Skip any whitespace before the {{.
    MatchStart = MatchStart.substr(MatchStart.find_first_not_of(" \t"));

    size_t TextStartIdx = MatchStart.find("{{");
    if (TextStartIdx == StringRef::npos) {
      addError(MatchStart.data(), "expected {{ in expectation");
      continue;
    }

    Expectation Expected(DiagnosticLoc, ExpectedKind, ExpectedFlavor);

    size_t End = MatchStart.find("}}");
    if (End == StringRef::npos) {
      addError(MatchStart.data(),
               "didn't find '}}' to match '{{' in expectation");
      continue;
    }

    llvm::SmallString<256> Buf;
    Expected.MessageRange = MatchStart.slice(2, End);

    // Check if the next expectation should be in the same line.
    StringRef AfterEnd = MatchStart.substr(End + strlen("}}"));
    AfterEnd = AfterEnd.substr(AfterEnd.find_first_not_of(" \t"));
    Expected.ExpectedEnd = AfterEnd.data();

    // Strip out the trailing whitespace.
    while (isspace(Expected.ExpectedEnd[-1]))
      --Expected.ExpectedEnd;

    Expectations.push_back(Expected);
  }
  return false;
}

bool DependencyVerifier::constructObligations(const SourceFile *SF,
                                              ObligationMap &Obligations) {
  auto *tracker = SF->getReferencedNameTracker();
  assert(tracker && "Constructed source file without referenced name tracker!");

  // Top-level names match via unqualified references.
  for (const auto &p : tracker->getTopLevelNames()) {
    auto str = p.getFirst().userFacingName();
    Obligations.insert({Obligation::Key::forProvides(str),
                        {p.getFirst(), Expectation::Kind::Provides,
                         Expectation::Flavor::None, SourceLoc()}});
  }

  // Dynamic member names match via unqualified references.
  for (const auto &p : tracker->getDynamicLookupNames()) {
    auto str = p.getFirst().userFacingName();
    Obligations.insert({Obligation::Key::forDynamicMember(str),
                        {p.getFirst(), Expectation::Kind::DynamicMember,
                         p.getSecond() ? Expectation::Flavor::Cascading
                                       : Expectation::Flavor::Private,
                         SourceLoc()}});
  }

  llvm::SmallString<128> Str;
  llvm::raw_svector_ostream OS(Str);

  PrintOptions Options = PrintOptions::printEverything();
  Options.FullyQualifiedTypes = true;
  for (const auto &p : tracker->getUsedMembers()) {
    Str.clear();
    StreamPrinter printer{OS};

    auto *nominalDeclContext = p.getFirst().first;
    nominalDeclContext->getDeclaredType().print(printer, Options);

    // Potential members match via qualified reference to the subject.
    auto memberName = p.getFirst().second;
    if (memberName.empty()) {
      auto key = allocateString(OS.str());
      Obligations.insert(
          {Obligation::Key::forPotentialMember(key),
           {p.getFirst().second, Expectation::Kind::PotentialMember,
            p.getSecond() ? Expectation::Flavor::Cascading
                          : Expectation::Flavor::Private,
            SourceLoc()}});
    } else {
      // Member constraints match via fully-qualified name.
      OS << "." << memberName.userFacingName();
      auto key = allocateString(OS.str());
      Obligations.insert({Obligation::Key::forMember(key),
                          {p.getFirst().second, Expectation::Kind::Member,
                           p.getSecond() ? Expectation::Flavor::Cascading
                                         : Expectation::Flavor::Private,
                           nominalDeclContext->getLoc()}});
    }
  }

  return false;
}

bool DependencyVerifier::verifyObligations(
    const SourceFile *SF, const std::vector<Expectation> &ExpectedDependencies,
    ObligationMap &OM, llvm::StringMap<Expectation> &NegativeExpectations) {
  auto *tracker = SF->getReferencedNameTracker();
  assert(tracker && "Constructed source file without referenced name tracker!");

  for (auto &expectation : ExpectedDependencies) {
    const bool wantsCascade = expectation.isCascading();
    switch (expectation.Info.first) {
    case Expectation::Kind::Negative:
      // We'll verify negative expectations separately.
      NegativeExpectations.insert({expectation.MessageRange, expectation});
      break;
    case Expectation::Kind::Member:
      matchExpectationOrFail(
          OM, expectation,
          [&](Obligation &p) {
            const auto haveCascade = p.getCascades();
            if (haveCascade != wantsCascade) {
              addFormattedDiagnostic(
                  expectation,
                  "expected {0} dependency; found {1} dependency instead",
                  wantsCascade ? "cascading" : "non-cascading",
                  haveCascade ? "cascading" : "non-cascading");
              return p.fail();
            }

            return p.fullfill();
          },
          [this](const Expectation &e) {
            addFormattedDiagnostic(
                e, "expected member dependency does not exist: {0}",
                e.MessageRange);
          });
      break;
    case Expectation::Kind::PotentialMember:
      matchExpectationOrFail(
          OM, expectation,
          [&](Obligation &p) {
            assert(p.getName().empty());
            const auto haveCascade = p.getCascades();
            if (haveCascade != wantsCascade) {
              addFormattedDiagnostic(
                  expectation,
                  "expected {0} potential member dependency; found {1} "
                  "potential member dependency instead",
                  wantsCascade ? "cascading" : "non-cascading",
                  haveCascade ? "cascading" : "non-cascading");
              return p.fail();
            }

            return p.fullfill();
          },
          [this](const Expectation &e) {
            addFormattedDiagnostic(
                e, "expected superclass dependency does not exist: {0}",
                e.MessageRange);
          });
      break;
    case Expectation::Kind::Provides:
      matchExpectationOrFail(
          OM, expectation, [](Obligation &O) { return O.fullfill(); },
          [this](const Expectation &e) {
            addFormattedDiagnostic(
                e, "expected provided dependency does not exist: {0}",
                e.MessageRange);
          });
      break;
    case Expectation::Kind::DynamicMember:
      matchExpectationOrFail(
          OM, expectation, [](Obligation &O) { return O.fullfill(); },
          [this](const Expectation &e) {
            addFormattedDiagnostic(
                e, "expected dynamic member dependency does not exist: {0}",
                e.MessageRange);
          });
      break;
    }
  }

  return false;
}

bool DependencyVerifier::verifyNegativeExpectations(
    ObligationMap &Obligations, NegativeExpectationMap &NegativeExpectations) {
  forEachActiveObligation(Obligations, [&](StringRef key, Obligation &p) {
    auto entry = NegativeExpectations.find(key);
    if (entry == NegativeExpectations.end()) {
      return;
    }

    auto &expectation = entry->second;
    addFormattedDiagnostic(expectation, "unexpected dependency exists: {0}",
                           expectation.MessageRange);
    p.fail();
  });
  return false;
}

bool DependencyVerifier::diagnoseUnfulfilledObligations(
    const SourceFile *SF, ObligationMap &Obligations) {
  CharSourceRange EntireRange = SM.getRangeForBuffer(*SF->getBufferID());
  StringRef InputFile = SM.extractText(EntireRange);
  forEachActiveObligation(Obligations, [&](StringRef key, Obligation &p) {
    const char *Loc = NULL;
    if (p.getLocation().isValid()) {
      // If we have a recommended location, try to use it.
      Loc = (const char *)p.getLocation().getOpaquePointerValue();
    } else {
      // Otherwise diagnose the input buffer.
      // HACK: Diagnosing the end of the buffer will print a carat pointing
      // at the file path, but not print any of the buffer's contents, which
      // might be misleading.
      Loc = InputFile.end();
    }

    switch (p.getKind()) {
    case Expectation::Kind::Negative:
      llvm_unreachable("Obligations may not be negative; only Expectations!");
    case Expectation::Kind::Member:
      addFormattedDiagnostic(Loc, "unexpected {0} dependency: {1}",
                             p.describeCascade(), key);
      break;
    case Expectation::Kind::DynamicMember:
      addFormattedDiagnostic(Loc,
                             "unexpected {0} dynamic member dependency: {1}",
                             p.describeCascade(), p.getName().userFacingName());
      break;
    case Expectation::Kind::PotentialMember:
      addFormattedDiagnostic(Loc,
                             "unexpected {0} potential member dependency: {1}",
                             p.describeCascade(), key);
      break;
    case Expectation::Kind::Provides:
      addFormattedDiagnostic(Loc, "unexpected provided entity: {0}",
                             p.getName().userFacingName());
      break;
    }
  });

  return false;
}

bool DependencyVerifier::verifyFile(const SourceFile *SF) {
  std::vector<Expectation> ExpectedDependencies;
  if (parseExpectations(SF, ExpectedDependencies)) {
    return true;
  }

  ObligationMap Obligations;
  if (constructObligations(SF, Obligations)) {
    return true;
  }

  NegativeExpectationMap Negatives;
  if (verifyObligations(SF, ExpectedDependencies, Obligations, Negatives)) {
    return true;
  }

  if (verifyNegativeExpectations(Obligations, Negatives)) {
    return true;
  }

  if (diagnoseUnfulfilledObligations(SF, Obligations)) {
    return true;
  }

  // Sort the diagnostics by location so we get a stable ordering.
  std::sort(Errors.begin(), Errors.end(),
            [&](const llvm::SMDiagnostic &lhs,
                const llvm::SMDiagnostic &rhs) -> bool {
              return lhs.getLoc().getPointer() < rhs.getLoc().getPointer();
            });

  for (auto Err : Errors)
    SM.getLLVMSourceMgr().PrintMessage(llvm::errs(), Err);

  return !Errors.empty();
}

//===----------------------------------------------------------------------===//
// Main entrypoints
//===----------------------------------------------------------------------===//

bool swift::verifyDependencies(SourceManager &SM, ArrayRef<FileUnit *> SFs) {
  bool HadError = false;
  DependencyVerifier Verifier{SM};
  for (const auto *FU : SFs) {
    if (const auto *SF = dyn_cast<SourceFile>(FU))
      HadError |= Verifier.verifyFile(SF);
  }
  return HadError;
}

bool swift::verifyDependencies(SourceManager &SM, ArrayRef<SourceFile *> SFs) {
  bool HadError = false;
  DependencyVerifier Verifier{SM};
  for (const auto *SF : SFs) {
    HadError |= Verifier.verifyFile(SF);
  }
  return HadError;
}

#undef EXPECTATION_MATRIX
