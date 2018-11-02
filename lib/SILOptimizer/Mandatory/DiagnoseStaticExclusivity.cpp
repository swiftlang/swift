//===--- DiagnoseStaticExclusivity.cpp - Find violations of exclusivity ---===//
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
// This file implements a diagnostic pass that finds violations of the
// "Law of Exclusivity" at compile time. The Law of Exclusivity requires
// that the access duration of any access to an address not overlap
// with an access to the same address unless both accesses are reads.
//
// This pass relies on 'begin_access' and 'end_access' SIL instruction
// markers inserted during SILGen to determine when an access to an address
// begins and ends. It models the in-progress accesses with a map from
// storage locations to the counts of read and write-like accesses in progress
// for that location.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "static-exclusivity"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/AccessSummaryAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

using namespace swift;

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(ASTContext &Context, SourceLoc loc,
                                   Diag<T...> diag, U &&... args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

namespace {

enum class RecordedAccessKind {
  /// The access was for a 'begin_access' instruction in the current function
  /// being checked.
  BeginInstruction,

  /// The access was inside noescape closure that we either
  /// passed to function or called directly. It results from applying the
  /// the summary of the closure to the closure's captures.
  NoescapeClosureCapture
};

/// Records an access to an address and the single subpath of projections
/// that was performed on the address, if such a single subpath exists.
class RecordedAccess {
private:
  RecordedAccessKind RecordKind;

  union {
   BeginAccessInst *Inst;
    struct {
      SILAccessKind ClosureAccessKind;
      SILLocation ClosureAccessLoc;
    };
  };

  const IndexTrieNode *SubPath;
public:
  RecordedAccess(BeginAccessInst *BAI, const IndexTrieNode *SubPath) :
      RecordKind(RecordedAccessKind::BeginInstruction), Inst(BAI),
      SubPath(SubPath) { }

  RecordedAccess(SILAccessKind ClosureAccessKind,
                 SILLocation ClosureAccessLoc, const IndexTrieNode *SubPath) :
      RecordKind(RecordedAccessKind::NoescapeClosureCapture),
      ClosureAccessKind(ClosureAccessKind), ClosureAccessLoc(ClosureAccessLoc),
      SubPath(SubPath) { }

  RecordedAccessKind getRecordKind() const {
    return RecordKind;
  }

  BeginAccessInst *getInstruction() const {
    assert(RecordKind == RecordedAccessKind::BeginInstruction);
    return Inst;
  }

  SILAccessKind getAccessKind() const {
    switch (RecordKind) {
      case RecordedAccessKind::BeginInstruction:
        return Inst->getAccessKind();
      case RecordedAccessKind::NoescapeClosureCapture:
        return ClosureAccessKind;
    };
  }

  SILLocation getAccessLoc() const {
    switch (RecordKind) {
      case RecordedAccessKind::BeginInstruction:
        return Inst->getLoc();
      case RecordedAccessKind::NoescapeClosureCapture:
        return ClosureAccessLoc;
    };
  }

  const IndexTrieNode *getSubPath() const {
    return SubPath;
  }
};


/// Records the in-progress accesses to a given sub path.
class SubAccessInfo {
public:
  SubAccessInfo(const IndexTrieNode *P) : Path(P) {}

  const IndexTrieNode *Path;

  /// The number of in-progress 'read' accesses (that is 'begin_access [read]'
  /// instructions that have not yet had the corresponding 'end_access').
  unsigned Reads = 0;

  /// The number of in-progress write-like accesses.
  unsigned NonReads = 0;

  /// The instruction that began the first in-progress access to the storage
  /// location. Used for diagnostic purposes.
  Optional<RecordedAccess> FirstAccess = None;

public:
  /// Increment the count for given access.
  void beginAccess(BeginAccessInst *BAI, const IndexTrieNode *SubPath) {
    if (!FirstAccess) {
      assert(Reads == 0 && NonReads == 0);
      FirstAccess = RecordedAccess(BAI, SubPath);
    }

    if (BAI->getAccessKind() == SILAccessKind::Read)
      Reads++;
    else
      NonReads++;
  }

  /// Decrement the count for given access.
  void endAccess(EndAccessInst *EAI) {
    if (EAI->getBeginAccess()->getAccessKind() == SILAccessKind::Read)
      Reads--;
    else
      NonReads--;

    // If all open accesses are now ended, forget the location of the
    // first access.
    if (Reads == 0 && NonReads == 0)
      FirstAccess = None;
  }

  /// Returns true when there are any accesses to this location in progress.
  bool hasAccessesInProgress() const { return Reads > 0 || NonReads > 0; }

  /// Returns true when there must have already been a conflict diagnosed
  /// for an in-progress access. Used to suppress multiple diagnostics for
  /// the same underlying access violation.
  bool alreadyHadConflict() const {
    return (NonReads > 0 && Reads > 0) || (NonReads > 1);
  }

  // Returns true when beginning an access of the given Kind can
  // result in a conflict with a previous access.
  bool canConflictWithAccessOfKind(SILAccessKind Kind) const {
    if (Kind == SILAccessKind::Read) {
      // A read conflicts with any non-read accesses.
      return NonReads > 0;
    }

    // A non-read access conflicts with any other access.
    return NonReads > 0 || Reads > 0;
  }

  bool conflictsWithAccess(SILAccessKind Kind,
                           const IndexTrieNode *SubPath) const {
    if (!canConflictWithAccessOfKind(Kind))
      return false;

    return pathsConflict(Path, SubPath);
  }

  /// Returns true when the two subpaths access overlapping memory.
  bool pathsConflict(const IndexTrieNode *Path1,
                     const IndexTrieNode *Path2) const {
    return Path1->isPrefixOf(Path2) || Path2->isPrefixOf(Path1);
  }
};

/// Models the in-progress accesses for an address on which access has begun
/// with a begin_access instruction. For a given address, tracks the
/// count and kinds of accesses as well as the subpaths (i.e., projections) that
/// were accessed.
class AccessInfo {
  using SubAccessVector = SmallVector<SubAccessInfo, 4>;

  SubAccessVector SubAccesses;

  /// Returns the SubAccess info for accessing at the given SubPath.
  SubAccessInfo &findOrCreateSubAccessInfo(const IndexTrieNode *SubPath) {
    for (auto &Info : SubAccesses) {
      if (Info.Path == SubPath)
        return Info;
    }

    SubAccesses.emplace_back(SubPath);
    return SubAccesses.back();
  }

  SubAccessVector::const_iterator
  findFirstSubPathWithConflict(SILAccessKind OtherKind,
                               const IndexTrieNode *OtherSubPath) const {
    // Note this iteration requires deterministic ordering for repeatable
    // diagnostics.
    for (auto I = SubAccesses.begin(), E = SubAccesses.end(); I != E; ++I) {
      const SubAccessInfo &Access = *I;
      if (Access.conflictsWithAccess(OtherKind, OtherSubPath))
        return I;
    }

    return SubAccesses.end();
  }

public:
  // Returns the previous access when beginning an access of the given Kind will
  // result in a conflict with a previous access.
  Optional<RecordedAccess>
  conflictsWithAccess(SILAccessKind Kind, const IndexTrieNode *SubPath) const {
    auto I = findFirstSubPathWithConflict(Kind, SubPath);
    if (I == SubAccesses.end())
      return None;

    return I->FirstAccess;
  }

  /// Returns true if any subpath of has already had a conflict.
  bool alreadyHadConflict() const {
    for (const auto &SubAccess : SubAccesses) {
      if (SubAccess.alreadyHadConflict())
        return true;
    }
    return false;
  }

  /// Returns true when there are any accesses to this location in progress.
  bool hasAccessesInProgress() const {
    for (const auto &SubAccess : SubAccesses) {
      if (SubAccess.hasAccessesInProgress())
        return true;
    }
    return false;
  }

  /// Increment the count for given access.
  void beginAccess(BeginAccessInst *BAI, const IndexTrieNode *SubPath) {
    SubAccessInfo &SubAccess = findOrCreateSubAccessInfo(SubPath);
    SubAccess.beginAccess(BAI, SubPath);
  }

  /// Decrement the count for given access.
  void endAccess(EndAccessInst *EAI, const IndexTrieNode *SubPath) {
    SubAccessInfo &SubAccess = findOrCreateSubAccessInfo(SubPath);
    SubAccess.endAccess(EAI);
  }
};

/// Indicates whether a 'begin_access' requires exclusive access
/// or allows shared access. This needs to be kept in sync with
/// diag::exclusivity_access_required, exclusivity_access_required_swift3,
/// and diag::exclusivity_conflicting_access.
enum class ExclusiveOrShared_t : unsigned {
  ExclusiveAccess = 0,
  SharedAccess = 1
};


/// Tracks the in-progress accesses on per-storage-location basis.
using StorageMap = llvm::SmallDenseMap<AccessedStorage, AccessInfo, 4>;

/// Represents two accesses that conflict and their underlying storage.
struct ConflictingAccess {
private:

  /// If true, always diagnose this conflict as a warning. This is useful for
  /// staging in fixes for false negatives without affecting source
  /// compatibility.
  bool AlwaysDiagnoseAsWarning = false;
public:
  /// Create a conflict for two begin_access instructions in the same function.
  ConflictingAccess(const AccessedStorage &Storage, const RecordedAccess &First,
                    const RecordedAccess &Second)
      : Storage(Storage), FirstAccess(First), SecondAccess(Second) {}

  const AccessedStorage Storage;
  const RecordedAccess FirstAccess;
  const RecordedAccess SecondAccess;

  bool getAlwaysDiagnoseAsWarning() const { return AlwaysDiagnoseAsWarning; }
  void setAlwaysDiagnoseAsWarning(bool AlwaysDiagnoseAsWarning) {
    this->AlwaysDiagnoseAsWarning = AlwaysDiagnoseAsWarning;
  }
};

} // end anonymous namespace

/// Returns whether an access of the given kind requires exclusive or shared
/// access to its storage.
static ExclusiveOrShared_t getRequiredAccess(SILAccessKind Kind) {
  if (Kind == SILAccessKind::Read)
    return ExclusiveOrShared_t::SharedAccess;

  return ExclusiveOrShared_t::ExclusiveAccess;
}

/// Extract the text for the given expression.
static StringRef extractExprText(const Expr *E, SourceManager &SM) {
  const auto CSR = Lexer::getCharSourceRangeFromSourceRange(SM,
      E->getSourceRange());
  return SM.extractText(CSR);
}

/// Returns true when the call expression is a call to swap() in the Standard
/// Library.
/// This is a helper function that is only used in an assertion, which is why
/// it is in the ifndef.
#ifndef NDEBUG
static bool isCallToStandardLibrarySwap(CallExpr *CE, ASTContext &Ctx) {
  if (CE->getCalledValue() == Ctx.getSwap(nullptr))
    return true;

  // Is the call module qualified, i.e. Swift.swap(&a[i], &[j)?
  if (auto *DSBIE = dyn_cast<DotSyntaxBaseIgnoredExpr>(CE->getFn())) {
    if (auto *DRE = dyn_cast<DeclRefExpr>(DSBIE->getRHS())) {
      return DRE->getDecl() == Ctx.getSwap(nullptr);
    }
  }

  return false;
}
#endif

/// Do a syntactic pattern match to determine whether the call is a call
/// to swap(&base[index1], &base[index2]), which can
/// be replaced with a call to MutableCollection.swapAt(_:_:) on base.
///
/// Returns true if the call can be replaced. Returns the call expression,
/// the base expression, and the two indices as out expressions.
///
/// This method takes an array of all the ApplyInsts for calls to swap()
/// in the function to avoid needing to construct a parent map over the AST
/// to find the CallExpr for the inout accesses.
static bool
canReplaceWithCallToCollectionSwapAt(const BeginAccessInst *Access1,
                                     const BeginAccessInst *Access2,
                                     ArrayRef<ApplyInst *> CallsToSwap,
                                     ASTContext &Ctx,
                                     CallExpr *&FoundCall,
                                     Expr *&Base,
                                     Expr *&Index1,
                                     Expr *&Index2) {
  if (CallsToSwap.empty())
    return false;

  // Inout arguments must be modifications.
  if (Access1->getAccessKind() != SILAccessKind::Modify ||
      Access2->getAccessKind() != SILAccessKind::Modify) {
    return false;
  }

  SILLocation Loc1 = Access1->getLoc();
  SILLocation Loc2 = Access2->getLoc();
  if (Loc1.isNull() || Loc2.isNull())
    return false;

  auto *InOut1 = Loc1.getAsASTNode<InOutExpr>();
  auto *InOut2 = Loc2.getAsASTNode<InOutExpr>();
  if (!InOut1 || !InOut2)
    return false;

  FoundCall = nullptr;
  // Look through all the calls to swap() recorded in the function to find
  // which one we're diagnosing.
  for (ApplyInst *AI : CallsToSwap) {
    SILLocation CallLoc = AI->getLoc();
    if (CallLoc.isNull())
      continue;

    auto *CE = CallLoc.getAsASTNode<CallExpr>();
    if (!CE)
      continue;

    assert(isCallToStandardLibrarySwap(CE, Ctx));
    // swap() takes two arguments.
    auto *ArgTuple = cast<TupleExpr>(CE->getArg());
    const Expr *Arg1 = ArgTuple->getElement(0);
    const Expr *Arg2 = ArgTuple->getElement(1);
    if ((Arg1 == InOut1 && Arg2 == InOut2)) {
        FoundCall = CE;
      break;
    }
  }
  if (!FoundCall)
    return false;

  // We found a call to swap(&e1, &e2). Now check to see whether it
  // matches the form swap(&someCollection[index1], &someCollection[index2]).
  auto *SE1 = dyn_cast<SubscriptExpr>(InOut1->getSubExpr());
  if (!SE1)
    return false;
  auto *SE2 = dyn_cast<SubscriptExpr>(InOut2->getSubExpr());
  if (!SE2)
    return false;

  // Do the two subscripts refer to the same subscript declaration?
  auto *Decl1 = cast<SubscriptDecl>(SE1->getDecl().getDecl());
  auto *Decl2 = cast<SubscriptDecl>(SE2->getDecl().getDecl());
  if (Decl1 != Decl2)
    return false;

  ProtocolDecl *MutableCollectionDecl = Ctx.getMutableCollectionDecl();

  // Is the subcript either (1) on MutableCollection itself or (2) a
  // a witness for a subscript on MutableCollection?
  bool IsSubscriptOnMutableCollection = false;
  ProtocolDecl *ProtocolForDecl =
      Decl1->getDeclContext()->getAsProtocolOrProtocolExtensionContext();
  if (ProtocolForDecl) {
    IsSubscriptOnMutableCollection = (ProtocolForDecl == MutableCollectionDecl);
  } else {
    for (ValueDecl *Req : Decl1->getSatisfiedProtocolRequirements()) {
      DeclContext *ReqDC = Req->getDeclContext();
      ProtocolDecl *ReqProto = ReqDC->getAsProtocolOrProtocolExtensionContext();
      assert(ReqProto && "Protocol requirement not in a protocol?");

      if (ReqProto == MutableCollectionDecl) {
        IsSubscriptOnMutableCollection = true;
        break;
      }
    }
  }

  if (!IsSubscriptOnMutableCollection)
    return false;

  // We're swapping two subscripts on mutable collections -- but are they
  // the same collection? Approximate this by checking for textual
  // equality on the base expressions. This is just an approximation,
  // but is fine for a best-effort Fix-It.
  SourceManager &SM = Ctx.SourceMgr;
  StringRef Base1Text = extractExprText(SE1->getBase(), SM);
  StringRef Base2Text = extractExprText(SE2->getBase(), SM);

  if (Base1Text != Base2Text)
    return false;

  auto *Index1Paren = dyn_cast<ParenExpr>(SE1->getIndex());
  if (!Index1Paren)
    return false;

  auto *Index2Paren = dyn_cast<ParenExpr>(SE2->getIndex());
  if (!Index2Paren)
    return false;

  Base = SE1->getBase();
  Index1 = Index1Paren->getSubExpr();
  Index2 = Index2Paren->getSubExpr();
  return true;
}

/// Suggest replacing with call with a call to swapAt().
static void addSwapAtFixit(InFlightDiagnostic &Diag, CallExpr *&FoundCall,
                           Expr *Base, Expr *&Index1, Expr *&Index2,
                           SourceManager &SM) {
  StringRef BaseText = extractExprText(Base, SM);
  StringRef Index1Text = extractExprText(Index1, SM);
  StringRef Index2Text = extractExprText(Index2, SM);
  SmallString<64> FixItText;
  {
    llvm::raw_svector_ostream Out(FixItText);
    Out << BaseText << ".swapAt(" << Index1Text << ", " << Index2Text << ")";
  }

  Diag.fixItReplace(FoundCall->getSourceRange(), FixItText);
}

/// Returns a string representation of the BaseName and the SubPath
/// suitable for use in diagnostic text. Only supports the Projections
/// that stored-property relaxation supports: struct stored properties
/// and tuple elements.
static std::string getPathDescription(DeclName BaseName, SILType BaseType,
                                      const IndexTrieNode *SubPath,
                                      SILModule &M) {
  std::string sbuf;
  llvm::raw_string_ostream os(sbuf);

  os << "'" << BaseName;
  os << AccessSummaryAnalysis::getSubPathDescription(BaseType, SubPath, M);
  os << "'";

  return os.str();
}

/// Emits a diagnostic if beginning an access with the given in-progress
/// accesses violates the law of exclusivity. Returns true when a
/// diagnostic was emitted.
static void diagnoseExclusivityViolation(const ConflictingAccess &Violation,
                                         ArrayRef<ApplyInst *> CallsToSwap,
                                         ASTContext &Ctx) {

  const AccessedStorage &Storage = Violation.Storage;
  const RecordedAccess &FirstAccess = Violation.FirstAccess;
  const RecordedAccess &SecondAccess = Violation.SecondAccess;
  SILFunction *F = FirstAccess.getInstruction()->getFunction();

  DEBUG(llvm::dbgs() << "Conflict on " << *FirstAccess.getInstruction()
                     << "\n  vs " << *SecondAccess.getInstruction()
                     << "\n  in function " << *F);

  // Can't have a conflict if both accesses are reads.
  assert(!(FirstAccess.getAccessKind() == SILAccessKind::Read &&
           SecondAccess.getAccessKind() == SILAccessKind::Read));

  ExclusiveOrShared_t FirstRequires =
      getRequiredAccess(FirstAccess.getAccessKind());

  // Diagnose on the first access that requires exclusivity.
  bool FirstIsMain = (FirstRequires == ExclusiveOrShared_t::ExclusiveAccess);
  const RecordedAccess &MainAccess = (FirstIsMain ? FirstAccess : SecondAccess);
  const RecordedAccess &NoteAccess = (FirstIsMain ? SecondAccess : FirstAccess);

  SourceRange RangeForMain = MainAccess.getAccessLoc().getSourceRange();
  unsigned AccessKindForMain =
      static_cast<unsigned>(MainAccess.getAccessKind());

  // For now, all exclusivity violations are warning in Swift 3 mode.
  // Also treat some violations as warnings to allow them to be staged in.
  bool DiagnoseAsWarning = Violation.getAlwaysDiagnoseAsWarning() ||
      Ctx.LangOpts.isSwiftVersion3();

  if (const ValueDecl *VD = Storage.getDecl(F)) {
    // We have a declaration, so mention the identifier in the diagnostic.
    auto DiagnosticID = (DiagnoseAsWarning ?
                         diag::exclusivity_access_required_warn :
                         diag::exclusivity_access_required);
    SILType BaseType = FirstAccess.getInstruction()->getType().getAddressType();
    SILModule &M = FirstAccess.getInstruction()->getModule();
    std::string PathDescription = getPathDescription(
        VD->getBaseName(), BaseType, MainAccess.getSubPath(), M);

    // Determine whether we can safely suggest replacing the violation with
    // a call to MutableCollection.swapAt().
    bool SuggestSwapAt = false;
    CallExpr *CallToReplace = nullptr;
    Expr *Base = nullptr;
    Expr *SwapIndex1 = nullptr;
    Expr *SwapIndex2 = nullptr;
    if (SecondAccess.getRecordKind() == RecordedAccessKind::BeginInstruction) {
        SuggestSwapAt = canReplaceWithCallToCollectionSwapAt(
            FirstAccess.getInstruction(), SecondAccess.getInstruction(),
            CallsToSwap, Ctx, CallToReplace, Base, SwapIndex1, SwapIndex2);
    }

    auto D =
        diagnose(Ctx, MainAccess.getAccessLoc().getSourceLoc(), DiagnosticID,
                 PathDescription, AccessKindForMain, SuggestSwapAt);
    D.highlight(RangeForMain);
    if (SuggestSwapAt)
      addSwapAtFixit(D, CallToReplace, Base, SwapIndex1, SwapIndex2,
                     Ctx.SourceMgr);
  } else {
    auto DiagnosticID = (DiagnoseAsWarning ?
                         diag::exclusivity_access_required_unknown_decl_warn :
                         diag::exclusivity_access_required_unknown_decl);
    diagnose(Ctx, MainAccess.getAccessLoc().getSourceLoc(), DiagnosticID,
             AccessKindForMain)
        .highlight(RangeForMain);
  }
  diagnose(Ctx, NoteAccess.getAccessLoc().getSourceLoc(),
           diag::exclusivity_conflicting_access)
      .highlight(NoteAccess.getAccessLoc().getSourceRange());
}

/// Look through a value to find the underlying storage accessed.
static AccessedStorage findValidAccessedStorage(SILValue Source) {
  const AccessedStorage &Storage = findAccessedStorage(Source);
  if (!Storage) {
    llvm::dbgs() << "Bad memory access source: " << Source;
    llvm_unreachable("Unexpected access source.");
  }
  return Storage;
}

/// Returns true when the apply calls the Standard Library swap().
/// Used for fix-its to suggest replacing with Collection.swapAt()
/// on exclusivity violations.
static bool isCallToStandardLibrarySwap(ApplyInst *AI, ASTContext &Ctx) {
  SILFunction *SF = AI->getReferencedFunction();
  if (!SF)
    return false;

  if (!SF->hasLocation())
    return false;

  auto *FD = SF->getLocation().getAsASTNode<FuncDecl>();
  if (!FD)
    return false;

  return FD == Ctx.getSwap(nullptr);
}

static llvm::cl::opt<bool> ShouldAssertOnFailure(
    "sil-assert-on-exclusivity-failure",
    llvm::cl::desc("Should the compiler assert when it diagnoses conflicting "
                   "accesses rather than emitting a diagnostic? Intended for "
                   "use only with debugging."));

/// If making an access of the given kind at the given subpath would
/// would conflict, returns the first recorded access it would conflict
/// with. Otherwise, returns None.
static Optional<RecordedAccess>
shouldReportAccess(const AccessInfo &Info,swift::SILAccessKind Kind,
                   const IndexTrieNode *SubPath) {
  if (Info.alreadyHadConflict())
    return None;

  auto result = Info.conflictsWithAccess(Kind, SubPath);
  if (ShouldAssertOnFailure && result.hasValue())
    llvm_unreachable("Standard assertion routine.");
  return result;
}

/// For each projection that the summarized function accesses on its
/// capture, check whether the access conflicts with already-in-progress
/// access. Returns the most general summarized conflict -- so if there are
/// two conflicts in the called function and one is for an access to an
/// aggregate and another is for an access to a projection from the aggregate,
/// this will return the conflict for the aggregate. This approach guarantees
/// determinism and makes it more  likely that we'll diagnose the most helpful
/// conflict.
static Optional<ConflictingAccess>
findConflictingArgumentAccess(const AccessSummaryAnalysis::ArgumentSummary &AS,
                              const AccessedStorage &AccessedStorage,
                              const AccessInfo &InProgressInfo) {
  Optional<RecordedAccess> BestInProgressAccess;
  Optional<RecordedAccess> BestArgAccess;

  for (const auto &MapPair : AS.getSubAccesses()) {
    const IndexTrieNode *SubPath = MapPair.getFirst();
    const auto &SubAccess = MapPair.getSecond();
    SILAccessKind Kind = SubAccess.getAccessKind();
    auto InProgressAccess = shouldReportAccess(InProgressInfo, Kind, SubPath);
    if (!InProgressAccess)
      continue;

    if (!BestArgAccess ||
        AccessSummaryAnalysis::compareSubPaths(SubPath,
                                               BestArgAccess->getSubPath())) {
        SILLocation AccessLoc = SubAccess.getAccessLoc();

        BestArgAccess = RecordedAccess(Kind, AccessLoc, SubPath);
        BestInProgressAccess = InProgressAccess;
    }
  }

  if (!BestArgAccess)
    return None;

  return ConflictingAccess(AccessedStorage, *BestInProgressAccess,
                           *BestArgAccess);
}

/// Use the summary analysis to check whether a call to the given
/// function would conflict with any in progress accesses. The starting
/// index indicates what index into the the callee's parameters the
/// arguments array starts at -- this is useful for partial_apply functions,
/// which pass only a suffix of the callee's arguments at the apply site.
static void checkForViolationWithCall(
    const StorageMap &Accesses, SILFunction *Callee, unsigned StartingAtIndex,
    OperandValueArrayRef Arguments, AccessSummaryAnalysis *ASA,
    bool DiagnoseAsWarning,
    llvm::SmallVectorImpl<ConflictingAccess> &ConflictingAccesses) {
  const AccessSummaryAnalysis::FunctionSummary &FS =
      ASA->getOrCreateSummary(Callee);

  // For each argument in the suffix of the callee arguments being passed
  // at this call site, determine whether the arguments will be accessed
  // in a way that conflicts with any currently in progress accesses.
  // If so, diagnose.
  for (unsigned ArgumentIndex : indices(Arguments)) {
    unsigned CalleeIndex = StartingAtIndex + ArgumentIndex;

    const AccessSummaryAnalysis::ArgumentSummary &AS =
        FS.getAccessForArgument(CalleeIndex);

    const auto &SubAccesses = AS.getSubAccesses();

    // Is the capture accessed in the callee?
    if (SubAccesses.empty())
      continue;

    SILValue Argument = Arguments[ArgumentIndex];
    assert(Argument->getType().isAddress());

    const AccessedStorage &Storage = findValidAccessedStorage(Argument);
    auto AccessIt = Accesses.find(Storage);

    // Are there any accesses in progress at the time of the call?
    if (AccessIt == Accesses.end())
      continue;

    const AccessInfo &Info = AccessIt->getSecond();
    if (auto Conflict = findConflictingArgumentAccess(AS, Storage, Info)) {
      Conflict->setAlwaysDiagnoseAsWarning(DiagnoseAsWarning);
      ConflictingAccesses.push_back(*Conflict);
    }
  }
}

/// If the given values has a SILFunctionType or an Optional<SILFunctionType>,
/// return the SILFunctionType. Otherwise, return an invalid type.
static CanSILFunctionType getSILFunctionTypeForValue(SILValue arg) {
  SILType argTy = arg->getType();
  // Handle `Optional<@convention(block) @noescape (_)->(_)>`
  if (auto optionalObjTy = argTy.getOptionalObjectType())
    argTy = optionalObjTy;

  return argTy.getAs<SILFunctionType>();
}

/// Checks whether any of the arguments to the apply are closures and diagnoses
/// if any of the @inout_aliasable captures passed to those closures have
/// in-progress accesses that would conflict with any access the summary
/// says the closure would perform.
static void checkForViolationsInNoEscapeClosureArguments(
    const StorageMap &Accesses, ApplySite AS, AccessSummaryAnalysis *ASA,
    llvm::SmallVectorImpl<ConflictingAccess> &ConflictingAccesses,
    bool DiagnoseAsWarning) {

  // Check for violation with closures passed as arguments
  for (SILValue Argument : AS.getArguments()) {
    auto fnType = getSILFunctionTypeForValue(Argument);
    if (!fnType || !fnType->isNoEscape())
      continue;

    FindClosureResult result = findClosureForAppliedArg(Argument);
    if (!result.PAI)
      continue;

    SILFunction *Callee = result.PAI->getCalleeFunction();
    if (!Callee || Callee->empty())
      continue;

    // Check the closure's captures, which are a suffix of the closure's
    // parameters.
    unsigned StartIndex =
        Callee->getArguments().size() - result.PAI->getNumArguments();
    checkForViolationWithCall(Accesses, Callee, StartIndex,
                              result.PAI->getArguments(), ASA,
                              /*DiagnoseAsWarning=*/false, ConflictingAccesses);
  }
}

/// Given a full apply site, diagnose if the apply either calls a closure
/// directly that conflicts with an in-progress access or takes a noescape
/// argument that, when called, would conflict with an in-progress access.
static void checkForViolationsInNoEscapeClosures(
    const StorageMap &Accesses, FullApplySite FAS, AccessSummaryAnalysis *ASA,
    llvm::SmallVectorImpl<ConflictingAccess> &ConflictingAccesses) {
  // Check to make sure that calling a closure immediately will not result in
  // a conflict. This diagnoses in cases where there is a conflict between an
  // argument passed inout to the closure and an access inside the closure to a
  // captured variable:
  //
  //  var i = 7
  //  ({ (p: inout Int) in i = 8})(&i) // Overlapping access to 'i'
  //
  SILFunction *Callee = FAS.getCalleeFunction();
  if (Callee && !Callee->empty()) {
    // Check for violation with directly called closure
    checkForViolationWithCall(Accesses, Callee, 0, FAS.getArguments(), ASA,
                              /*DiagnoseAsWarning=*/false, ConflictingAccesses);
  }

  // Check to make sure that any arguments to the apply are not themselves
  // noescape closures that -- when called -- might conflict with an in-progress
  // access. For example, this will diagnose on the following:
  //
  // var i = 7
  // takesInoutAndClosure(&i) { i = 8 } // Overlapping access to 'i'
  //
  checkForViolationsInNoEscapeClosureArguments(Accesses, FAS, ASA,
                                               ConflictingAccesses,
                                               /*DiagnoseAsWarning=*/false);
}

#ifndef NDEBUG
// If a partial apply has @inout_aliasable arguments, it may only be used as
// a @noescape function type in a way that is recognized by
// DiagnoseStaticExclusivity.
static void checkNoEscapePartialApply(PartialApplyInst *PAI) {
  SmallVector<Operand *, 8> uses(PAI->getUses());
  while (!uses.empty()) {
    Operand *oper = uses.pop_back_val();
    SILInstruction *user = oper->getUser();

    if (isIncidentalUse(user) || onlyAffectsRefCount(user))
      continue;

    if (SingleValueInstruction *copy = getSingleValueCopyOrCast(user)) {
      uses.append(copy->getUses().begin(), copy->getUses().end());
      continue;
    }
    if (auto *cvt = dyn_cast<ConvertEscapeToNoEscapeInst>(user)) {
       uses.append(cvt->getUses().begin(), cvt->getUses().end());
      continue;
    }
    // @noescape block storage can be passed as an Optional (Nullable).
    if (EnumInst *EI = dyn_cast<EnumInst>(user)) {
      uses.append(EI->getUses().begin(), EI->getUses().end());
      continue;
    }
    if (isa<ApplySite>(user)) {
      SILValue arg = oper->get();
      auto ArgumentFnType = getSILFunctionTypeForValue(arg);
      if (ArgumentFnType && ArgumentFnType->isNoEscape()) {
        // Verify that the inverse operation, finding a partial_apply from a
        // @noescape argument, is consistent.
        assert(findClosureForAppliedArg(arg).PAI &&
               "cannot find partial_apply from @noescape function argument");
        continue;
      }
      llvm::dbgs() << "Argument must be @noescape function type: " << *arg;
      llvm_unreachable("A partial_apply with @inout_aliasable may only be "
                       "used as a @noescape function type argument.");
    }
    auto *store = dyn_cast<StoreInst>(user);
    if (store && oper->getOperandNumber() == StoreInst::Src) {
      if (auto *PBSI = dyn_cast<ProjectBlockStorageInst>(store->getDest())) {
        SILValue storageAddr = PBSI->getOperand();
        // The closure is stored to block storage. Recursively visit all
        // uses of any initialized block storage values derived from this
        // storage address..
        for (Operand *oper : storageAddr->getUses()) {
          if (auto *IBS = dyn_cast<InitBlockStorageHeaderInst>(oper->getUser()))
            uses.append(IBS->getUses().begin(), IBS->getUses().end());
        }
        continue;
      }
    }
    llvm::dbgs() << "Unexpected partial_apply use: " << *user;
    llvm_unreachable("A partial_apply with @inout_aliasable may only be "
                     "used as a @noescape function type argument.");
  }
}
#endif

// Check that the given address-type operand is guarded by begin/end access
// markers.
static void checkAccessedAddress(Operand *memOper, StorageMap &Accesses) {
  SILValue address = memOper->get();
  SILInstruction *memInst = memOper->getUser();

  auto error = [address, memInst]() {
    llvm::dbgs() << "Memory access not protected by begin_access:\n";
    memInst->printInContext(llvm::dbgs());
    llvm::dbgs() << "Accessing: " << address;
    llvm::dbgs() << "In function:\n";
    memInst->getFunction()->print(llvm::dbgs());
    abort();
  };

  // If the memory instruction is only used for initialization, it doesn't need
  // an access marker.
  if (memInstMustInitialize(memOper))
    return;

  if (auto apply = ApplySite::isa(memInst)) {
    SILArgumentConvention conv =
        apply.getArgumentConvention(apply.getCalleeArgIndex(*memOper));
    // Captured addresses currently use the @inout_aliasable convention. They
    // are considered an access at any call site that uses the closure. However,
    // those accesses are never explictly protected by access markers. Instead,
    // exclusivity uses AccessSummaryAnalysis to check for conflicts. Here, we
    // can simply ignore any @inout_aliasable arguments.
    if (conv == SILArgumentConvention::Indirect_InoutAliasable)
      return;

    assert(!isa<PartialApplyInst>(memInst)
           && "partial apply can only capture an address as inout_aliasable");
    // TODO: We currently assume @in/@in_guaranteed are only used for
    // pass-by-value arguments. i.e. the address points a local copy of the
    // argument, which is only passed by address for abstraction
    // reasons. However, in the future, @in_guaranteed may be used for
    // borrowed values, which should be recognized as a formal read.
    if (conv != SILArgumentConvention::Indirect_Inout)
      return;
  }

  // Strip off address projections, but not ref_element_addr.
  const AccessedStorage &storage = findAccessedStorage(address);
  // findAccessedStorage may return an invalid storage object if the address
  // producer is not recognized by its whitelist. For the purpose of
  // verification, we assume that this can only happen for local
  // initialization, not a formal memory access. The strength of
  // verification rests on the completeness of the opcode list inside
  // findAccessedStorage.
  if (!storage || !isPossibleFormalAccessBase(storage, memInst->getFunction()))
    return;

  // A box or stack variable may represent lvalues, but they can only conflict
  // with call sites in the same scope. Some initialization patters (stores to
  // the local value) aren't protected by markers, so we need this check.
  if (!isa<ApplySite>(memInst)
      && (storage.getKind() == AccessedStorage::Box
          || storage.getKind() == AccessedStorage::Stack)) {
    return;
  }

  // Otherwise, the address base should be an in-scope begin_access.
  if (storage.getKind() == AccessedStorage::Nested) {
    auto *BAI = cast<BeginAccessInst>(storage.getValue());
    const AccessedStorage &Storage = findValidAccessedStorage(BAI->getSource());
    AccessInfo &Info = Accesses[Storage];
    if (!Info.hasAccessesInProgress())
      error();
    return;
  }
  error();
}

static void checkStaticExclusivity(SILFunction &Fn, PostOrderFunctionInfo *PO,
                                   AccessSummaryAnalysis *ASA) {
  // The implementation relies on the following SIL invariants:
  //    - All incoming edges to a block must have the same in-progress
  //      accesses. This enables the analysis to not perform a data flow merge
  //      on incoming edges.
  //    - Further, for a given address each of the in-progress
  //      accesses must have begun in the same order on all edges. This ensures
  //      consistent diagnostics across changes to the exploration of the CFG.
  //    - On return from a function there are no in-progress accesses. This
  //      enables a sanity check for lean analysis state at function exit.
  //    - Each end_access instruction corresponds to exactly one begin access
  //      instruction. (This is encoded in the EndAccessInst itself)
  //    - begin_access arguments cannot be basic block arguments.
  //      This enables the analysis to look back to find the *single* storage
  //      storage location accessed.

  if (Fn.empty())
    return;

  bool VerifyMemOps = Fn.getModule().getOptions().VerifyExclusivity;

  // Collects calls the Standard Library swap() for Fix-Its.
  llvm::SmallVector<ApplyInst *, 8> CallsToSwap;

  // Stores the accesses that have been found to conflict. Used to defer
  // emitting diagnostics until we can determine whether they should
  // be suppressed.
  llvm::SmallVector<ConflictingAccess, 4> ConflictingAccesses;

  // For each basic block, track the stack of current accesses on
  // exit from that block.
  llvm::SmallDenseMap<SILBasicBlock *, Optional<StorageMap>, 32>
      BlockOutAccesses;

  BlockOutAccesses[Fn.getEntryBlock()] = StorageMap();

  for (auto *BB : PO->getReversePostOrder()) {
    Optional<StorageMap> &BBState = BlockOutAccesses[BB];

    // Because we use a reverse post-order traversal, unless this is the entry
    // at least one of its predecessors must have been reached. Use the out
    // state for that predecessor as our in state. The SIL verifier guarantees
    // that all incoming edges must have the same current accesses.
    for (auto *Pred : BB->getPredecessorBlocks()) {
      auto it = BlockOutAccesses.find(Pred);
      if (it == BlockOutAccesses.end())
        continue;

      const Optional<StorageMap> &PredAccesses = it->getSecond();
      if (PredAccesses) {
        BBState = PredAccesses;
        break;
      }
    }

    // The in-progress accesses for the current program point, represented
    // as map from storage locations to the accesses in progress for the
    // location.
    StorageMap &Accesses = *BBState;

    for (auto &I : *BB) {
      // Apply transfer functions. Beginning an access
      // increments the read or write count for the storage location;
      // Ending onr decrements the count.
      if (auto *BAI = dyn_cast<BeginAccessInst>(&I)) {
        SILAccessKind Kind = BAI->getAccessKind();
        const AccessedStorage &Storage =
          findValidAccessedStorage(BAI->getSource());
        AccessInfo &Info = Accesses[Storage];
        const IndexTrieNode *SubPath = ASA->findSubPathAccessed(BAI);
        if (auto Conflict = shouldReportAccess(Info, Kind, SubPath)) {
          ConflictingAccesses.emplace_back(Storage, *Conflict,
                                           RecordedAccess(BAI, SubPath));
        }

        Info.beginAccess(BAI, SubPath);
        continue;
      }

      if (auto *EAI = dyn_cast<EndAccessInst>(&I)) {
        auto It = Accesses.find(findValidAccessedStorage(EAI->getSource()));
        AccessInfo &Info = It->getSecond();

        BeginAccessInst *BAI = EAI->getBeginAccess();
        const IndexTrieNode *SubPath = ASA->findSubPathAccessed(BAI);
        Info.endAccess(EAI, SubPath);

        // If the storage location has no more in-progress accesses, remove
        // it to keep the StorageMap lean.
        if (!Info.hasAccessesInProgress())
          Accesses.erase(It);
        continue;
      }

      if (VerifyMemOps && I.mayReadOrWriteMemory()) {
        visitAccessedAddress(&I, [&Accesses](Operand *memOper) {
          checkAccessedAddress(memOper, Accesses);
        });
      }

      if (auto *AI = dyn_cast<ApplyInst>(&I)) {
        // Record calls to swap() for potential Fix-Its.
        if (isCallToStandardLibrarySwap(AI, Fn.getASTContext()))
          CallsToSwap.push_back(AI);
        else
          checkForViolationsInNoEscapeClosures(Accesses, AI, ASA,
                                               ConflictingAccesses);
        continue;
      }

      if (auto *TAI = dyn_cast<TryApplyInst>(&I)) {
        checkForViolationsInNoEscapeClosures(Accesses, TAI, ASA,
                                             ConflictingAccesses);
        continue;
      }
#ifndef NDEBUG
      // FIXME: Once AllocBoxToStack is fixed to correctly set noescape
      // closure types, move this PartialApply verification into the
      // SILVerifier to better pinpoint the offending pass.
      if (auto *PAI = dyn_cast<PartialApplyInst>(&I)) {
        ApplySite apply(PAI);
        if (llvm::any_of(range(apply.getNumArguments()),
                         [apply](unsigned argIdx) {
                           return apply.getArgumentConvention(argIdx)
                             == SILArgumentConvention::Indirect_InoutAliasable;
                         })) {
          checkNoEscapePartialApply(PAI);
        }
      }
#endif
      // Sanity check to make sure entries are properly removed.
      assert((!isa<ReturnInst>(&I) || Accesses.empty()) &&
             "Entries were not properly removed?!");
    }
  }

  // Now that we've collected violations and suppressed calls, emit
  // diagnostics.
  for (auto &Violation : ConflictingAccesses) {
    diagnoseExclusivityViolation(Violation, CallsToSwap, Fn.getASTContext());
  }
}

namespace {

class DiagnoseStaticExclusivity : public SILFunctionTransform {
public:
  DiagnoseStaticExclusivity() {}

private:
  void run() override {
    // Don't rerun diagnostics on deserialized functions.
    if (getFunction()->wasDeserializedCanonical())
      return;

    SILFunction *Fn = getFunction();
    // This is a staging flag. Eventually the ability to turn off static
    // enforcement will be removed.
    if (!Fn->getModule().getOptions().EnforceExclusivityStatic)
      return;

    PostOrderFunctionInfo *PO = getAnalysis<PostOrderAnalysis>()->get(Fn);
    auto *ASA = getAnalysis<AccessSummaryAnalysis>();
    checkStaticExclusivity(*Fn, PO, ASA);
  }
};

} // end anonymous namespace

SILTransform *swift::createDiagnoseStaticExclusivity() {
  return new DiagnoseStaticExclusivity();
}
