//===--- SendNonSendable.cpp ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "send-non-sendable"

#include "DiagnosticHelpers.h"

#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Concurrency.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Type.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/ImmutablePointerSet.h"
#include "swift/SIL/BasicBlockData.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/NodeDatastructures.h"
#include "swift/SIL/OperandDatastructures.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/Test.h"
#include "swift/SILOptimizer/Analysis/RegionAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/PartitionUtils.h"
#include "swift/SILOptimizer/Utils/VariableNameUtils.h"
#include "swift/Sema/Concurrency.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

STATISTIC(NumRequireLivenessProcess,
          "# of calls to RequireLiveness::process");
STATISTIC(NumRequireLivenessInstScans,
          "# of instructions scanned inside RequireLiveness block loops");

using namespace swift;
using namespace swift::siloptimizer;
using namespace swift::PartitionPrimitives;
using namespace swift::PatternMatch;
using namespace swift::regionanalysisimpl;

namespace {

using SendingOperandSetFactory = Partition::SendingOperandSetFactory;
using Element = PartitionPrimitives::Element;
using Region = PartitionPrimitives::Region;

} // namespace

// This option is used so we can test typed errors. Typed errors are a fallback
// case which are emitted when we are unable to infer the name of a value. We in
// most cases do succeed inferring, so it makes sense to add an asserts only
// option that can be used by the compiler to test that we emit these correctly.
static llvm::cl::opt<bool> ForceTypedErrors(
    "sil-regionbasedisolation-force-use-of-typed-errors",
    llvm::cl::desc("Force the usage of typed instead of named errors to make "
                   "it easier to test typed errors"),
    llvm::cl::Hidden);

//===----------------------------------------------------------------------===//
//                              MARK: Utilities
//===----------------------------------------------------------------------===//

static SILValue stripFunctionConversions(SILValue val) {
  while (true) {
    if (auto ti = dyn_cast<ThinToThickFunctionInst>(val)) {
      val = ti->getOperand();
      continue;
    }

    if (auto cfi = dyn_cast<ConvertFunctionInst>(val)) {
      val = cfi->getOperand();
      continue;
    }

    if (auto cvt = dyn_cast<ConvertEscapeToNoEscapeInst>(val)) {
      val = cvt->getOperand();
      continue;
    }

    // Look through thunks.
    if (auto pai = dyn_cast<PartialApplyInst>(val)) {
      if (pai->getCalleeFunction()->isThunk()) {
        val = pai->getArgument(0);
        continue;
      }
    }

    break;
  }

  return val;
}

/// Find the most conservative diagnostic behavior by taking the max over all
/// DiagnosticBehavior for the captured values.
static std::optional<DiagnosticBehavior>
getDiagnosticBehaviorLimitForOperands(SILFunction *fn,
                                      ArrayRef<Operand *> capturedValues) {
  std::optional<DiagnosticBehavior> diagnosticBehavior;
  for (auto value : capturedValues) {
    auto lhs = diagnosticBehavior.value_or(DiagnosticBehavior::Unspecified);
    auto limit = value->get()->getType().getConcurrencyDiagnosticBehavior(fn);
    auto rhs = limit.value_or(DiagnosticBehavior::Unspecified);
    auto result = lhs.merge(rhs);
    if (result != DiagnosticBehavior::Unspecified)
      diagnosticBehavior = result;
  }
  return diagnosticBehavior;
}

static std::optional<SILDeclRef> getDeclRefForCallee(SILInstruction *inst) {
  auto fas = FullApplySite::isa(inst);
  if (!fas)
    return {};

  SILValue calleeOrigin = fas.getCalleeOrigin();

  while (true) {
    // Intentionally don't lookup through dynamic_function_ref and
    // previous_dynamic_function_ref as the target of those functions is not
    // statically known.
    if (auto *fri = dyn_cast<FunctionRefInst>(calleeOrigin)) {
      if (auto *callee = fri->getReferencedFunctionOrNull()) {
        if (auto declRef = callee->getDeclRef())
          return declRef;
      }
    }

    if (auto *mi = dyn_cast<MethodInst>(calleeOrigin)) {
      return mi->getMember();
    }

    if (auto *pai = dyn_cast<PartialApplyInst>(calleeOrigin)) {
      calleeOrigin = pai->getCalleeOrigin();
      continue;
    }

    return {};
  }
}

static std::optional<ValueDecl *> getSendingApplyCallee(SILInstruction *inst) {
  auto declRef = getDeclRefForCallee(inst);
  if (!declRef)
    return {};

  auto *decl = declRef->getDecl();
  if (!decl)
    return {};

  return decl;
}

static Expr *inferArgumentExprFromApplyExpr(ApplyExpr *sourceApply,
                                            FullApplySite fai,
                                            const Operand *op) {

  Expr *foundExpr = nullptr;

  // If we have self, then infer it.
  if (fai.hasSelfArgument() && op == &fai.getSelfArgumentOperand()) {
    if (auto callExpr = dyn_cast<CallExpr>(sourceApply))
      if (auto calledExpr =
              dyn_cast<DotSyntaxCallExpr>(callExpr->getDirectCallee()))
        foundExpr = calledExpr->getBase();
  } else {
    // Otherwise, try to infer using the operand of the ApplyExpr.
    unsigned argNum = [&]() -> unsigned {
      if (fai.isCalleeOperand(*op))
        return op->getOperandNumber();
      return fai.getAppliedArgIndexWithoutIndirectResults(*op);
    }();

    // Something happened that we do not understand.
    if (argNum >= sourceApply->getArgs()->size()) {
      return nullptr;
    }

    foundExpr = sourceApply->getArgs()->getExpr(argNum);

    // If we have an erasure expression, lets use the original type. We do
    // this since we are not saying the specific parameter that is the
    // issue and we are using the type to explain it to the user.
    if (auto *erasureExpr = dyn_cast<ErasureExpr>(foundExpr))
      foundExpr = erasureExpr->getSubExpr();
  }

  return foundExpr;
}

/// Attempt to infer a name for \p value. Returns none if we fail or if we are
/// asked to force typed errors since we are testing.
static std::optional<Identifier> inferNameHelper(SILValue value) {
  if (ForceTypedErrors)
    return {};
  return VariableNameInferrer::inferName(value);
}

/// Attempt to infer a name and root for \p value. Returns none if we fail or if
/// we are asked to force typed errors since we are testing.
static std::optional<std::pair<Identifier, SILValue>>
inferNameAndRootHelper(SILValue value) {
  if (ForceTypedErrors)
    return {};
  return VariableNameInferrer::inferNameAndRoot(value);
}

/// Run the name inferrer over \p value purely to recover the source location
/// of the leaf-most component pushed onto the inferred name path -- e.g. the
/// seai for `customField` in `self.customField`. Returns an invalid SourceLoc
/// if no name can be inferred.
///
/// Unlike \c inferNameHelper, this is *not* gated by \c ForceTypedErrors -- we
/// want a usable loc even when name recovery has been suppressed for the
/// bare-note testing path, so the note can still anchor at the leaf access.
static SourceLoc inferFirstNameProvidingLocHelper(SILValue value) {
  if (auto pair = VariableNameInferrer::inferNameAndFirstPathComponent(value))
    return pair->second;
  return SourceLoc();
}

/// Pick the most precise SourceLoc we can find for \p value: prefer the AST
/// decl location for function arguments (so notes land on the parameter name,
/// not the `func` keyword), otherwise the defining instruction's source loc,
/// finally the SILValue's own coarse loc.
///
/// Returns SourceLoc() when we have nothing better than the function-level
/// fallback (e.g. for a phi argument with no defining instruction). In that
/// case the caller should consider the first-pushed name-path component as a
/// better anchor.
static SourceLoc preciseSourceLocForValue(SILValue value) {
  if (auto *arg = dyn_cast<SILFunctionArgument>(value))
    if (auto *decl = arg->getDecl())
      if (auto loc = decl->getLoc())
        return loc;
  if (auto *defInst = value->getDefiningInstruction())
    if (auto astLoc = defInst->getLoc().getSourceLoc())
      return astLoc;
  return SourceLoc();
}

/// Emit a per-value note describing one side of an incompatible region merge.
/// \p ownIso is the isolation of \p value (the textual rendering used when the
/// value is *not* task-isolated). \p ownIsTaskIsolated drives a `%select` that
/// renders "code in the current isolation context" instead of "X-isolated code" for the
/// task-isolated case, matching the wording used elsewhere in the diagnostic
/// suite.
///
/// Falls back to a bare note if name inference fails -- we deliberately do not
/// try to recover an AST type, since SIL values often do not carry a reliable
/// one. Skips emission entirely when no precise SourceLoc is available, since
/// a note at <unknown>:0 is unhelpful and prevents test verifiers from being
/// able to anchor on the note.
///
/// We prefer the value's own precise loc (the use site) when available. Only
/// when the value has nothing better than the function-level fallback (e.g.
/// the value is a phi argument with no defining instruction, as in
/// `switch_enum` over a stored property accessed in an init) do we fall back
/// to the location of the leaf-most component pushed onto the inferred name
/// path -- e.g. the seai for `customField` in `self.customField`. That keeps
/// notes on direct use-site values anchored at the use, while still producing
/// usable locs for the phi-arg case (where the old code would land on the
/// `init` keyword far from the access).
static void emitMergeRegionValueNote(SILValue value, StringRef ownIso,
                                     bool ownIsTaskIsolated) {
  auto &ctx = value->getFunction()->getASTContext();
  SourceLoc loc = preciseSourceLocForValue(value);
  if (!loc)
    loc = inferFirstNameProvidingLocHelper(value);
  if (!loc)
    return;
  if (auto name = inferNameHelper(value)) {
    diagnoseNote(ctx, loc, diag::rbi_merge_failure_value_note_named, *name,
                 ownIso, ownIsTaskIsolated);
    return;
  }
  diagnoseNote(ctx, loc, diag::rbi_merge_failure_value_note_bare, ownIso,
               ownIsTaskIsolated);
}

/// Sometimes we use a store_borrow + temporary to materialize a borrowed value
/// to be passed to another function. We want to emit the error on the function
/// itself, not the store_borrow so we get the best location. We only do this if
/// we can prove that we have a store_borrow to an alloc_stack, the store_borrow
/// is the only thing stored into the alloc_stack, and except for the
/// store_borrow, the alloc_stack has a single non_destroy user, a function we
/// are calling.
static Operand *
findClosureUseThroughStoreBorrowTemporary(StoreBorrowInst *sbi) {
  auto *asi = dyn_cast<AllocStackInst>(sbi->getDest());
  if (!asi)
    return {};
  Operand *resultUse = nullptr;
  for (auto *use : asi->getUses()) {
    if (use == &sbi->getAllOperands()[StoreBorrowInst::Dest])
      continue;
    if (isa<EndBorrowInst>(use->getUser()))
      continue;
    auto fas = FullApplySite::isa(use->getUser());
    if (!fas)
      return {};
    resultUse = use;
  }
  return resultUse;
}

/// Find a use corresponding to the potentially recursive capture of \p
/// initialOperand that would be appropriate for diagnostics.
///
/// \returns the use and the function argument that is used. We return the
/// function argument since it is a clever way to correctly grab the name of the
/// captured value since the ValueDecl will point at the actual ValueDecl in the
/// AST that is captured.
static std::optional<std::pair<Operand *, SILArgument *>>
findClosureUse(Operand *initialOperand) {
  // We have to use a small vector worklist here since we are iterating through
  // uses from different functions.
  llvm::SmallVector<std::pair<Operand *, SILArgument *>, 64> worklist;
  llvm::SmallPtrSet<Operand *, 8> visitedOperand;

  // Initialize our worklist with uses in the initial closure. We do not want to
  // analyze uses in the original function.
  {
    auto as = ApplySite::isa(initialOperand->getUser());
    if (!as)
      return {};

    auto *arg = as.getCalleeArgument(*initialOperand);
    if (!arg)
      return {};
    for (auto *use : arg->getUses()) {
      worklist.emplace_back(use, arg);
      visitedOperand.insert(use);
    }
  }

  while (!worklist.empty()) {
    auto pair = worklist.pop_back_val();
    auto *op = pair.first;
    auto *fArg = pair.second;
    auto *user = op->getUser();

    // Ignore incidental uses that are not specifically ignored use. We want to
    // visit those since they represent `let _ = $VAR` and `_ = $VAR`
    if (isIncidentalUse(user) && !isa<IgnoredUseInst>(user))
      continue;

    // Ignore some instructions we do not care about.
    if (isa<DestroyValueInst, EndBorrowInst, EndAccessInst, DeallocStackInst>(
            user))
      continue;

    // Look through some insts we do not care about.
    if (isa<CopyValueInst, BeginBorrowInst, ProjectBoxInst, BeginAccessInst>(
            user) ||
        isa<MarkUnresolvedNonCopyableValueInst>(user) ||
        isMoveOnlyWrapperUse(user) ||
        // We want to treat move_value [var_decl] as a real use since we are
        // assigning to a var.
        (isa<MoveValueInst>(user) &&
         !cast<MoveValueInst>(user)->isFromVarDecl())) {
      for (auto result : user->getResults()) {
        for (auto *use : result->getUses()) {
          if (visitedOperand.insert(use).second)
            worklist.emplace_back(use, fArg);
        }
      }
      continue;
    }

    // See if we have a callee function. In such a case, find our operand in the
    // callee and visit its uses.
    if (auto as = dyn_cast<PartialApplyInst>(op->getUser())) {
      if (auto *fArg = ApplySite(as).getCalleeArgument(*op)) {
        for (auto *use : fArg->getUses()) {
          if (visitedOperand.insert(use).second)
            worklist.emplace_back(use, fArg);
        }
        continue;
      }
    }

    // See if we have a full apply site that was from a closure that was
    // immediately invoked. In such a case, we can emit a better diagnostic in
    // the called closure.
    if (auto fas = FullApplySite::isa(op->getUser())) {
      if (auto *fArg =
              cast_or_null<SILFunctionArgument>(fas.getCalleeArgument(*op))) {
        if (fArg->isClosureCapture()) {
          for (auto *use : fArg->getUses()) {
            if (visitedOperand.insert(use).second)
              worklist.emplace_back(use, fArg);
          }
          continue;
        }
      }
    }

    // If we have a store_borrow, see if we are using it to just marshal a use
    // to a full apply site.
    if (auto *sbi = dyn_cast<StoreBorrowInst>(op->getUser());
        sbi && op == &sbi->getAllOperands()[StoreBorrowInst::Src]) {
      if (auto *use = findClosureUseThroughStoreBorrowTemporary(sbi)) {
        if (visitedOperand.insert(use).second)
          worklist.emplace_back(use, fArg);
        continue;
      }
    }

    // Otherwise, we have a real use. Return it and the function argument that
    // it was derived from.
    return pair;
  }

  return {};
}

//===----------------------------------------------------------------------===//
//                             MARK: Diagnostics
//===----------------------------------------------------------------------===//

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseError(const PartitionOp &op, Diag<T...> diag,
                                        U &&...args) {
  return siloptimizer::diagnoseError(
      op.getSourceInst()->getFunction()->getASTContext(),
      op.getSourceLoc().getSourceLoc(), diag, std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseNote(const PartitionOp &op, Diag<T...> diag,
                                       U &&...args) {
  return siloptimizer::diagnoseNote(
      op.getSourceInst()->getFunction()->getASTContext(),
      op.getSourceLoc().getSourceLoc(), diag, std::forward<U>(args)...);
}

//===----------------------------------------------------------------------===//
//                           MARK: IsolationHistory
//===----------------------------------------------------------------------===//

// Logging for the isolation history walk that backs the optional notes we
// attach to sent-non-Sendable errors. Since the walk rewinds history nodes that
// no longer exist by the time a diagnostic is read, the only practical way to
// debug a missing or misplaced note is to watch the walk happen.
static llvm::cl::opt<bool> LogIsolationHistory(
    "sil-regionbasedisolation-log-isolation-history",
    llvm::cl::desc("Dump the isolation history of a sent-non-Sendable error "
                   "and log the walk over it that emits isolation history "
                   "notes"),
    llvm::cl::Hidden);

#define ISOLATION_HISTORY_LOG(...)                                             \
  do {                                                                         \
    if (LogIsolationHistory) {                                                 \
      __VA_ARGS__;                                                             \
    }                                                                          \
  } while (0)

namespace {

/// Helper that emits the optional notes attached to a SentNeverSendable error
/// when isolation-history emission is enabled: a chain of "X is connected
/// to Y" notes anchored at each named intermediate's declaration, ending
/// with an "originating" note at the merge that brought the sent element
/// into an isolated region.
///
/// Construct with the full context needed to walk the isolation history,
/// then call \c emit(). \c emit() is a no-op when isolation-history emission
/// is disabled, when the sent element is itself isolated, or when no useful
/// chain was found.
class IsolationHistoryNoteEmitter {
  /// Composition wrapper that extends the partition being rewound with the
  /// isolation queries this walk needs. Defined below the class since only the
  /// out-of-line \c collectIsolationHistoryNotes uses it.
  struct PartitionWrapper;

  /// Begin a log line at \p depth levels of indentation. Every line begins with
  /// the same prefix so the log can be grepped out of a larger dump.
  static llvm::raw_ostream &log(unsigned depth = 0) {
    auto &os = llvm::dbgs();
    os << "[isolation-history] ";
    os.indent(2 * depth);
    return os;
  }

  /// Print the instruction a sequence boundary was pushed for on a single line,
  /// without a trailing newline. The SIL printer emits leading comment lines
  /// for some instructions (e.g. the callee name above a function_ref) and a
  /// trailing newline, so print into a scratch buffer and keep only the
  /// instruction itself — a multi-line value here would break out of the log's
  /// line prefix.
  static void printBoundaryInstForLog(llvm::raw_ostream &os,
                                      SILInstruction *inst) {
    if (!inst) {
      os << "<no instruction>";
      return;
    }

    SmallString<128> buffer;
    llvm::raw_svector_ostream bufferStream(buffer);
    inst->print(bufferStream);

    SmallVector<StringRef, 4> instLines;
    StringRef(buffer).split(instLines, '\n', -1, false /*KeepEmpty*/);
    for (StringRef line : instLines) {
      StringRef trimmed = line.trim();
      if (trimmed.empty() || trimmed.starts_with("//"))
        continue;
      os << trimmed;
      return;
    }

    // Every line was a comment — fall back to the first one so the log still
    // says something about this instruction.
    os << (instLines.empty() ? StringRef("<unprintable>")
                             : instLines[0].trim());
  }

public:
  /// What a chain of isolation-history notes should explain.
  ///
  /// The walk answers questions of the form "how does element A reach element
  /// B"; a request is the root question, and it comes in two shapes. Both are
  /// already handled by the walk -- only the entry point distinguished them.
  struct Request {
    /// Which of the two questions this request asks.
    enum class Kind {
      /// "How did \c from become isolated to \c regionInfo's isolation?" The
      /// chain terminates at the isolated value that put it there.
      HowDidBecomeIsolatedTo,

      /// "How did \c from become connected to \c to?" Every link is a plain
      /// connection; there is no isolated value to terminate at.
      HowDidBecomeConnectedTo,
    };

    /// The element whose route the chain explains: the near end, which the
    /// notes read outward from.
    Element from;

    /// The far end, or None when the chain explains how \c from's region came
    /// to be isolated rather than how it reaches one specific element.
    std::optional<Element> to;

    /// The isolation of the region \c from ended up in. Set only for
    /// \c HowDidBecomeIsolatedTo requests, where it supplies the terminal
    /// note's wording; a connect request has no isolated terminus.
    std::optional<SILDynamicMergedIsolationInfo> regionInfo;

    /// "How did \p elt become isolated to \p info's isolation?" The chain
    /// terminates at the isolated value.
    static Request howDidBecomeIsolatedTo(Element elt,
                                          SILDynamicMergedIsolationInfo info) {
      return Request{elt, std::nullopt, info};
    }

    /// "How did \p from become connected to \p to, i.e. how did the two come to
    /// be in one region?" Every link is a plain connection; there is no
    /// isolated value to terminate at.
    static Request howDidBecomeConnectedTo(Element from, Element to) {
      return Request{from, to, std::nullopt};
    }

    Kind getKind() const {
      return to.has_value() ? Kind::HowDidBecomeConnectedTo
                            : Kind::HowDidBecomeIsolatedTo;
    }
  };

  /// Single-use entry point. Returns true when at least one note was emitted.
  static bool emit(SILFunction *fn, RegionAnalysisFunctionInfo *functionInfo,
                   RegionAnalysisValueMap &valueMap, Request request,
                   Partition &&partition) {
    IsolationHistoryNoteEmitter emitter(fn, functionInfo, valueMap, request,
                                        std::move(partition));
    return emitter.emitHelper();
  }

private:
  struct NoteToEmit {
    Element lhsElt;
    Element rhsElt;
    SILLocation mergeLoc;
    std::optional<unsigned> prevNoteIndex;

    /// True when \c rhsElt is the actor-isolated value the chain terminates at
    /// rather than another disconnected link, so this note gets the
    /// "which is accessible to <isolation>" phrasing.
    bool rhsIsIsolatedSource;
  };

  /// The chain the walk settled on, one entry per note to emit. Filled in from
  /// \c walkNotes once a path explains the chain.
  SmallVector<NoteToEmit, 0> notes;

  /// The notes the walk has discovered, shared by every path.
  ///
  /// Along one path this is append-only, but a path does two things to it that
  /// the next one must not see: it appends notes of its own, and it fills in
  /// locations on notes it inherited. Both are undone when the walk backtracks
  /// onto another path, using that path's \c State::noteMark and
  /// \c State::pendingStart. Sharing one buffer is what keeps State small
  /// enough to copy per path without dragging a note vector along.
  SmallVector<NoteToEmit, 0> walkNotes;

  /// An unanswered "how does \c from reach \c to" question.
  ///
  /// The walk is pairwise rather than single-focus because a merge can separate
  /// the two from/to ends of a question while naming neither of them -- both
  /// are passengers carried along by the merge, on opposite sides of the split.
  /// Such a merge does not advance one focus by a hop; it splits the question
  /// into two independent halves, each resolved by its own older merge.
  ///
  /// Example:
  ///
  ///   let y = Box1(x)     // merge %y with %box1result
  ///   let z = Box2(y)     // merge %z with %box2result
  ///   await send(z)
  ///
  /// The walk starts asking how %z reached the isolated region. Rewinding the
  /// Box2 call answers that by separating %z from everything on %y's side, so
  /// the question splits: how does %z reach %box2result, and how does
  /// %box2result reach the isolated value. Rewinding the Box1 call then answers
  /// the second half while naming neither %box2result nor %x -- both are
  /// passengers -- and splits it again. A single-focus walk has nowhere to put
  /// that: there is no one element to advance.
  struct ChainQuestion {
    /// The end of the question on the sent value's side.
    Element from;

    /// The other end, or None when we do not know it yet because we are still
    /// looking for whatever made \c from's region isolated.
    std::optional<Element> to;

    /// Index in \c notes of the note this question was split out of, or
    /// none for the question we started with.
    std::optional<unsigned> parentNote;
  };

  /// One rewind in progress: the partition being rewound and everything the
  /// walk has worked out from it so far.
  ///
  /// A CFG history join leaves the walk several paths to try, since the chain
  /// may continue in any of the joined predecessors, and each path carries a
  /// copy of this. So this is copied once per candidate predecessor and wants
  /// to stay small. The notes themselves live in \c walkNotes rather than here;
  /// a state refers to its share of them by the two marks below.
  struct State {
    /// The partition to keep rewinding. For the initial state this is the
    /// partition at the moment of the send; for a later path it is the joined
    /// predecessor's exit partition.
    Partition partition;

    /// The questions still to answer. The walk starts with one -- how did the
    /// sent element come to be in an isolated region -- and answering a
    /// question can split it in two, so this grows as well as shrinks. The
    /// chain is fully explained when it drains.
    ///
    /// Unlike the notes this cannot be shared: \c processState rewrites it
    /// wholesale for every merge it answers, so it is not append-only. One
    /// question is always in flight, so keep room for that one inline.
    SmallVector<ChainQuestion, 1> openQuestions;

    /// Size of \c walkNotes when this state was created, i.e. the notes it
    /// inherited. Backtracking onto this path drops everything the path tried
    /// before it appended past this point.
    unsigned noteMark = 0;

    /// Index in \c walkNotes of the first inherited note still waiting for a
    /// location, so the inherited pending notes are [pendingStart, noteMark).
    ///
    /// A PartitionOp pushes its sequence boundary before its nodes, so
    /// rewinding reaches a merge before the boundary that names the instruction
    /// behind it: a note's location is not known when the note is created and
    /// is filled in when the boundary arrives. A merge the dataflow join
    /// performed has no boundary in this block at all, so its notes stay
    /// pending across the join and are located only once the walk takes a path
    /// into the predecessor. Backtracking onto this path un-locates this range,
    /// undoing whatever the path tried before it wrote there.
    unsigned pendingStart = 0;

    /// Set once \c openQuestions drains. The walk cannot stop the moment that
    /// happens: the notes the answering merge created are still waiting for the
    /// location of the instruction behind them.
    bool allQuestionsAnswered = false;
  };

  /// The SIL function whose AST context is used for diagnostic emission and
  /// whose decl-attribute / cl-flag state gates emission via
  /// \c shouldEmitIsolationHistory.
  SILFunction *inputFn;

  /// The region analysis value map; used to look up element isolation state
  /// and to recover representative SILValues for name inference.
  RegionAnalysisValueMap &inputValueMap;

  /// The region analysis for this function; used to recover a predecessor
  /// block's exit partition when following a CFG history join.
  RegionAnalysisFunctionInfo *inputFunctionInfo;

  /// Predecessor blocks the walk has already taken a path into. A join can be
  /// reached from more than one path -- and a loop's history joins the block
  /// back onto itself -- so without this the walk would rewind the same branch
  /// repeatedly.
  BasicBlockSet visitedBlocks;

  /// The question this chain of notes answers: which element's route to
  /// explain, where it ends, and -- for a \c HowDidBecomeIsolatedTo request
  /// -- the isolation of the region it ended up in.
  Request inputRequest;

  /// The input partition to use for our emission.
  Partition inputPartition;

  IsolationHistoryNoteEmitter(SILFunction *fn,
                              RegionAnalysisFunctionInfo *functionInfo,
                              RegionAnalysisValueMap &valueMap, Request request,
                              Partition &&partition)
      : inputFn(fn), inputValueMap(valueMap), inputFunctionInfo(functionInfo),
        visitedBlocks(fn), inputRequest(request),
        inputPartition(std::move(partition)) {}

  /// Announce the question being answered, in whichever of the two shapes the
  /// request has. Only called from under \c ISOLATION_HISTORY_LOG.
  void logRequestPreamble() const {
    if (inputRequest.getKind() == Request::Kind::HowDidBecomeIsolatedTo) {
      log() << "===== Explaining why the sent value in " << inputFn->getName()
            << " is isolated =====\n";
      log() << "The value being sent is %%" << inputRequest.from << '\n';
      log() << "It is being sent into: "
            << inputRequest.regionInfo->printForDiagnostics(inputFn)
            << ((*inputRequest.regionInfo)->isTaskIsolated()
                    ? " (task isolated)"
                    : "")
            << '\n';
    } else {
      log() << "===== Explaining how the two ends of a region in "
            << inputFn->getName() << " came together =====\n";
      log() << "The near end is %%" << inputRequest.from
            << ", the far end is %%" << *inputRequest.to << '\n';
    }

    log() << "Regions at the moment of the send: ";
    inputPartition.print(llvm::dbgs());
  }

  bool emitHelper() {
    ISOLATION_HISTORY_LOG(logRequestPreamble());

    if (!shouldEmit()) {
      ISOLATION_HISTORY_LOG(
          log()
          << "Nothing to explain: either these notes are turned off for this "
             "function, or the sent value is itself actor isolated rather than "
             "having been made isolated by a merge.\n");
      return false;
    }

    collectIsolationHistoryNotes();

    ISOLATION_HISTORY_LOG(log() << "Chain, in the order it was discovered:\n";
                          for (unsigned i = 0, e = notes.size(); i != e; ++i) {
                            auto &note = notes[i];
                            log(1) << "[" << i << "] ";
                            printElementForLog(llvm::dbgs(), note.lhsElt);
                            llvm::dbgs() << " is connected to ";
                            printElementForLog(llvm::dbgs(), note.rhsElt);
                            if (note.rhsIsIsolatedSource)
                              llvm::dbgs() << " (the isolated value)";
                            llvm::dbgs() << ", parent: ";
                            if (!note.prevNoteIndex)
                              llvm::dbgs() << "none";
                            else
                              llvm::dbgs() << note.prevNoteIndex;
                            llvm::dbgs() << ", at ";
                            note.mergeLoc.getSourceLoc().print(
                                llvm::dbgs(),
                                inputFn->getASTContext().SourceMgr);
                            llvm::dbgs() << '\n';
                          });

    return emitNotes();
  }

  /// How an element should be spelled in a note.
  struct ElementName {
    Identifier name;

    /// True when \c name names the call that produced the value rather than a
    /// variable, so the note has to read "result of 'foo()'".
    bool isCallResult;
  };

  /// The name to print for \p elt, or None when the element has nothing worth
  /// showing the user.
  std::optional<ElementName> inferElementName(Element elt) const {
    SILValue rep = inputValueMap.maybeGetRepresentative(elt);
    if (!rep)
      return {};

    // These notes explain where a value came from, so when a link is the result
    // of a call we want the callee's name rather than the name of the variable
    // the result happened to be bound to.
    VariableNameInferrer::Options nameOptions(
        VariableNameInferrer::Flag::NameCallResultAfterCallee);
    bool isCallResult = false;
    auto namePair = VariableNameInferrer::inferNameAndFirstPathComponent(
        rep, nameOptions, &isCallResult);

    // VariableNameInferrer falls back to the literal identifier "unknown" when
    // it cannot recover a name; that is never useful in a diagnostic, so treat
    // it as no name at all.
    if (!namePair || namePair->first.str() == "unknown")
      return {};

    return ElementName{namePair->first, isCallResult};
  }

  /// Print \p elt as its user-visible name when one can be inferred, falling
  /// back to the element number. Only used by the log.
  void printElementForLog(llvm::raw_ostream &os, Element elt) const {
    if (SILValue rep = inputValueMap.maybeGetRepresentative(elt)) {
      if (auto name = VariableNameInferrer::inferName(rep)) {
        os << "'" << name->str() << "'";
        return;
      }
    }
    os << "%%" << elt;
  }

  /// True when \p lhs and \p rhs are two spellings of one user-visible value,
  /// so the merge that connected them is an artifact of lowering rather than
  /// something the user wrote.
  ///
  /// The element actually sent for a call argument is such a spelling: SILGen
  /// allocates a stack slot, stores the variable into it, and passes the slot,
  /// which the region analysis tracks as an element of its own merged with the
  /// variable's. A note about that merge reads "'y' is connected to 'y'".
  ///
  /// Recognised by the temporary having no var_decl while naming the same
  /// variable: two values the user actually declared are never collapsed, even
  /// when shadowing makes them share a name.
  bool isSameUserValue(Element lhs, Element rhs) const {
    SILValue lhsRep = inputValueMap.maybeGetRepresentative(lhs);
    SILValue rhsRep = inputValueMap.maybeGetRepresentative(rhs);
    if (!lhsRep || !rhsRep)
      return false;

    if (lhsRep->isFromVarDecl() && rhsRep->isFromVarDecl())
      return false;

    // Compare the names the notes would actually print. A value named after the
    // call that produced it does not read as the same thing as the variable
    // that call's result was bound to, even though the inferrer will happily
    // name both after that variable when not asked to prefer the callee.
    auto lhsName = inferElementName(lhs);
    auto rhsName = inferElementName(rhs);
    return lhsName && rhsName && lhsName->name == rhsName->name &&
           lhsName->isCallResult == rhsName->isCallResult;
  }

  void collectIsolationHistoryNotes();

  /// Rewind \p state's partition until the chain is explained or the state's
  /// history runs out. Returns true when the chain was explained, in which case
  /// the chain has been copied from \c walkNotes onto \c notes. Otherwise
  /// records a path on \p states for each joined predecessor the chain may
  /// continue in and returns false.
  bool processState(State &&state, SmallVectorImpl<State> &states);

  /// Record a path on \p states for each predecessor in \p joinedBlocks that
  /// the chain may continue in.
  ///
  /// The chain -- or the location of its last notes -- is in the predecessors
  /// whose exit partitions were joined into \p state's partition at a CFG merge
  /// point. Only some of them can be where what the walk is looking for
  /// happened, so a predecessor whose exit partition cannot answer anything we
  /// want is skipped rather than rewound.
  ///
  /// \p pendingStart is the state's pending-note mark as it stands now, which
  /// rewinding has moved past the mark the state itself carries.
  void prepareStatesForPredecessors(
      const State &state, ArrayRef<SILBasicBlock *> joinedBlocks,
      unsigned pendingStart, SmallVectorImpl<State> &states);

  /// Emit a diagnostic for each link of the chain. Returns true when at least
  /// one was emitted.
  bool emitNotes();

  /// Returns true when isolation-history emission is on for this function and
  /// the request is one the walk can say anything about.
  bool shouldEmit() const {
    if (!swift::shouldEmitIsolationHistoryFor(inputFn))
      return false;

    if (inputRequest.getKind() == Request::Kind::HowDidBecomeIsolatedTo) {
      // Never format an isolation we cannot prove valid: printForDiagnostics
      // report_fatal_error()s on the Invalid lattice element, and emitNotes()
      // prints this one. An error carrying an isolation nobody validated gets
      // no chain rather than a crash. (Some errors default their isolation to
      // {} -- see InOutSendingReturnedError.)
      if (!*inputRequest.regionInfo)
        return false;

      // An intrinsically isolated element was not *made* isolated by a merge,
      // so there is no chain to describe.
      return inputValueMap.getIsolationRegion(inputRequest.from)
          .isDisconnected();
    }

    // For a connect request the two ends must actually be in one region here,
    // since the walk explains a chain by finding the merge that separates them.
    // Nothing to explain otherwise -- and no isolation requirement either:
    // neither end need be isolated, so there is nothing to validate.
    if (!inputPartition.isTrackingElement(inputRequest.from) ||
        !inputPartition.isTrackingElement(*inputRequest.to))
      return false;
    return inputPartition.getRegion(inputRequest.from) ==
           inputPartition.getRegion(*inputRequest.to);
  }

  void emitGenericOriginatingNote(SourceLoc loc, StringRef descriptiveKind) {
    inputFn->getASTContext().Diags.diagnose(
        loc, diag::regionbasedisolation_value_became_part_of_isolated_region,
        descriptiveKind, (*inputRequest.regionInfo)->isTaskIsolated());
  }
};

/// A composition wrapper that adds behavior to a Partition without changing
/// Partition itself: it holds the partition alongside the value map the extra
/// behavior needs, and forwards through \c operator-> and an implicit
/// conversion so it can be used anywhere the underlying partition can. In Swift
/// this would just be a fileprivate extension on Partition; C++ has no
/// extensions, so the added methods live on a local wrapper instead.
///
/// The behavior being added is "which elements here are isolated", which
/// Partition cannot answer on its own since isolation lives in the value map
/// rather than in the element-to-region mapping.
struct IsolationHistoryNoteEmitter::PartitionWrapper {
  const Partition &partition;
  RegionAnalysisValueMap &valueMap;

  PartitionWrapper(const Partition &partition, RegionAnalysisValueMap &valueMap)
      : partition(partition), valueMap(valueMap) {}

  const Partition *operator->() const { return &partition; }

  enum ElementIsolationStatus {
    Disconnected = 0,
    DirectlyIsolated = 1,
    IndirectlyIsolated = 2,
  };

  // An element with no value-map entry is tracked in the history but is no
  // longer a value we can reason about. Its isolation info is Invalid, which is
  // a distinct kind from Disconnected, so isDisconnected() answers false for it
  // and it would otherwise read as isolated.
  bool isIsolated(Element element) const {
    auto info = valueMap.getIsolationRegion(element);
    return info && !info.isDisconnected();
  }

  bool isInIsolatedRegion(Element element) const {
    auto reg = partition.getRegion(element);
    for (auto iter : partition.range()) {
      if (iter.second == reg && isIsolated(iter.first)) {
        return true;
      }
    }
    return false;
  }

  ElementIsolationStatus getStatus(Element element) const {
    if (isIsolated(element))
      return ElementIsolationStatus::DirectlyIsolated;
    if (isInIsolatedRegion(element))
      return ElementIsolationStatus::IndirectlyIsolated;
    return ElementIsolationStatus::Disconnected;
  }

  /// True when this partition -- the exit partition of a predecessor joined at
  /// a CFG history join -- could answer \p question, i.e. when the chain could
  /// continue through that predecessor.
  ///
  /// It could when what the question asks about has already come together here:
  ///
  ///   - For a question with both ends known, when the two ends are in one
  ///     region. The merge that put them together is then somewhere in this
  ///     predecessor's history. Where they are in different regions it was the
  ///     join itself that brought them together, so that branch cannot explain
  ///     the note.
  ///
  ///   - For a question still looking for whatever isolated its element, when
  ///     that element's region is isolated here. A branch where it is
  ///     disconnected is not where its isolation came from.
  bool couldAnswer(const ChainQuestion &question) const {
    if (!partition.isTrackingElement(question.from))
      return false;

    if (question.to) {
      if (!partition.isTrackingElement(*question.to))
        return false;
      return partition.getRegion(question.from) ==
             partition.getRegion(*question.to);
    }

    return isInIsolatedRegion(question.from);
  }
};

} // namespace

/// Work out how the request's near end reached its far end -- or, when there is
/// no far end, how its region came to be isolated -- by *rewinding* the
/// send-time partition one history node at a time
/// (\c Partition::popHistoryOnce), undoing each node's effect as we go.
///
/// The walk answers questions of the form "how does this element reach that
/// one", starting with the request itself. A merge that separates the two ends
/// of a question answers it: the pair the merge names becomes a link of the
/// chain, and the question splits into the two halves left over on either side
/// of it. The walk is done when every question has been answered.
///
/// On exit, populates \c notes with one entry per link, each carrying the two
/// elements to name, the location of the instruction that merged them, and
/// whether the far end is the isolated value the chain terminates at.
/// \c emitNotes turns that into diagnostics.
///
/// Names are not resolved here: a link records elements, and \c emitNotes asks
/// the value map for a name when it is about to print one.
///
/// When rewinding reaches a CFGHistoryJoin the chain may continue in a
/// predecessor block, whose history lives in that block's exit partition. We
/// record a State for each such predecessor carrying the walk as it stands at
/// the join, and explore them depth-first so a dead-end predecessor path simply
/// backtracks onto the next one.
void IsolationHistoryNoteEmitter::collectIsolationHistoryNotes() {
  // Both kinds of partition the walk rewinds have to be stripped of sending
  // operand state first, since popHistoryOnce rewinds region membership alone
  // and does not maintain the send information alongside it. For a joined
  // predecessor's exit partition that happens where the path is recorded; the
  // send-time partition we start from is stripped here.
  State initial{inputPartition.removingSendingOperandState(), {}};
  initial.openQuestions.push_back(
      ChainQuestion{inputRequest.from, inputRequest.to, std::nullopt});

  // Paths still to try, newest first: a path is followed until it explains the
  // chain or dies, and only then does the walk back up to the next one.
  SmallVector<State, 4> states;
  states.push_back(std::move(initial));

  while (!states.empty()) {
    State state = std::move(states.back());
    states.pop_back();

    // Put the shared note buffer back the way this path left it. A path tried
    // in the meantime appended notes of its own and located notes this one
    // inherited, and neither belongs here.
    walkNotes.truncate(state.noteMark);
    for (unsigned i : range(state.pendingStart, state.noteMark))
      walkNotes[i].mergeLoc = SILLocation::invalid();

    ISOLATION_HISTORY_LOG(log() << "Rewinding a partition (" << states.size()
                                << " other path(s) still to try): ";
                          state.partition.print(llvm::dbgs()));

    if (processState(std::move(state), states))
      return;
  }

  ISOLATION_HISTORY_LOG(
      log() << "Tried every path and never explained the whole chain.\n");
}

bool IsolationHistoryNoteEmitter::processState(State &&state,
                                               SmallVectorImpl<State> &states) {
  using Node = IsolationHistory::Node;

  // Read-only view of the state's partition, for the isolation queries. The
  // rewind itself mutates state.partition directly.
  PartitionWrapper partition(state.partition, inputValueMap);

  // Predecessor blocks reached at CFG joins while rewinding this state.
  SmallVector<SILBasicBlock *, 4> joinedBlocks;

  // The instruction of the most recently rewound sequence boundary. History is
  // rewound newest-first, and a package's boundary is popped just before the
  // nodes that belong to it, so this names the instruction whose effect the
  // nodes being rewound right now are undoing. Only read by the logging below.
  SILInstruction *currentBoundaryInst = nullptr;

  auto &openQuestions = state.openQuestions;
  bool &allQuestionsAnswered = state.allQuestionsAnswered;

  // Index of the first note still waiting for a location. Everything from here
  // to the end of walkNotes is pending; everything before it is located.
  unsigned pendingStart = state.pendingStart;

  // Scratch for rebuilding the open questions while answering them against a
  // single merge. Declared out here so the loop below does not reallocate.
  SmallVector<ChainQuestion, 8> stillUnanswered;

  while (const auto *node = state.partition.popHistoryOnce(joinedBlocks)) {
    SWIFT_DEFER {
      ISOLATION_HISTORY_LOG(
          auto &os = log(1); os << "Rewound: ";
          node->printOneLine(os, inputFn->getASTContext().SourceMgr);
          os << '\n';
          // Name the instruction being undone: its own for a boundary, the
          // enclosing package's for everything else.
          auto &instOS = log(2);
          instOS << "inst: ";
          printBoundaryInstForLog(instOS, currentBoundaryInst); instOS << '\n';
          // Only the node kinds that actually undo a region change alter the
          // partition, so print the new partition just for those.
          if (node->doesRewindingMutatePartition()) {
            log(2) << "Partition is now: ";
            partition->print(llvm::dbgs());
          });
    };

    switch (node->getKind()) {
    case Node::CFGHistoryJoin:
      // popHistoryOnce has recorded the predecessor block for us; the branch
      // that was joined in is rewound as its own state once this one runs out
      // of history.
      continue;
    case Node::AddNewRegionForElement:
    case Node::RemoveLastElementFromRegion:
    case Node::RemoveElementFromRegion:
      continue;

    case Node::MergeElementRegions: {
      // The two elements the program actually merged. Undoing the node has
      // already put them in separate regions, along with whatever each of them
      // carried: everything the merge moved is on snd's side, everything that
      // stayed is on fst's.
      //
      // These are the values to NAME in a note. They are not what decides
      // whether the note is emitted -- see below.
      Element fst = node->getFirstArgAsElement();
      Element snd = node->getAdditionalElementArgs().front();

      if (fst == snd)
        continue;

      // Both must still be in the partition to ask which side anything is on.
      if (!partition->isTrackingElement(fst) ||
          !partition->isTrackingElement(snd))
        continue;

      Region fstRegion = partition->getRegion(fst);
      Region sndRegion = partition->getRegion(snd);

      // The undo should have separated them. If it did not, this node cannot
      // have split any question's ends apart either.
      if (fstRegion == sndRegion)
        continue;

      // Answer every question this merge separates.
      //
      // Recognition is REGIONAL, not by operand: a question is answered here if
      // undoing this merge put its two ends in different regions. A merge whose
      // operands are neither end still separates them -- both ends can be
      // passengers carried along on opposite sides of the split -- and asking
      // about regions does not care which element happened to be named or which
      // side survived the split.
      stillUnanswered.clear();
      for (const ChainQuestion &question : openQuestions) {
        if (!partition->isTrackingElement(question.from)) {
          stillUnanswered.push_back(question);
          continue;
        }

        // Which side of the split is the sent-value end on?
        Region fromRegion = partition->getRegion(question.from);
        bool fromOnFstSide = fromRegion == fstRegion;
        if (!fromOnFstSide && fromRegion != sndRegion) {
          // Neither side: this merge did not touch this question.
          stillUnanswered.push_back(question);
          continue;
        }

        // Orient the operand pair so 'near' is the one on the sent value's
        // side. Notes read near-to-far, matching the chain's direction.
        Element nearElt = fromOnFstSide ? fst : snd;
        Element farElt = fromOnFstSide ? snd : fst;
        Region farRegion = fromOnFstSide ? sndRegion : fstRegion;

        bool farIsIsolatedSource = false;
        if (question.to) {
          // Both ends known. This merge answers the question only if the far
          // end is on the other side of the split.
          if (!partition->isTrackingElement(*question.to) ||
              partition->getRegion(*question.to) != farRegion) {
            stillUnanswered.push_back(question);
            continue;
          }
        } else {
          // Looking for whatever isolated this element's region. Before the
          // undo that region was isolated; if it still is, the isolation came
          // from an older merge and this is not the one.
          if (partition.isInIsolatedRegion(question.from)) {
            stillUnanswered.push_back(question);
            continue;
          }
          // from's region is disconnected now, so the isolated element went to
          // the other side of this split.
          farIsIsolatedSource = partition.isIsolated(farElt);
        }

        ISOLATION_HISTORY_LOG(
            log(2) << "This merge separates %%" << question.from << " from ";
            if (question.to) llvm::dbgs() << "%%" << *question.to;
            else llvm::dbgs() << "the isolated value";
            llvm::dbgs() << ". The program merged %%" << nearElt << " and %%"
                         << farElt
                         << ", so that is the connection to report.\n");

        // A merge between two spellings of one value explains nothing, so it
        // gets no note. The question still advances past it: whatever we needed
        // to reach through 'near' we now need to reach through 'far'.
        auto noteIndex = question.parentNote;
        if (isSameUserValue(nearElt, farElt)) {
          ISOLATION_HISTORY_LOG(
              log(3)
              << "%%" << nearElt << " and %%" << farElt
              << " are the same value spelled two ways, so this merge is an "
                 "artifact of lowering and gets no note.\n");
        } else {
          // A new note is pending by construction: it goes on the end, and
          // pendingStart already points at or before the end.
          noteIndex = walkNotes.size();
          walkNotes.push_back(
              NoteToEmit{nearElt, farElt, SILLocation::invalid(),
                         question.parentNote, farIsIsolatedSource});
        }

        // The question splits in two: how does the sent-value end reach the
        // operand on its side, and how does the operand on the far side reach
        // the far end. Either half is already answered when its two elements
        // coincide.
        if (question.from != nearElt) {
          ISOLATION_HISTORY_LOG(log(3)
                                << "Still need to connect %%" << question.from
                                << " to %%" << nearElt << ".\n");
          stillUnanswered.push_back(
              ChainQuestion{question.from, nearElt, noteIndex});
        }
        if (question.to) {
          if (*question.to != farElt) {
            ISOLATION_HISTORY_LOG(log(3) << "Still need to connect %%" << farElt
                                         << " to %%" << *question.to << ".\n");
            stillUnanswered.push_back(
                ChainQuestion{farElt, *question.to, noteIndex});
          }
        } else if (!farIsIsolatedSource) {
          // The far side is isolated but this element is not the isolated value
          // itself, so it is another carrier and its own route still needs
          // explaining.
          ISOLATION_HISTORY_LOG(log(3)
                                << "%%" << farElt
                                << " is not the isolated value itself, so it "
                                   "still needs its own explanation.\n");
          stillUnanswered.push_back(
              ChainQuestion{farElt, std::nullopt, noteIndex});
        }
      }
      openQuestions.assign(stillUnanswered.begin(), stillUnanswered.end());

      if (openQuestions.empty()) {
        ISOLATION_HISTORY_LOG(
            log(2)
            << "Every connection is explained. Waiting for this merge's "
               "boundary to learn which instruction to point the last notes "
               "at.\n");
        allQuestionsAnswered = true;
      }
      continue;
    }

    case Node::SequenceBoundary: {
      currentBoundaryInst = node->getHistoryBoundaryInst();

      // This boundary names the instruction behind every merge rewound since
      // the last one, so it is the location of the notes they created.
      auto loc = node->getHistoryBoundaryLoc();
      assert(loc);
      for (unsigned i = pendingStart, e = walkNotes.size(); i != e; ++i)
        walkNotes[i].mergeLoc = *loc;
      pendingStart = walkNotes.size();

      // Every question has been answered and the last notes now have their
      // locations, so there is nothing older worth rewinding for.
      if (allQuestionsAnswered) {
        ISOLATION_HISTORY_LOG(log(2)
                              << "Done: the whole chain is explained.\n");
        notes = walkNotes;
        return true;
      }
      continue;
    }
    }
  }

  ISOLATION_HISTORY_LOG(
      log(1) << "Ran out of history here. Questions still unanswered: "
             << openQuestions.size() << ", notes found: " << walkNotes.size()
             << ", branches to continue into: " << joinedBlocks.size() << '\n');

  // Every question was answered, so nothing older can add a note. What can
  // still be missing is a location: a note is left waiting when the merge
  // behind it was performed by the dataflow join rather than by a PartitionOp,
  // so no boundary in this block names it. The instruction to blame for such a
  // merge is the last one in the branch where the two ends actually came
  // together, which is the first boundary the walk rewinds after taking a path
  // into that predecessor.
  //
  // Keep the chain as it stands in case no other path improves on it, and stop
  // here when there is no path left to try.
  if (allQuestionsAnswered) {
    if (notes.empty())
      notes = walkNotes;
    if (joinedBlocks.empty()) {
      ISOLATION_HISTORY_LOG(
          log(1) << "The chain is explained as far as the history goes.\n");
      return true;
    }
  }

  prepareStatesForPredecessors(state, joinedBlocks, pendingStart, states);
  return false;
}

void IsolationHistoryNoteEmitter::prepareStatesForPredecessors(
    const State &state, ArrayRef<SILBasicBlock *> joinedBlocks,
    unsigned pendingStart, SmallVectorImpl<State> &states) {
  // Point at the predecessors' exit partitions rather than copying them.
  // Deciding whether a predecessor is worth trying only reads its exit
  // partition, and a Partition copy also copies its element-to-region map, so
  // the copy waits until we know the walk is going there.
  struct PredExit {
    SILBasicBlock *block;
    const Partition *exit;
  };
  SmallVector<PredExit, 4> preds;
  for (SILBasicBlock *predBlock : joinedBlocks) {
    if (!visitedBlocks.insert(predBlock)) {
      ISOLATION_HISTORY_LOG(log(1) << "Not rewinding joined pred bb"
                                   << predBlock->getDebugID() << " again.\n");
      continue;
    }

    auto predBlockState = inputFunctionInfo->getBlockState(predBlock);
    if (!predBlockState) {
      ISOLATION_HISTORY_LOG(log(1) << "Cannot rewind joined pred bb"
                                   << predBlock->getDebugID()
                                   << ": it has no block state.\n");
      continue;
    }

    preds.push_back({predBlock, &predBlockState.get()->getExitPartition()});
  }

  // What the walk still wants from a predecessor: the open questions when there
  // are any, and otherwise the ends of the notes that are waiting for a
  // location.
  SmallVector<ChainQuestion, 4> wanted;
  if (!state.openQuestions.empty()) {
    wanted.append(state.openQuestions.begin(), state.openQuestions.end());
  } else {
    for (unsigned i = pendingStart, e = walkNotes.size(); i != e; ++i)
      wanted.push_back(ChainQuestion{walkNotes[i].lhsElt, walkNotes[i].rhsElt,
                                     std::nullopt});
  }

  // Rewinding a predecessor the chain never came through would report merges
  // from a branch that has nothing to do with the isolation -- the arm of a
  // diamond that assigns a fresh value has a history full of merges, none of
  // them an answer.
  //
  // A predecessor is a candidate when its exit partition can answer something
  // we want (see \c couldAnswer). If none of them can, the walk has lost track
  // of what it is looking for, so rather than guess we try them all.
  SmallBitVector isCandidate(preds.size());
  for (unsigned i : indices(preds)) {
    PartitionWrapper predPartition(*preds[i].exit, inputValueMap);
    for (const ChainQuestion &question : wanted) {
      if (predPartition.couldAnswer(question)) {
        isCandidate[i] = true;
        break;
      }
    }
  }

  for (unsigned i : indices(preds)) {
    if (isCandidate.any() && !isCandidate[i]) {
      ISOLATION_HISTORY_LOG(
          log(1) << "Nothing we are looking for came together in bb"
                 << preds[i].block->getDebugID()
                 << ", so the chain does not run through it. Skipping it.\n");
      continue;
    }

    ISOLATION_HISTORY_LOG(log(1) << "Will continue the walk into bb"
                                 << preds[i].block->getDebugID() << ".\n");
    // Now the copy is worth making. The history is rewound over region
    // membership alone, so the sending operands recorded in the exit partition
    // have to go.
    //
    // Every path inherits the notes as they stand now, and the pending run
    // within them. Backtracking onto the path restores walkNotes to exactly
    // this.
    states.push_back(State{preds[i].exit->removingSendingOperandState(),
                           state.openQuestions, unsigned(walkNotes.size()),
                           pendingStart, state.allQuestionsAnswered});
  }
}

/// Emit the collected chain, one note per link, anchored at the merge that
/// created the link.
///
/// The notes are reported in the order the walk discovered them, which is not
/// the order the chain reads in: a merge that separates the two ends of a
/// question is rewound before the merges that explain either half. Nothing
/// depends on that order -- every note names both of its ends and points at its
/// own merge, so it reads on its own.
///
/// Returns true when at least one note was emitted.
bool IsolationHistoryNoteEmitter::emitNotes() {
  // Only a HowDidBecomeIsolatedTo request has an isolated terminus to
  // describe, and only it carries an isolation to name it with.
  StringRef descriptiveKindStr;
  if (inputRequest.regionInfo)
    descriptiveKindStr = inputRequest.regionInfo->printForDiagnostics(inputFn);
  auto &diags = inputFn->getASTContext().Diags;
  bool emittedNote = false;

  for (unsigned i = 0, e = notes.size(); i != e; ++i) {
    const NoteToEmit &note = notes[i];

    // rhsIsIsolatedSource is only ever set on the "looking for whatever
    // isolated this" path in processState, which a connect request never takes.
    assert((!note.rhsIsIsolatedSource || inputRequest.regionInfo) &&
           "Connect request produced an isolated-source link?!");

    // The location comes from the sequence boundary behind the merge, which a
    // truncated history may never have reached.
    if (note.mergeLoc.isNull()) {
      ISOLATION_HISTORY_LOG(log(1) << "No note for link [" << i
                                   << "]: we never learned which instruction "
                                      "performed the merge.\n");
      continue;
    }

    auto nearName = inferElementName(note.lhsElt);
    auto farName = inferElementName(note.rhsElt);

    // The last link names the isolated value the chain ends at, so it gets the
    // "which is accessible to <isolation>" phrasing. With no name to show for
    // either end, all that is left to say is where the value entered the
    // isolated region.
    if (note.rhsIsIsolatedSource) {
      if (!nearName || !farName || nearName->name == farName->name) {
        ISOLATION_HISTORY_LOG(
            log(1) << "Link [" << i
                   << "] reaches the isolated value but cannot name both ends, "
                      "so it only says where the value became isolated.\n");
        emitGenericOriginatingNote(note.mergeLoc.getSourceLoc(),
                                   descriptiveKindStr);
        emittedNote = true;
        continue;
      }

      diags.diagnose(
          note.mergeLoc.getSourceLoc(),
          diag::regionbasedisolation_chain_value_exposed_to_isolated_named,
          nearName->name, nearName->isCallResult, farName->name,
          farName->isCallResult, descriptiveKindStr,
          (*inputRequest.regionInfo)->isTaskIsolated());
      emittedNote = true;
      continue;
    }

    if (!nearName || !farName) {
      ISOLATION_HISTORY_LOG(log(1)
                            << "No note for link [" << i
                            << "]: one of its ends has no name we could show "
                               "the user.\n");
      continue;
    }

    // A call result is not something the reader can point at in their own code
    // -- it names where a value came from -- so it reads as the object of a
    // link, never its subject. Where the merge puts it on the left, turn the
    // link around rather than saying "result of 'Foo.init()' is connected to
    // 'c'".
    const ElementName *subject = &*nearName;
    const ElementName *object = &*farName;
    if (subject->isCallResult && !object->isCallResult)
      std::swap(subject, object);

    // Name inference can resolve both ends to one identifier -- e.g. a closure
    // literal and the `let` it is bound to. "'f' is connected to 'f'" explains
    // nothing.
    if (subject->name == object->name) {
      ISOLATION_HISTORY_LOG(log(1)
                            << "No note for link [" << i << "]: both ends are '"
                            << subject->name << "'.\n");
      continue;
    }

    diags.diagnose(note.mergeLoc.getSourceLoc(),
                   diag::regionbasedisolation_chain_value_exposed,
                   subject->name, subject->isCallResult, object->name,
                   object->isCallResult);
    emittedNote = true;
  }

  return emittedNote;
}

//===----------------------------------------------------------------------===//
//               MARK: Unknown Pattern Error Helper
//===----------------------------------------------------------------------===//

// Helper to emit unknown pattern errors with diagnostic context.
static void
emitUnknownPatternErrorHelper(const char *emitterName, SILInstruction *inst,
                              std::optional<DiagnosticBehavior> behaviorLimit,
                              bool pushToFuture, const char *file, int line) {
  if (inst->getFunction()
          ->getModule()
          .getOptions()
          .AbortOnUnknownRegionIsolationPatternError) {
    llvm::report_fatal_error(
        "RegionIsolation: Found unknown SIL pattern in diagnostic emitter. "
        "See -sil-region-isolation-assert-on-unknown-pattern");
  }

  REGIONBASEDISOLATION_LOG(llvm::dbgs()
                           << "Emitting "
                           << (pushToFuture ? "Warning (pushed to future). "
                                            : "Error. ")
                           << "DiagnosticEmission "
                           << (pushToFuture ? "Warning" : "Error")
                           << ": Unknown Code Pattern.\n"
                           << "  Emitter: " << emitterName << "\n"
                           << "  Instruction: " << *inst
                           << "  Location: " << file << ":" << line << "\n");

  if (pushToFuture) {
    // Bypass the file-scope `siloptimizer::diagnoseError` (which wraps in
    // `warnUntilLanguageMode(v6)`) and emit directly with
    // `warnUntilLanguageMode(future)` so this becomes a real warning even
    // under `-swift-version 6`. Matches the IncompatibleRegionMergeErrorEmitter
    // convention.
    inst->getFunction()
        ->getASTContext()
        .Diags
        .diagnose(inst->getLoc().getSourceLoc(),
                  diag::regionbasedisolation_unknown_pattern)
        .warnUntilLanguageMode(LanguageMode::future)
        .limitBehaviorIf(behaviorLimit);
    return;
  }

  diagnoseError(inst, diag::regionbasedisolation_unknown_pattern)
      .limitBehaviorIf(behaviorLimit);
}

#ifdef EMIT_UNKNOWN_PATTERN_ERROR
#error "EMIT_UNKNOWN_PATTERN_ERROR macro is already defined"
#endif

#define EMIT_UNKNOWN_PATTERN_ERROR(emitterName, inst, behaviorLimit,           \
                                   pushToFuture)                               \
  emitUnknownPatternErrorHelper(#emitterName, inst, behaviorLimit,             \
                                pushToFuture, __FILE__, __LINE__)

//===----------------------------------------------------------------------===//
//                           MARK: Require Liveness
//===----------------------------------------------------------------------===//

namespace {

class BlockLivenessInfo {
  // Generation counter so we do not need to reallocate.
  unsigned generation = 0;
  SILInstruction *firstRequireInst = nullptr;

  void resetIfNew(unsigned newGeneration) {
    if (generation == newGeneration)
      return;
    generation = newGeneration;
    firstRequireInst = nullptr;
  }

public:
  SILInstruction *getInst(unsigned callerGeneration) {
    resetIfNew(callerGeneration);
    return firstRequireInst;
  }

  void setInst(unsigned callerGeneration, SILInstruction *newValue) {
    resetIfNew(callerGeneration);
    firstRequireInst = newValue;
  }
};

/// We only want to emit errors for the first requires along a path from a
/// sending instruction. We discover this by walking from user blocks to
struct RequireLiveness {
  unsigned generation;
  SILInstruction *sendingInst;
  BasicBlockData<BlockLivenessInfo> &blockLivenessInfo;
  InstructionSet allRequires;
  InstructionSetWithSize finalRequires;

  /// If we have requires in the def block before our send, this is the
  /// first require.
  SILInstruction *firstRequireBeforeSendInDefBlock = nullptr;

  RequireLiveness(unsigned generation, Operand *sendingOp,
                  BasicBlockData<BlockLivenessInfo> &blockLivenessInfo)
      : generation(generation), sendingInst(sendingOp->getUser()),
        blockLivenessInfo(blockLivenessInfo),
        allRequires(sendingOp->getParentFunction()),
        finalRequires(sendingOp->getParentFunction()) {}

  template <typename Collection>
  void process(Collection collection);

  /// Attempt to process requireInst for our def block. Returns false if
  /// requireInst was before our def and we need to do interprocedural
  /// processing. Returns true if requireInst was after our seningInst and we
  /// were able to appropriately determine if we should emit it or not.
  void processDefBlock();

  /// Process all requires in block, updating blockLivenessInfo.
  void processNonDefBlock(SILBasicBlock *block);
};

} // namespace

void RequireLiveness::processDefBlock() {
  REGIONBASEDISOLATION_LOG(llvm::dbgs() << "    Processing def block!\n");
  // First walk from the beginning of the block to the send instruction to
  // see if we have any requires before our def. Once we find one, we can skip
  // the traversal and jump straight to the send.
  for (auto ii = sendingInst->getParent()->begin(),
            ie = sendingInst->getIterator();
       ii != ie; ++ii) {
    if (allRequires.contains(&*ii) && !firstRequireBeforeSendInDefBlock) {
      firstRequireBeforeSendInDefBlock = &*ii;
      REGIONBASEDISOLATION_LOG(llvm::dbgs()
                               << "        Found send before def: "
                               << *firstRequireBeforeSendInDefBlock);
      break;
    }
  }

  // Then walk from our sendingInst to the end of the block looking for the
  // first require inst. Once we find it... return.
  //
  // NOTE: We start walking at the sendingInst since the sendingInst could use
  // the requireInst as well.
  for (auto ii = sendingInst->getIterator(),
            ie = sendingInst->getParent()->end();
       ii != ie; ++ii) {
    if (!allRequires.contains(&*ii))
      continue;

    finalRequires.insert(&*ii);
    REGIONBASEDISOLATION_LOG(llvm::dbgs()
                             << "        Found send after def: " << *ii);
    return;
  }
}

void RequireLiveness::processNonDefBlock(SILBasicBlock *block) {
  // Walk from the bottom to the top... assigning to our block state.
  auto blockState = blockLivenessInfo.get(block);
  for (auto &inst : llvm::make_range(block->rbegin(), block->rend())) {
    if (!finalRequires.contains(&inst))
      continue;
    blockState.get()->setInst(generation, &inst);
  }
}

template <typename Collection>
void RequireLiveness::process(Collection requireInstList) {
  ++NumRequireLivenessProcess;
  REGIONBASEDISOLATION_LOG(
      llvm::dbgs() << "==> Performing Require Liveness for: " << *sendingInst);

  // Then put all of our requires into our allRequires set.
  BasicBlockWorklist initializingWorklist(sendingInst->getFunction());
  for (auto require : requireInstList) {
    REGIONBASEDISOLATION_LOG(llvm::dbgs()
                             << "        Require Inst: " << **require);
    allRequires.insert(*require);
    initializingWorklist.pushIfNotVisited(require->getParent());
  }

  // Then process our def block to see if we have any requires before and after
  // the sendingInst...
  processDefBlock();

  // If we found /any/ requries after the sendingInst, we can bail early since
  // that is guaranteed to dominate all further requires.
  if (!finalRequires.empty()) {
    REGIONBASEDISOLATION_LOG(
        llvm::dbgs()
        << "        Found send after def in def block! Exiting early!\n");
    return;
  }

  REGIONBASEDISOLATION_LOG(llvm::dbgs()
                           << "        Did not find send after def in def "
                              "block! Walking blocks!\n");

  // If we found a send in the def block before our def, add it to the block
  // state for the def.
  if (firstRequireBeforeSendInDefBlock) {
    REGIONBASEDISOLATION_LOG(
        llvm::dbgs()
        << "        Found a require before send! Adding to block state!\n");
    auto blockState = blockLivenessInfo.get(sendingInst->getParent());
    blockState.get()->setInst(generation, firstRequireBeforeSendInDefBlock);
  }

  // Then for each require block that isn't a def block send, find the
  // earliest send inst.
  while (auto *requireBlock = initializingWorklist.pop()) {
    auto blockState = blockLivenessInfo.get(requireBlock);
    for (auto &inst : *requireBlock) {
      ++NumRequireLivenessInstScans;
      if (!allRequires.contains(&inst))
        continue;
      REGIONBASEDISOLATION_LOG(llvm::dbgs() << "        Mapping Block bb"
                                            << requireBlock->getDebugID()
                                            << " to: " << inst);
      blockState.get()->setInst(generation, &inst);
      break;
    }
  }

  // Then walk from our def block looking for setInst blocks.
  auto *sendingBlock = sendingInst->getParent();
  BasicBlockWorklist worklist(sendingInst->getFunction());
  for (auto *succBlock : sendingBlock->getSuccessorBlocks())
    worklist.pushIfNotVisited(succBlock);

  while (auto *next = worklist.pop()) {
    // Check if we found an earliest requires... if so, add that to final
    // requires and continue. We don't want to visit successors.
    auto blockState = blockLivenessInfo.get(next);
    if (auto *inst = blockState.get()->getInst(generation)) {
      finalRequires.insert(inst);
      continue;
    }

    // Do not look at successors of the sending block.
    if (next == sendingBlock)
      continue;

    // Otherwise, we did not find a requires and need to search further
    // successors.
    for (auto *succBlock : next->getSuccessorBlocks())
      worklist.pushIfNotVisited(succBlock);
  }
}

//===----------------------------------------------------------------------===//
//            MARK: Forward Declaration Of SendNonSendableImpl
//===----------------------------------------------------------------------===//

namespace {

/// Wrapper around a SILInstruction that internally specifies whether we are
/// dealing with an inout reinitialization needed or if it is just a normal
/// use after send.
class RequireInst {
public:
  enum Kind {
    UseAfterSend,
    InOutReinitializationNeeded,
  };

private:
  llvm::PointerIntPair<SILInstruction *, 1> instAndKind;

  RequireInst(SILInstruction *inst, Kind kind) : instAndKind(inst, kind) {}

public:
  static RequireInst forUseAfterSend(SILInstruction *inst) {
    return {inst, Kind::UseAfterSend};
  }

  static RequireInst forInOutReinitializationNeeded(SILInstruction *inst) {
    return {inst, Kind::InOutReinitializationNeeded};
  }

  SILInstruction *getInst() const { return instAndKind.getPointer(); }
  Kind getKind() const { return Kind(instAndKind.getInt()); }

  SILInstruction *operator*() const { return getInst(); }
  SILInstruction *operator->() const { return getInst(); }
};

class SendNonSendableImpl {
  RegionAnalysisFunctionInfo *info;
  SmallFrozenMultiMap<Operand *, RequireInst, 8> sendingOpToRequireInstMultiMap;
  SmallVector<PartitionOpError, 8> foundVerbatimErrors;

public:
  SendNonSendableImpl(RegionAnalysisFunctionInfo *info) : info(info) {}

  void emitDiagnostics();

private:
  void runDiagnosticEvaluator();

  void emitUseAfterSendDiagnostics();
  void emitVerbatimErrors();
};

} // namespace

//===----------------------------------------------------------------------===//
//                         MARK: Diagnostic Evaluator
//===----------------------------------------------------------------------===//

namespace {

struct DiagnosticEvaluator final
    : PartitionOpEvaluatorBaseImpl<DiagnosticEvaluator> {
  RegionAnalysisFunctionInfo *info;
  SmallFrozenMultiMap<Operand *, RequireInst, 8>
      &sendingOpToRequireInstMultiMap;

  /// An error that we know how to emit verbatim without needing to preprocess.
  ///
  /// A contrasting case here is the use after send error where we need to pair
  /// sending operands to require insts.
  SmallVectorImpl<PartitionOpError> &foundVerbatimErrors;

  DiagnosticEvaluator(Partition &workingPartition,
                      RegionAnalysisFunctionInfo *info,
                      SmallFrozenMultiMap<Operand *, RequireInst, 8>
                          &sendingOpToRequireInstMultiMap,
                      SmallVectorImpl<PartitionOpError> &foundVerbatimErrors,
                      SendingOperandToStateMap &operandToStateMap)
      : PartitionOpEvaluatorBaseImpl(
            workingPartition, info->getOperandSetFactory(), operandToStateMap),
        info(info),
        sendingOpToRequireInstMultiMap(sendingOpToRequireInstMultiMap),
        foundVerbatimErrors(foundVerbatimErrors) {}

  void handleLocalUseAfterSendError(LocalUseAfterSendError &&error) const {
    const auto &partitionOp = *error.op;
    REGIONBASEDISOLATION_LOG(error.print(llvm::dbgs(), info->getValueMap()));

    // Ignore this if we are erroring on a mutable base of a Sendable value and
    // if when we sent the value's region was not closure captured.
    if (error.op->getOptions().containsOnly(
            PartitionOp::Flag::RequireOfMutableBaseOfSendableValue) &&
        !operandToStateMap.get(error.sendingOp).isClosureCaptured)
      return;

    sendingOpToRequireInstMultiMap.insert(
        error.sendingOp, RequireInst::forUseAfterSend(partitionOp.getSourceInst()));
  }

  void handleInOutSendingNotInitializedAtExitError(
      InOutSendingNotInitializedAtExitError &&error) const {
    const PartitionOp &partitionOp = *error.op;
    Operand *sendingOp = error.sendingOp;

    REGIONBASEDISOLATION_LOG(error.print(llvm::dbgs(), info->getValueMap()));

    sendingOpToRequireInstMultiMap.insert(
        sendingOp, RequireInst::forInOutReinitializationNeeded(
                       partitionOp.getSourceInst()));
  }

  /// Dispatch an error discovered during dataflow to the appropriate handler.
  ///
  /// This uses X-macros from PartitionOpError.def to generate the switch cases:
  /// - PARTITION_OP_ERROR: Data-dependent errors are handled immediately via
  ///   handle<Name>Error() methods, which may record state for later emission.
  /// - PARTITION_OP_VERBATIM_ERROR: Self-contained errors are collected into
  ///   foundVerbatimErrors for batch emission later in emitVerbatimErrors().
  void handleError(PartitionOpError &&error) {
    switch (error.getKind()) {
#define PARTITION_OP_ERROR(NAME)                                               \
  case PartitionOpError::NAME:                                                 \
    return handle##NAME##Error(std::move(error).get##NAME##Error());
#define PARTITION_OP_VERBATIM_ERROR(NAME)                                      \
  case PartitionOpError::NAME:                                                 \
    REGIONBASEDISOLATION_LOG(error.print(llvm::dbgs(), info->getValueMap()));  \
    foundVerbatimErrors.emplace_back(std::move(error));                        \
    return;
#include "swift/SILOptimizer/Utils/PartitionOpError.def"
    }
    llvm_unreachable("Covered switch isn't covered?!");
  }

  bool isActorDerived(Element element) const {
    return info->getValueMap().getIsolationRegion(element).isActorIsolated();
  }

  /// If \p element's representative is an indirect out parameter, return
  /// that parameter.
  SILValue getIndirectOutParameter(Element element) const {
    auto rep = info->getValueMap().getRepresentativeValue(element);
    if (!rep)
      return {};
    if (auto value = dyn_cast_or_null<SILFunctionArgument>(rep.maybeGetValue());
        value && value->getArgumentConvention().isIndirectOutParameter())
      return value;
    return {};
  }

  /// If \p element's representative is an indirect out parameter, return
  /// that parameter.
  SILValue getSendingIndirectOutParameter(Element element) const {
    auto rep = info->getValueMap().getRepresentativeValue(element);
    if (!rep)
      return {};
    if (auto value = dyn_cast_or_null<SILFunctionArgument>(rep.maybeGetValue());
        value && value->getArgumentConvention().isIndirectOutParameter() &&
        value->isSending())
      return value;
    return {};
  }
  
  SILValue getInOutSendingParameter(Element elt) const {
    auto rep = info->getValueMap().getRepresentativeValue(elt);
    if (!rep)
      return {};
    if (auto value = dyn_cast_or_null<SILFunctionArgument>(rep.maybeGetValue());
        value && value->getArgumentConvention().isInoutConvention() &&
        value->isSending())
      return value;
    return {};
  }

  bool isTaskIsolatedDerived(Element element) const {
    return info->getValueMap().getIsolationRegion(element).isTaskIsolated();
  }

  SILIsolationInfo::Kind hasSpecialDerivation(Element element) const {
    return info->getValueMap().getIsolationRegion(element).getKind();
  }

  SILIsolationInfo getIsolationRegionInfo(Element element) const {
    return info->getValueMap().getIsolationRegion(element);
  }

  /// Only return an element if we are already tracking value and it is
  /// non-Sendable.
  ///
  /// TODO: Can we return only
  std::optional<Element> getElement(SILValue value) const {
    auto trackableValue = info->getValueMap().getTrackableValue(value);
    if (trackableValue.value.isSendable())
      return {};
    return trackableValue.value.getID();
  }

  SILValue getRepresentative(SILValue value) const {
    return info->getValueMap()
        .getTrackableValue(value)
        .value.getRepresentative()
        .maybeGetValue();
  }

  RepresentativeValue getRepresentativeValue(Element element) const {
    return info->getValueMap().getRepresentativeValue(element);
  }

  bool isClosureCaptured(Element element, Operand *op) const {
    auto value = info->getValueMap().maybeGetRepresentative(element);
    if (!value)
      return false;
    return info->isClosureCaptured(value, op);
  }
};

} // namespace

void SendNonSendableImpl::runDiagnosticEvaluator() {
  // Then for each block...
  REGIONBASEDISOLATION_LOG(llvm::dbgs() << "Walking blocks for diagnostics.\n");
  for (auto [block, blockState] : info->getRange()) {
    REGIONBASEDISOLATION_LOG(llvm::dbgs()
                             << "|--> Block bb" << block.getDebugID() << "\n");

    if (!blockState.getLiveness()) {
      REGIONBASEDISOLATION_LOG(llvm::dbgs() << "Dead block... skipping!\n");
      continue;
    }

    REGIONBASEDISOLATION_LOG(
        llvm::dbgs() << "Entry Partition: ";
        blockState.getEntryPartition().print(llvm::dbgs()));

    // Grab its entry partition and setup an evaluator for the partition that
    // has callbacks that emit diagnsotics...
    Partition workingPartition = blockState.getEntryPartition();
    DiagnosticEvaluator eval(
        workingPartition, info, sendingOpToRequireInstMultiMap,
        foundVerbatimErrors, info->getSendingOperandToStateMap());

    // And then evaluate all of our partition ops on the entry partition.
    for (auto &partitionOp : blockState.getPartitionOps()) {
      eval.apply(partitionOp);
    }

    REGIONBASEDISOLATION_LOG(llvm::dbgs() << "Exit Partition: ";
                             workingPartition.print(llvm::dbgs()));
  }

  REGIONBASEDISOLATION_LOG(llvm::dbgs()
                           << "Finished walking blocks for diagnostics.\n");

  // Now that we have found all of our sendingInsts/Requires emit errors.
  sendingOpToRequireInstMultiMap.setFrozen();
}

//===----------------------------------------------------------------------===//
//                MARK: UseAfterSend Diagnostic Inference
//===----------------------------------------------------------------------===//

namespace {

class UseAfterSendDiagnosticEmitter {
  Operand *sendingOp;
  SmallVectorImpl<RequireInst> &requireInsts;
  bool emittedErrorDiagnostic = false;

public:
  UseAfterSendDiagnosticEmitter(Operand *sendingOp,
                                SmallVectorImpl<RequireInst> &requireInsts)
      : sendingOp(sendingOp), requireInsts(requireInsts) {}

  ~UseAfterSendDiagnosticEmitter() {
    // If we were supposed to emit a diagnostic and didn't emit an unknown
    // pattern error.
    if (!emittedErrorDiagnostic)
      emitUnknownPatternError();
  }

  SILFunction *getFunction() const { return sendingOp->getFunction(); }

  std::optional<DiagnosticBehavior> getBehaviorLimit() const {
    // If the apply that performed the send is preconcurrency on either side
    // of its isolation crossing, downgrade to a warning. This mirrors the
    // AST-level ActorIsolatedCall policy in TypeCheckConcurrency.cpp (search
    // for `actor_isolated_call`): both diagnostics observe the same
    // isolation crossing on the same ApplyExpr, so they should observe the
    // same severity policy. Read the crossing from the AST ApplyExpr —
    // SILGen does not propagate it onto the SIL apply for synchronous
    // direct calls, so `ApplySite::getIsolationCrossing()` is typically
    // empty here.
    if (auto *applyExpr =
            sendingOp->getUser()->getLoc().getAsASTNode<ApplyExpr>()) {
      if (auto crossing = applyExpr->getIsolationCrossing()) {
        if (crossing->getCallerIsolation().preconcurrency() ||
            crossing->getCalleeIsolation().preconcurrency())
          return DiagnosticBehavior::Warning;
      }
    }

    return sendingOp->get()->getType().getConcurrencyDiagnosticBehavior(
        getFunction());
  }

  /// Attempts to retrieve and return the callee declaration.
  std::optional<const ValueDecl *> getSendingCallee() const {
    return getSendingApplyCallee(sendingOp->getUser());
  }

  void
  emitNamedIsolationCrossingError(SILLocation loc, Identifier name,
                                  SILIsolationInfo namedValuesIsolationInfo,
                                  ApplyIsolationCrossing isolationCrossing) {
    // Emit the short error.
    diagnoseError(loc, diag::regionbasedisolation_named_send_yields_race, name)
        .limitBehaviorIf(getBehaviorLimit());

    // Then emit the note with greater context.
    auto descriptiveKindStr =
        namedValuesIsolationInfo.printForDiagnostics(getFunction());
    auto calleeIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), isolationCrossing.getCalleeIsolation());
    auto callerIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), isolationCrossing.getCallerIsolation());

    bool isDisconnected = namedValuesIsolationInfo.isDisconnected() ||
                          namedValuesIsolationInfo.isTaskIsolated();
    if (auto callee = getSendingCallee()) {
      diagnoseNote(
          loc, diag::regionbasedisolation_named_info_send_yields_race_callee,
          isDisconnected, name, descriptiveKindStr, calleeIsolationStr,
          callee.value(), callerIsolationStr);
    } else {
      diagnoseNote(loc, diag::regionbasedisolation_named_info_send_yields_race,
                   isDisconnected, name, descriptiveKindStr, calleeIsolationStr,
                   callerIsolationStr);
    }
    emitRequireInstDiagnostics();
  }

  void
  emitNamedIsolationCrossingError(SILLocation loc, Identifier name,
                                  SILIsolationInfo namedValuesIsolationInfo,
                                  ApplyIsolationCrossing isolationCrossing,
                                  const ValueDecl *callee) {
    // Emit the short error.
    diagnoseError(loc, diag::regionbasedisolation_named_send_yields_race, name)
        .limitBehaviorIf(getBehaviorLimit());

    // Then emit the note with greater context.
    auto descriptiveKindStr =
        namedValuesIsolationInfo.printForDiagnostics(getFunction());
    auto calleeIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), isolationCrossing.getCalleeIsolation());
    auto callerIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), isolationCrossing.getCallerIsolation());
    bool isDisconnected = namedValuesIsolationInfo.isDisconnected() ||
                          namedValuesIsolationInfo.isTaskIsolated();

    diagnoseNote(loc,
                 diag::regionbasedisolation_named_info_send_yields_race_callee,
                 isDisconnected, name, descriptiveKindStr, calleeIsolationStr,
                 callee, callerIsolationStr);
    emitRequireInstDiagnostics();
  }

  void emitNamedAsyncLetNoIsolationCrossingError(SILLocation loc,
                                                 Identifier name) {
    // Emit the short error.
    diagnoseError(loc, diag::regionbasedisolation_named_send_yields_race, name)
        .limitBehaviorIf(getBehaviorLimit());

    diagnoseNote(
        loc, diag::regionbasedisolation_named_nonisolated_asynclet_name, name);
    emitRequireInstDiagnostics();
  }

  void emitTypedIsolationCrossing(SILLocation loc, Type inferredType,
                                  ApplyIsolationCrossing isolationCrossing) {
    diagnoseError(loc, diag::regionbasedisolation_type_send_yields_race,
                  inferredType)
        .limitBehaviorIf(getBehaviorLimit());

    auto calleeIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), isolationCrossing.getCalleeIsolation());
    auto callerIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), isolationCrossing.getCallerIsolation());

    if (auto callee = getSendingCallee()) {
      diagnoseNote(loc, diag::regionbasedisolation_type_use_after_send_callee,
                   inferredType, calleeIsolationStr, callee.value(),
                   callerIsolationStr);
    } else {
      diagnoseNote(loc, diag::regionbasedisolation_type_use_after_send,
                   inferredType, calleeIsolationStr, callerIsolationStr);
    }
    emitRequireInstDiagnostics();
  }

  void emitNamedUseofStronglySentValue(SILLocation loc, Identifier name) {
    // Emit the short error.
    diagnoseError(loc, diag::regionbasedisolation_named_send_yields_race, name)
        .limitBehaviorIf(getBehaviorLimit());

    // Then emit the note with greater context.
    diagnoseNote(
        loc, diag::regionbasedisolation_named_value_used_after_explicit_sending,
        name);

    // Finally the require points.
    emitRequireInstDiagnostics();
  }

  void emitTypedUseOfStronglySentValue(SILLocation loc, Type inferredType) {
    diagnoseError(loc, diag::regionbasedisolation_type_send_yields_race,
                  inferredType)
        .limitBehaviorIf(getBehaviorLimit());
    if (auto callee = getSendingCallee()) {
      diagnoseNote(loc,
                   diag::regionbasedisolation_typed_use_after_sending_callee,
                   inferredType, callee.value());
    } else {
      diagnoseNote(loc, diag::regionbasedisolation_typed_use_after_sending,
                   inferredType);
    }
    emitRequireInstDiagnostics();
  }

  /// Emit an error when a closure capturing a mutable variable (box) is sent
  /// as a 'sending' argument and the variable is used afterwards.
  void emitSendingClosureCapturesMutableVar(SILLocation errorLoc,
                                            Type closureType,
                                            Operand *actualUse,
                                            SILArgument *fArg,
                                            bool isAutoclosure) {
    if (isAutoclosure) {
      diagnoseError(
          actualUse->getUser()->getLoc(),
          diag::regionbasedisolation_sent_autoclosure_captures_mutable_var,
          fArg->getDecl())
          .limitBehaviorIf(getBehaviorLimit());
    } else {
      diagnoseError(
          actualUse->getUser()->getLoc(),
          diag::regionbasedisolation_sent_closure_captures_mutable_var,
          fArg->getDecl())
          .limitBehaviorIf(getBehaviorLimit());
    }
    emitRequireInstDiagnostics();
  }

  /// Emit an error when a closure capturing a non-Sendable value is sent as a
  /// 'sending' argument and the value is used afterwards.
  void emitSendingClosureCapturesValue(SILLocation errorLoc, Type closureType,
                                       Operand *actualUse, SILArgument *fArg,
                                       bool isAutoclosure) {
    if (isAutoclosure) {
      diagnoseError(
          actualUse->getUser()->getLoc(),
          diag::regionbasedisolation_sent_autoclosure_captures_value,
          fArg->getDecl()->getName())
          .limitBehaviorIf(getBehaviorLimit());
    } else {
      diagnoseError(actualUse->getUser()->getLoc(),
                    diag::regionbasedisolation_sent_closure_captures_value,
                    fArg->getDecl()->getName())
          .limitBehaviorIf(getBehaviorLimit());
    }
    emitRequireInstDiagnostics();
  }

  /// Emit an error when a closure capturing multiple non-Sendable values is
  /// sent as a 'sending' argument and the values are used afterwards.
  void emitSendingClosureCapturesMultipleValues(
      SILLocation errorLoc, Type closureType,
      ArrayRef<std::pair<Operand *, SILArgument *>> capturedValues,
      bool isAutoclosure) {
    // Emit the rest of the capturedValues.
    for (auto pair : capturedValues) {
      auto &use = pair.first;
      auto *arg = cast<SILFunctionArgument>(pair.second);

      if (auto boxTy = arg->getType().getAs<SILBoxType>();
          boxTy && boxTy->getNumFields() == 1 && boxTy->isFieldMutable(0) &&
          SILIsolationInfo::boxTypeContainsOnlySendableFields(
              boxTy, arg->getFunction())) {
        if (isAutoclosure) {
          diagnoseError(
              use->getUser()->getLoc(),
              diag::regionbasedisolation_sent_autoclosure_captures_mutable_var,
              arg->getDecl())
              .limitBehaviorIf(getBehaviorLimit());
        } else {
          diagnoseError(
              use->getUser()->getLoc(),
              diag::regionbasedisolation_sent_closure_captures_mutable_var,
              arg->getDecl())
              .limitBehaviorIf(getBehaviorLimit());
        }
        continue;
      }

      if (isAutoclosure) {
        diagnoseError(
            use->getUser()->getLoc(),
            diag::regionbasedisolation_sent_autoclosure_captures_value,
            arg->getDecl()->getName())
            .limitBehaviorIf(getBehaviorLimit());
      } else {
        diagnoseError(use->getUser()->getLoc(),
                      diag::regionbasedisolation_sent_closure_captures_value,
                      arg->getDecl()->getName())
            .limitBehaviorIf(getBehaviorLimit());
      }
    }

    emitRequireInstDiagnostics();
  }

  void emitNamedIsolationCrossingDueToCapture(
      SILLocation loc, Identifier name,
      SILIsolationInfo namedValuesIsolationInfo,
      ApplyIsolationCrossing isolationCrossing) {
    // Emit the short error.
    diagnoseError(loc, diag::regionbasedisolation_named_send_yields_race, name)
        .limitBehaviorIf(getBehaviorLimit());

    auto descriptiveKindStr =
        namedValuesIsolationInfo.printForDiagnostics(getFunction());
    auto calleeIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), isolationCrossing.getCalleeIsolation());
    auto callerIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), isolationCrossing.getCallerIsolation());
    bool isDisconnected = namedValuesIsolationInfo.isDisconnected() ||
                          namedValuesIsolationInfo.isTaskIsolated();
    bool isTaskIsolated = namedValuesIsolationInfo.isTaskIsolated();
    diagnoseNote(loc,
                 diag::regionbasedisolation_named_isolated_closure_yields_race,
                 isDisconnected, descriptiveKindStr, name, calleeIsolationStr,
                 callerIsolationStr, isTaskIsolated);
    emitRequireInstDiagnostics();
  }

  void emitTypedIsolationCrossingDueToCapture(
      SILLocation loc, Type inferredType,
      ApplyIsolationCrossing isolationCrossing) {
    diagnoseError(loc, diag::regionbasedisolation_type_send_yields_race,
                  inferredType)
        .limitBehaviorIf(getBehaviorLimit());

    auto calleeIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), isolationCrossing.getCalleeIsolation());
    auto callerIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), isolationCrossing.getCallerIsolation());

    diagnoseNote(loc,
                 diag::regionbasedisolation_type_isolated_capture_yields_race,
                 inferredType, calleeIsolationStr, callerIsolationStr);
    emitRequireInstDiagnostics();
  }

  void emitUnknownPatternError() {
    EMIT_UNKNOWN_PATTERN_ERROR(UseAfterSendDiagnosticEmitter,
                               sendingOp->getUser(), getBehaviorLimit(),
                               /*pushToFuture=*/false);
  }

private:
  ASTContext &getASTContext() const {
    return sendingOp->getFunction()->getASTContext();
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SourceLoc loc, Diag<T...> diag,
                                   U &&...args) {
    emittedErrorDiagnostic = true;
    return std::move(getASTContext()
                         .Diags.diagnose(loc, diag, std::forward<U>(args)...)
                         .warnUntilLanguageMode(LanguageMode::v6));
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILLocation loc, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SourceLoc loc, Diag<T...> diag, U &&...args) {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILLocation loc, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILInstruction *inst, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  void emitRequireInstDiagnostics() {
    // Now actually emit the require notes.
    while (!requireInsts.empty()) {
      auto require = requireInsts.pop_back_val();
      switch (require.getKind()) {
      case RequireInst::UseAfterSend:
        diagnoseNote(*require, diag::regionbasedisolation_maybe_race);
        continue;
      case RequireInst::InOutReinitializationNeeded:
        diagnoseNote(
            *require,
            diag::regionbasedisolation_inout_sending_must_be_reinitialized);
        continue;
      }
      llvm_unreachable("Covered switch isn't covered?!");
    }
  }
};

class UseAfterSendDiagnosticInferrer {
  Operand *sendingOp;
  UseAfterSendDiagnosticEmitter diagnosticEmitter;
  RegionAnalysisValueMap &valueMap;
  SendingOperandToStateMap &sendingOpToStateMap;
  SILLocation baseLoc = SILLocation::invalid();
  Type baseInferredType;

  struct AutoClosureWalker;

public:
  UseAfterSendDiagnosticInferrer(Operand *sendOp,
                                 SmallVectorImpl<RequireInst> &requireInsts,
                                 RegionAnalysisValueMap &valueMap,
                                 SendingOperandToStateMap &sendingOpToStateMap)
      : sendingOp(sendOp), diagnosticEmitter(sendOp, requireInsts),
        valueMap(valueMap), sendingOpToStateMap(sendingOpToStateMap),
        baseLoc(sendOp->getUser()->getLoc()),
        baseInferredType(sendOp->get()->getType().getASTType()) {}
  void infer();

  Operand *getSendingOperand() const { return sendingOp; }

private:
  bool initForIsolatedPartialApply(Operand *op, AbstractClosureExpr *ace);
  bool initForSendingPartialApply(FullApplySite fas, Operand *callsiteOp);

  void initForApply(Operand *op, ApplyExpr *expr);
  void initForAutoclosure(Operand *op, AutoClosureExpr *expr);
};

} // namespace

bool UseAfterSendDiagnosticInferrer::initForIsolatedPartialApply(
    Operand *op, AbstractClosureExpr *ace) {
  auto diagnosticPair = findClosureUse(op);
  if (!diagnosticPair) {
    return false;
  }

  auto *diagnosticOp = diagnosticPair->first;

  ApplyIsolationCrossing crossing(
      op->getFunction()->getActorIsolation(),
      diagnosticOp->getFunction()->getActorIsolation());

  auto &state = sendingOpToStateMap.get(sendingOp);
  if (auto rootValueAndName = inferNameAndRootHelper(sendingOp->get())) {
    diagnosticEmitter.emitNamedIsolationCrossingDueToCapture(
        diagnosticOp->getUser()->getLoc(), rootValueAndName->first,
        state.getIsolationInfo(), crossing);
    return true;
  }

  diagnosticEmitter.emitTypedIsolationCrossingDueToCapture(
      diagnosticOp->getUser()->getLoc(), baseInferredType, crossing);
  return true;
}

bool UseAfterSendDiagnosticInferrer::initForSendingPartialApply(
    FullApplySite fas, Operand *callsiteOp) {
  // See if the sending operand is a partial_apply (closure literal or
  // autoclosure expr).
  auto *sendingPAI =
      dyn_cast<PartialApplyInst>(stripFunctionConversions(callsiteOp->get()));
  if (!sendingPAI || (!sendingPAI->getLoc().getAsASTNode<ClosureExpr>() &&
                      !sendingPAI->getLoc().getAsASTNode<AutoClosureExpr>()))
    return false;

  bool isAutoclosure =
      sendingPAI->getLoc().getAsASTNode<AutoClosureExpr>() != nullptr;

  // Loop over captured parameters looking for non-sendable captured values.
  SmallVector<Operand *, 8> nonSendableOps;
  for (auto &sendingPAIOp : sendingPAI->getArgumentOperands()) {
    auto trackableValue = valueMap.getTrackableValue(sendingPAIOp.get());
    if (trackableValue.value.isSendable())
      continue;
    // Skip nonisolated(unsafe) captures -- the user has opted out of isolation
    // checking for these values, so they should not be diagnosed as
    // non-Sendable captures of a sending closure.
    if (trackableValue.value.getIsolationRegionInfo().isUnsafeNonIsolated())
      continue;
    nonSendableOps.push_back(&sendingPAIOp);
  }

  // If we did not find any non-sendable captured values, bail.
  if (nonSendableOps.empty())
    return false;

  // Find the closure use for each of our captures.
  SmallVector<std::pair<Operand *, SILArgument *>, 4> capturedValues;
  for (auto *captured : nonSendableOps) {
    if (auto capturedValue = findClosureUse(captured))
      capturedValues.push_back(*capturedValue);
  }

  if (capturedValues.empty())
    return false;

  // Then emit the closure capture diagnostic.
  diagnosticEmitter.emitSendingClosureCapturesMultipleValues(
      baseLoc, baseInferredType, capturedValues, isAutoclosure);
  return true;
}

void UseAfterSendDiagnosticInferrer::initForApply(Operand *op,
                                                  ApplyExpr *sourceApply) {
  auto isolationCrossing = sourceApply->getIsolationCrossing().value();

  // Grab out full apply site and see if we can find a better expr.
  SILInstruction *i = const_cast<SILInstruction *>(op->getUser());
  auto fai = FullApplySite::isa(i);

  assert(!fai.getArgumentConvention(*op).isIndirectOutParameter() &&
         "An indirect out parameter is never sent");
  auto *foundExpr = inferArgumentExprFromApplyExpr(sourceApply, fai, op);

  auto inferredArgType =
      foundExpr ? foundExpr->findOriginalType() : baseInferredType;
  diagnosticEmitter.emitTypedIsolationCrossing(baseLoc, inferredArgType,
                                               isolationCrossing);
}

/// This walker visits an AutoClosureExpr and looks for uses of a specific
/// captured value. We want to error on the uses in the autoclosure.
struct UseAfterSendDiagnosticInferrer::AutoClosureWalker : ASTWalker {
  UseAfterSendDiagnosticInferrer &foundTypeInfo;
  ValueDecl *targetDecl;
  SILIsolationInfo targetDeclIsolationInfo;
  SmallPtrSet<Expr *, 8> visitedCallExprDeclRefExprs;

  AutoClosureWalker(UseAfterSendDiagnosticInferrer &foundTypeInfo,
                    ValueDecl *targetDecl,
                    SILIsolationInfo targetDeclIsolationInfo)
      : foundTypeInfo(foundTypeInfo), targetDecl(targetDecl),
        targetDeclIsolationInfo(targetDeclIsolationInfo) {}

  Expr *lookThroughArgExpr(Expr *expr) {
    while (true) {
      if (auto *memberRefExpr = dyn_cast<MemberRefExpr>(expr)) {
        expr = memberRefExpr->getBase();
        continue;
      }

      if (auto *cvt = dyn_cast<ImplicitConversionExpr>(expr)) {
        expr = cvt->getSubExpr();
        continue;
      }

      if (auto *e = dyn_cast<ForceValueExpr>(expr)) {
        expr = e->getSubExpr();
        continue;
      }

      if (auto *t = dyn_cast<TupleElementExpr>(expr)) {
        expr = t->getBase();
        continue;
      }

      return expr;
    }
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
    if (auto *declRef = dyn_cast<DeclRefExpr>(expr)) {
      // If this decl ref expr was not visited as part of a callExpr and is our
      // target decl... emit a simple async let error.
      if (!visitedCallExprDeclRefExprs.count(declRef)) {
        if (declRef->getDecl() == targetDecl) {
          foundTypeInfo.diagnosticEmitter
              .emitNamedAsyncLetNoIsolationCrossingError(
                  foundTypeInfo.baseLoc, targetDecl->getBaseIdentifier());
          return Action::Continue(expr);
        }
      }
    }

    // If we have a call expr, see if any of its arguments will cause our sent
    // value to be sent into another isolation domain.
    if (auto *callExpr = dyn_cast<CallExpr>(expr)) {
      // Search callExpr's arguments to see if we have our targetDecl.
      auto *argList = callExpr->getArgs();
      for (auto pair : llvm::enumerate(argList->getArgExprs())) {
        auto *arg = lookThroughArgExpr(pair.value());
        auto *declRef = dyn_cast<DeclRefExpr>(arg);
        if (!declRef)
          continue;

        if (declRef->getDecl() != targetDecl)
          continue;

        // Found our target!
        visitedCallExprDeclRefExprs.insert(declRef);

        auto isolationCrossing = callExpr->getIsolationCrossing();

        // If we do not have an isolation crossing, then we must be just sending
        // a value in a nonisolated fashion into an async let. So emit the
        // simple async let error.
        if (!isolationCrossing) {
          foundTypeInfo.diagnosticEmitter
              .emitNamedAsyncLetNoIsolationCrossingError(
                  foundTypeInfo.baseLoc, targetDecl->getBaseIdentifier());
          continue;
        }

        // Otherwise, we are calling an actor-isolated function in the async
        // let. Emit a better error.

        // See if we can find a valueDecl/name for our callee so we can
        // emit a nicer error.
        ConcreteDeclRef concreteDecl =
            callExpr->getDirectCallee()->getReferencedDecl();

        // If we do not find a direct one, see if we are calling a method
        // on a nominal type.
        if (!concreteDecl) {
          if (auto *dot =
                  dyn_cast<DotSyntaxCallExpr>(callExpr->getDirectCallee())) {
            concreteDecl = dot->getSemanticFn()->getReferencedDecl();
          }
        }

        if (!concreteDecl)
          continue;

        auto *valueDecl = concreteDecl.getDecl();
        assert(valueDecl && "Should be non-null if concreteDecl is valid");

        if (auto isolationCrossing = callExpr->getIsolationCrossing()) {
          // If we have an isolation crossing, use that information.
          if (valueDecl->hasName()) {
            foundTypeInfo.diagnosticEmitter.emitNamedIsolationCrossingError(
                foundTypeInfo.baseLoc, targetDecl->getBaseIdentifier(),
                targetDeclIsolationInfo, *isolationCrossing, valueDecl);
            continue;
          }

          // Otherwise default back to the "callee" error.
          foundTypeInfo.diagnosticEmitter.emitNamedIsolationCrossingError(
              foundTypeInfo.baseLoc, targetDecl->getBaseIdentifier(),
              targetDeclIsolationInfo, *isolationCrossing);
          continue;
        }
      }
    }

    return Action::Continue(expr);
  }
};

void UseAfterSendDiagnosticInferrer::infer() {
  // Otherwise, see if our operand's instruction is a sending parameter.
  if (auto fas = FullApplySite::isa(sendingOp->getUser())) {
    assert(!fas.getArgumentConvention(*sendingOp).isIndirectOutParameter() &&
           "We should never send an indirect out parameter");
    if (fas.getArgumentParameterInfo(*sendingOp)
            .hasOption(SILParameterInfo::Sending)) {

      // Before we try anything else, see if we are passing a closure literal
      // that captures non-Sendable values. If so, emit a diagnostic that
      // identifies the specific captured values.
      if (initForSendingPartialApply(fas, sendingOp))
        return;

      // First try to do the named diagnostic if we can find a name.
      if (auto rootValueAndName = inferNameAndRootHelper(sendingOp->get())) {
        return diagnosticEmitter.emitNamedUseofStronglySentValue(
            baseLoc, rootValueAndName->first);
      }

      // See if we have an ApplyExpr and if we can infer a better type.
      Type type = baseInferredType;
      if (auto *applyExpr =
              sendingOp->getUser()->getLoc().getAsASTNode<ApplyExpr>()) {
        if (auto *foundExpr =
                inferArgumentExprFromApplyExpr(applyExpr, fas, sendingOp))
          type = foundExpr->findOriginalType();
      }

      // Otherwise, emit the typed diagnostic.
      return diagnosticEmitter.emitTypedUseOfStronglySentValue(baseLoc, type);
    }
  }

  auto loc = sendingOp->getUser()->getLoc();

  // If we have a partial_apply that is actor isolated, see if we found a
  // send error due to us sending a value into the partial apply.
  if (auto *ace = loc.getAsASTNode<AbstractClosureExpr>()) {
    if (ace->getActorIsolation().isActorIsolated()) {
      if (initForIsolatedPartialApply(sendingOp, ace)) {
        return;
      }
    }
  }

  if (auto *sourceApply = loc.getAsASTNode<ApplyExpr>()) {
    // Before we do anything further, see if we can find a name and emit a name
    // error.
    if (auto rootValueAndName = inferNameAndRootHelper(sendingOp->get())) {
      auto &state = sendingOpToStateMap.get(sendingOp);
      return diagnosticEmitter.emitNamedIsolationCrossingError(
          baseLoc, rootValueAndName->first, state.getIsolationInfo(),
          *sourceApply->getIsolationCrossing());
    }

    // Otherwise, try to infer from the ApplyExpr.
    return initForApply(sendingOp, sourceApply);
  }

  if (auto fas = FullApplySite::isa(sendingOp->getUser())) {
    if (auto isolationCrossing = fas.getIsolationCrossing()) {
      return diagnosticEmitter.emitTypedIsolationCrossing(
          baseLoc, baseInferredType, *isolationCrossing);
    }
  }

  if (auto *pai = dyn_cast<PartialApplyInst>(sendingOp->getUser())) {
    if (pai->isCalledOnce() && ApplySite(pai)
                                   .getParamInfoForOperand(*sendingOp)
                                   .hasOption(SILParameterInfo::Sending)) {
      if (auto rootValueAndName = inferNameAndRootHelper(sendingOp->get())) {
        return diagnosticEmitter.emitNamedUseofStronglySentValue(
            baseLoc, rootValueAndName->first);
      }
      return diagnosticEmitter.emitTypedUseOfStronglySentValue(
          baseLoc, baseInferredType);
    }
  }

  auto *autoClosureExpr = loc.getAsASTNode<AutoClosureExpr>();
  if (!autoClosureExpr) {
    return diagnosticEmitter.emitUnknownPatternError();
  }

  auto *i = sendingOp->getUser();
  auto pai = ApplySite::isa(i);
  unsigned captureIndex = pai.getASTAppliedArgIndex(*sendingOp);

  auto &state = sendingOpToStateMap.get(sendingOp);
  auto captureInfo =
      autoClosureExpr->getCaptureInfo().getCaptures()[captureIndex];
  auto *captureDecl = captureInfo.getDecl();
  AutoClosureWalker walker(*this, captureDecl, state.getIsolationInfo());
  autoClosureExpr->walk(walker);
}

// Top level entrypoint for use after send diagnostics.
void SendNonSendableImpl::emitUseAfterSendDiagnostics() {
  auto *function = info->getFunction();
  BasicBlockData<BlockLivenessInfo> blockLivenessInfo(function);
  // We use a generation counter so we can lazily reset blockLivenessInfo
  // since we cannot clear it without iterating over it.
  unsigned blockLivenessInfoGeneration = 0;

  if (sendingOpToRequireInstMultiMap.empty())
    return;

  REGIONBASEDISOLATION_LOG(
      llvm::dbgs() << "Emitting Error. Kind: Use After Send diagnostics.\n");

  for (auto [sendingOp, requireInsts] :
       sendingOpToRequireInstMultiMap.getRange()) {
    REGIONBASEDISOLATION_LOG(
        llvm::dbgs() << "Sending Op. Number: " << sendingOp->getOperandNumber()
                     << ". User: " << *sendingOp->getUser());

    // Then look for our requires before we emit any error. We want to emit a
    // single we don't understand error if we do not find the require.
    bool didEmitRequireNote = false;
    InstructionSet requireInstsUnique(function);
    RequireLiveness liveness(blockLivenessInfoGeneration, sendingOp,
                             blockLivenessInfo);
    ++blockLivenessInfoGeneration;
    liveness.process(requireInsts);

    SmallVector<RequireInst, 8> requireInstsForError;
    for (auto require : requireInsts) {
      // We can have multiple of the same require insts if we had a require
      // and an assign from the same instruction. Our liveness checking
      // above doesn't care about that, but we still need to make sure we do
      // not emit twice.
      if (!requireInstsUnique.insert(*require))
        continue;

      // If this was not a last require, do not emit an error.
      if (!liveness.finalRequires.contains(*require))
        continue;

      requireInstsForError.push_back(require);
      didEmitRequireNote = true;
    }

    // If we did not emit a require, emit an "unknown pattern" error that
    // tells the user to file a bug. This importantly ensures that we can
    // guarantee that we always find the require if we successfully compile.
    if (!didEmitRequireNote) {
      auto behaviorLimit =
          sendingOp->get()->getType().getConcurrencyDiagnosticBehavior(
              function);
      EMIT_UNKNOWN_PATTERN_ERROR(emitUseAfterSendDiagnostics,
                                 sendingOp->getUser(), behaviorLimit,
                                 /*pushToFuture=*/false);
      continue;
    }

    UseAfterSendDiagnosticInferrer diagnosticInferrer(
        sendingOp, requireInstsForError, info->getValueMap(),
        info->getSendingOperandToStateMap());
    diagnosticInferrer.infer();
  }
}

//===----------------------------------------------------------------------===//
//            MARK: Send Never-Sent Diagnostic Inference
//===----------------------------------------------------------------------===//

namespace {

/// Helper class that provides the core diagnostic emission logic for
/// SentNeverSendable errors.
///
/// This class handles the mechanics of emitting diagnostics: tracking whether
/// an error was emitted, providing diagnostic convenience methods, and emitting
/// an "unknown pattern" error if no specific diagnostic was produced. It's used
/// as a composition member by SentNeverSendableDiagnosticEmitter, which handles
/// the higher-level logic of determining WHAT diagnostic to emit based on the
/// error context.
///
/// The separation exists because the diagnostic emission logic is reusable,
/// while the inference logic for determining the appropriate diagnostic varies.
class SendNeverSentDiagnosticEmitterHelper {
  /// The use that actually caused the send.
  Operand *sendingOperand;

  /// The never-sent value that is in the same region as \p
  /// sendingOperand.get().
  llvm::PointerUnion<SILValue, SILInstruction *> neverSent;

  /// The region info that describes the dynamic dataflow derived isolation
  /// region info for the never-sent value.
  ///
  /// This is equal to the merge of the IsolationRegionInfo from all elements in
  /// the never-sent value's region when the error was diagnosed.
  SILDynamicMergedIsolationInfo isolationRegionInfo;

  bool emittedErrorDiagnostic = false;

public:
  SendNeverSentDiagnosticEmitterHelper(
      Operand *sendingOperand,
      llvm::PointerUnion<SILValue, SILInstruction *> neverSent,
      SILDynamicMergedIsolationInfo isolationRegionInfo)
      : sendingOperand(sendingOperand), neverSent(neverSent),
        isolationRegionInfo(isolationRegionInfo) {}

  ~SendNeverSentDiagnosticEmitterHelper() {
    if (!emittedErrorDiagnostic) {
      emitUnknownPatternError();
    } else if (auto proto = isolationRegionInfo.getIsolationInfo()
                   .getIsolatedConformance()) {
       // If the diagnostic comes from a (potentially) isolated conformance,
       // add a note saying so and indicating where the isolated conformance
       // can come in.
       if (auto value = isolationRegionInfo.getIsolationInfo().getIsolatedValue()) {
         if (auto loc = value.getLoc()) {
           diagnoseNote(
               loc, diag::regionbasedisolation_isolated_conformance_introduced,
               proto);
         }
       }
    }
  }

  Operand *getOperand() const { return sendingOperand; }

  SILFunction *getFunction() const { return getOperand()->getFunction(); }

  SILValue getNeverSentValue() const { return neverSent.dyn_cast<SILValue>(); }

  SILInstruction *getNeverSentActorIntroducingInst() const {
    return neverSent.dyn_cast<SILInstruction *>();
  }

  /// Return Warning iff the consuming apply's `ApplyExpr` isolation
  /// crossing is preconcurrency on either side.
  ///
  /// Mirrors the AST-level ActorIsolatedCall policy in
  /// TypeCheckConcurrency.cpp (search for `actor_isolated_call`): when
  /// the SIL pass and the AST diagnostic observe the same crossing,
  /// their severity decisions must agree.
  ///
  /// Read the crossing from the AST `ApplyExpr` rather than the SIL
  /// apply — SILGen does not propagate it onto SIL apply instructions
  /// for synchronous direct calls, so
  /// `ApplySite::getIsolationCrossing()` is typically empty here.
  std::optional<DiagnosticBehavior>
  getApplyPreconcurrencyBehaviorLimit() const {
    auto *applyExpr =
        sendingOperand->getUser()->getLoc().getAsASTNode<ApplyExpr>();
    if (!applyExpr)
      return {};
    auto crossing = applyExpr->getIsolationCrossing();
    if (!crossing)
      return {};
    if (crossing->getCallerIsolation().preconcurrency() ||
        crossing->getCalleeIsolation().preconcurrency())
      return DiagnosticBehavior::Warning;
    return {};
  }

  std::optional<DiagnosticBehavior> getBehaviorLimit() const {
    // If the failure is due to an isolated conformance, downgrade the error
    // to a warning prior to Swift 7.
    if (isolationRegionInfo.getIsolationInfo().getIsolatedConformance() &&
        !sendingOperand->get()
             ->getType()
             .getASTType()
             ->getASTContext()
             .isLanguageModeAtLeast(LanguageMode::future))
      return DiagnosticBehavior::Warning;

    if (auto limit = getApplyPreconcurrencyBehaviorLimit())
      return limit;

    return sendingOperand->get()->getType().getConcurrencyDiagnosticBehavior(
        getOperand()->getFunction());
  }

  /// Attempts to retrieve and return the callee declaration.
  std::optional<const ValueDecl *> getSendingCallee() const {
    return getSendingApplyCallee(sendingOperand->getUser());
  }

  SILLocation getLoc() const { return sendingOperand->getUser()->getLoc(); }

  /// Return the isolation region info for \p getNeverSentValue().
  SILDynamicMergedIsolationInfo getIsolationRegionInfo() const {
    return isolationRegionInfo;
  }

  void emitUnknownPatternError() {
    emittedErrorDiagnostic = true;
    EMIT_UNKNOWN_PATTERN_ERROR(SendNeverSentDiagnosticEmitter,
                               getOperand()->getUser(), getBehaviorLimit(),
                               /*pushToFuture=*/false);
  }

  void emitUnknownUse(SILLocation loc) {
    // TODO: This will eventually be an unknown pattern error.
    diagnoseError(loc, diag::regionbasedisolation_task_or_actor_isolated_sent)
        .limitBehaviorIf(getBehaviorLimit());
  }

  void emitPassToApply(SILLocation loc, Type inferredType,
                       ApplyIsolationCrossing crossing) {
    diagnoseError(loc, diag::regionbasedisolation_type_send_yields_race,
                  inferredType)
        .limitBehaviorIf(getBehaviorLimit());

    auto descriptiveKindStr =
        getIsolationRegionInfo().printForDiagnostics(getFunction());
    auto calleeIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), crossing.getCalleeIsolation());

    if (auto callee = getSendingCallee()) {
      diagnoseNote(
          loc,
          diag::regionbasedisolation_typed_sendneversendable_via_arg_callee,
          descriptiveKindStr, inferredType, calleeIsolationStr, callee.value(),
          getIsolationRegionInfo()->isTaskIsolated());
    } else {
      diagnoseNote(loc,
                   diag::regionbasedisolation_typed_sendneversendable_via_arg,
                   descriptiveKindStr, inferredType, calleeIsolationStr,
                   getIsolationRegionInfo()->isTaskIsolated());
    }
  }

  void emitNamedFunctionArgumentClosure(SILLocation loc, Identifier name,
                                        ApplyIsolationCrossing crossing) {
    emitNamedOnlyError(loc, name);

    auto descriptiveKindStr =
        getIsolationRegionInfo().printForDiagnostics(getFunction());
    auto calleeIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), crossing.getCalleeIsolation());
    auto callerIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), crossing.getCallerIsolation());
    bool isDisconnected = getIsolationRegionInfo().isDisconnected() ||
                          getIsolationRegionInfo()->isTaskIsolated();
    bool isTaskIsolated = getIsolationRegionInfo()->isTaskIsolated();
    diagnoseNote(loc,
                 diag::regionbasedisolation_named_isolated_closure_yields_race,
                 isDisconnected, descriptiveKindStr, name, calleeIsolationStr,
                 callerIsolationStr, isTaskIsolated);
  }

  void
  emitNamedFunctionArgumentClosureMutable(SILLocation loc, Identifier name,
                                          ApplyIsolationCrossing crossing) {
    emitNamedOnlyError(loc, name);

    auto calleeIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), crossing.getCalleeIsolation());
    auto callerIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), crossing.getCallerIsolation());
    diagnoseNote(
        loc,
        diag::regionbasedisolation_named_isolated_closure_yields_race_mutable,
        calleeIsolationStr, name, getIsolationRegionInfo()->isTaskIsolated(),
        callerIsolationStr);
  }

  void emitTypedSendingNeverSendableToSendingParam(SILLocation loc,
                                                   Type inferredType) {
    diagnoseError(loc, diag::regionbasedisolation_type_send_yields_race,
                  inferredType)
        .limitBehaviorIf(getBehaviorLimit());

    auto descriptiveKindStr =
        getIsolationRegionInfo().printForDiagnostics(getFunction());

    if (auto callee = getSendingCallee()) {
      diagnoseNote(
          loc, diag::regionbasedisolation_typed_tns_passed_to_sending_callee,
          descriptiveKindStr, inferredType, callee.value());
    } else {
      diagnoseNote(loc, diag::regionbasedisolation_typed_tns_passed_to_sending,
                   descriptiveKindStr, inferredType);
    }
  }

  /// Emit an error for a case where we have captured a value like an actor and
  /// thus a sending closure has become actor isolated (and thus unable to be
  /// sent).
  void emitClosureErrorWithCapturedActor(Operand *partialApplyOp,
                                         Operand *actualUse,
                                         SILArgument *fArg) {
    auto descriptiveKindStr =
        getIsolationRegionInfo().printForCodeDiagnostic(getFunction());

    diagnoseError(partialApplyOp,
                  diag::regionbasedisolation_typed_tns_passed_sending_closure,
                  descriptiveKindStr)
        .limitBehaviorIf(getDiagnosticBehaviorLimitForOperands(
            actualUse->getFunction(), {actualUse}));

    descriptiveKindStr =
        getIsolationRegionInfo().printForDiagnostics(getFunction());
    diagnoseNote(actualUse, diag::regionbasedisolation_closure_captures_actor,
                 fArg->getDecl()->getName(), descriptiveKindStr);
  }

  /// Emit a typed error for an isolated closure being passed as a sending
  /// parameter.
  ///
  /// \arg partialApplyOp the operand of the outermost partial apply.
  /// \arg actualUse the operand inside the closure that actually caused the
  /// capture to occur. This maybe inside a different function from the partial
  /// apply since we want to support a use inside a recursive closure.
  void emitSendingClosureParamDirectlyIsolated(Operand *partialApplyOp,
                                               Operand *actualUse,
                                               SILArgument *fArg) {
    auto descriptiveKindStr =
        getIsolationRegionInfo().printForCodeDiagnostic(getFunction());

    diagnoseError(partialApplyOp,
                  diag::regionbasedisolation_typed_tns_passed_sending_closure,
                  descriptiveKindStr)
        .limitBehaviorIf(getDiagnosticBehaviorLimitForOperands(
            actualUse->getFunction(), {actualUse}));

    // If we have a closure capture box, emit a special diagnostic.
    if (getIsolationRegionInfo().getIsolationInfo().isTaskIsolated()) {
      if (cast<SILFunctionArgument>(fArg)->isClosureCapture() &&
          fArg->getType().is<SILBoxType>()) {
        auto diag = diag::
            regionbasedisolation_typed_tns_passed_to_sending_closure_helper_have_boxed_value_task_isolated;
        diagnoseNote(actualUse, diag, fArg->getDecl());
        return;
      }

      auto diag = diag::
          regionbasedisolation_typed_tns_passed_to_sending_closure_helper_have_value_task_isolated;
      diagnoseNote(actualUse, diag, fArg->getDecl()->getName());
      return;
    }

    descriptiveKindStr =
        getIsolationRegionInfo().printForDiagnostics(getFunction());

    auto diag = diag::
        regionbasedisolation_typed_tns_passed_to_sending_closure_helper_have_value;
    diagnoseNote(actualUse, diag, descriptiveKindStr,
                 fArg->getDecl()->getName());
  }

  void emitSendingClosureMultipleCapturedOperandError(
      SILLocation loc, ArrayRef<Operand *> capturedOperands) {
    // Our caller should have passed at least one operand. Emit an unknown error
    // to signal we need a bug report.
    if (capturedOperands.empty()) {
      emitUnknownPatternError();
      return;
    }

    auto descriptiveKindStr =
        getIsolationRegionInfo()->printForCodeDiagnostic(getFunction());

    auto emitMainError = [&] {
      auto behaviorLimit = getDiagnosticBehaviorLimitForOperands(
          getFunction(), capturedOperands);
      diagnoseError(loc,
                    diag::regionbasedisolation_typed_tns_passed_sending_closure,
                    descriptiveKindStr)
          .limitBehaviorIf(behaviorLimit);
    };

    if (capturedOperands.size() == 1) {
      auto captured = capturedOperands.front();
      auto actualUseInfo = findClosureUse(captured);

      // If we fail to find actual use info, emit an unknown error.
      if (!actualUseInfo) {
        emitUnknownPatternError();
        return;
      }

      if (getIsolationRegionInfo()->isTaskIsolated()) {
        emitMainError();
        auto diag = diag::
            regionbasedisolation_typed_tns_passed_to_sending_closure_helper_have_value_task_isolated;
        diagnoseNote(actualUseInfo->first, diag,
                     actualUseInfo->second->getDecl()->getName());
        return;
      }

      emitMainError();
      descriptiveKindStr =
          getIsolationRegionInfo().printForDiagnostics(getFunction());
      auto diag = diag::
          regionbasedisolation_typed_tns_passed_to_sending_closure_helper_have_value_region;
      diagnoseNote(actualUseInfo->first, diag, descriptiveKindStr,
                   actualUseInfo->second->getDecl()->getName());
      return;
    }

    emitMainError();

    bool emittedDiagnostic = false;
    for (auto captured : capturedOperands) {
      auto actualUseInfo = findClosureUse(captured);
      if (!actualUseInfo)
        continue;
      emittedDiagnostic = true;
      auto diag = diag::
          regionbasedisolation_typed_tns_passed_to_sending_closure_helper_multiple_value;
      diagnoseNote(actualUseInfo->first, diag,
                   actualUseInfo->second->getDecl()->getName());
    }

    // Check if we did not emit a diagnostic. In such a case, we need to emit an
    // unknown patten error so that we get a bug report from the user.
    if (!emittedDiagnostic) {
      emitUnknownPatternError();
    }
  }

  void emitNamedOnlyError(SILLocation loc, Identifier name) {
    diagnoseError(loc, diag::regionbasedisolation_named_send_yields_race, name)
        .limitBehaviorIf(getBehaviorLimit());
  }

  void emitNamedAsyncLetCapture(SILLocation loc, Identifier name,
                                SILIsolationInfo sentValueIsolation) {
    assert(!getIsolationRegionInfo().isDisconnected() &&
           "Should never be disconnected?!");
    emitNamedOnlyError(loc, name);

    auto descriptiveKindStr =
        getIsolationRegionInfo().printForDiagnostics(getFunction());

    diagnoseNote(loc, diag::regionbasedisolation_named_send_nt_asynclet_capture,
                 name, descriptiveKindStr,
                 getIsolationRegionInfo()->isTaskIsolated());
  }

  void emitNamedIsolation(SILLocation loc, Identifier name,
                          ApplyIsolationCrossing isolationCrossing) {
    emitNamedOnlyError(loc, name);

    auto descriptiveKindStr =
        getIsolationRegionInfo().printForDiagnostics(getFunction());
    auto calleeIsolationStr =
        SILIsolationInfo::printActorIsolationForDiagnostics(
            getFunction(), isolationCrossing.getCalleeIsolation());
    bool isDisconnected = getIsolationRegionInfo().isDisconnected() ||
                          getIsolationRegionInfo()->isTaskIsolated();
    bool isTaskIsolated = getIsolationRegionInfo()->isTaskIsolated();

    if (auto callee = getSendingCallee()) {
      diagnoseNote(loc,
                   diag::regionbasedisolation_named_send_never_sendable_callee,
                   isDisconnected, name, descriptiveKindStr, calleeIsolationStr,
                   callee.value(), descriptiveKindStr, isTaskIsolated);
    } else {
      diagnoseNote(loc, diag::regionbasedisolation_named_send_never_sendable,
                   isDisconnected, name, descriptiveKindStr, calleeIsolationStr,
                   descriptiveKindStr, isTaskIsolated);
    }
  }

  void emitNamedSendingNeverSendableToSendingParam(SILLocation loc,
                                                   Identifier varName) {
    emitNamedOnlyError(loc, varName);

    auto descriptiveKindStr =
        getIsolationRegionInfo().printForDiagnostics(getFunction());
    bool isTaskIsolated = getIsolationRegionInfo()->isTaskIsolated();
    bool isDisconnected = getIsolationRegionInfo().isDisconnected() ||
                          isTaskIsolated;
    auto diag = diag::regionbasedisolation_named_send_into_sending_param;
    diagnoseNote(loc, diag, isDisconnected, descriptiveKindStr, varName,
                 isTaskIsolated);
  }

  void emitNamedSendingReturn(SILLocation loc, Identifier varName) {
    emitNamedOnlyError(loc, varName);
    auto descriptiveKindStr =
        getIsolationRegionInfo().printForDiagnostics(getFunction());
    bool isTaskIsolated = getIsolationRegionInfo()->isTaskIsolated();
    bool isDisconnected = getIsolationRegionInfo().isDisconnected() ||
                          isTaskIsolated;
    auto diag = diag::regionbasedisolation_named_nosend_send_into_result;
    diagnoseNote(loc, diag, isDisconnected, descriptiveKindStr, varName,
                 descriptiveKindStr, isTaskIsolated);
  }

private:
  ASTContext &getASTContext() const {
    return getOperand()->getFunction()->getASTContext();
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SourceLoc loc, Diag<T...> diag,
                                   U &&...args) {
    emittedErrorDiagnostic = true;
    return std::move(getASTContext()
                         .Diags.diagnose(loc, diag, std::forward<U>(args)...)
                         .warnUntilLanguageMode(LanguageMode::v6));
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILLocation loc, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(Operand *op, Diag<T...> diag, U &&...args) {
    return diagnoseError(op->getUser()->getLoc(), diag,
                         std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SourceLoc loc, Diag<T...> diag, U &&...args) {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILLocation loc, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILInstruction *inst, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(Operand *op, Diag<T...> diag, U &&...args) {
    return diagnoseNote(op->getUser()->getLoc(), diag,
                        std::forward<U>(args)...);
  }
};

/// Diagnostic emitter for SentNeverSendable errors.
///
/// This is a verbatim error emitter (see PartitionOpError.def) that follows
/// the standard pattern: constructor takes (RegionAnalysisFunctionInfo*, Error)
/// and provides an emit() method.
///
/// The emitter analyzes the error context to determine the most appropriate
/// diagnostic. For example, it handles:
/// - Sending actor-isolated values to functions expecting sendable values
/// - Capturing isolated values in closures passed to sending parameters
/// - Various special cases like autoclosures and async let
class SentNeverSendableDiagnosticEmitter {
  struct AutoClosureWalker;

  RegionAnalysisValueMap &valueMap;
  SendNeverSentDiagnosticEmitterHelper diagnosticEmitter;

  /// The element that was sent. Used to check if it is itself disconnected
  /// (and thus needs isolation history to explain why it is in an isolated
  /// region).
  Element sentElement;

  /// The partition at the point of the error, or None when isolation-history
  /// recording is off for this function and nobody will rewind it.
  std::optional<Partition> partition;

  /// The region analysis for this function; forwarded to the isolation-history
  /// note emitter so it can recover predecessor exit partitions across joins.
  RegionAnalysisFunctionInfo *info;

  using SentNeverSendableError = PartitionOpError::SentNeverSendableError;

public:
  SentNeverSendableDiagnosticEmitter(RegionAnalysisFunctionInfo *info,
                                     SentNeverSendableError &&error)
      : valueMap(info->getValueMap()),
        diagnosticEmitter(error.op->getSourceOp(),
                          valueMap.getRepresentative(error.sentElement),
                          error.isolationRegionInfo),
        sentElement(error.sentElement), partition(std::move(error.partition)),
        info(info) {}

  /// Gathers diagnostics. Returns false if we emitted a "I don't understand
  /// error". If we emit such an error, we should bail without emitting any
  /// further diagnostics, since we may not have any diagnostics or be in an
  /// inconcistent state.
  bool emit();

private:
  /// \p actualCallerIsolation is used to override the caller isolation we use
  /// when emitting the error if the closure would have the incorrect one.
  bool initForIsolatedPartialApply(
      Operand *op, AbstractClosureExpr *ace,
      std::optional<ActorIsolation> actualCallerIsolation = {});

  bool initForSendingPartialApply(FullApplySite fas, Operand *pai);

  std::optional<unsigned>
  getIsolatedValuePartialApplyIndex(PartialApplyInst *pai,
                                    SILValue isolatedValue) {
    for (auto &paiOp : ApplySite(pai).getArgumentOperands()) {
      if (valueMap.getTrackableValue(paiOp.get()).value.getRepresentative() ==
          isolatedValue) {
        return ApplySite(pai).getASTAppliedArgIndex(paiOp);
      }
    }

    return {};
  }

  /// If isolation-history emission is enabled for this function (see
  /// \c shouldEmitIsolationHistoryFor) and \p sentElement is disconnected
  /// (i.e., not itself actor-isolated), walk the isolation history DAG to find
  /// the most recent instruction where the element was merged into an isolated
  /// region and emit a note there.
  void emitIsolationHistoryNoteIfNeeded();
};

} // namespace

void SentNeverSendableDiagnosticEmitter::emitIsolationHistoryNoteIfNeeded() {
  if (!partition)
    return;
  IsolationHistoryNoteEmitter::emit(
      diagnosticEmitter.getOperand()->getFunction(), info, valueMap,
      IsolationHistoryNoteEmitter::Request::howDidBecomeIsolatedTo(
          sentElement, diagnosticEmitter.getIsolationRegionInfo()),
      std::move(*partition));
}

bool SentNeverSendableDiagnosticEmitter::initForSendingPartialApply(
    FullApplySite fas, Operand *callsiteOp) {
  // This is the partial apply that is being passed as a sending parameter.
  auto *sendingPAI =
      dyn_cast<PartialApplyInst>(stripFunctionConversions(callsiteOp->get()));
  if (!sendingPAI)
    return false;

  // Make sure that we only handle closure literals.
  //
  // TODO: This should be marked on closures at the SIL level... I shouldn't
  // have to refer to the AST.
  if (!sendingPAI->getLoc().getAsASTNode<ClosureExpr>())
    return false;

  // Ok, we have a closure literal. First we handle a potential capture of
  // 'self' before we do anything by looping over our captured parameters.
  //
  // DISCUSSION: The reason why we do this early is that as a later heuristic,
  // we check if any of the values are directly task isolated (i.e. they are
  // actually the task isolated value, not a value that is in the same region as
  // something that is task isolated). This could potentially result in us
  // emitting a task isolated error instead of an actor isolated error here if
  // for some reason SILGen makes the self capture come later in the capture
  // list. From a compile time perspective, going over a list of captures twice
  // is not going to hurt especially since we are going to emit a diagnostic
  // here anyways.
  for (auto &sendingPAIOp : sendingPAI->getArgumentOperands()) {
    // NOTE: If we access a field on self in the closure, we will still just
    // capture self... so we do not have to handle that case due to the way
    // SILGen codegens today. This is also true if we use a capture list [x =
    // self.field] (i.e. a closure that captures a field from self is still
    // nonisolated).
    if (!sendingPAIOp.get()->getType().isAnyActor())
      continue;

    auto *fArg = dyn_cast<SILFunctionArgument>(
        lookThroughOwnershipInsts(sendingPAIOp.get()));
    if (!fArg || !fArg->isSelf())
      continue;

    auto capturedValue = findClosureUse(&sendingPAIOp);
    if (!capturedValue) {
      // If we failed to find the direct capture of self, emit an unknown code
      // pattern error so the user knows to send a bug report. This should never
      // fail.
      diagnosticEmitter.emitUnknownPatternError();
      return true;
    }

    // Otherwise, emit our captured actor error.
    diagnosticEmitter.emitClosureErrorWithCapturedActor(
        &sendingPAIOp, capturedValue->first, capturedValue->second);
    return true;
  }

  // Ok, we know that we have a closure expr. We now need to find the specific
  // closure captured value that is actor or task isolated. Then we search for
  // the potentially recursive closure use so we can show a nice loc to the
  // user.
  auto maybeIsolatedValue =
      diagnosticEmitter.getIsolationRegionInfo()->maybeGetIsolatedValue();

  // If we do not find an actual task isolated value while looping below, this
  // contains the non sendable captures of the partial apply that we want to
  // emit a more heuristic based error for. See documentation below.
  SmallVector<Operand *, 8> nonSendableOps;

  for (auto &sendingPAIOp : sendingPAI->getArgumentOperands()) {
    // If our value's rep is task isolated or is the dynamic isolated
    // value... then we are done. This is a 'correct' error value to emit.
    auto trackableValue = valueMap.getTrackableValue(sendingPAIOp.get());
    if (trackableValue.value.isSendable())
      continue;
    if (trackableValue.value.getIsolationRegionInfo().isUnsafeNonIsolated())
      continue;

    auto rep = trackableValue.value.getRepresentative().maybeGetValue();
    nonSendableOps.push_back(&sendingPAIOp);

    if (trackableValue.value.getIsolationRegionInfo().isTaskIsolated() ||
        rep == maybeIsolatedValue) {
      if (auto capturedValue = findClosureUse(&sendingPAIOp)) {
        diagnosticEmitter.emitSendingClosureParamDirectlyIsolated(
            callsiteOp, capturedValue->first, capturedValue->second);
        return true;
      }
    }
  }

  // If we did not find a clear answer in terms of an isolated value, we emit a
  // more general error based on:
  //
  // 1. If we have one non-Sendable value then we know that must be the value.
  // 2. Otherwise, we emit a generic captured non-Sendable value error to give
  //    people something to work off of.
  diagnosticEmitter.emitSendingClosureMultipleCapturedOperandError(
      callsiteOp->getUser()->getLoc(), nonSendableOps);
  return true;
}

bool SentNeverSendableDiagnosticEmitter::initForIsolatedPartialApply(
    Operand *op, AbstractClosureExpr *ace,
    std::optional<ActorIsolation> actualCallerIsolation) {
  auto diagnosticPair = findClosureUse(op);
  if (!diagnosticPair)
    return false;

  auto *diagnosticOp = diagnosticPair->first;

  ApplyIsolationCrossing crossing(
      actualCallerIsolation.value_or(op->getFunction()->getActorIsolation()),
      diagnosticOp->getFunction()->getActorIsolation());

  // We do not need to worry about failing to infer a name here since we are
  // going to be returning some form of a SILFunctionArgument which is always
  // easy to find a name for.
  if (auto rootValueAndName = inferNameAndRootHelper(op->get())) {
    // See if our underlying value is a SILBox type that contains a Sendable
    // type. In that case, we could have only emitted this error if we were
    // escaping the box itself. In such a case, we want to emit a better
    // diagnostic that mentions that the reason we are emitting an error is b/c
    // the value is mutable.
    if (auto boxTy = op->get()->getType().getAs<SILBoxType>();
        boxTy && boxTy->getNumFields() == 1 && boxTy->isFieldMutable(0) &&
        SILIsolationInfo::boxTypeContainsOnlySendableFields(
            boxTy, op->getFunction())) {
      diagnosticEmitter.emitNamedFunctionArgumentClosureMutable(
          diagnosticOp->getUser()->getLoc(), rootValueAndName->first, crossing);
      return true;
    }

    diagnosticEmitter.emitNamedFunctionArgumentClosure(
        diagnosticOp->getUser()->getLoc(), rootValueAndName->first, crossing);
    return true;
  }

  return false;
}

/// This walker visits an AutoClosureExpr and looks for uses of a specific
/// captured value. We want to error on the uses in the autoclosure.
struct SentNeverSendableDiagnosticEmitter::AutoClosureWalker : ASTWalker {
  SendNeverSentDiagnosticEmitterHelper &foundTypeInfo;
  ValueDecl *targetDecl;
  SILIsolationInfo targetDeclIsolationInfo;
  SmallPtrSet<Expr *, 8> visitedCallExprDeclRefExprs;
  SILLocation captureLoc;
  bool isAsyncLet;

  AutoClosureWalker(SendNeverSentDiagnosticEmitterHelper &foundTypeInfo,
                    ValueDecl *targetDecl,
                    SILIsolationInfo targetDeclIsolationInfo,
                    SILLocation captureLoc, bool isAsyncLet)
      : foundTypeInfo(foundTypeInfo), targetDecl(targetDecl),
        targetDeclIsolationInfo(targetDeclIsolationInfo),
        captureLoc(captureLoc), isAsyncLet(isAsyncLet) {}

  Expr *lookThroughArgExpr(Expr *expr) {
    while (true) {
      if (auto *memberRefExpr = dyn_cast<MemberRefExpr>(expr)) {
        expr = memberRefExpr->getBase();
        continue;
      }

      if (auto *cvt = dyn_cast<ImplicitConversionExpr>(expr)) {
        expr = cvt->getSubExpr();
        continue;
      }

      if (auto *e = dyn_cast<ForceValueExpr>(expr)) {
        expr = e->getSubExpr();
        continue;
      }

      if (auto *t = dyn_cast<TupleElementExpr>(expr)) {
        expr = t->getBase();
        continue;
      }

      return expr;
    }
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
    if (auto *declRef = dyn_cast<DeclRefExpr>(expr)) {
      // If this decl ref expr was not visited as part of a callExpr and is our
      // target decl... emit a simple async let error.
      //
      // This occurs if we do:
      //
      // ```
      // let x = ...
      // async let y = x
      // ```
      if (declRef->getDecl() == targetDecl) {
        foundTypeInfo.emitNamedAsyncLetCapture(captureLoc,
                                               targetDecl->getBaseIdentifier(),
                                               targetDeclIsolationInfo);
        return Action::Continue(expr);
      }
    }

    return Action::Continue(expr);
  }
};

bool SentNeverSendableDiagnosticEmitter::emit() {
  // Emit isolation history notes after the primary diagnostic returns.
  SWIFT_DEFER { emitIsolationHistoryNoteIfNeeded(); };

  // We need to find the isolation info.
  auto *op = diagnosticEmitter.getOperand();
  auto loc = op->getUser()->getLoc();

  if (auto *sourceApply = loc.getAsASTNode<ApplyExpr>()) {
    // First see if we have a 'sending' argument.
    if (auto fas = FullApplySite::isa(op->getUser())) {
      if (fas.getArgumentParameterInfo(*op).hasOption(
              SILParameterInfo::Sending)) {
        // Before we do anything, lets see if we are passing a sendable closure
        // literal. If we do, we want to emit a special error that states which
        // captured value caused the actual error.
        if (initForSendingPartialApply(fas, op))
          return true;

        // See if we can infer a name from the value.
        if (auto varName = inferNameHelper(op->get())) {
          diagnosticEmitter.emitNamedSendingNeverSendableToSendingParam(
              loc, *varName);
          return true;
        }

        Type type = op->get()->getType().getASTType();
        if (auto *inferredArgExpr =
                inferArgumentExprFromApplyExpr(sourceApply, fas, op)) {
          type = inferredArgExpr->findOriginalType();
        }

        diagnosticEmitter.emitTypedSendingNeverSendableToSendingParam(loc,
                                                                      type);
        return true;
      }
    }

    // First try to get the apply from the isolation crossing.
    auto isolation = sourceApply->getIsolationCrossing();

    // If we could not infer an isolation...
    if (!isolation) {
      // Otherwise, emit a "we don't know error" that tells the user to file a
      // bug.
      diagnosticEmitter.emitUnknownPatternError();
      return false;
    }
    assert(isolation && "Expected non-null");

    // Then if we are calling a closure expr. If so, we should use the loc of
    // the closure.
    if (auto *closureExpr =
            dyn_cast<AbstractClosureExpr>(sourceApply->getFn())) {
      initForIsolatedPartialApply(op, closureExpr,
                                  isolation->getCallerIsolation());
      return true;
    }

    // See if we can infer a name from the value.
    if (auto name = inferNameHelper(op->get())) {
      diagnosticEmitter.emitNamedIsolation(loc, *name, *isolation);
      return true;
    }

    // Attempt to find the specific sugared ASTType if we can to emit a better
    // diagnostic.
    Type type = op->get()->getType().getASTType();
    if (auto fas = FullApplySite::isa(op->getUser())) {
      if (auto *inferredArgExpr =
              inferArgumentExprFromApplyExpr(sourceApply, fas, op)) {
        type = inferredArgExpr->findOriginalType();
      }
    }

    diagnosticEmitter.emitPassToApply(loc, type, *isolation);
    return true;
  }

  if (auto *ace = loc.getAsASTNode<AbstractClosureExpr>()) {
    if (ace->getActorIsolation().isActorIsolated()) {
      if (initForIsolatedPartialApply(op, ace)) {
        return true;
      }
    }

    // A `sending` capture of an non-isolated `@called(once)` closure is
    // translated by `translateSILCalledOncePartialApply` as a send of
    // that specific operand -- so reaching this operand here means the
    // capture itself crossed isolation, independent of whether the closure
    // as a whole is isolated. Let's use the captured value's tracked
    // isolation (when it is actor-isolated) as the caller isolation.
    if (auto *pai = dyn_cast<PartialApplyInst>(op->getUser());
        pai && pai->isCalledOnce()) {
      ApplySite apply(pai);
      if (apply.getParamInfoForOperand(*op).hasOption(
              SILParameterInfo::Sending)) {
        std::optional<ActorIsolation> callerIsolation;
        if (diagnosticEmitter.getIsolationRegionInfo()->hasActorIsolation())
          callerIsolation =
              diagnosticEmitter.getIsolationRegionInfo()->getActorIsolation();
        if (initForIsolatedPartialApply(op, ace, callerIsolation))
          return true;
      }
    }
  }

  // See if we are in SIL and have an apply site specified isolation.
  if (auto fas = FullApplySite::isa(op->getUser())) {
    if (auto isolation = fas.getIsolationCrossing()) {
      diagnosticEmitter.emitPassToApply(loc, op->get()->getType().getASTType(),
                                        *isolation);
      return true;
    }
  }

  if (auto *ri = dyn_cast<ReturnInst>(op->getUser())) {
    auto fType = ri->getFunction()->getLoweredFunctionType();
    if (fType->getNumResults() &&
        fType->getResults()[0].hasOption(SILResultInfo::IsSending)) {
      assert(llvm::all_of(fType->getResults(),
                          [](SILResultInfo resultInfo) {
                            return resultInfo.hasOption(
                                SILResultInfo::IsSending);
                          }) &&
             "All result info must be the same... if that changes... update "
             "this code!");
      if (auto name = inferNameHelper(op->get())) {
        diagnosticEmitter.emitNamedSendingReturn(loc, *name);
        return true;
      }
    } else {
      assert(llvm::none_of(fType->getResults(),
                           [](SILResultInfo resultInfo) {
                             return resultInfo.hasOption(
                                 SILResultInfo::IsSending);
                           }) &&
             "All result info must be the same... if that changes... update "
             "this code!");
    }
  }

  // If we are failing due to an autoclosure... see if we can find the captured
  // value that is causing the issue.
  if (auto *autoClosureExpr = loc.getAsASTNode<AutoClosureExpr>()) {
    // To split up this work, we only do this for async let for now.
    if (autoClosureExpr->getThunkKind() == AutoClosureExpr::Kind::AsyncLet) {
      auto *i = op->getUser();
      auto pai = ApplySite::isa(i);
      unsigned captureIndex = pai.getASTAppliedArgIndex(*op);
      auto captureInfo =
          autoClosureExpr->getCaptureInfo().getCaptures()[captureIndex];
      auto loc = RegularLocation(captureInfo.getLoc(), false /*implicit*/);
      AutoClosureWalker walker(
          diagnosticEmitter, captureInfo.getDecl(),
          diagnosticEmitter.getIsolationRegionInfo().getIsolationInfo(), loc,
          autoClosureExpr->getThunkKind() == AutoClosureExpr::Kind::AsyncLet);
      autoClosureExpr->walk(walker);
      return true;
    }
  }

  diagnosticEmitter.emitUnknownUse(loc);
  return true;
}

//===----------------------------------------------------------------------===//
//              MARK: InOutSendingReturned Error Emitter
//===----------------------------------------------------------------------===//

namespace {

/// Diagnostic emitter for InOutSendingReturned errors.
///
/// This is a verbatim error emitter (see PartitionOpError.def) that follows
/// the standard pattern: constructor takes (RegionAnalysisFunctionInfo*, Error)
/// and provides an emit() method.
///
/// This error occurs when a function returns a value that is in the same region
/// as an 'inout sending' parameter. This creates a problematic situation where
/// both the return value and the inout parameter reference the same value, but
/// 'sending' implies the value could be sent to another isolation
/// domain safely.
class InOutSendingReturnedDiagnosticEmitter {
public:
  using Error = PartitionOpError::InOutSendingReturnedError;

private:
  /// A region analysis function info that we use if we need to determine which
  /// incoming block edge had an error upon it.
  RegionAnalysisFunctionInfo *raFuncInfo;

  /// The function exiting inst where the 'inout sending' parameter was actor
  /// isolated.
  TermInst *functionExitingInst;

  /// The 'inout sending' param that we are emitting an error for.
  SILValue inoutSendingParam;

  /// The element number of the 'inout sending' param.
  Element inoutSendingParamElement;

  /// If we did not return the 'inout sending' param directly but instead
  /// returned something else in the same region. This is that value.
  ///
  /// If we actually returned the inout sending parameter this is SILValue().
  SILValue returnedValue;

  /// If we had a failure due to returning a value that is actor isolated due to
  /// it being in the same region as an out parameter. THis is the actor
  /// isolation.
  SILDynamicMergedIsolationInfo isolationInfo;

  /// The element of the returned value, and the partition at the exit, for the
  /// isolation-history notes. Kept alongside \c returnedValue because the
  /// history walk works in elements, and because \c returnedValue is null'd out
  /// in the "returned the parameter itself" case.
  Element returnedValueElement;
  std::optional<Partition> partition;

  bool downgradeToWarning = false;
  bool emittedErrorDiagnostic = false;

public:
  class LastValueEnum;

  InOutSendingReturnedDiagnosticEmitter(RegionAnalysisFunctionInfo *raFuncInfo,
                                        Error &&error)
      : raFuncInfo(raFuncInfo),
        functionExitingInst(cast<TermInst>(error.op->getSourceInst())),
        inoutSendingParam(raFuncInfo->getValueMap().getRepresentative(
            error.inoutSendingElement)),
        inoutSendingParamElement(error.inoutSendingElement),
        returnedValue(
            raFuncInfo->getValueMap().getRepresentative(error.returnedValue)),
        isolationInfo(error.isolationInfo),
        returnedValueElement(error.returnedValue),
        partition(std::move(error.partition)),
        downgradeToWarning(error.downgradeToWarning) {}

  ~InOutSendingReturnedDiagnosticEmitter() {
    // If we were supposed to emit a diagnostic and didn't emit an unknown
    // pattern error.
    if (!emittedErrorDiagnostic)
      emitUnknownPatternError();
  }

  SILFunction *getFunction() const { return inoutSendingParam->getFunction(); }

  std::optional<DiagnosticBehavior> getBehaviorLimit() const {
    return inoutSendingParam->getType().getConcurrencyDiagnosticBehavior(
        getFunction());
  }

  void emitUnknownPatternError(bool pushToFuture = false) {
    emittedErrorDiagnostic = true;
    EMIT_UNKNOWN_PATTERN_ERROR(InOutSendingReturnedDiagnosticEmitter,
                               functionExitingInst, getBehaviorLimit(),
                               pushToFuture);
  }

  /// Explain the region fact behind this error, when isolation-history emission
  /// is on for this function.
  ///
  /// Exactly one chain per error, anchored on the elements the error carries
  /// rather than on anything emit() re-derives. emit() walks epilogue phis and
  /// resolves indirect out-parameters through LastValueEnum, and can emit
  /// several primary diagnostics from one error -- but those paths exist to
  /// find a better diagnostic *location*, not to describe more regions.
  /// Following them here would be wrong twice over: LastValueEnum yields
  /// SILValues that may have no Element in the value map at all, so there would
  /// be nothing to ask the walk about; and the region fact being explained is
  /// singular, so one chain is what matches it. The walk's own sequence
  /// boundaries place the notes.
  ///
  /// Branch on the elements, not on the inoutSendingParam == returnedValue
  /// SILValue comparison emit() uses: two elements can share a representative
  /// through load look-through, and it is the element identity the request
  /// needs.
  void emitIsolationHistoryNoteIfNeeded() {
    // A note has to follow the diagnostic it annotates. When emit() bailed
    // without emitting anything the unknown-pattern error comes from our
    // destructor -- after this defer runs -- so there is nothing to attach to
    // yet.
    if (!emittedErrorDiagnostic || !partition)
      return;

    auto request =
        returnedValueElement == inoutSendingParamElement
            ? IsolationHistoryNoteEmitter::Request::howDidBecomeIsolatedTo(
                  inoutSendingParamElement, isolationInfo)
            : IsolationHistoryNoteEmitter::Request::howDidBecomeConnectedTo(
                  returnedValueElement, inoutSendingParamElement);

    IsolationHistoryNoteEmitter::emit(getFunction(), raFuncInfo,
                                      raFuncInfo->getValueMap(), request,
                                      std::move(*partition));
  }

  void emit();

  /// Called if we return the actual inout sending value.
  void emitReturnInOutSendingError(SILDynamicMergedIsolationInfo isolationInfo,
                                   SILLocation loc) {
    // We should always be able to find a name for an inout sending param. If we
    // do not, emit an unknown pattern error.
    auto inoutSendingParamName = inferNameHelper(inoutSendingParam);
    if (!inoutSendingParamName) {
      return emitUnknownPatternError();
    }

    diagnoseError(
        loc, diag::regionbasedisolation_inout_sending_cannot_be_returned_param,
        *inoutSendingParamName)
        .limitBehaviorIf(getBehaviorLimit());
    if (isolationInfo->isActorIsolated()) {
      diagnoseNote(
          loc,
          diag::
              regionbasedisolation_inout_sending_cannot_be_returned_note_actor_param,
          *inoutSendingParamName,
          isolationInfo->printForDiagnostics(getFunction()));
      return;
    }
    diagnoseNote(
        loc,
        diag::regionbasedisolation_inout_sending_cannot_be_returned_note_param,
        *inoutSendingParamName);
  }

  void emitValueError(SILValue value,
                      SILDynamicMergedIsolationInfo isolationInfo,
                      SILLocation loc) {
    // We should always be able to find a name for an inout sending param. If we
    // do not, emit an unknown pattern error.
    auto inoutSendingParamName = inferNameHelper(inoutSendingParam);
    if (!inoutSendingParamName) {
      return emitUnknownPatternError();
    }

    std::optional<Identifier> erroringEltName = inferNameHelper(value);
    if (!erroringEltName) {
      // We could not infer a name for the returned value.
      return emitUnknownPatternError(/*pushToFuture=*/downgradeToWarning);
    }

    diagnoseError(
        loc, diag::regionbasedisolation_inout_sending_cannot_be_returned_value,
        *erroringEltName)
        .limitBehaviorIf(getBehaviorLimit());
    if (isolationInfo->isActorIsolated()) {
      diagnoseNote(
          loc,
          diag::
              regionbasedisolation_inout_sending_cannot_be_returned_note_actor_value,
          *erroringEltName, *inoutSendingParamName,
          isolationInfo->printForDiagnostics(getFunction()));
      return;
    }
    diagnoseNote(
        loc,
        diag::regionbasedisolation_inout_sending_cannot_be_returned_note_value,
        *erroringEltName, *inoutSendingParamName);
  }

  /// Print \p value in a standard way. Allow for loc to override the specific
  /// location used to emit the diagnostic fi non-standard.
  void emitLastValueEnum(const LastValueEnum &value,
                         SILDynamicMergedIsolationInfo isolationInfo,
                         SILLocation loc = SILLocation::invalid());

  /// Emit an error for use when we are processing the incoming value of a phi
  /// argument or of the dynamic value of an indirect out parameter.
  bool emitOutParamIncomingValueError(const LastValueEnum &value);

  void emitDeclRefError(SILDeclRef declRef,
                        SILDynamicMergedIsolationInfo isolationInfo,
                        SILLocation loc) {
    // We should always be able to find a name for an inout sending param. If we
    // do not, emit an unknown pattern error.
    auto inoutSendingParamName = inferNameHelper(inoutSendingParam);
    if (!inoutSendingParamName) {
      return emitUnknownPatternError();
    }

    diagnoseError(
        loc,
        diag::
            regionbasedisolation_inout_sending_cannot_be_returned_value_result,
        declRef.getAbstractFunctionDecl())
        .limitBehaviorIf(getBehaviorLimit());

    if (isolationInfo->isActorIsolated()) {
      diagnoseNote(
          loc,
          diag::
              regionbasedisolation_inout_sending_cannot_be_returned_note_actor_return_value,
          declRef.getAbstractFunctionDecl(), *inoutSendingParamName,
          isolationInfo.printForDiagnostics(getFunction()));
      return;
    }
    diagnoseNote(
        loc,
        diag::
            regionbasedisolation_inout_sending_cannot_be_returned_note_return_value,
        declRef.getAbstractFunctionDecl(), *inoutSendingParamName);
  }

  ASTContext &getASTContext() const {
    return functionExitingInst->getFunction()->getASTContext();
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SourceLoc loc, Diag<T...> diag,
                                   U &&...args) {
    emittedErrorDiagnostic = true;
    auto localDiag =
        getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
    if (downgradeToWarning)
      return std::move(localDiag.warnUntilLanguageMode(LanguageMode::future));
    return std::move(localDiag.warnUntilLanguageMode(LanguageMode::v6));
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILLocation loc, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SourceLoc loc, Diag<T...> diag, U &&...args) {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILLocation loc, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILInstruction *inst, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(inst->getLoc(), diag, std::forward<U>(args)...);
  }
};

} // namespace

static SILValue lookThroughAccess(SILValue value) {
  while (true) {
    // We look through access, copies, and loads.
    if (auto *bai = dyn_cast<BeginAccessInst>(value)) {
      value = bai->getOperand();
      continue;
    }

    if (auto *cvi = dyn_cast<CopyValueInst>(value)) {
      value = cvi->getOperand();
      continue;
    }

    if (auto *bbi = dyn_cast<BeginBorrowInst>(value);
        bbi && !bbi->isFromVarDecl()) {
      value = bbi->getOperand();
      continue;
    }

    if (auto *mvi = dyn_cast<MoveValueInst>(value);
        mvi && !mvi->isFromVarDecl()) {
      value = mvi->getOperand();
      continue;
    }

    if (auto *li = dyn_cast<LoadInst>(value)) {
      value = li->getOperand();
      continue;
    }

    if (auto *lbi = dyn_cast<LoadBorrowInst>(value)) {
      value = lbi->getOperand();
      continue;
    }

    return value;
  }
}

class InOutSendingReturnedDiagnosticEmitter::LastValueEnum {
public:
  enum Kind {
    None,
    ApplySite,
    CopyAddr,
    Store,
    Value,
    ReturnTerm,
  };

private:
  Kind kind;
  std::variant<FullApplySite, SILValue, CopyAddrInst *, StoreInst *, Operand *>
      value;

  LastValueEnum() : kind(Kind::None), value() {}
  LastValueEnum(FullApplySite inst) : kind(Kind::ApplySite), value(inst) {}
  LastValueEnum(SILValue value) : kind(Kind::Value), value(value) {}
  LastValueEnum(CopyAddrInst *inst) : kind(Kind::CopyAddr), value(inst) {}
  LastValueEnum(StoreInst *inst) : kind(Kind::Store), value(inst) {}
  LastValueEnum(Operand *branchUse) : kind(Kind::ReturnTerm), value(branchUse) {
    assert(isa<TermInst>(branchUse->getUser()));
    assert(branchUse->getUser()->getLoc().getKind() == SILLocation::ReturnKind);
  }

public:
  static LastValueEnum get(SILValue value);
  static LastValueEnum get(SILInstruction *inst);
  static LastValueEnum get(Operand *use);
  static bool isSupported(Operand *use);

  operator bool() const { return getKind() != Kind::None; }

  Kind getKind() const { return kind; }
  bool isApplySite() const { return getKind() == Kind::ApplySite; }
  bool isValue() const { return getKind() == Kind::Value; }
  bool isStore() const { return getKind() == Kind::Store; }
  bool isCopyAddr() const { return getKind() == Kind::CopyAddr; }

  /// Is this a terminator instruction that is an incoming value to an epilogue
  /// block and thus forms part of the return from a function.
  bool isReturnTerm() const { return getKind() == Kind::ReturnTerm; }

  FullApplySite getFullApplySite() const {
    return std::get<FullApplySite>(value);
  }
  SILDeclRef getDeclRef() const {
    return getFullApplySite().getCalleeFunction()->getDeclRef();
  }
  SILValue getValue() const {
    switch (getKind()) {
    case None:
    case ApplySite:
      return SILValue();
    case CopyAddr:
      return getCopyAddr()->getAllOperands()[CopyLikeInstruction::Src].get();
    case Store:
      return getStore()->getAllOperands()[CopyLikeInstruction::Src].get();
    case Value:
      return std::get<SILValue>(value);
    case ReturnTerm:
      return std::get<Operand *>(value)->get();
    }
  }
  CopyAddrInst *getCopyAddr() const { return std::get<CopyAddrInst *>(value); }
  StoreInst *getStore() const { return std::get<StoreInst *>(value); }
  Operand *getReturnTerm() const { return std::get<Operand *>(value); }

  SILBasicBlock *getParentBlock() const {
    switch (getKind()) {
    case None:
      return nullptr;
    case ApplySite:
      return getFullApplySite()->getParent();
    case Value:
      return getValue()->getParentBlock();
    case CopyAddr:
      return getCopyAddr()->getParentBlock();
    case Store:
      return getStore()->getParentBlock();
    case ReturnTerm:
      return getReturnTerm()->getParentBlock();
    }
  }

  SILInstruction *getDefiningInsertionPoint() const {
    switch (getKind()) {
    case None:
      return nullptr;
    case ApplySite:
      return *getFullApplySite();
    case Value:
      return getValue()->getDefiningInsertionPoint();
    case CopyAddr:
      return getCopyAddr();
    case Store:
      return getStore();
    case ReturnTerm:
      return getReturnTerm()->getUser();
    }
  }

  SILLocation getLoc() const {
    switch (getKind()) {
    case None:
      return SILLocation::invalid();
    case ApplySite:
    case Value:
    case CopyAddr:
    case Store:
    case ReturnTerm:
      return getDefiningInsertionPoint()->getLoc();
    }
  }

  bool operator==(SILValue otherValue) const {
    switch (getKind()) {
    case None:
    case ApplySite:
      return false;
    case CopyAddr:
    case Store:
    case Value:
    case ReturnTerm:
      return lookThroughAccess(getValue()) == lookThroughAccess(otherValue);
    }
  }

  static bool
  findLastValuesForOutParam(SILFunctionArgument *fArg,
                            SmallVectorImpl<LastValueEnum> &lastValues);

  void print(llvm::raw_ostream &os) const {
    switch (getKind()) {
    case None:
      os << "none\n";
      return;
    case ApplySite:
      os << "apply site: " << *getFullApplySite();
      return;
    case CopyAddr:
      os << "copy_addr: " << *getCopyAddr();
      return;
    case Store:
      os << "store: " << *getStore();
      return;
    case Value:
      os << "value: " << *getValue();
      break;
    case ReturnTerm:
      os << "return term: " << *getReturnTerm()->getUser();
    }
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

InOutSendingReturnedDiagnosticEmitter::LastValueEnum
InOutSendingReturnedDiagnosticEmitter::LastValueEnum::get(
    SILInstruction *user) {
  if (auto *cai = dyn_cast<CopyAddrInst>(user)) {
    return {cai};
  }

  if (auto *si = dyn_cast<StoreInst>(user)) {
    return {si};
  }

  if (auto fas = FullApplySite::isa(user)) {
    return {fas};
  }

  return {};
}

InOutSendingReturnedDiagnosticEmitter::LastValueEnum
InOutSendingReturnedDiagnosticEmitter::LastValueEnum::get(Operand *use) {
  if (auto value = get(use->getUser()))
    return value;

  if (auto *ti = dyn_cast<TermInst>(use->getUser())) {
    if (ti->getLoc().getKind() == SILLocation::ReturnKind) {
      return {use};
    }
  }

  return {};
}

bool InOutSendingReturnedDiagnosticEmitter::LastValueEnum::isSupported(
    Operand *use) {
  if (auto fas = FullApplySite::isa(use->getUser())) {
    if (fas.isIndirectResultOperand(*use)) {
      auto *fn = fas.getCalleeFunction();
      return fn && fn->getDeclRef();
    }
  }

  if (auto *ti = dyn_cast<TermInst>(use->getUser())) {
    if (ti->getLoc().getKind() == SILLocation::ReturnKind) {
      return true;
    }
  }

  return isa<CopyAddrInst, StoreInst>(use->getUser());
}

InOutSendingReturnedDiagnosticEmitter::LastValueEnum
InOutSendingReturnedDiagnosticEmitter::LastValueEnum::get(SILValue value) {
  if (auto *inst = value->getDefiningInstruction()) {
    if (auto e = get(inst))
      return e;
  }

  return {value};
}

void InOutSendingReturnedDiagnosticEmitter::emitLastValueEnum(
    const LastValueEnum &value, SILDynamicMergedIsolationInfo isolationInfo,
    SILLocation loc) {
  switch (value.getKind()) {
  case LastValueEnum::None:
    llvm::report_fatal_error("Found .none enum elt");
  case LastValueEnum::ApplySite:
    emitDeclRefError(value.getDeclRef(), isolationInfo,
                     loc ? loc : value.getLoc());
    return;
  case LastValueEnum::Store:
  case LastValueEnum::CopyAddr:
    emitValueError(value.getValue(), isolationInfo, loc ? loc : value.getLoc());
    return;
  case LastValueEnum::Value:
    emitValueError(value.getValue(), isolationInfo,
                   loc ? loc : functionExitingInst->getLoc());
    return;
  case LastValueEnum::ReturnTerm:
    emitValueError(value.getValue(), isolationInfo, loc ? loc : value.getLoc());
    return;
  }
  llvm::report_fatal_error("Should never hit this");
}

bool InOutSendingReturnedDiagnosticEmitter::emitOutParamIncomingValueError(
    const LastValueEnum &finalValue) {
  auto *block = finalValue.getParentBlock();
  auto state = raFuncInfo->getBlockState(block);
  assert(state.isNonNull());

  // Grab the exit partition of the block. We make a copy here on purpose
  // since it /is/ possible for DiagnosticEvaluator to modify
  auto workingExitPartition = state.get()->getExitPartition();

  // See if given the exit partition of the block if we would emit an error
  // when we execute an InOutSendingAtFunctionExit partition op.
  SmallFrozenMultiMap<Operand *, RequireInst, 8> sendingOpToRequireInstMultiMap;
  SmallVector<PartitionOpError, 2> errors;
  DiagnosticEvaluator evaluator(workingExitPartition, raFuncInfo,
                                sendingOpToRequireInstMultiMap, errors,
                                raFuncInfo->getSendingOperandToStateMap());
  PartitionOp op = PartitionOp::InOutSendingAtFunctionExit(
      inoutSendingParamElement, finalValue.getDefiningInsertionPoint());
  evaluator.apply(op);

  // Check if we got an error. If we did not, then this is not one of the
  // guilty code paths. Continue.
  if (errors.empty()) {
    return false;
  }

  // Loop through the errors to find our InOutReturnedError. We emit one error
  // per return... but we emit for multiple returns separate diagnostics.
  while (!errors.empty()) {
    auto err = errors.pop_back_val();
    if (err.getKind() != PartitionOpError::InOutSendingReturned)
      continue;
    auto castError = std::move(err).getInOutSendingReturnedError();
    if (castError.inoutSendingElement != inoutSendingParamElement)
      continue;
    // Ok, we found an error. Lets emit it and return early.
    if (finalValue == inoutSendingParam) {
      emitReturnInOutSendingError(isolationInfo, finalValue.getLoc());
      return true;
    }

    // Force us to use the location of the finalValue. We do not want to use the
    // function exiting inst.
    emitLastValueEnum(finalValue, isolationInfo, finalValue.getLoc());
    return true;
  }

  return false;
}

void InOutSendingReturnedDiagnosticEmitter::emit() {
  // Emit isolation history notes after the primary diagnostic returns.
  SWIFT_DEFER { emitIsolationHistoryNoteIfNeeded(); };

  // Check if we had a separate erroring value that is our returned value. If we
  // do not then we just returned the 'inout sending' parameter. Emit a special
  // message and return.
  if (inoutSendingParam == returnedValue) {
    return emitReturnInOutSendingError(isolationInfo,
                                       functionExitingInst->getLoc());
  }

  // Before we do anything, see if we have a phi argument that is an epilogue
  // phi. In such a case, we want to process the incoming values.
  if (auto *phi = dyn_cast<SILPhiArgument>(returnedValue)) {
    bool handledValue = false;
    phi->visitIncomingPhiOperands([&](Operand *op) {
      if (op->getUser()->getLoc().getKind() != SILLocation::ReturnKind)
        return true;
      handledValue |=
          emitOutParamIncomingValueError(LastValueEnum::get(op->get()));
      return true;
    });
    if (handledValue)
      return;
  }

  // Check if we have an indirect out parameter as our erroring returned value.
  auto *fArg = dyn_cast<SILFunctionArgument>(returnedValue);
  if (!fArg || !fArg->isIndirectResult()) {
    // If we do not have one... emit a normal error.
    return emitLastValueEnum(LastValueEnum::get(returnedValue), isolationInfo);
  }

  SmallVector<LastValueEnum, 8> finalValues;
  if (!LastValueEnum::findLastValuesForOutParam(fArg, finalValues))
    return emitLastValueEnum(LastValueEnum::get(returnedValue), isolationInfo);

  // If we have a single value, then we have found our value.
  if (finalValues.size() == 1) {
    if (finalValues[0] == inoutSendingParam) {
      return emitReturnInOutSendingError(isolationInfo,
                                         finalValues[0].getLoc());
    }
    emitLastValueEnum(finalValues[0], isolationInfo);
    return;
  }

  // If we have multiple finalValues, then that means that we had multiple
  // return values. In such a case, we must have a single epilog block
  // that our finalValues jointly dominate. That means that we need to
  // determine which block the error actually occured along and emit an
  // error for the value in that block. We know we are going to emit an
  // error, so we can spend more time finding the better diagnostic since
  // we are going to fail.
  for (auto finalValue : finalValues) {
    emitOutParamIncomingValueError(finalValue);
  }
}

bool InOutSendingReturnedDiagnosticEmitter::LastValueEnum::
    findLastValuesForOutParam(SILFunctionArgument *fArg,
                              SmallVectorImpl<LastValueEnum> &lastValues) {
  assert(fArg->isIndirectResult());
  SmallVector<SILBasicBlock *, 8> visitedBlocks;
  SSAPrunedLiveness prunedLiveRange(fArg->getFunction(), &visitedBlocks);

  // Look for copy_addr and store uses into fArg. They should all be complete
  // stores.
  prunedLiveRange.initializeDef(fArg);
  for (auto *use : fArg->getUses()) {
    if (LastValueEnum::isSupported(use)) {
      prunedLiveRange.updateForUse(use->getUser(), false /*lifetime ending*/);
      continue;
    }
    return false;
  }

  PrunedLivenessBoundary boundary;
  prunedLiveRange.computeBoundary(boundary);

  for (auto *user : boundary.lastUsers) {
    if (auto e = LastValueEnum::get(user)) {
      lastValues.emplace_back(e);
      continue;
    }

    llvm::report_fatal_error("Out of sync with earlier check");
  }

  return true;
}

//===----------------------------------------------------------------------===//
//              MARK: InOutSendingNotDisconnected Error Emitter
//===----------------------------------------------------------------------===//

namespace {

/// Diagnostic emitter for InOutSendingNotDisconnectedAtExit errors.
///
/// This is a verbatim error emitter (see PartitionOpError.def) that follows
/// the standard pattern: constructor takes (RegionAnalysisFunctionInfo*, Error)
/// and provides an emit() method.
///
/// This error occurs when an 'inout sending' parameter ends up in an isolated
/// region at function exit. The contract requires such parameters to be
/// "disconnected" (not associated with any actor) when the function returns,
/// so the caller can safely send the value elsewhere.
class InOutSendingNotDisconnectedAtExitDiagnosticEmitter {
public:
  using Error = PartitionOpError::InOutSendingNotDisconnectedAtExitError;

private:
  /// The function exiting inst where the 'inout sending' parameter was actor
  /// isolated.
  TermInst *functionExitingInst;

  /// The 'inout sending' param that we are emitting an error for.
  SILValue inoutSendingParam;

  /// The dynamic actor isolated region info of our 'inout sending' value's
  /// region at the terminator inst.
  SILDynamicMergedIsolationInfo actorIsolatedRegionInfo;

  /// The region analysis for this function; forwarded to the isolation-history
  /// note emitter so it can recover predecessor exit partitions across joins.
  RegionAnalysisFunctionInfo *info;

  /// The element of the 'inout sending' param, and the partition at the exit,
  /// for the isolation-history notes.
  Element inoutSendingElement;
  std::optional<Partition> partition;

  bool emittedErrorDiagnostic = false;

public:
  InOutSendingNotDisconnectedAtExitDiagnosticEmitter(
      RegionAnalysisFunctionInfo *info, Error &&error)
      : functionExitingInst(cast<TermInst>(error.op->getSourceInst())),
        inoutSendingParam(
            info->getValueMap().getRepresentative(error.inoutSendingElement)),
        actorIsolatedRegionInfo(error.isolationInfo), info(info),
        inoutSendingElement(error.inoutSendingElement),
        partition(std::move(error.partition)) {}

  ~InOutSendingNotDisconnectedAtExitDiagnosticEmitter() {
    // If we were supposed to emit a diagnostic and didn't emit an unknown
    // pattern error.
    if (!emittedErrorDiagnostic)
      emitUnknownPatternError();
  }

  SILFunction *getFunction() const { return inoutSendingParam->getFunction(); }

  std::optional<DiagnosticBehavior> getBehaviorLimit() const {
    return inoutSendingParam->getType().getConcurrencyDiagnosticBehavior(
        getFunction());
  }

  void emitUnknownPatternError() {
    EMIT_UNKNOWN_PATTERN_ERROR(InOutSendingNotDisconnectedDiagnosticEmitter,
                               functionExitingInst, getBehaviorLimit(),
                               /*pushToFuture=*/false);
  }

  /// Explain how the 'inout sending' parameter's region came to be isolated,
  /// when isolation-history emission is on for this function.
  ///
  /// Deliberately still runs on emit()'s early unknown-pattern path: that is
  /// an error about this same value, a chain is useful context for it, and
  /// emitUnknownPatternError sets emittedErrorDiagnostic, so the note still
  /// follows its error. Do not "fix" that by narrowing the defer.
  void emitIsolationHistoryNoteIfNeeded() {
    // A note has to follow the diagnostic it annotates. When emit() bailed
    // without emitting anything the unknown-pattern error comes from our
    // destructor -- after this defer runs -- so there is nothing to attach to
    // yet.
    if (!emittedErrorDiagnostic || !partition)
      return;
    IsolationHistoryNoteEmitter::emit(
        getFunction(), info, info->getValueMap(),
        IsolationHistoryNoteEmitter::Request::howDidBecomeIsolatedTo(
            inoutSendingElement, actorIsolatedRegionInfo),
        std::move(*partition));
  }

  void emit();

  ASTContext &getASTContext() const {
    return functionExitingInst->getFunction()->getASTContext();
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SourceLoc loc, Diag<T...> diag,
                                   U &&...args) {
    emittedErrorDiagnostic = true;
    return std::move(getASTContext()
                         .Diags.diagnose(loc, diag, std::forward<U>(args)...)
                         .warnUntilLanguageMode(LanguageMode::v6));
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILLocation loc, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SourceLoc loc, Diag<T...> diag, U &&...args) {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILLocation loc, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILInstruction *inst, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(inst->getLoc(), diag, std::forward<U>(args)...);
  }
};

} // namespace

void InOutSendingNotDisconnectedAtExitDiagnosticEmitter::emit() {
  // Emit isolation history notes after the primary diagnostic returns.
  SWIFT_DEFER { emitIsolationHistoryNoteIfNeeded(); };

  // We should always be able to find a name for an inout sending param. If we
  // do not, emit an unknown pattern error.
  auto varName = inferNameHelper(inoutSendingParam);
  if (!varName) {
    return emitUnknownPatternError();
  }

  // Then emit the note with greater context.
  auto descriptiveKindStr =
      actorIsolatedRegionInfo.printForDiagnostics(getFunction());

  diagnoseError(
      functionExitingInst,
      diag::regionbasedisolation_inout_sending_cannot_be_actor_isolated,
      *varName, descriptiveKindStr,
      actorIsolatedRegionInfo->isTaskIsolated())
      .limitBehaviorIf(getBehaviorLimit());

  diagnoseNote(
      functionExitingInst,
      diag::regionbasedisolation_inout_sending_cannot_be_actor_isolated_note,
      *varName, descriptiveKindStr,
      actorIsolatedRegionInfo->isTaskIsolated());
}

//===----------------------------------------------------------------------===//
//         MARK: AssignNeverSendableIntoSendingResult Error Emitter
//===----------------------------------------------------------------------===//

namespace {

/// Diagnostic emitter for AssignNeverSendableIntoSendingResult errors.
///
/// This is a verbatim error emitter (see PartitionOpError.def) that follows
/// the standard pattern: constructor takes (RegionAnalysisFunctionInfo*, Error)
/// and provides an emit() method.
///
/// This error occurs when an isolated (non-Sendable) value is assigned into a
/// 'sending' out-parameter or result. The caller expects such results to be
/// "disconnected" and safe to send, but the value is tied to an isolation
/// domain and cannot be safely sent.
class AssignNeverSendableIntoSendingResultDiagnosticEmitter {
public:
  using Error = PartitionOpError::AssignNeverSendableIntoSendingResultError;

private:
  /// The use that actually caused the send.
  Operand *srcOperand;

  /// The specific out sending result.
  SILFunctionArgument *outSendingResult;

  /// The never-sent value that is in the same region as \p
  /// outSendingResult.
  SILValue neverSentValue;

  /// The region info that describes the dynamic dataflow derived isolation
  /// region info for the never-sent value.
  ///
  /// This is equal to the merge of the IsolationRegionInfo from all elements in
  /// the never-sent value's region when the error was diagnosed.
  SILDynamicMergedIsolationInfo isolatedValueIsolationRegionInfo;

  /// The region analysis for this function; forwarded to the isolation-history
  /// note emitter so it can recover predecessor exit partitions across joins.
  RegionAnalysisFunctionInfo *info;

  /// The element of the never-sent value, and the partition at the assignment,
  /// for the isolation-history notes.
  Element neverSentElement;
  std::optional<Partition> partition;

  bool emittedErrorDiagnostic = false;

public:
  AssignNeverSendableIntoSendingResultDiagnosticEmitter(
      RegionAnalysisFunctionInfo *info, Error &&error)
      : srcOperand(error.op->getSourceOp()), outSendingResult(error.destValue),
        neverSentValue(error.srcValue),
        isolatedValueIsolationRegionInfo(error.srcIsolationRegionInfo),
        info(info), neverSentElement(error.srcElement),
        partition(std::move(error.partition)) {}

  ~AssignNeverSendableIntoSendingResultDiagnosticEmitter() {
    // If we were supposed to emit a diagnostic and didn't emit an unknown
    // pattern error.
    if (!emittedErrorDiagnostic)
      emitUnknownPatternError();
  }

  SILFunction *getFunction() const { return srcOperand->getFunction(); }

  std::optional<DiagnosticBehavior> getConcurrencyDiagnosticBehavior() const {
    return outSendingResult->getType().getConcurrencyDiagnosticBehavior(
        getFunction());
  }

  void emitUnknownPatternError() {
    EMIT_UNKNOWN_PATTERN_ERROR(AssignIsolatedIntoSendingResultDiagnosticEmitter,
                               srcOperand->getUser(),
                               getConcurrencyDiagnosticBehavior(),
                               /*pushToFuture=*/false);
  }

  /// Explain how the assigned value's region came to be isolated, when
  /// isolation-history emission is on for this function.
  ///
  /// Deliberately still runs on emit()'s unknown-pattern paths: that error is
  /// about this same value, a chain is useful context for it, and
  /// emitUnknownPatternError sets emittedErrorDiagnostic, so the note still
  /// follows its error. Do not "fix" that by narrowing the defer.
  void emitIsolationHistoryNoteIfNeeded() {
    // A note has to follow the diagnostic it annotates. When emit() bailed
    // without emitting anything the unknown-pattern error comes from our
    // destructor -- after this defer runs -- so there is nothing to attach to
    // yet.
    if (!emittedErrorDiagnostic || !partition)
      return;
    IsolationHistoryNoteEmitter::emit(
        getFunction(), info, info->getValueMap(),
        IsolationHistoryNoteEmitter::Request::howDidBecomeIsolatedTo(
            neverSentElement, isolatedValueIsolationRegionInfo),
        std::move(*partition));
  }

  void emit();

  ASTContext &getASTContext() const {
    return srcOperand->getFunction()->getASTContext();
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SourceLoc loc, Diag<T...> diag,
                                   U &&...args) {
    emittedErrorDiagnostic = true;
    return std::move(getASTContext()
                         .Diags.diagnose(loc, diag, std::forward<U>(args)...)
                         .warnUntilLanguageMode(LanguageMode::v6));
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILLocation loc, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(Operand *op, Diag<T...> diag, U &&...args) {
    return diagnoseError(op->getUser()->getLoc(), diag,
                         std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SourceLoc loc, Diag<T...> diag, U &&...args) {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILLocation loc, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILInstruction *inst, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(Operand *op, Diag<T...> diag, U &&...args) {
    return diagnoseNote(op->getUser()->getLoc(), diag,
                        std::forward<U>(args)...);
  }
};

} // namespace

/// Look through values looking for our out parameter. We want to tightly
/// control this to be conservative... so we handroll this.
static SILValue findOutParameter(SILValue v) {
  while (true) {
    SILValue temp = v;
    if (auto *initOpt = dyn_cast<InitEnumDataAddrInst>(temp)) {
      if (initOpt->getElement()->getParentEnum() ==
          initOpt->getFunction()->getASTContext().getOptionalDecl()) {
        temp = initOpt->getOperand();
      }
    }

    if (temp == v) {
      return v;
    }

    v = temp;
  }
}

void AssignNeverSendableIntoSendingResultDiagnosticEmitter::emit() {
  // Emit isolation history notes after the primary diagnostic returns.
  SWIFT_DEFER { emitIsolationHistoryNoteIfNeeded(); };

  // Then emit the note with greater context.
  auto descriptiveKindStr =
      isolatedValueIsolationRegionInfo.printForDiagnostics(getFunction());
  bool isTaskIsolated = isolatedValueIsolationRegionInfo->isTaskIsolated();

  // Grab the var name if we can find it.
  if (auto varName = VariableNameInferrer::inferName(srcOperand->get())) {
    // In general, when we do an assignment like this, we assume that srcOperand
    // and our outSendingResult have the same type. This doesn't always happen
    // though especially if our outSendingResult is used as an out parameter of
    // a class_method. Check for such a case and if so, add to the end of our
    // string a path component for that class_method.
    if (srcOperand->get()->getType() != outSendingResult->getType()) {
      if (auto fas = FullApplySite::isa(srcOperand->getUser())) {
        if (fas.hasSelfArgument() &&
            fas.getSelfArgument() == srcOperand->get() &&
            fas.getNumIndirectSILResults() == 1) {
          // First check if our function argument is exactly our out parameter.
          bool canEmit = outSendingResult == fas.getIndirectSILResults()[0];

          // If that fails, see if we are storing into a temporary
          // alloc_stack. In such a case, find the root value that the temporary
          // is initialized to and see if that is our target function
          // argument. In such a case, we also want to add the decl name to our
          // type.
          if (!canEmit) {
            canEmit = outSendingResult ==
                      findOutParameter(fas.getIndirectSILResults()[0]);
          }

          if (canEmit) {
            if (auto *callee =
                    dyn_cast_or_null<MethodInst>(fas.getCalleeOrigin())) {
              SmallString<64> resultingString;
              resultingString.append(varName->str());
              resultingString += '.';
              resultingString += VariableNameInferrer::getNameFromDecl(
                  callee->getMember().getDecl());
              varName = fas->getFunction()->getASTContext().getIdentifier(
                  resultingString);
            }
          }
        }
      }
    }

    diagnoseError(
        srcOperand,
        diag::regionbasedisolation_out_sending_cannot_be_actor_isolated_named,
        *varName, descriptiveKindStr, isTaskIsolated)
        .limitBehaviorIf(getConcurrencyDiagnosticBehavior());

    diagnoseNote(
        srcOperand,
        diag::
            regionbasedisolation_out_sending_cannot_be_actor_isolated_note_named,
        *varName, descriptiveKindStr, isTaskIsolated);
    return;
  }

  Type type = neverSentValue->getType().getASTType();

  diagnoseError(
      srcOperand,
      diag::regionbasedisolation_out_sending_cannot_be_actor_isolated_type,
      type, descriptiveKindStr, isTaskIsolated)
      .limitBehaviorIf(getConcurrencyDiagnosticBehavior());

  diagnoseNote(
      srcOperand,
      diag::regionbasedisolation_out_sending_cannot_be_actor_isolated_note_type,
      type, descriptiveKindStr, isTaskIsolated);
  diagnoseNote(srcOperand, diag::regionbasedisolation_type_is_non_sendable,
               type);
}

//===----------------------------------------------------------------------===//
//              MARK: NonSendableIsolationCrossingResult Emitter
//===----------------------------------------------------------------------===//

/// Add Fix-It text for the given nominal type to adopt Sendable.
static void addSendableFixIt(const NominalTypeDecl *nominal,
                             InFlightDiagnostic &diag, bool unchecked) {
  if (nominal->getInherited().empty()) {
    SourceLoc fixItLoc = nominal->getBraces().Start;
    diag.fixItInsert(fixItLoc,
                     unchecked ? ": @unchecked Sendable" : ": Sendable");
  } else {
    auto fixItLoc = nominal->getInherited().getEndLoc();
    diag.fixItInsertAfter(fixItLoc,
                          unchecked ? ", @unchecked Sendable" : ", Sendable");
  }
}

/// Add Fix-It text for the given generic param declaration type to adopt
/// Sendable.
static void addSendableFixIt(const GenericTypeParamDecl *genericArgument,
                             InFlightDiagnostic &diag) {
  if (genericArgument->getInherited().empty()) {
    auto fixItLoc = genericArgument->getLoc();
    diag.fixItInsertAfter(fixItLoc, ": Sendable");
  } else {
    auto fixItLoc = genericArgument->getInherited().getEndLoc();
    diag.fixItInsertAfter(fixItLoc, " & Sendable");
  }
}

//===----------------------------------------------------------------------===//
//         MARK: NonSendableIsolationCrossingResult Error Emitter
//===----------------------------------------------------------------------===//

namespace {

/// Diagnostic emitter for NonSendableIsolationCrossingResult errors.
///
/// This is a verbatim error emitter (see PartitionOpError.def) that follows
/// the standard pattern: constructor takes (RegionAnalysisFunctionInfo*, Error)
/// and provides an emit() method.
///
/// This error occurs when a function that crosses isolation boundaries (e.g.,
/// calling from one actor to another) returns a non-Sendable value. Since the
/// result will cross isolation domains, it must be Sendable.
struct NonSendableIsolationCrossingResultDiagnosticEmitter {
  RegionAnalysisValueMap &valueMap;

  using Error = PartitionOpError::NonSendableIsolationCrossingResultError;
  Error error;

  bool emittedErrorDiagnostic = false;

  /// The value assigned as the equivalence class representative. It is
  /// guaranteed to be from the isolation crossing function since we never treat
  /// isolation crossing functions as being look through.
  SILValue representative;

  NonSendableIsolationCrossingResultDiagnosticEmitter(
      RegionAnalysisFunctionInfo *info, Error &&error)
      : valueMap(info->getValueMap()), error(std::move(error)),
        representative(valueMap.getRepresentative(error.returnValueElement)) {}

  SILFunction *getFunction() const {
    return error.op->getSourceInst()->getFunction();
  }

  void emit();

  ASTContext &getASTContext() const {
    return error.op->getSourceInst()->getFunction()->getASTContext();
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SourceLoc loc, Diag<T...> diag,
                                   U &&...args) {
    emittedErrorDiagnostic = true;
    return std::move(getASTContext()
                         .Diags.diagnose(loc, diag, std::forward<U>(args)...)
                         .warnUntilLanguageMode(LanguageMode::v6));
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILLocation loc, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SourceLoc loc, Diag<T...> diag, U &&...args) {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILLocation loc, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILInstruction *inst, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  std::optional<DiagnosticBehavior> getBehaviorLimit() const {
    return representative->getType().getConcurrencyDiagnosticBehavior(
        representative->getFunction());
  }

  void emitUnknownPatternError() {
    emittedErrorDiagnostic = true;
    EMIT_UNKNOWN_PATTERN_ERROR(
        NonSendableIsolationCrossingResultDiagnosticEmitter,
        error.op->getSourceInst(), getBehaviorLimit(),
        /*pushToFuture=*/false);
  }

  Type getType() const {
    if (auto *applyExpr =
            error.op->getSourceInst()->getLoc().getAsASTNode<ApplyExpr>()) {
      return applyExpr->getType();
    }

    // If we do not have an ApplyExpr, see if we can just infer the type from
    // the SILFunction type. This is only used in SIL test cases.
    if (auto fas = FullApplySite::isa(error.op->getSourceInst())) {
      return fas.getSubstCalleeType()
          ->getAllResultsSubstType(fas.getModule(),
                                   fas.getFunction()->getTypeExpansionContext())
          .getASTType();
    }

    return Type();
  }

  const ValueDecl *getCalledDecl() const {
    if (auto *applyExpr =
            error.op->getSourceInst()->getLoc().getAsASTNode<ApplyExpr>()) {
      if (auto calledValue =
              applyExpr->getCalledValue(true /*look through conversions*/)) {
        return calledValue;
      }
    }

    return nullptr;
  }

  std::optional<ApplyIsolationCrossing> getIsolationCrossing() const {
    if (auto *applyExpr =
            error.op->getSourceInst()->getLoc().getAsASTNode<ApplyExpr>()) {
      if (auto isolationCrossing = applyExpr->getIsolationCrossing()) {
        return *isolationCrossing;
      }
    }

    // If we have a SIL based test case, just return the actual isolation
    // crossing.
    if (auto fas = FullApplySite::isa(error.op->getSourceInst())) {
      if (auto isolationCrossing = fas.getIsolationCrossing())
        return *isolationCrossing;
    }

    return {};
  }
};

} // namespace

void NonSendableIsolationCrossingResultDiagnosticEmitter::emit() {
  auto isolationCrossing = getIsolationCrossing();
  if (!isolationCrossing)
    return emitUnknownPatternError();

  auto calleeIsolationStr = SILIsolationInfo::printActorIsolationForDiagnostics(
      getFunction(), isolationCrossing->getCalleeIsolation());
  auto callerIsolationStr = SILIsolationInfo::printActorIsolationForDiagnostics(
      getFunction(), isolationCrossing->getCallerIsolation());

  auto type = getType();
  if (getCalledDecl()) {
    diagnoseError(error.op->getSourceInst(),
                  diag::rbi_isolation_crossing_result, type, calleeIsolationStr,
                  getCalledDecl(), callerIsolationStr)
        .limitBehaviorIf(getBehaviorLimit());
  } else {
    diagnoseError(error.op->getSourceInst(),
                  diag::rbi_isolation_crossing_result_no_decl, type,
                  calleeIsolationStr, callerIsolationStr)
        .limitBehaviorIf(getBehaviorLimit());
  }
  if (type->is<FunctionType>()) {
    diagnoseNote(error.op->getSourceInst(),
                 diag::rbi_nonsendable_function_type);
    return;
  }

  auto *moduleDecl = error.op->getSourceInst()->getModule().getSwiftModule();
  if (auto *nominal = type->getNominalOrBoundGenericNominal()) {
    // If the nominal type is in the current module, suggest adding `Sendable`
    // if it makes sense.
    if (nominal->getParentModule() == moduleDecl &&
        (isa<StructDecl>(nominal) || isa<EnumDecl>(nominal))) {
      auto note = nominal->diagnose(diag::rbi_add_nominal_sendable_conformance,
                                    nominal);
      addSendableFixIt(nominal, note, /*unchecked*/ false);
    } else {
      nominal->diagnose(diag::rbi_non_sendable_nominal, nominal);
    }
    return;
  }

  if (auto genericArchetype = type->getAs<ArchetypeType>()) {
    auto interfaceType = genericArchetype->getInterfaceType();
    if (auto genericParamType = interfaceType->getAs<GenericTypeParamType>()) {
      auto *genericParamTypeDecl = genericParamType->getDecl();
      if (genericParamTypeDecl &&
          genericParamTypeDecl->getModuleContext() == moduleDecl) {
        auto diag = genericParamTypeDecl->diagnose(
            diag::rbi_add_generic_parameter_sendable_conformance, type);
        addSendableFixIt(genericParamTypeDecl, diag);
        return;
      }
    }
  }
}

//===----------------------------------------------------------------------===//
//         MARK: InOutSendingParametersInSameRegion Error Emitter
//===----------------------------------------------------------------------===//

namespace {

/// Diagnostic emitter for InOutSendingParametersInSameRegion errors.
///
/// Unlike standard verbatim error emitters, this uses
/// PARTITION_OP_VERBATIM_SPECIAL_EMISSION_EMITTER because:
/// 1. A single error may contain multiple pairs of conflicting parameters
/// 2. Each pair requires filtering based on diagnostic behavior settings
/// 3. The emitter is invoked multiple times from a loop in emitVerbatimErrors()
///
/// This error occurs when two 'inout sending' parameters end up in the same
/// region, making them interdependent. Since each must be independently
/// sendable, sharing a region violates the contract.
class InOutSendingParametersInSameRegionDiagnosticEmitter {
  /// The function exiting inst where the 'inout sending' parameter was actor
  /// isolated.
  TermInst *functionExitingInst;

  /// The first 'inout sending' param in the region.
  SILValue firstInOutSendingParam;

  /// The second 'inout sending' param in the region.
  SILValue secondInOutSendingParam;

  /// The region analysis for this function; forwarded to the isolation-history
  /// note emitter so it can recover predecessor exit partitions across joins.
  RegionAnalysisFunctionInfo *info;

  /// The elements of the two parameters, and the partition at the exit, for the
  /// isolation-history notes. This emitter is constructed once per reported
  /// pair, so each instance explains exactly its own pair.
  Element firstElement;
  Element secondElement;
  std::optional<Partition> partition;

  bool emittedErrorDiagnostic = false;

public:
  InOutSendingParametersInSameRegionDiagnosticEmitter(
      RegionAnalysisFunctionInfo *info, TermInst *functionExitingInst,
      SILValue firstInOutSendingParam, SILValue secondInOutSendingParam,
      Element firstElement, Element secondElement,
      std::optional<Partition> &&partition)
      : functionExitingInst(functionExitingInst),
        firstInOutSendingParam(firstInOutSendingParam),
        secondInOutSendingParam(secondInOutSendingParam), info(info),
        firstElement(firstElement), secondElement(secondElement),
        partition(std::move(partition)) {}

  ~InOutSendingParametersInSameRegionDiagnosticEmitter() {
    // If we were supposed to emit a diagnostic and didn't emit an unknown
    // pattern error.
    if (!emittedErrorDiagnostic)
      emitUnknownPatternError();
  }

  SILFunction *getFunction() const {
    return functionExitingInst->getFunction();
  }

  std::optional<DiagnosticBehavior> getBehaviorLimit() const {
    auto first =
        firstInOutSendingParam->getType().getConcurrencyDiagnosticBehavior(
            getFunction());
    auto second =
        secondInOutSendingParam->getType().getConcurrencyDiagnosticBehavior(
            getFunction());
    if (!first)
      return second;
    if (!second)
      return first;
    return first->merge(*second);
  }

  void emitUnknownPatternError() {
    EMIT_UNKNOWN_PATTERN_ERROR(
        InOutSendingParametersInSameRegionDiagnosticEmitter,
        functionExitingInst, getBehaviorLimit(),
        /*pushToFuture=*/false);
  }

  /// Explain how the two 'inout sending' parameters came to share a region,
  /// when isolation-history emission is on for this function.
  ///
  /// Both parameters are disconnected -- this diagnostic is purely about them
  /// sharing a region -- so the request is a connect one and every link is a
  /// plain "'a' is connected to 'b'" with no isolated terminus.
  void emitIsolationHistoryNoteIfNeeded() {
    // A note has to follow the diagnostic it annotates. When emit() bailed
    // without emitting anything the unknown-pattern error comes from our
    // destructor -- after this defer runs -- so there is nothing to attach to
    // yet.
    if (!emittedErrorDiagnostic || !partition)
      return;
    IsolationHistoryNoteEmitter::emit(
        getFunction(), info, info->getValueMap(),
        IsolationHistoryNoteEmitter::Request::howDidBecomeConnectedTo(
            firstElement, secondElement),
        std::move(*partition));
  }

  void emit();

  ASTContext &getASTContext() const {
    return functionExitingInst->getFunction()->getASTContext();
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SourceLoc loc, Diag<T...> diag,
                                   U &&...args) {
    emittedErrorDiagnostic = true;
    return std::move(getASTContext()
                         .Diags.diagnose(loc, diag, std::forward<U>(args)...)
                         .warnUntilLanguageMode(LanguageMode::v6));
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILLocation loc, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    return diagnoseError(inst->getLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SourceLoc loc, Diag<T...> diag, U &&...args) {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILLocation loc, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(loc.getSourceLoc(), diag, std::forward<U>(args)...);
  }

  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseNote(SILInstruction *inst, Diag<T...> diag,
                                  U &&...args) {
    return diagnoseNote(inst->getLoc(), diag, std::forward<U>(args)...);
  }
};

} // namespace

void InOutSendingParametersInSameRegionDiagnosticEmitter::emit() {
  // Emit isolation history notes after the primary diagnostic returns.
  SWIFT_DEFER { emitIsolationHistoryNoteIfNeeded(); };

  // We should always be able to find a name for an inout sending param. If we
  // do not, emit an unknown pattern error.
  auto firstName = inferNameHelper(firstInOutSendingParam);
  if (!firstName) {
    return emitUnknownPatternError();
  }
  auto secondName = inferNameHelper(secondInOutSendingParam);
  if (!secondName) {
    return emitUnknownPatternError();
  }

  diagnoseError(functionExitingInst,
                diag::regionbasedisolation_inout_sending_in_same_region,
                *firstName, *secondName)
      .limitBehaviorIf(getBehaviorLimit());

  diagnoseNote(functionExitingInst,
               diag::regionbasedisolation_inout_sending_in_same_region_note,
               *firstName, *secondName);
}

//===----------------------------------------------------------------------===//
//                 MARK: UnknownCodePatternDiagnosticEmitter
//===----------------------------------------------------------------------===//

namespace {

class UnknownCodePatternDiagnosticEmitter {
public:
  using Error = PartitionOpError::UnknownCodePatternError;

private:
  SILInstruction *inst;

public:
  UnknownCodePatternDiagnosticEmitter(RegionAnalysisFunctionInfo *info,
                                      Error &&error)
      : inst(error.op->getSourceInst()) {}

  void emit() {
    if (inst->getFunction()->getModule().getOptions()
            .AbortOnUnknownRegionIsolationPatternError) {
      llvm::report_fatal_error(
          "RegionIsolation: Found unknown SIL pattern in verbatim emitter. "
          "See -sil-region-isolation-assert-on-unknown-pattern");
    }

    REGIONBASEDISOLATION_LOG(llvm::dbgs() << "Emitting Error. Verbatim Error: "
                                             "Unknown Code Pattern.\n"
                                          << "  Inst: " << *inst);
    diagnoseError(inst, diag::regionbasedisolation_unknown_pattern);
  }
};

} // namespace

//===----------------------------------------------------------------------===//
//                      MARK: Incompatible Region Merge
//===----------------------------------------------------------------------===//

struct IncompatibleRegionMergeDiagnosticEmitter {
  using Error = DiagnosticEvaluator::IncompatibleRegionMergeError;
  RegionAnalysisValueMap &valueMap;
  Operand *op;
  Element srcRegionValueElt;
  SILDynamicMergedIsolationInfo srcIsolationInfo;
  Element dstRegionValueElt;
  SILDynamicMergedIsolationInfo dstIsolationInfo;
  RegionMergeReason reason;

  /// The region analysis for this function; forwarded to the isolation-history
  /// note emitter so it can recover predecessor exit partitions across joins.
  RegionAnalysisFunctionInfo *info;

  /// The partition before the failed merge. See the error struct's comment: the
  /// two regions are still separate here, so each side is explained by its own
  /// independent walk. None when history recording is off for this function.
  std::optional<Partition> partition;

  IncompatibleRegionMergeDiagnosticEmitter(RegionAnalysisFunctionInfo *info,
                                           Error &&error)
      : valueMap(info->getValueMap()), op(error.op->getSourceOp()),
        srcRegionValueElt(error.srcRegionElt),
        srcIsolationInfo(error.srcIsolationRegionInfo),
        dstRegionValueElt(error.dstRegionElt),
        dstIsolationInfo(error.dstIsolationRegionInfo),
        // NOTE: We purposely use the reason from the error, not from the op in
        // the error since we may have an error reason that was specified
        // explicitly when the error was created, not from the actual
        // PartitionOp.
        reason(error.reason), info(info),
        partition(std::move(error.partition)) {}

  void emit();

  SILFunction *getFunction() const { return op->getFunction(); }

  std::optional<DiagnosticBehavior> getBehaviorLimit() const {
    return op->get()->getType().getConcurrencyDiagnosticBehavior(getFunction());
  }

private:
  void emitUnknownPatternError() {
    EMIT_UNKNOWN_PATTERN_ERROR(IncompatibleRegionMergeErrorEmitter,
                               op->getUser(), getBehaviorLimit(),
                               /*pushToFuture=*/true);
  }

  /// Explain one side of the failed merge: how that side's region came to have
  /// the isolation it has.
  ///
  /// The isolation-history chain is the deep version of the shallow
  /// "X is exposed to <iso> code" note -- it names every hop rather than just
  /// the endpoint -- so when a chain is emitted the shallow note would only
  /// restate its last link. Fall back to the shallow note when there is no
  /// chain to show, which is the common case: history recording is off unless
  /// the function opted in. The fallback keys off whether the walk actually
  /// emitted anything, not off whether recording is on: recording can be on and
  /// the walk still find nothing, and the user must keep the shallow note in
  /// that case.
  ///
  /// Called exactly where the shallow note was called, i.e. after the primary
  /// diagnostic, rather than from a SWIFT_DEFER -- the unknown-pattern paths
  /// return before reaching here and must stay note-free. Being downstream of
  /// each sub-emitter's leading !srcIsolationInfo / !dstIsolationInfo guards is
  /// also what makes the isolation safe to format at all.
  void emitSideNote(Element elt, RepresentativeValue regionValue,
                    SILDynamicMergedIsolationInfo isolation,
                    StringRef isolationStr) {
    if (partition) {
      // Each walk consumes the partition it rewinds, and there are two sides to
      // explain, so hand it a copy.
      if (IsolationHistoryNoteEmitter::emit(
              getFunction(), info, valueMap,
              IsolationHistoryNoteEmitter::Request::howDidBecomeIsolatedTo(
                  elt, isolation),
              Partition(*partition)))
        return;
    }

    if (auto value = regionValue.maybeGetValue())
      emitMergeRegionValueNote(value, isolationStr,
                               isolation->isTaskIsolated());
  }

  // Emit incompatible-region-merge diagnostics as warnings until the future
  // language mode. This shadows the file-scope siloptimizer::diagnoseError
  // (which wraps in warnUntilLanguageMode(v6)) so we do not double-wrap.
  template <typename... T, typename... U>
  InFlightDiagnostic diagnoseError(SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
    auto &ctx = inst->getFunction()->getASTContext();
    return std::move(
        ctx.Diags
            .diagnose(inst->getLoc().getSourceLoc(), diag,
                      std::forward<U>(args)...)
            .warnUntilLanguageMode(LanguageMode::future));
  }

  void emitUnknown();
  void emitAssign();
  void emitNonisolatedFunction();
  void emitIsolatedFunction();
  void emitCast();
};

void IncompatibleRegionMergeDiagnosticEmitter::emitUnknown() {
  if (!srcIsolationInfo)
    return emitUnknownPatternError();
  if (!dstIsolationInfo)
    return emitUnknownPatternError();

  auto srcRegionValue = valueMap.getRepresentativeValue(srcRegionValueElt);
  auto dstRegionValue = valueMap.getRepresentativeValue(dstRegionValueElt);

  auto srcIsolation = srcIsolationInfo;
  auto dstIsolation = dstIsolationInfo;

  // Canonicalize so that srcRegionValue is always the task-isolated value when
  // possible. This only affects which isolation string is rendered first.
  // The elements must travel with the values they name: a swapped isolation
  // paired with an unswapped element would explain the wrong region.
  auto srcElt = srcRegionValueElt;
  auto dstElt = dstRegionValueElt;
  if (!srcIsolation->isTaskIsolated() && dstIsolation->isTaskIsolated()) {
    std::swap(srcIsolation, dstIsolation);
    std::swap(srcRegionValue, dstRegionValue);
    std::swap(srcElt, dstElt);
  }

  auto srcIsolationStr = srcIsolation.printForDiagnostics(getFunction());
  auto dstIsolationStr = dstIsolation.printForDiagnostics(getFunction());

  diagnoseError(op->getUser(), diag::rbi_merge_failure_unknown, srcIsolationStr,
                dstIsolationStr, srcIsolation->isTaskIsolated(),
                dstIsolation->isTaskIsolated())
      .limitBehaviorIf(getBehaviorLimit());

  emitSideNote(srcElt, srcRegionValue, srcIsolation, srcIsolationStr);
  emitSideNote(dstElt, dstRegionValue, dstIsolation, dstIsolationStr);
}

void IncompatibleRegionMergeDiagnosticEmitter::emitAssign() {
  if (!srcIsolationInfo)
    return emitUnknownPatternError();
  if (!dstIsolationInfo)
    return emitUnknownPatternError();

  auto srcRegionValue = valueMap.getRepresentativeValue(srcRegionValueElt);
  auto dstRegionValue = valueMap.getRepresentativeValue(dstRegionValueElt);

  auto srcIsolation = srcIsolationInfo;
  auto dstIsolation = dstIsolationInfo;

  // Canonicalize so that srcRegionValue is always the task-isolated value when
  // possible. This only affects which isolation string is rendered first.
  // The elements must travel with the values they name: a swapped isolation
  // paired with an unswapped element would explain the wrong region.
  auto srcElt = srcRegionValueElt;
  auto dstElt = dstRegionValueElt;
  if (!srcIsolation->isTaskIsolated() && dstIsolation->isTaskIsolated()) {
    std::swap(srcIsolation, dstIsolation);
    std::swap(srcRegionValue, dstRegionValue);
    std::swap(srcElt, dstElt);
  }

  auto srcIsolationStr = srcIsolation.printForDiagnostics(getFunction());
  auto dstIsolationStr = dstIsolation.printForDiagnostics(getFunction());

  diagnoseError(op->getUser(), diag::rbi_merge_failure_assign, srcIsolationStr,
                dstIsolationStr, srcIsolation->isTaskIsolated(),
                dstIsolation->isTaskIsolated())
      .limitBehaviorIf(getBehaviorLimit());

  emitSideNote(srcElt, srcRegionValue, srcIsolation, srcIsolationStr);
  emitSideNote(dstElt, dstRegionValue, dstIsolation, dstIsolationStr);
}

void IncompatibleRegionMergeDiagnosticEmitter::emitNonisolatedFunction() {
  if (!srcIsolationInfo)
    return emitUnknownPatternError();
  if (!dstIsolationInfo)
    return emitUnknownPatternError();

  auto srcIsolation = srcIsolationInfo;
  auto dstIsolation = dstIsolationInfo;
  auto srcRegionValue = valueMap.getRepresentativeValue(srcRegionValueElt);
  auto dstRegionValue = valueMap.getRepresentativeValue(dstRegionValueElt);

  // Canonicalize so that srcRegionValue is always the task-isolated value when
  // possible. This only affects which isolation string is rendered first.
  // The elements must travel with the values they name: a swapped isolation
  // paired with an unswapped element would explain the wrong region.
  auto srcElt = srcRegionValueElt;
  auto dstElt = dstRegionValueElt;
  if (!srcIsolation->isTaskIsolated() && dstIsolation->isTaskIsolated()) {
    std::swap(srcIsolation, dstIsolation);
    std::swap(srcRegionValue, dstRegionValue);
    std::swap(srcElt, dstElt);
  }

  auto srcIsolationStr = srcIsolation.printForDiagnostics(getFunction());
  auto dstIsolationStr = dstIsolation.printForDiagnostics(getFunction());

  // If we cannot recover a callee decl, fall through to the generic unknown
  // shape rather than emitting an unknown-pattern error.
  auto as = ApplySite::isa(op->getUser());
  if (!as)
    return emitUnknown();
  auto declRef = as.getCalleeDeclRef();
  if (!declRef)
    return emitUnknown();

  diagnoseError(op->getUser(), diag::rbi_merge_failure_nonisolatedfn,
                srcIsolationStr, dstIsolationStr, declRef.getDecl(),
                srcIsolation->isTaskIsolated(), dstIsolation->isTaskIsolated())
      .limitBehaviorIf(getBehaviorLimit());

  emitSideNote(srcElt, srcRegionValue, srcIsolation, srcIsolationStr);
  emitSideNote(dstElt, dstRegionValue, dstIsolation, dstIsolationStr);
}

void IncompatibleRegionMergeDiagnosticEmitter::emitIsolatedFunction() {
  if (!srcIsolationInfo)
    return emitUnknownPatternError();
  if (!dstIsolationInfo)
    return emitUnknownPatternError();

  // The dstRegionValue is always going to be the actor introducing inst.
  auto dstRegionValue = valueMap.getRepresentativeValue(dstRegionValueElt);
  assert(dstRegionValue.hasRegionIntroducingInst());

  auto srcRegionValue = valueMap.getRepresentativeValue(srcRegionValueElt);
  auto srcIsolation = srcIsolationInfo;
  auto dstIsolation = dstIsolationInfo;

  auto as = ApplySite::isa(dstRegionValue.getActorRegionIntroducingInst());
  if (!as)
    return emitUnknownPatternError();
  auto declRef = as.getCalleeDeclRef();
  if (!declRef)
    return emitUnknownPatternError();

  auto srcIsolationStr = srcIsolation.printForDiagnostics(getFunction());
  auto dstIsolationStr = dstIsolation.printForDiagnostics(getFunction());

  // Prefer a named primary when we can recover a name for the source value;
  // fall back to the unnamed form otherwise. The named primary embeds the name
  // directly so the user does not have to read the follow-up note to see which
  // value triggered the diagnostic.
  std::optional<Identifier> srcName;
  if (auto srcValue = srcRegionValue.maybeGetValue())
    srcName = inferNameHelper(srcValue);

  if (srcName) {
    diagnoseError(op->getUser(), diag::rbi_merge_failure_isolatedfn_named,
                  *srcName, srcIsolationStr, declRef.getDecl(), dstIsolationStr,
                  srcIsolation->isTaskIsolated(),
                  dstIsolation->isTaskIsolated())
        .limitBehaviorIf(getBehaviorLimit());
  } else {
    diagnoseError(op->getUser(), diag::rbi_merge_failure_isolatedfn,
                  srcIsolationStr, declRef.getDecl(), dstIsolationStr,
                  srcIsolation->isTaskIsolated(),
                  dstIsolation->isTaskIsolated())
        .limitBehaviorIf(getBehaviorLimit());
  }

  emitSideNote(srcRegionValueElt, srcRegionValue, srcIsolation,
               srcIsolationStr);
}

void IncompatibleRegionMergeDiagnosticEmitter::emitCast() {
  if (!srcIsolationInfo)
    return emitUnknownPatternError();
  if (!dstIsolationInfo)
    return emitUnknownPatternError();

  // The dstRegionValue is always going to be the actor introducing inst.
  auto dstRegionValue = valueMap.getRepresentativeValue(dstRegionValueElt);

  auto cast = [&]() {
    // Check if we have a region introducing inst.
    if (dstRegionValue.hasRegionIntroducingInst())
      return SILDynamicCastInst::getAs(
          dstRegionValue.getActorRegionIntroducingInst());

    // Then see if we have a phi argument.
    if (auto *phiArg =
            dyn_cast_or_null<SILPhiArgument>(dstRegionValue.maybeGetValue())) {
      if (auto *termInst = dyn_cast_or_null<CheckedCastBranchInst>(
              phiArg->getSingleTerminator());
          termInst && termInst->getSuccessBB() == phiArg->getParent()) {
        return SILDynamicCastInst::getAs(termInst);
      }
    }

    return SILDynamicCastInst();
  }();
  if (!cast)
    return emitUnknownPatternError();

  auto srcRegionValue = valueMap.getRepresentativeValue(srcRegionValueElt);
  auto srcIsolation = srcIsolationInfo;
  auto dstIsolation = dstIsolationInfo;
  auto srcIsolationStr = srcIsolation.printForDiagnostics(getFunction());
  auto dstIsolationStr = dstIsolation.printForDiagnostics(getFunction());

  diagnoseError(op->getUser(), diag::rbi_merge_failure_cast, srcIsolationStr,
                cast.getTargetFormalType(), dstIsolationStr,
                srcIsolation->isTaskIsolated(), dstIsolation->isTaskIsolated())
      .limitBehaviorIf(getBehaviorLimit());

  emitSideNote(srcRegionValueElt, srcRegionValue, srcIsolation,
               srcIsolationStr);
}

void IncompatibleRegionMergeDiagnosticEmitter::emit() {
  switch (reason) {
  case RegionMergeReason::NonisolatedFunction:
    return emitNonisolatedFunction();
  case RegionMergeReason::NonisolatedClosure:
  case RegionMergeReason::Builtin:
  case RegionMergeReason::Unknown:
    return emitUnknown();
  case RegionMergeReason::Assign:
    return emitAssign();
  case RegionMergeReason::ActorIntroducingInst: {
    auto dstRegionValue = valueMap.getRepresentativeValue(dstRegionValueElt);
    assert(dstRegionValue.hasRegionIntroducingInst());
    if (ApplySite::isa(dstRegionValue.getActorRegionIntroducingInst()))
      return emitIsolatedFunction();
    return emitCast();
  }
  case RegionMergeReason::Cast: {
    return emitCast();
  }
  }
  llvm_unreachable("Unhandled case");
}

//===----------------------------------------------------------------------===//
//                         MARK: Top Level Entrypoint
//===----------------------------------------------------------------------===//

/// Emit diagnostics for all collected verbatim errors.
///
/// Verbatim errors are self-contained and were collected during dataflow
/// evaluation in handleError(). This method iterates through them and
/// dispatches to the appropriate diagnostic emitter.
///
/// This uses X-macros from PartitionOpError.def to generate the switch cases:
/// - PARTITION_OP_ERROR: Should never appear here (handled immediately during
///   dataflow), so we assert unreachable.
/// - PARTITION_OP_VERBATIM_ERROR: Creates a <Name>DiagnosticEmitter and calls
///   emit(). Emitters must follow the naming convention and constructor
///   signature documented in PartitionOpError.def.
/// - PARTITION_OP_VERBATIM_SPECIAL_EMISSION_EMITTER: Generates no code; these
///   errors are handled explicitly below the X-macro expansion.
void SendNonSendableImpl::emitVerbatimErrors() {
  for (auto &erasedError : foundVerbatimErrors) {
    switch (erasedError.getKind()) {
#define PARTITION_OP_ERROR(NAME)                                               \
  case PartitionOpError::NAME:                                                 \
    llvm_unreachable("Handled elsewhere");
#define PARTITION_OP_VERBATIM_ERROR(NAME)                                      \
  case PartitionOpError::NAME: {                                               \
    auto error = std::move(erasedError).get##NAME##Error();                    \
    REGIONBASEDISOLATION_LOG(error.print(llvm::dbgs(), info->getValueMap()));  \
    NAME##DiagnosticEmitter emitter(info, std::move(error));                   \
    emitter.emit();                                                            \
    continue;                                                                  \
  }
#define PARTITION_OP_VERBATIM_SPECIAL_EMISSION_EMITTER(NAME)
#include "swift/SILOptimizer/Utils/PartitionOpError.def"
    // Special handling for InOutSendingParametersInSameRegion: a single error
    // may produce multiple diagnostics (one per pair of conflicting params),
    // and requires filtering based on diagnostic behavior.
    case PartitionOpError::InOutSendingParametersInSameRegion: {
      auto e =
          std::move(erasedError).getInOutSendingParametersInSameRegionError();
      REGIONBASEDISOLATION_LOG(e.print(llvm::dbgs(), info->getValueMap()));
      auto firstParam =
          info->getValueMap().getRepresentativeValue(e.firstInoutSendingParam);
      // Walk through our secondInOutSendingParams and use one that is not
      // ignore.
      for (auto paramElt : e.otherInOutSendingParams) {
        auto paramValue =
            info->getValueMap().getRepresentativeValue(paramElt).getValue();
        if (auto behavior =
                paramValue->getType().getConcurrencyDiagnosticBehavior(
                    info->getFunction());
            behavior && *behavior == DiagnosticBehavior::Ignore) {
          continue;
        }
        // Each reported pair gets its own chain, emitted from inside this loop:
        // a pair skipped above gets no primary diagnostic, so a chain for it
        // would be a note with no error to attach to. The walk consumes the
        // partition it rewinds, so hand each pair its own copy rather than
        // moving the error's.
        std::optional<Partition> pairPartition;
        if (e.partition)
          pairPartition.emplace(*e.partition);
        InOutSendingParametersInSameRegionDiagnosticEmitter diagnosticEmitter(
            info, cast<TermInst>(e.op->getSourceInst()), firstParam.getValue(),
            paramValue, e.firstInoutSendingParam, paramElt,
            std::move(pairPartition));
        diagnosticEmitter.emit();
      }
      continue;
    }
    }
    llvm_unreachable("Covered switch isn't covered?!");
  }
}

/// Once we have reached a fixpoint, this routine runs over all blocks again
/// reporting any failures by applying our ops to the converged dataflow
/// state.
void SendNonSendableImpl::emitDiagnostics() {
  auto *function = info->getFunction();
  REGIONBASEDISOLATION_LOG(llvm::dbgs() << "Emitting diagnostics for function "
                                        << function->getName() << "\n");

  runDiagnosticEvaluator();
  emitUseAfterSendDiagnostics();
  emitVerbatimErrors();
}

namespace {

class SendNonSendable : public SILFunctionTransform {
  void run() override {
    SILFunction *function = getFunction();

    auto *functionInfo = getAnalysis<RegionAnalysis>()->get(function);
    if (!functionInfo->isSupportedFunction()) {
      REGIONBASEDISOLATION_LOG(llvm::dbgs()
                               << "===> SKIPPING UNSUPPORTED FUNCTION: "
                               << function->getName() << '\n');

      return;
    }

    REGIONBASEDISOLATION_LOG(
        llvm::dbgs() << "===> PROCESSING: " << function->getName() << '\n');

    SendNonSendableImpl impl(functionInfo);
    impl.emitDiagnostics();
  }
};

} // end anonymous namespace

SILTransform *swift::createSendNonSendable() { return new SendNonSendable(); }
