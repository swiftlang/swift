//===--- TransferNonSendable.cpp ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/ImmutablePointerSet.h"
#include "swift/SIL/BasicBlockData.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/MemoryLocations.h"
#include "swift/SIL/NodeDatastructures.h"
#include "swift/SIL/OperandDatastructures.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/Test.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/PartitionUtils.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "transfer-non-sendable"

using namespace swift;
using namespace swift::PartitionPrimitives;

namespace {
using TransferringOperandSetFactory = Partition::TransferringOperandSetFactory;
} // namespace

//===----------------------------------------------------------------------===//
//                              MARK: Utilities
//===----------------------------------------------------------------------===//

/// SILApplyCrossesIsolation determines if a SIL instruction is an isolation
/// crossing apply expression. This is done by checking its correspondence to an
/// ApplyExpr AST node, and then checking the internal flags of that AST node to
/// see if the ActorIsolationChecker determined it crossed isolation.  It's
/// possible this is brittle and a more nuanced check is needed, but this
/// suffices for all cases tested so far.
static bool isIsolationBoundaryCrossingApply(const SILInstruction *inst) {
  if (ApplyExpr *apply = inst->getLoc().getAsASTNode<ApplyExpr>())
    return apply->getIsolationCrossing().has_value();

  // We assume that any instruction that does not correspond to an ApplyExpr
  // cannot cross an isolation domain.
  return false;
}

/// Check if the passed in type is NonSendable.
///
/// NOTE: We special case RawPointer and NativeObject to ensure they are
/// treated as non-Sendable and strict checking is applied to it.
static bool isNonSendableType(SILType type, SILFunction *fn) {
  // Treat Builtin.NativeObject and Builtin.RawPointer as non-Sendable.
  if (type.getASTType()->is<BuiltinNativeObjectType>() ||
      type.getASTType()->is<BuiltinRawPointerType>()) {
    return true;
  }

  // Otherwise, delegate to seeing if type conforms to the Sendable protocol.
  return !type.isSendable(fn);
}

namespace {

struct UseDefChainVisitor
    : public AccessUseDefChainVisitor<UseDefChainVisitor, SILValue> {
  bool isMerge = false;

  SILValue visitAll(SILValue sourceAddr) {
    SILValue result = visit(sourceAddr);
    if (!result)
      return sourceAddr;

    while (SILValue nextAddr = visit(result))
      result = nextAddr;

    return result;
  }

  SILValue visitBase(SILValue base, AccessStorage::Kind kind) {
    // If we are passed a project_box, we want to return the box itself. The
    // reason for this is that the project_box is considered to be non-aliasing
    // memory. We want to treat it as part of the box which is
    // aliasing... meaning that we need to merge.
    if (kind == AccessStorage::Box)
      return cast<ProjectBoxInst>(base)->getOperand();
    return SILValue();
  }

  SILValue visitNonAccess(SILValue) { return SILValue(); }

  SILValue visitPhi(SILPhiArgument *phi) {
    llvm_unreachable("Should never hit this");
  }

  // Override AccessUseDefChainVisitor to ignore access markers and find the
  // outer access base.
  SILValue visitNestedAccess(BeginAccessInst *access) {
    return visitAll(access->getSource());
  }

  SILValue visitStorageCast(SingleValueInstruction *cast, Operand *sourceAddr,
                            AccessStorageCast castType) {
    // If this is a type case, see if the result of the cast is sendable. In
    // such a case, we do not want to look through this cast.
    if (castType == AccessStorageCast::Type &&
        !isNonSendableType(cast->getType(), cast->getFunction()))
      return SILValue();

    // If we do not have an identity cast, mark this as a merge.
    isMerge |= castType != AccessStorageCast::Identity;

    return sourceAddr->get();
  }

  SILValue visitAccessProjection(SingleValueInstruction *inst,
                                 Operand *sourceAddr) {
    // See if this access projection is into a single element value. If so, we
    // do not want to treat this as a merge.
    if (auto p = Projection(inst)) {
      switch (p.getKind()) {
      case ProjectionKind::Upcast:
      case ProjectionKind::RefCast:
      case ProjectionKind::BitwiseCast:
      case ProjectionKind::TailElems:
      case ProjectionKind::Box:
      case ProjectionKind::Class:
        llvm_unreachable("Shouldn't see this here");
      case ProjectionKind::Index:
        // Index is always a merge.
        isMerge = true;
        break;
      case ProjectionKind::Enum:
        // Enum is never a merge since it always has a single field.
        break;
      case ProjectionKind::Tuple: {
        // These are merges if we have multiple fields.
        auto *tti = cast<TupleElementAddrInst>(inst);

        // See if our result type is a sendable type. In such a case, we do not
        // want to look through the tuple_element_addr since we do not want to
        // identify the sendable type with the non-sendable operand. These we
        // are always going to ignore anyways since a sendable let/var field of
        // a struct can always be used.
        if (!isNonSendableType(tti->getType(), tti->getFunction()))
          return SILValue();

        isMerge |= tti->getOperand()->getType().getNumTupleElements() > 1;
        break;
      }
      case ProjectionKind::Struct:
        auto *sea = cast<StructElementAddrInst>(inst);

        // See if our result type is a sendable type. In such a case, we do not
        // want to look through the struct_element_addr since we do not want to
        // identify the sendable type with the non-sendable operand. These we
        // are always going to ignore anyways since a sendable let/var field of
        // a struct can always be used.
        if (!isNonSendableType(sea->getType(), sea->getFunction()))
          return SILValue();

        // These are merges if we have multiple fields.
        isMerge |= sea->getOperand()->getType().getNumNominalFields() > 1;
        break;
      }
    }

    return sourceAddr->get();
  }
};

} // namespace

static SILValue getUnderlyingTrackedObjectValue(SILValue value) {
  SILValue result = value;
  while (true) {
    SILValue temp = result;

    temp = getUnderlyingObject(temp);

    if (auto *dsi = dyn_cast_or_null<DestructureStructInst>(
            temp->getDefiningInstruction())) {
      temp = dsi->getOperand();
    }
    if (auto *dti = dyn_cast_or_null<DestructureTupleInst>(
            temp->getDefiningInstruction())) {
      temp = dti->getOperand();
    }

    if (temp != result) {
      result = temp;
      continue;
    }

    return result;
  }
}

static SILValue getUnderlyingTrackedValue(SILValue value) {
  if (!value->getType().isAddress()) {
    return getUnderlyingTrackedObjectValue(value);
  }

  UseDefChainVisitor visitor;
  SILValue base = visitor.visitAll(value);
  assert(base);
  if (isa<GlobalAddrInst>(base))
    return value;
  if (base->getType().isObject())
    return getUnderlyingObject(base);
  return base;
}

namespace {

struct TermArgSources {
  SmallFrozenMultiMap<SILValue, SILValue, 8> argSources;

  template <typename ValueRangeTy = ArrayRef<SILValue>>
  void addValues(ValueRangeTy valueRange, SILBasicBlock *destBlock) {
    for (auto pair : llvm::enumerate(valueRange))
      argSources.insert(destBlock->getArgument(pair.index()), pair.value());
  }
};

} // namespace

static bool isProjectedFromAggregate(SILValue value) {
  assert(value->getType().isAddress());
  UseDefChainVisitor visitor;
  visitor.visitAll(value);
  return visitor.isMerge;
}

static PartialApplyInst *findAsyncLetPartialApplyFromStart(BuiltinInst *bi) {
  // If our operand is Sendable then we want to return nullptr. We only want to
  // return a value if we are not
  SILValue value = bi->getOperand(1);
  auto fType = value->getType().castTo<SILFunctionType>();
  if (fType->isSendable())
    return nullptr;

  SILValue temp = value;
  while (true) {
    if (isa<ConvertEscapeToNoEscapeInst>(temp) ||
        isa<ConvertFunctionInst>(temp)) {
      temp = cast<SingleValueInstruction>(temp)->getOperand(0);
    }
    if (temp == value)
      break;
    value = temp;
  }

  return cast<PartialApplyInst>(value);
}

static PartialApplyInst *findAsyncLetPartialApplyFromGet(ApplyInst *ai) {
  auto *bi = cast<BuiltinInst>(FullApplySite(ai).getArgument(0));
  assert(*bi->getBuiltinKind() ==
         BuiltinValueKind::StartAsyncLetWithLocalBuffer);
  return findAsyncLetPartialApplyFromStart(bi);
}

static bool isAsyncLetBeginPartialApply(PartialApplyInst *pai) {
  auto *cfi = pai->getSingleUserOfType<ConvertFunctionInst>();
  if (!cfi)
    return false;

  auto *cvt = cfi->getSingleUserOfType<ConvertEscapeToNoEscapeInst>();
  if (!cvt)
    return false;

  auto *bi = cvt->getSingleUserOfType<BuiltinInst>();
  if (!bi)
    return false;

  auto kind = bi->getBuiltinKind();
  if (!kind)
    return false;

  return *kind == BuiltinValueKind::StartAsyncLetWithLocalBuffer;
}

//===----------------------------------------------------------------------===//
//                             MARK: Diagnostics
//===----------------------------------------------------------------------===//

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(ASTContext &context, SourceLoc loc,
                                   Diag<T...> diag, U &&...args) {
  return context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(const PartitionOp &op, Diag<T...> diag,
                                   U &&...args) {
  return ::diagnose(op.getSourceInst()->getFunction()->getASTContext(),
                    op.getSourceLoc().getSourceLoc(), diag,
                    std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(const Operand *op, Diag<T...> diag,
                                   U &&...args) {
  return ::diagnose(op->getUser()->getFunction()->getASTContext(),
                    op->getUser()->getLoc().getSourceLoc(), diag,
                    std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnose(const SILInstruction *inst, Diag<T...> diag,
                                   U &&...args) {
  return ::diagnose(inst->getFunction()->getASTContext(),
                    inst->getLoc().getSourceLoc(), diag,
                    std::forward<U>(args)...);
}

//===----------------------------------------------------------------------===//
//                      MARK: Partial Apply Reachability
//===----------------------------------------------------------------------===//

namespace {

/// We need to be able to know if instructions that extract sendable fields from
/// non-sendable addresses are reachable from a partial_apply that captures the
/// non-sendable value or its underlying object by reference. In such a case, we
/// need to require the value to not be transferred when the extraction happens
/// since we could race on extracting the value.
///
/// The reason why we use a dataflow to do this is that:
///
/// 1. We do not want to recompute this for each individual instruction that
/// might be reachable from the partial apply.
///
/// 2. Just computing reachability early is a very easy way to do this.
struct PartialApplyReachabilityDataflow {
  PostOrderFunctionInfo *pofi;
  llvm::DenseMap<SILValue, unsigned> valueToBit;
  std::vector<std::pair<SILValue, SILInstruction *>> valueToGenInsts;

  struct BlockState {
    SmallBitVector entry;
    SmallBitVector exit;
    SmallBitVector gen;
    bool needsUpdate = true;
  };

  BasicBlockData<BlockState> blockData;
  bool propagatedReachability = false;

  PartialApplyReachabilityDataflow(SILFunction *fn, PostOrderFunctionInfo *pofi)
      : pofi(pofi), blockData(fn) {}

  /// Begin tracking an operand of a partial apply.
  void add(Operand *op);

  /// Once we have finished adding data to the data, propagate reachability.
  void propagateReachability();

  bool isReachable(SILValue value, SILInstruction *user) const;
  bool isReachable(Operand *op) const {
    return isReachable(op->get(), op->getUser());
  }

  bool isGenInstruction(SILValue value, SILInstruction *inst) const {
    assert(propagatedReachability && "Only valid once propagated reachability");
    auto iter =
        std::lower_bound(valueToGenInsts.begin(), valueToGenInsts.end(),
                         std::make_pair(value, nullptr),
                         [](const std::pair<SILValue, SILInstruction *> &p1,
                            const std::pair<SILValue, SILInstruction *> &p2) {
                           return p1 < p2;
                         });
    return iter != valueToGenInsts.end() && iter->first == value &&
           iter->second == inst;
  }

  void print(llvm::raw_ostream &os) const;

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

private:
  SILValue getRootValue(SILValue value) const {
    return getUnderlyingTrackedValue(value);
  }

  unsigned getBitForValue(SILValue value) const {
    unsigned size = valueToBit.size();
    auto &self = const_cast<PartialApplyReachabilityDataflow &>(*this);
    auto iter = self.valueToBit.try_emplace(value, size);
    return iter.first->second;
  }
};

} // namespace

void PartialApplyReachabilityDataflow::add(Operand *op) {
  assert(!propagatedReachability &&
         "Cannot add more operands once reachability is computed");
  SILValue underlyingValue = getRootValue(op->get());
  LLVM_DEBUG(llvm::dbgs() << "PartialApplyReachability::add.\nValue: "
                          << underlyingValue << "User: " << *op->getUser());

  unsigned bit = getBitForValue(underlyingValue);
  auto &state = blockData[op->getParentBlock()];
  state.gen.resize(bit + 1);
  state.gen.set(bit);
  valueToGenInsts.emplace_back(underlyingValue, op->getUser());
}

bool PartialApplyReachabilityDataflow::isReachable(SILValue value,
                                                   SILInstruction *user) const {
  assert(
      propagatedReachability &&
      "Can only check for reachability once reachability has been propagated");
  SILValue baseValue = getRootValue(value);
  auto iter = valueToBit.find(baseValue);
  // If we aren't tracking this value... just bail.
  if (iter == valueToBit.end())
    return false;
  unsigned bitNum = iter->second;
  auto &state = blockData[user->getParent()];

  // If we are reachable at entry, then we are done.
  if (state.entry.test(bitNum)) {
    return true;
  }

  // Otherwise, check if we are reachable at exit. If we are not, then we are
  // not reachable.
  if (!state.exit.test(bitNum)) {
    return false;
  }

  // We were not reachable at entry but are at our exit... walk the block and
  // see if our user is before a gen instruction.
  auto genStart = std::lower_bound(
      valueToGenInsts.begin(), valueToGenInsts.end(),
      std::make_pair(baseValue, nullptr),
      [](const std::pair<SILValue, SILInstruction *> &p1,
         const std::pair<SILValue, SILInstruction *> &p2) { return p1 < p2; });
  if (genStart == valueToGenInsts.end() || genStart->first != baseValue)
    return false;

  auto genEnd = genStart;
  while (genEnd->first == baseValue)
    ++genEnd;

  // Walk forward from the beginning of the block to user. If we do not find a
  // gen instruction, then we know the gen occurs after the op.
  return llvm::any_of(
      user->getParent()->getRangeEndingAtInst(user), [&](SILInstruction &inst) {
        auto iter = std::lower_bound(
            genStart, genEnd, std::make_pair(baseValue, &inst),
            [](const std::pair<SILValue, SILInstruction *> &p1,
               const std::pair<SILValue, SILInstruction *> &p2) {
              return p1 < p2;
            });
        return iter != valueToGenInsts.end() && iter->first == baseValue &&
               iter->second == &inst;
      });
}

void PartialApplyReachabilityDataflow::propagateReachability() {
  assert(!propagatedReachability && "Cannot propagate reachability twice");
  propagatedReachability = true;

  // Now that we have finished initializing, resize all of our bitVectors to the
  // final number of bits.
  unsigned numBits = valueToBit.size();

  // If numBits is none, we have nothing to process.
  if (numBits == 0)
    return;

  for (auto iter : blockData) {
    iter.data.entry.resize(numBits);
    iter.data.exit.resize(numBits);
    iter.data.gen.resize(numBits);
    iter.data.needsUpdate = true;
  }

  // Freeze our value to gen insts map so we can perform in block checks.
  sortUnique(valueToGenInsts);

  // We perform a simple gen-kill dataflow with union. Since we are just
  // propagating reachability, there isn't any kill.
  bool anyNeedUpdate = true;
  SmallBitVector temp(numBits);
  blockData[&*blockData.getFunction()->begin()].needsUpdate = true;
  while (anyNeedUpdate) {
    anyNeedUpdate = false;

    for (auto *block : pofi->getReversePostOrder()) {
      auto &state = blockData[block];

      if (!state.needsUpdate) {
        continue;
      }

      state.needsUpdate = false;
      temp.reset();
      for (auto *predBlock : block->getPredecessorBlocks()) {
        auto &predState = blockData[predBlock];
        temp |= predState.exit;
      }

      state.entry = temp;

      temp |= state.gen;

      if (temp != state.exit) {
        state.exit = temp;
        for (auto *succBlock : block->getSuccessorBlocks()) {
          anyNeedUpdate = true;
          blockData[succBlock].needsUpdate = true;
        }
      }
    }
  }

  LLVM_DEBUG(llvm::dbgs() << "Propagating Captures Result!\n";
             print(llvm::dbgs()));
}

void PartialApplyReachabilityDataflow::print(llvm::raw_ostream &os) const {
  // This is only invoked for debugging purposes, so make nicer output.
  std::vector<std::pair<unsigned, SILValue>> data;
  for (auto [value, bitNo] : valueToBit) {
    data.emplace_back(bitNo, value);
  }
  std::sort(data.begin(), data.end());

  os << "(BitNo, Value):\n";
  for (auto [bitNo, value] : data) {
    os << "    " << bitNo << ": " << value;
  }

  os << "(Block,GenBits):\n";
  for (auto [block, state] : blockData) {
    os << "    bb" << block.getDebugID() << ".\n"
       << "        Entry: " << state.entry << '\n'
       << "        Gen: " << state.gen << '\n'
       << "        Exit: " << state.exit << '\n';
  }
}

//===----------------------------------------------------------------------===//
//                 MARK: Expr/Type Inference for Diagnostics
//===----------------------------------------------------------------------===//

namespace {

struct InferredCallerArgumentTypeInfo {
  Type baseInferredType;
  SmallVector<std::pair<Type, std::optional<ApplyIsolationCrossing>>, 4>
      applyUses;

  void init(const Operand *op);

  void initForApply(const Operand *op, ApplyExpr *expr);
  void initForAutoclosure(const Operand *op, AutoClosureExpr *expr);

  Expr *getFoundExprForSelf(ApplyExpr *sourceApply) {
    if (auto callExpr = dyn_cast<CallExpr>(sourceApply))
      if (auto calledExpr =
              dyn_cast<DotSyntaxCallExpr>(callExpr->getDirectCallee()))
        return calledExpr->getBase();
    return nullptr;
  }

  Expr *getFoundExprForParam(ApplyExpr *sourceApply, unsigned argNum) {
    auto *expr = sourceApply->getArgs()->getExpr(argNum);

    // If we have an erasure expression, lets use the original type. We do
    // this since we are not saying the specific parameter that is the
    // issue and we are using the type to explain it to the user.
    if (auto *erasureExpr = dyn_cast<ErasureExpr>(expr))
      expr = erasureExpr->getSubExpr();

    return expr;
  }
};

} // namespace

void InferredCallerArgumentTypeInfo::initForApply(const Operand *op,
                                                  ApplyExpr *sourceApply) {
  auto isolationCrossing = *sourceApply->getIsolationCrossing();

  // Grab out full apply site and see if we can find a better expr.
  SILInstruction *i = const_cast<SILInstruction *>(op->getUser());
  auto fai = FullApplySite::isa(i);

  Expr *foundExpr = nullptr;

  // If we have self, then infer it.
  if (fai.hasSelfArgument() && op == &fai.getSelfArgumentOperand()) {
    foundExpr = getFoundExprForSelf(sourceApply);
  } else {
    // Otherwise, try to infer using the operand of the ApplyExpr.
    unsigned argNum = [&]() -> unsigned {
      if (fai.isCalleeOperand(*op))
        return op->getOperandNumber();
      return fai.getAppliedArgIndex(*op);
    }();
    assert(argNum < sourceApply->getArgs()->size());
    foundExpr = getFoundExprForParam(sourceApply, argNum);
  }

  auto inferredArgType =
      foundExpr ? foundExpr->findOriginalType() : baseInferredType;
  applyUses.emplace_back(inferredArgType, isolationCrossing);
}

namespace {

struct Walker : ASTWalker {
  InferredCallerArgumentTypeInfo &foundTypeInfo;
  ValueDecl *targetDecl;
  SmallPtrSet<Expr *, 8> visitedCallExprDeclRefExprs;

  Walker(InferredCallerArgumentTypeInfo &foundTypeInfo, ValueDecl *targetDecl)
      : foundTypeInfo(foundTypeInfo), targetDecl(targetDecl) {}

  Expr *lookThroughExpr(Expr *expr) {
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
      // If this decl ref expr was not visited as part of a callExpr, add it as
      // something without isolation crossing.
      if (!visitedCallExprDeclRefExprs.count(declRef)) {
        if (declRef->getDecl() == targetDecl) {
          visitedCallExprDeclRefExprs.insert(declRef);
          foundTypeInfo.applyUses.emplace_back(declRef->findOriginalType(),
                                               std::nullopt);
          return Action::Continue(expr);
        }
      }
    }

    if (auto *callExpr = dyn_cast<CallExpr>(expr)) {
      if (auto isolationCrossing = callExpr->getIsolationCrossing()) {
        // Search callExpr's arguments to see if we have our targetDecl.
        auto *argList = callExpr->getArgs();
        for (auto pair : llvm::enumerate(argList->getArgExprs())) {
          auto *arg = lookThroughExpr(pair.value());
          if (auto *declRef = dyn_cast<DeclRefExpr>(arg)) {
            if (declRef->getDecl() == targetDecl) {
              // Found our target!
              visitedCallExprDeclRefExprs.insert(declRef);
              foundTypeInfo.applyUses.emplace_back(declRef->findOriginalType(),
                                                   isolationCrossing);
              return Action::Continue(expr);
            }
          }
        }
      }
    }

    return Action::Continue(expr);
  }
};

} // namespace

void InferredCallerArgumentTypeInfo::init(const Operand *op) {
  baseInferredType = op->get()->getType().getASTType();

  auto loc = op->getUser()->getLoc();
  if (auto *sourceApply = loc.getAsASTNode<ApplyExpr>()) {
    initForApply(op, sourceApply);
  } else {
    auto *autoClosureExpr = loc.getAsASTNode<AutoClosureExpr>();
    if (!autoClosureExpr) {
      llvm::report_fatal_error("Unknown node");
    }

    SILInstruction *i = const_cast<SILInstruction *>(op->getUser());
    auto pai = ApplySite::isa(i);
    unsigned captureIndex = pai.getAppliedArgIndex(*op);

    auto captureInfo =
        autoClosureExpr->getCaptureInfo().getCaptures()[captureIndex];
    auto *captureDecl = captureInfo.getDecl();
    Walker walker(*this, captureDecl);
    autoClosureExpr->walk(walker);
  }
}

//===----------------------------------------------------------------------===//
//                       MARK: Instruction Level Model
//===----------------------------------------------------------------------===//

namespace {

constexpr const char *SEP_STR = "╾──────────────────────────────╼\n";

using TrackableValueID = PartitionPrimitives::Element;
using Region = PartitionPrimitives::Region;

enum class TrackableValueFlag {
  /// Base value that says a value is uniquely represented and is
  /// non-sendable. Example: an alloc_stack of a non-Sendable type that isn't
  /// captured by a closure.
  None = 0x0,

  /// Set to true if this TrackableValue's representative is not uniquely
  /// represented so may have aliases. Example: a value that isn't an
  /// alloc_stack.
  isMayAlias = 0x1,

  /// Set to true if this TrackableValue's representative is Sendable.
  isSendable = 0x2,

  /// Set to true if this TrackableValue is a non-sendable object derived from
  /// an actor. Example: a value loaded from a ref_element_addr from an actor.
  ///
  /// NOTE: We track values with an actor representative even though actors are
  /// sendable to be able to properly identify values that escape an actor since
  /// if we escape an actor into a closure, we want to mark the closure as actor
  /// derived.
  isActorDerived = 0x4,
};

using TrackedValueFlagSet = OptionSet<TrackableValueFlag>;

class TrackableValueState {
  unsigned id;
  TrackedValueFlagSet flagSet = {TrackableValueFlag::isMayAlias};

public:
  TrackableValueState(unsigned newID) : id(newID) {}

  bool isMayAlias() const {
    return flagSet.contains(TrackableValueFlag::isMayAlias);
  }

  bool isNoAlias() const { return !isMayAlias(); }

  bool isSendable() const {
    return flagSet.contains(TrackableValueFlag::isSendable);
  }

  bool isNonSendable() const { return !isSendable(); }

  bool isActorDerived() const {
    return flagSet.contains(TrackableValueFlag::isActorDerived);
  }

  TrackableValueID getID() const { return TrackableValueID(id); }

  void addFlag(TrackableValueFlag flag) { flagSet |= flag; }

  void removeFlag(TrackableValueFlag flag) { flagSet -= flag; }

  void print(llvm::raw_ostream &os) const {
    os << "TrackableValueState[id: " << id
       << "][is_no_alias: " << (isNoAlias() ? "yes" : "no")
       << "][is_sendable: " << (isSendable() ? "yes" : "no")
       << "][is_actor_derived: " << (isActorDerived() ? "yes" : "no") << "].";
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

/// A tuple consisting of a base value and its value state.
///
/// DISCUSSION: We are computing regions among equivalence classes of values
/// with GEPs like struct_element_addr being considered equivalent from a value
/// perspective to their underlying base value.
///
/// Example:
///
/// ```
/// %0 = alloc_stack $Struct
/// %1 = struct_element_addr %0 : $Struct.childField
/// %2 = struct_element_addr %1 : $ChildField.grandchildField
/// ```
///
/// In the above example, %2 will be mapped to %0 by our value mapping.
class TrackableValue {
  SILValue representativeValue;
  TrackableValueState valueState;

public:
  TrackableValue(SILValue representativeValue, TrackableValueState valueState)
      : representativeValue(representativeValue), valueState(valueState) {}

  bool isMayAlias() const { return valueState.isMayAlias(); }

  bool isNoAlias() const { return !isMayAlias(); }

  bool isSendable() const { return valueState.isSendable(); }

  bool isNonSendable() const { return !isSendable(); }

  bool isActorDerived() const { return valueState.isActorDerived(); }

  TrackableValueID getID() const {
    return TrackableValueID(valueState.getID());
  }

  /// Return the representative value of this equivalence class of values.
  SILValue getRepresentative() const { return representativeValue; }

  void print(llvm::raw_ostream &os) const {
    os << "TrackableValue. State: ";
    valueState.print(os);
    os << "\n    Rep Value: " << *getRepresentative();
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

class PartitionOpTranslator;

struct PartitionOpBuilder {
  /// Parent translator that contains state.
  PartitionOpTranslator *translator;

  /// Used to statefully track the instruction currently being translated, for
  /// insertion into generated PartitionOps.
  SILInstruction *currentInst = nullptr;

  /// List of partition ops mapped to the current instruction. Used when
  /// generating partition ops.
  SmallVector<PartitionOp, 8> currentInstPartitionOps;

  void reset(SILInstruction *inst) {
    currentInst = inst;
    currentInstPartitionOps.clear();
  }

  TrackableValueID lookupValueID(SILValue value);
  bool valueHasID(SILValue value, bool dumpIfHasNoID = false);

  void addAssignFresh(SILValue value) {
    currentInstPartitionOps.emplace_back(
        PartitionOp::AssignFresh(lookupValueID(value), currentInst));
  }

  void addAssign(SILValue tgt, SILValue src) {
    assert(valueHasID(src, /*dumpIfHasNoID=*/true) &&
           "source value of assignment should already have been encountered");

    TrackableValueID srcID = lookupValueID(src);
    if (lookupValueID(tgt) == srcID) {
      LLVM_DEBUG(llvm::dbgs() << "    Skipping assign since tgt and src have "
                                 "the same representative.\n");
      LLVM_DEBUG(llvm::dbgs() << "    Rep ID: %%" << srcID.num << ".\n");
      return;
    }

    currentInstPartitionOps.emplace_back(PartitionOp::Assign(
        lookupValueID(tgt), lookupValueID(src), currentInst));
  }

  void addTransfer(SILValue representative, Operand *op) {
    assert(valueHasID(representative) &&
           "transferred value should already have been encountered");

    currentInstPartitionOps.emplace_back(
        PartitionOp::Transfer(lookupValueID(representative), op));
  }

  void addUndoTransfer(SILValue representative,
                       SILInstruction *untransferringInst) {
    assert(valueHasID(representative) &&
           "transferred value should already have been encountered");

    currentInstPartitionOps.emplace_back(PartitionOp::UndoTransfer(
        lookupValueID(representative), untransferringInst));
  }

  void addMerge(SILValue fst, SILValue snd) {
    assert(valueHasID(fst, /*dumpIfHasNoID=*/true) &&
           valueHasID(snd, /*dumpIfHasNoID=*/true) &&
           "merged values should already have been encountered");

    if (lookupValueID(fst) == lookupValueID(snd))
      return;

    currentInstPartitionOps.emplace_back(PartitionOp::Merge(
        lookupValueID(fst), lookupValueID(snd), currentInst));
  }

  void addRequire(SILValue value) {
    assert(valueHasID(value, /*dumpIfHasNoID=*/true) &&
           "required value should already have been encountered");
    currentInstPartitionOps.emplace_back(
        PartitionOp::Require(lookupValueID(value), currentInst));
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void print(llvm::raw_ostream &os) const;
};

/// PartitionOpTranslator is responsible for performing the translation from
/// SILInstructions to PartitionOps. Not all SILInstructions have an effect on
/// the region partition, and some have multiple effects - such as an
/// application pairwise merging its arguments - so the core functions like
/// translateSILBasicBlock map SILInstructions to std::vectors of PartitionOps.
/// No more than a single instance of PartitionOpTranslator should be used for
/// each SILFunction, as SILValues are assigned unique IDs through the
/// nodeIDMap. Some special correspondences between SIL values are also tracked
/// statefully by instances of this class, such as the "projection"
/// relationship: instructions like begin_borrow and begin_access create
/// effectively temporary values used for alternative access to base "projected"
/// values. These are tracked to implement "write-through" semantics for
/// assignments to projections when they're addresses.
///
/// TODO: when translating basic blocks, optimizations might be possible
///       that reduce lists of PartitionOps to smaller, equivalent lists
class PartitionOpTranslator {
  friend PartitionOpBuilder;

  SILFunction *function;

  /// A map from the representative of an equivalence class of values to their
  /// TrackableValueState. The state contains both the unique value id for the
  /// equivalence class of values as well as whether we determined if they are
  /// uniquely identified and sendable.
  ///
  /// nodeIDMap stores unique IDs for all SILNodes corresponding to
  /// non-Sendable values. Implicit conversion from SILValue used pervasively.
  /// ensure getUnderlyingTrackedValue is called on SILValues before entering
  /// into this map
  llvm::DenseMap<SILValue, TrackableValueState> equivalenceClassValuesToState;
  llvm::DenseMap<unsigned, SILValue> stateIndexToEquivalenceClass;

  /// A list of values that can never be transferred.
  ///
  /// This only includes function arguments.
  std::vector<TrackableValueID> neverTransferredValueIDs;

  /// A cache of argument IDs.
  std::optional<Partition> functionArgPartition;

  /// A builder struct that we use to convert individual instructions into lists
  /// of PartitionOps.
  PartitionOpBuilder builder;

  PartialApplyReachabilityDataflow partialApplyReachabilityDataflow;

  std::optional<TrackableValue> tryToTrackValue(SILValue value) const {
    auto state = getTrackableValue(value);
    if (state.isNonSendable())
      return state;
    return {};
  }

  /// If \p isAddressCapturedByPartialApply is set to true, then this value is
  /// an address that is captured by a partial_apply and we want to treat it as
  /// may alias.
  TrackableValue
  getTrackableValue(SILValue value,
                    bool isAddressCapturedByPartialApply = false) const {
    value = getUnderlyingTrackedValue(value);

    auto *self = const_cast<PartitionOpTranslator *>(this);
    auto iter = self->equivalenceClassValuesToState.try_emplace(
        value, TrackableValueState(equivalenceClassValuesToState.size()));

    // If we did not insert, just return the already stored value.
    if (!iter.second) {
      return {iter.first->first, iter.first->second};
    }

    self->stateIndexToEquivalenceClass[iter.first->second.getID()] = value;

    // Otherwise, we need to compute our flags. Begin by seeing if we have a
    // value that we can prove is not aliased.
    if (value->getType().isAddress()) {
      if (auto accessStorage = AccessStorage::compute(value))
        if (accessStorage.isUniquelyIdentified() &&
            !isAddressCapturedByPartialApply)
          iter.first->getSecond().removeFlag(TrackableValueFlag::isMayAlias);
    }

    // Then see if we have a sendable value. By default we assume values are not
    // sendable.
    if (auto *defInst = value.getDefiningInstruction()) {
      // Though these values are technically non-Sendable, we can safely and
      // consistently treat them as Sendable.
      if (isa<ClassMethodInst, FunctionRefInst>(defInst)) {
        iter.first->getSecond().addFlag(TrackableValueFlag::isSendable);
        return {iter.first->first, iter.first->second};
      }
    }

    // Otherwise refer to the oracle.
    if (!isNonSendableType(value->getType()))
      iter.first->getSecond().addFlag(TrackableValueFlag::isSendable);

    // Check if our base is a ref_element_addr from an actor. In such a case,
    // mark this value as actor derived.
    if (isa<LoadInst, LoadBorrowInst>(iter.first->first)) {
      auto *svi = cast<SingleValueInstruction>(iter.first->first);
      auto storage = AccessStorageWithBase::compute(svi->getOperand(0));
      if (storage.storage && isa<RefElementAddrInst>(storage.base)) {
        if (storage.storage.getRoot()->getType().isActor()) {
          iter.first->getSecond().addFlag(TrackableValueFlag::isActorDerived);
        }
      }
    }

    // If our access storage is from a class, then see if we have an actor. In
    // such a case, we need to add this id to the neverTransferred set.

    return {iter.first->first, iter.first->second};
  }

  bool markValueAsActorDerived(SILValue value) {
    value = getUnderlyingTrackedValue(value);
    auto iter = equivalenceClassValuesToState.find(value);
    if (iter == equivalenceClassValuesToState.end())
      return false;
    iter->getSecond().addFlag(TrackableValueFlag::isActorDerived);
    return true;
  }

  void gatherFlowInsensitiveInformationBeforeDataflow() {
    LLVM_DEBUG(llvm::dbgs() << ">>> Performing pre-dataflow scan to gather "
                               "flow insensitive information "
                            << function->getName() << ":\n");

    for (auto &block : *function) {
      for (auto &inst : block) {
        if (auto *pai = dyn_cast<PartialApplyInst>(&inst)) {
          ApplySite applySite(pai);
          for (Operand &op : applySite.getArgumentOperands()) {
            // See if this operand is inout_aliasable or is passed as a box. In
            // such a case, we are passing by reference so we need to add it to
            // the reachability.
            if (applySite.getArgumentConvention(op) ==
                    SILArgumentConvention::Indirect_InoutAliasable ||
                op.get()->getType().is<SILBoxType>())
              partialApplyReachabilityDataflow.add(&op);

            // See if this instruction is a partial apply whose non-sendable
            // address operands we need to mark as captured_uniquely identified.
            //
            // If we find an address or a box of a non-Sendable type that is
            // passed to a partial_apply, mark the value's representative as
            // being uniquely identified and captured.
            SILValue val = op.get();
            if (val->getType().isAddress() &&
                isNonSendableType(val->getType())) {
              auto trackVal = getTrackableValue(val, true);
              (void)trackVal;
              LLVM_DEBUG(trackVal.print(llvm::dbgs()));
              continue;
            }
            if (auto *pbi = dyn_cast<ProjectBoxInst>(val)) {
              if (isNonSendableType(
                      pbi->getType().getSILBoxFieldType(function))) {
                auto trackVal = getTrackableValue(val, true);
                (void)trackVal;
                continue;
              }
            }
          }
        }
      }
    }

    // Once we have finished processing all blocks, propagate reachability.
    partialApplyReachabilityDataflow.propagateReachability();
  }

public:
  PartitionOpTranslator(SILFunction *function, PostOrderFunctionInfo *pofi)
      : function(function), functionArgPartition(), builder(),
        partialApplyReachabilityDataflow(function, pofi) {
    builder.translator = this;
    gatherFlowInsensitiveInformationBeforeDataflow();

    LLVM_DEBUG(llvm::dbgs() << "Initializing Function Args:\n");
    auto functionArguments = function->getArguments();
    if (functionArguments.empty()) {
      LLVM_DEBUG(llvm::dbgs() << "    None.\n");
      functionArgPartition = Partition::singleRegion({});
      return;
    }

    llvm::SmallVector<Element, 8> nonSendableJoinedIndices;
    llvm::SmallVector<Element, 8> nonSendableSeparateIndices;
    for (SILArgument *arg : functionArguments) {
      if (auto state = tryToTrackValue(arg)) {
        LLVM_DEBUG(llvm::dbgs() << "    %%" << state->getID() << ": ");

        // If we have a function argument that is closure captured by a Sendable
        // closure, allow for the argument to be transferred.
        //
        // DISCUSSION: The reason that we do this is that in the case of us
        // having an actual Sendable closure there are two cases we can see:
        //
        // 1. If we have an actual Sendable closure, the AST will emit an
        // earlier error saying that we are capturing a non-Sendable value in a
        // Sendable closure. So we want to squelch the error that we would emit
        // otherwise. This only occurs when we are not in swift-6 mode since in
        // swift-6 mode we will error on the earlier error... but in the case of
        // us not being in swift 6 mode lets not emit extra errors.
        //
        // 2. If we have an async-let based Sendable closure, we want to allow
        // for the value to be transferred and not emit an error.
        if (!cast<SILFunctionArgument>(arg)->isClosureCapture() ||
            !function->getLoweredFunctionType()->isSendable()) {
          neverTransferredValueIDs.push_back(state->getID());
          nonSendableJoinedIndices.push_back(state->getID());
        } else {
          nonSendableSeparateIndices.push_back(state->getID());
        }
        LLVM_DEBUG(llvm::dbgs() << *arg);
      }
    }

    functionArgPartition = Partition::singleRegion(nonSendableJoinedIndices);
    for (Element elt : nonSendableSeparateIndices) {
      functionArgPartition->addElement(elt);
    }
  }

  std::optional<TrackableValue> getValueForId(TrackableValueID id) const {
    auto iter = stateIndexToEquivalenceClass.find(id);
    if (iter == stateIndexToEquivalenceClass.end())
      return {};
    auto iter2 = equivalenceClassValuesToState.find(iter->second);
    if (iter2 == equivalenceClassValuesToState.end())
      return {};
    return {{iter2->first, iter2->second}};
  }

  bool isClosureCaptured(SILValue value, SILInstruction *inst) const {
    return partialApplyReachabilityDataflow.isReachable(value, inst);
  }

private:
  bool valueHasID(SILValue value, bool dumpIfHasNoID = false) {
    assert(getTrackableValue(value).isNonSendable() &&
           "Can only accept non-Sendable values");
    bool hasID = equivalenceClassValuesToState.count(value);
    if (!hasID && dumpIfHasNoID) {
      llvm::errs() << "FAILURE: valueHasID of ";
      value->print(llvm::errs());
      llvm::report_fatal_error("standard compiler error");
    }
    return hasID;
  }

  /// Create or lookup the internally assigned unique ID of a SILValue.
  TrackableValueID lookupValueID(SILValue value) {
    auto state = getTrackableValue(value);
    assert(state.isNonSendable() &&
           "only non-Sendable values should be entered in the map");
    return state.getID();
  }

  /// Check if the passed in type is NonSendable.
  ///
  /// NOTE: We special case RawPointer and NativeObject to ensure they are
  /// treated as non-Sendable and strict checking is applied to it.
  bool isNonSendableType(SILType type) const {
    return ::isNonSendableType(type, function);
  }

  // ===========================================================================

public:
  /// Return the partition consisting of all function arguments.
  ///
  /// Used to initialize the entry blocko of our analysis.
  const Partition &getEntryPartition() const { return *functionArgPartition; }

  /// Get the vector of IDs that cannot be legally transferred at any point in
  /// this function.
  ArrayRef<TrackableValueID> getNeverTransferredValues() const {
    return llvm::makeArrayRef(neverTransferredValueIDs);
  }

  void sortUniqueNeverTransferredValues() {
    // TODO: Make a FrozenSetVector.
    sortUnique(neverTransferredValueIDs);
  }

  /// Get the results of an apply instruction.
  ///
  /// This is the single result value for most apply instructions, but for try
  /// apply it is the two arguments to each succ block.
  void getApplyResults(const SILInstruction *inst,
                       SmallVectorImpl<SILValue> &foundResults) {
    if (isa<ApplyInst, BeginApplyInst, BuiltinInst, PartialApplyInst>(inst)) {
      copy(inst->getResults(), std::back_inserter(foundResults));
      return;
    }

    if (auto tryApplyInst = dyn_cast<TryApplyInst>(inst)) {
      foundResults.emplace_back(tryApplyInst->getNormalBB()->getArgument(0));
      if (tryApplyInst->getErrorBB()->getNumArguments() > 0)
        foundResults.emplace_back(tryApplyInst->getErrorBB()->getArgument(0));
      return;
    }

    llvm::report_fatal_error("all apply instructions should be covered");
  }

#ifndef NDEBUG
  void dumpValues() const {
    // Since this is just used for debug output, be inefficient to make nicer
    // output.
    std::vector<std::pair<unsigned, SILValue>> temp;
    for (auto p : stateIndexToEquivalenceClass) {
      temp.emplace_back(p.first, p.second);
    }
    std::sort(temp.begin(), temp.end());
    for (auto p : temp) {
      LLVM_DEBUG(llvm::dbgs() << "%%" << p.first << ": " << p.second);
    }
  }
#endif

  enum SILMultiAssignFlags : uint8_t {
    None = 0x0,

    /// Set to true if this SILMultiAssign call should assume that we are
    /// creating a new value that is guaranteed to be propagating actor self.
    ///
    /// As an example, this is used when a partial_apply captures an actor. Even
    /// though we are doing an assign fresh, we want to make sure that the
    /// closure is viewed as coming from an actor.
    PropagatesActorSelf = 0x1,
  };
  using SILMultiAssignOptions = OptionSet<SILMultiAssignFlags>;

  /// Require all non-sendable sources, merge their regions, and assign the
  /// resulting region to all non-sendable targets, or assign non-sendable
  /// targets to a fresh region if there are no non-sendable sources.
  template <typename TargetRange, typename SourceRange>
  void translateSILMultiAssign(const TargetRange &resultValues,
                               const SourceRange &sourceValues,
                               SILMultiAssignOptions options = {}) {
    SmallVector<SILValue, 8> assignOperands;
    SmallVector<SILValue, 8> assignResults;

    for (SILValue src : sourceValues) {
      if (auto value = tryToTrackValue(src)) {
        assignOperands.push_back(value->getRepresentative());
      }
    }

    for (SILValue result : resultValues) {
      if (auto value = tryToTrackValue(result)) {
        assignResults.push_back(value->getRepresentative());
        // TODO: Can we pass back a reference to value perhaps?
        if (options.contains(SILMultiAssignFlags::PropagatesActorSelf)) {
          markValueAsActorDerived(result);
        }
      }
    }

    // Require all srcs.
    for (auto src : assignOperands)
      builder.addRequire(src);

    // Merge all srcs.
    for (unsigned i = 1; i < assignOperands.size(); i++) {
      builder.addMerge(assignOperands[i - 1], assignOperands[i]);
    }

    // If we do not have any non sendable results, return early.
    if (assignResults.empty())
      return;

    auto assignResultsRef = llvm::makeArrayRef(assignResults);
    SILValue front = assignResultsRef.front();
    assignResultsRef = assignResultsRef.drop_front();

    if (assignOperands.empty()) {
      // If no non-sendable srcs, non-sendable tgts get a fresh region.
      builder.addAssignFresh(front);
    } else {
      builder.addAssign(front, assignOperands.front());
    }

    // Assign all targets to the target region.
    while (assignResultsRef.size()) {
      SILValue next = assignResultsRef.front();
      assignResultsRef = assignResultsRef.drop_front();

      builder.addAssign(next, front);
    }
  }

  void translateAsyncLetStart(BuiltinInst *bi) {
    // Just track the result of the builtin inst as an assign fresh. We do this
    // so we properly track the partial_apply get. We already transferred the
    // parameters.
    builder.addAssignFresh(bi);
  }

  void translateAsyncLetGet(ApplyInst *ai) {
    auto *pai = findAsyncLetPartialApplyFromGet(ai);

    // We should always be able to derive a partial_apply since we pattern
    // matched against the actual function call to swift_asyncLet_get in our
    // caller.
    assert(pai && "AsyncLet Get should always have a derivable partial_apply");

    ApplySite applySite(pai);

    // For each of our partial apply operands...
    for (auto pair : llvm::enumerate(applySite.getArgumentOperands())) {
      Operand &op = pair.value();

      // If we are tracking the value...
      if (auto trackedArgValue = tryToTrackValue(op.get())) {

        // Gather the isolation info from the AST for this operand...
        InferredCallerArgumentTypeInfo typeInfo;
        typeInfo.init(&op);

        // Then see if /any/ of our uses are passed to over a isolation boundary
        // that is actor isolated... if we find one continue so we do not undo
        // the transfer for that element.
        if (llvm::any_of(
                typeInfo.applyUses,
                [](const std::pair<Type, std::optional<ApplyIsolationCrossing>>
                       &data) {
                  // If we do not have an apply isolation crossing, we just use
                  // undefined crossing since that is considered nonisolated.
                  ApplyIsolationCrossing crossing;
                  return data.second.value_or(crossing)
                      .CalleeIsolation.isActorIsolated();
                }))
          continue;

        builder.addUndoTransfer(trackedArgValue->getRepresentative(), ai);
      }
    }
  }

  void translateSILPartialApplyAsyncLetBegin(PartialApplyInst *pai) {
    // Grab our partial apply and transfer all of its non-sendable
    // parameters. We do not merge the parameters since each individual capture
    // of the async let at the program level is viewed as still being in
    // separate regions. Otherwise, we would need to error on the following
    // code:
    //
    // let x = NonSendable(), x2 = NonSendable()
    // async let y = transferToActor(x) + transferToNonIsolated(x2)
    // _ = await y
    // useValue(x2)
    for (auto &op : ApplySite(pai).getArgumentOperands()) {
      if (auto trackedArgValue = tryToTrackValue(op.get())) {
        builder.addRequire(trackedArgValue->getRepresentative());
        builder.addTransfer(trackedArgValue->getRepresentative(), &op);
      }
    }
  }

  void translateSILPartialApply(PartialApplyInst *pai) {
    assert(!isIsolationBoundaryCrossingApply(pai));

    // First check if our partial_apply is fed into an async let begin. If so,
    // handle it especially.
    if (isAsyncLetBeginPartialApply(pai)) {
      return translateSILPartialApplyAsyncLetBegin(pai);
    }

    SILMultiAssignOptions options;
    for (auto arg : pai->getOperandValues()) {
      if (auto value = tryToTrackValue(arg)) {
        if (value->isActorDerived()) {
          options |= SILMultiAssignFlags::PropagatesActorSelf;
        }
      } else {
        // NOTE: One may think that only sendable things can enter
        // here... but we treat things like function_ref/class_method which
        // are non-Sendable as sendable for our purposes.
        if (arg->getType().isActor()) {
          options |= SILMultiAssignFlags::PropagatesActorSelf;
        }
      }
    }

    SmallVector<SILValue, 8> applyResults;
    getApplyResults(pai, applyResults);
    translateSILMultiAssign(applyResults, pai->getOperandValues(), options);
  }

  void translateSILApply(SILInstruction *inst) {
    if (auto *bi = dyn_cast<BuiltinInst>(inst)) {
      if (auto kind = bi->getBuiltinKind()) {
        if (kind == BuiltinValueKind::StartAsyncLetWithLocalBuffer) {
          return translateAsyncLetStart(bi);
        }
      }
    }

    if (auto fas = FullApplySite::isa(inst)) {
      if (auto *f = fas.getCalleeFunction()) {
        // Check against the actual SILFunction.
        if (f->getName() == "swift_asyncLet_get") {
          return translateAsyncLetGet(cast<ApplyInst>(*fas));
        }
      }
    }

    // If this apply does not cross isolation domains, it has normal
    // non-transferring multi-assignment semantics
    if (!isIsolationBoundaryCrossingApply(inst)) {
      SILMultiAssignOptions options;
      if (auto fas = FullApplySite::isa(inst)) {
        if (fas.hasSelfArgument()) {
          if (auto self = fas.getSelfArgument()) {
            if (self->getType().isActor())
              options |= SILMultiAssignFlags::PropagatesActorSelf;
          }
        }
      }

      SmallVector<SILValue, 8> applyResults;
      getApplyResults(inst, applyResults);
      return translateSILMultiAssign(applyResults, inst->getOperandValues(),
                                     options);
    }

    if (auto cast = dyn_cast<ApplyInst>(inst))
      return translateIsolationCrossingSILApply(cast);
    if (auto cast = dyn_cast<BeginApplyInst>(inst))
      return translateIsolationCrossingSILApply(cast);
    if (auto cast = dyn_cast<TryApplyInst>(inst))
      return translateIsolationCrossingSILApply(cast);

    llvm_unreachable("Only ApplyInst, BeginApplyInst, and TryApplyInst should "
                     "cross isolation domains");
  }

  /// Handles the semantics for SIL applies that cross isolation.
  ///
  /// Semantically this causes all arguments of the applysite to be transferred.
  void translateIsolationCrossingSILApply(ApplySite applySite) {
    ApplyExpr *sourceApply = applySite.getLoc().getAsASTNode<ApplyExpr>();
    assert(sourceApply && "only ApplyExpr's should cross isolation domains");

    // require all operands
    for (auto op : applySite->getOperandValues())
      if (auto value = tryToTrackValue(op))
        builder.addRequire(value->getRepresentative());

    auto handleSILOperands = [&](MutableArrayRef<Operand> operands) {
      for (auto &op : operands) {
        if (auto value = tryToTrackValue(op.get())) {
          builder.addTransfer(value->getRepresentative(), &op);
        }
      }
    };

    auto handleSILSelf = [&](Operand *self) {
      if (auto value = tryToTrackValue(self->get())) {
        builder.addTransfer(value->getRepresentative(), self);
      }
    };

    if (applySite.hasSelfArgument()) {
      handleSILOperands(applySite.getOperandsWithoutSelf());
      handleSILSelf(&applySite.getSelfArgumentOperand());
    } else {
      handleSILOperands(applySite->getAllOperands());
    }

    // non-sendable results can't be returned from cross-isolation calls without
    // a diagnostic emitted elsewhere. Here, give them a fresh value for better
    // diagnostics hereafter
    SmallVector<SILValue, 8> applyResults;
    getApplyResults(*applySite, applyResults);
    for (auto result : applyResults)
      if (auto value = tryToTrackValue(result))
        builder.addAssignFresh(value->getRepresentative());
  }

  template <typename DestValues>
  void translateSILLookThrough(DestValues destValues, SILValue src) {
    auto srcID = tryToTrackValue(src);

    for (SILValue dest : destValues) {
      auto destID = tryToTrackValue(dest);
      assert(((!destID || !srcID) || destID->getID() == srcID->getID()) &&
             "srcID and dstID are different?!");
    }
  }

  /// Add a look through operation. This asserts that dest and src map to the
  /// same ID. Should only be used on instructions that are always guaranteed to
  /// have this property due to getUnderlyingTrackedValue looking through them.
  ///
  /// DISCUSSION: We use this to ensure that the connection in between
  /// getUnderlyingTrackedValue and these instructions is enforced and
  /// explicit. Previously, we always called translateSILAssign and relied on
  /// the builder to recognize these cases and not create an assign
  /// PartitionOp. Doing such a thing obscures what is actually happening.
  template <>
  void translateSILLookThrough<SILValue>(SILValue dest, SILValue src) {
    auto srcID = tryToTrackValue(src);
    auto destID = tryToTrackValue(dest);
    assert(((!destID || !srcID) || destID->getID() == srcID->getID()) &&
           "srcID and dstID are different?!");
  }

  template <typename Collection>
  void translateSILAssign(SILValue dest, Collection collection) {
    return translateSILMultiAssign(TinyPtrVector<SILValue>(dest), collection);
  }

  template <>
  void translateSILAssign<SILValue>(SILValue dest, SILValue src) {
    return translateSILAssign(dest, TinyPtrVector<SILValue>(src));
  }

  void translateSILAssign(SILInstruction *inst) {
    return translateSILMultiAssign(inst->getResults(),
                                   inst->getOperandValues());
  }

  /// If the passed SILValue is NonSendable, then create a fresh region for it,
  /// otherwise do nothing.
  void translateSILAssignFresh(SILValue val) {
    return translateSILMultiAssign(TinyPtrVector<SILValue>(val),
                                   TinyPtrVector<SILValue>());
  }

  template <typename Collection>
  void translateSILMerge(SILValue dest, Collection collection) {
    auto trackableDest = tryToTrackValue(dest);
    if (!trackableDest)
      return;
    for (SILValue elt : collection) {
      if (auto trackableSrc = tryToTrackValue(elt)) {
        builder.addMerge(trackableDest->getRepresentative(),
                         trackableSrc->getRepresentative());
      }
    }
  }

  template <>
  void translateSILMerge<SILValue>(SILValue dest, SILValue src) {
    return translateSILMerge(dest, TinyPtrVector<SILValue>(src));
  }

  /// If tgt is known to be unaliased (computed thropugh a combination of
  /// AccessStorage's inUniquelyIdenfitied check and a custom search for
  /// captures by applications), then these can be treated as assignments of tgt
  /// to src. If the tgt could be aliased, then we must instead treat them as
  /// merges, to ensure any aliases of tgt are also updated.
  void translateSILStore(SILValue dest, SILValue src) {
    if (auto nonSendableTgt = tryToTrackValue(dest)) {
      // In the following situations, we can perform an assign:
      //
      // 1. A store to unaliased storage.
      // 2. A store that is to an entire value.
      //
      // DISCUSSION: If we have case 2, we need to merge the regions since we
      // are not overwriting the entire region of the value. This does mean that
      // we artificially include the previous region that was stored
      // specifically in this projection... but that is better than
      // miscompiling. For memory like this, we probably need to track it on a
      // per field basis to allow for us to assign.
      if (nonSendableTgt.value().isNoAlias() && !isProjectedFromAggregate(dest))
        return translateSILAssign(dest, src);

      // Stores to possibly aliased storage must be treated as merges.
      return translateSILMerge(dest, src);
    }

    // Stores to storage of non-Sendable type can be ignored.
  }

  void translateSILTupleAddrConstructor(TupleAddrConstructorInst *inst) {
    SILValue dest = inst->getDest();
    if (auto nonSendableTgt = tryToTrackValue(dest)) {
      // In the following situations, we can perform an assign:
      //
      // 1. A store to unaliased storage.
      // 2. A store that is to an entire value.
      //
      // DISCUSSION: If we have case 2, we need to merge the regions since we
      // are not overwriting the entire region of the value. This does mean that
      // we artificially include the previous region that was stored
      // specifically in this projection... but that is better than
      // miscompiling. For memory like this, we probably need to track it on a
      // per field basis to allow for us to assign.
      if (nonSendableTgt.value().isNoAlias() && !isProjectedFromAggregate(dest))
        return translateSILAssign(dest, inst->getElements());

      // Stores to possibly aliased storage must be treated as merges.
      return translateSILMerge(dest, inst->getElements());
    }

    // Stores to storage of non-Sendable type can be ignored.
  }

  void translateSILRequire(SILValue val) {
    if (auto nonSendableVal = tryToTrackValue(val))
      return builder.addRequire(nonSendableVal->getRepresentative());
  }

  /// An enum select is just a multi assign.
  void translateSILSelectEnum(SelectEnumOperation selectEnumInst) {
    SmallVector<SILValue, 8> enumOperands;
    for (unsigned i = 0; i < selectEnumInst.getNumCases(); i++)
      enumOperands.push_back(selectEnumInst.getCase(i).second);
    if (selectEnumInst.hasDefault())
      enumOperands.push_back(selectEnumInst.getDefaultResult());
    return translateSILMultiAssign(
        TinyPtrVector<SILValue>(selectEnumInst->getResult(0)), enumOperands);
  }

  void translateSILSwitchEnum(SwitchEnumInst *switchEnumInst) {
    TermArgSources argSources;

    // accumulate each switch case that branches to a basic block with an arg
    for (unsigned i = 0; i < switchEnumInst->getNumCases(); i++) {
      SILBasicBlock *dest = switchEnumInst->getCase(i).second;
      if (dest->getNumArguments() > 0) {
        assert(dest->getNumArguments() == 1 &&
               "expected at most one bb arg in dest of enum switch");
        argSources.addValues({switchEnumInst->getOperand()}, dest);
      }
    }

    translateSILPhi(argSources);
  }

  // translate a SIL instruction corresponding to possible branches with args
  // to one or more basic blocks. This is the SIL equivalent of SSA Phi nodes.
  // each element of `branches` corresponds to the arguments passed to a bb,
  // and a pointer to the bb being branches to itself.
  // this is handled as assigning to each possible arg being branched to the
  // merge of all values that could be passed to it from this basic block.
  void translateSILPhi(TermArgSources &argSources) {
    argSources.argSources.setFrozen();
    for (auto pair : argSources.argSources.getRange()) {
      translateSILMultiAssign(TinyPtrVector<SILValue>(pair.first), pair.second);
    }
  }

  /// Top level switch that translates SIL instructions.
  void translateSILInstruction(SILInstruction *inst) {
    builder.reset(inst);
    SWIFT_DEFER { LLVM_DEBUG(builder.print(llvm::dbgs())); };

    switch (inst->getKind()) {
    // The following instructions are treated as assigning their result to a
    // fresh region.
    case SILInstructionKind::AllocBoxInst:
    case SILInstructionKind::AllocPackInst:
    case SILInstructionKind::AllocRefDynamicInst:
    case SILInstructionKind::AllocRefInst:
    case SILInstructionKind::AllocStackInst:
    case SILInstructionKind::KeyPathInst:
    case SILInstructionKind::FunctionRefInst:
    case SILInstructionKind::DynamicFunctionRefInst:
    case SILInstructionKind::PreviousDynamicFunctionRefInst:
    case SILInstructionKind::GlobalAddrInst:
    case SILInstructionKind::GlobalValueInst:
    case SILInstructionKind::IntegerLiteralInst:
    case SILInstructionKind::FloatLiteralInst:
    case SILInstructionKind::StringLiteralInst:
    case SILInstructionKind::HasSymbolInst:
    case SILInstructionKind::ObjCProtocolInst:
    case SILInstructionKind::WitnessMethodInst:
      return translateSILAssignFresh(inst->getResult(0));

    case SILInstructionKind::SelectEnumAddrInst:
    case SILInstructionKind::SelectEnumInst:
      return translateSILSelectEnum(inst);

    // These are instructions that we treat as true assigns since we want to
    // error semantically upon them if there is a use of one of these. For
    // example, a cast would be inappropriate here. This is implemented by
    // propagating the operand's region into the result's region and by
    // requiring all operands.
    case SILInstructionKind::LoadInst:
    case SILInstructionKind::LoadBorrowInst:
    case SILInstructionKind::LoadWeakInst:
    case SILInstructionKind::StrongCopyUnownedValueInst:
    case SILInstructionKind::ClassMethodInst:
    case SILInstructionKind::ObjCMethodInst:
    case SILInstructionKind::SuperMethodInst:
    case SILInstructionKind::ObjCSuperMethodInst:
      return translateSILAssign(inst->getResult(0), inst->getOperand(0));

    // Instructions that getUnderlyingTrackedValue is guaranteed to look through
    // and whose operand and result are guaranteed to be mapped to the same
    // underlying region.
    //
    // NOTE: translateSILLookThrough asserts that this property is true.
    case SILInstructionKind::BeginAccessInst:
    case SILInstructionKind::BeginBorrowInst:
    case SILInstructionKind::BeginDeallocRefInst:
    case SILInstructionKind::RefToBridgeObjectInst:
    case SILInstructionKind::BridgeObjectToRefInst:
    case SILInstructionKind::CopyValueInst:
    case SILInstructionKind::EndCOWMutationInst:
    case SILInstructionKind::ProjectBoxInst:
    case SILInstructionKind::EndInitLetRefInst:
    case SILInstructionKind::InitEnumDataAddrInst:
    case SILInstructionKind::OpenExistentialAddrInst:
    case SILInstructionKind::UncheckedRefCastInst:
    case SILInstructionKind::UncheckedTakeEnumDataAddrInst:
    case SILInstructionKind::UpcastInst:
      return translateSILLookThrough(inst->getResult(0), inst->getOperand(0));

    case SILInstructionKind::TupleElementAddrInst:
    case SILInstructionKind::StructElementAddrInst: {
      auto *svi = cast<SingleValueInstruction>(inst);

      // If our result is non-Sendable, just treat this as a lookthrough.
      if (isNonSendableType(svi->getType()))
        return translateSILLookThrough(svi->getResult(0), svi->getOperand(0));

      // Otherwise, we are extracting a sendable field from a non-Sendable base
      // type. We need to track this as an assignment so that if we transferred
      // the value we emit an error. Since we do not track uses of Sendable
      // values this is the best place to emit the error since we do not look
      // further to find the actual use site.
      //
      // TODO: We could do a better job here and attempt to find the actual use
      // of the Sendable addr. That would require adding more logic though.
      return translateSILRequire(svi->getOperand(0));
    }

    // We identify tuple results with their operand's id.
    case SILInstructionKind::DestructureTupleInst:
    case SILInstructionKind::DestructureStructInst:
      return translateSILLookThrough(inst->getResults(), inst->getOperand(0));

    case SILInstructionKind::UnconditionalCheckedCastInst:
      if (SILDynamicCastInst(inst).isRCIdentityPreserving())
        return translateSILLookThrough(inst->getResult(0), inst->getOperand(0));
      return translateSILAssign(inst);

    // Just make the result part of the operand's region without requiring.
    //
    // This is appropriate for things like object casts and object
    // geps.
    case SILInstructionKind::AddressToPointerInst:
    case SILInstructionKind::BaseAddrForOffsetInst:
    case SILInstructionKind::ConvertEscapeToNoEscapeInst:
    case SILInstructionKind::ConvertFunctionInst:
    case SILInstructionKind::CopyBlockInst:
    case SILInstructionKind::CopyBlockWithoutEscapingInst:
    case SILInstructionKind::IndexAddrInst:
    case SILInstructionKind::InitBlockStorageHeaderInst:
    case SILInstructionKind::InitExistentialAddrInst:
    case SILInstructionKind::InitExistentialRefInst:
    case SILInstructionKind::OpenExistentialBoxInst:
    case SILInstructionKind::OpenExistentialRefInst:
    case SILInstructionKind::PointerToAddressInst:
    case SILInstructionKind::ProjectBlockStorageInst:
    case SILInstructionKind::RefToUnmanagedInst:
    case SILInstructionKind::TailAddrInst:
    case SILInstructionKind::ThickToObjCMetatypeInst:
    case SILInstructionKind::ThinToThickFunctionInst:
    case SILInstructionKind::UncheckedAddrCastInst:
    case SILInstructionKind::UncheckedEnumDataInst:
    case SILInstructionKind::UncheckedOwnershipConversionInst:
    case SILInstructionKind::UnmanagedToRefInst:
      return translateSILAssign(inst);

    // RefElementAddrInst is not considered to be a lookThrough since we want to
    // consider the address projected from the class to be a separate value that
    // is in the same region as the parent operand. The reason that we want to
    // do this is to ensure that if we assign into the ref_element_addr memory,
    // we do not consider writes into the struct that contains the
    // ref_element_addr to be merged into.
    case SILInstructionKind::RefElementAddrInst: {
      auto *reai = cast<RefElementAddrInst>(inst);
      // If we are accessing a let of a Sendable type, do not treat the
      // ref_element_addr as a require use.
      if (reai->getField()->isLet() && !isNonSendableType(reai->getType())) {
        LLVM_DEBUG(llvm::dbgs() << "    Found a let! Not tracking!\n");
        return;
      }
      return translateSILAssign(inst);
    }

    case SILInstructionKind::TupleExtractInst:
    case SILInstructionKind::StructExtractInst: {
      auto *svi = cast<SingleValueInstruction>(inst);
      // If our result is a Sendable type regardless of if it is a let or a var,
      // we do not need to track it.
      if (!isNonSendableType(svi->getType())) {
        LLVM_DEBUG(llvm::dbgs()
                   << "    Found a sendable field... Not Tracking!\n");
        return;
      }
      return translateSILAssign(inst);
    }

    /// Enum inst is handled specially since if it does not have an argument,
    /// we must assign fresh. Otherwise, we must propagate.
    case SILInstructionKind::EnumInst: {
      auto *ei = cast<EnumInst>(inst);
      if (ei->getNumOperands() == 0)
        return translateSILAssignFresh(ei);
      return translateSILAssign(ei);
    }

    // These are treated as stores - meaning that they could write values into
    // memory. The beahvior of this depends on whether the tgt addr is aliased,
    // but conservative behavior is to treat these as merges of the regions of
    // the src value and tgt addr
    case SILInstructionKind::CopyAddrInst:
    case SILInstructionKind::ExplicitCopyAddrInst:
    case SILInstructionKind::StoreInst:
    case SILInstructionKind::StoreBorrowInst:
    case SILInstructionKind::StoreWeakInst:
      return translateSILStore(inst->getOperand(1), inst->getOperand(0));

    case SILInstructionKind::TupleAddrConstructorInst:
      return translateSILTupleAddrConstructor(
          cast<TupleAddrConstructorInst>(inst));

    case SILInstructionKind::PartialApplyInst:
      return translateSILPartialApply(cast<PartialApplyInst>(inst));

    // Applies are handled specially since we need to merge their results.
    case SILInstructionKind::ApplyInst:
    case SILInstructionKind::BeginApplyInst:
    case SILInstructionKind::BuiltinInst:
    case SILInstructionKind::TryApplyInst:
      return translateSILApply(inst);

    // These are used by SIL to aggregate values together in a gep like way. We
    // want to look at uses of structs, not the struct uses itself. So just
    // propagate.
    case SILInstructionKind::ObjectInst:
    case SILInstructionKind::StructInst:
    case SILInstructionKind::TupleInst:
      return translateSILAssign(inst);

    // Handle returns and throws - require the operand to be non-transferred
    case SILInstructionKind::ReturnInst:
    case SILInstructionKind::ThrowInst:
      return translateSILRequire(inst->getOperand(0));

    // Handle branching terminators.
    case SILInstructionKind::BranchInst: {
      auto *branchInst = cast<BranchInst>(inst);
      assert(branchInst->getNumArgs() ==
             branchInst->getDestBB()->getNumArguments());
      TermArgSources argSources;
      argSources.addValues(branchInst->getArgs(), branchInst->getDestBB());
      return translateSILPhi(argSources);
    }

    case SILInstructionKind::CondBranchInst: {
      auto *condBranchInst = cast<CondBranchInst>(inst);
      assert(condBranchInst->getNumTrueArgs() ==
             condBranchInst->getTrueBB()->getNumArguments());
      assert(condBranchInst->getNumFalseArgs() ==
             condBranchInst->getFalseBB()->getNumArguments());
      TermArgSources argSources;
      argSources.addValues(condBranchInst->getTrueArgs(),
                           condBranchInst->getTrueBB());
      argSources.addValues(condBranchInst->getFalseArgs(),
                           condBranchInst->getFalseBB());
      return translateSILPhi(argSources);
    }

    case SILInstructionKind::SwitchEnumInst:
      return translateSILSwitchEnum(cast<SwitchEnumInst>(inst));

    case SILInstructionKind::DynamicMethodBranchInst: {
      auto *dmBranchInst = cast<DynamicMethodBranchInst>(inst);
      assert(dmBranchInst->getHasMethodBB()->getNumArguments() <= 1);
      TermArgSources argSources;
      argSources.addValues({dmBranchInst->getOperand()},
                           dmBranchInst->getHasMethodBB());
      return translateSILPhi(argSources);
    }

    case SILInstructionKind::CheckedCastBranchInst: {
      auto *ccBranchInst = cast<CheckedCastBranchInst>(inst);
      assert(ccBranchInst->getSuccessBB()->getNumArguments() <= 1);
      TermArgSources argSources;
      argSources.addValues({ccBranchInst->getOperand()},
                           ccBranchInst->getSuccessBB());
      return translateSILPhi(argSources);
    }

    case SILInstructionKind::CheckedCastAddrBranchInst: {
      auto *ccAddrBranchInst = cast<CheckedCastAddrBranchInst>(inst);
      assert(ccAddrBranchInst->getSuccessBB()->getNumArguments() <= 1);

      // checked_cast_addr_br does not have any arguments in its resulting
      // block. We should just use a multi-assign on its operands.
      //
      // TODO: We should be smarter and treat the success/fail branches
      // differently depending on what the result of checked_cast_addr_br
      // is. For now just keep the current behavior. It is more conservative,
      // but still correct.
      return translateSILMultiAssign(ArrayRef<SILValue>(),
                                     ccAddrBranchInst->getOperandValues());
    }

    // These instructions are ignored because they cannot affect the partition
    // state - they do not manipulate what region non-sendable values lie in
    case SILInstructionKind::AllocGlobalInst:
    case SILInstructionKind::DeallocBoxInst:
    case SILInstructionKind::DeallocStackInst:
    case SILInstructionKind::DebugValueInst:
    case SILInstructionKind::DestroyAddrInst:
    case SILInstructionKind::DestroyValueInst:
    case SILInstructionKind::EndAccessInst:
    case SILInstructionKind::EndBorrowInst:
    case SILInstructionKind::EndLifetimeInst:
    case SILInstructionKind::HopToExecutorInst:
    case SILInstructionKind::InjectEnumAddrInst:
    case SILInstructionKind::IsEscapingClosureInst: // ignored because result is
                                                    // always in
    case SILInstructionKind::MarkDependenceInst:
    case SILInstructionKind::MetatypeInst:
    case SILInstructionKind::EndApplyInst:
    case SILInstructionKind::AbortApplyInst:

    // Ignored terminators.
    case SILInstructionKind::CondFailInst:
    case SILInstructionKind::SwitchEnumAddrInst: // ignored as long as
                                                 // destinations can take no arg
    case SILInstructionKind::SwitchValueInst: // ignored as long as destinations
                                              // can take no args
    case SILInstructionKind::UnreachableInst:
    case SILInstructionKind::UnwindInst:
    case SILInstructionKind::YieldInst: // TODO: yield should be handled
      return;

    default:
      break;
    }

    LLVM_DEBUG(llvm::dbgs() << "warning: ";
               llvm::dbgs() << "unhandled instruction kind "
                            << getSILInstructionName(inst->getKind()) << "\n";);

    return;
  }

  /// Translate the instruction's in \p basicBlock to a vector of PartitionOps
  /// that define the block's dataflow.
  void translateSILBasicBlock(SILBasicBlock *basicBlock,
                              std::vector<PartitionOp> &foundPartitionOps) {
    LLVM_DEBUG(llvm::dbgs() << SEP_STR << "Compiling basic block for function "
                            << basicBlock->getFunction()->getName() << ": ";
               basicBlock->dumpID(); llvm::dbgs() << SEP_STR;
               basicBlock->print(llvm::dbgs());
               llvm::dbgs() << SEP_STR << "Results:\n";);
    // Translate each SIL instruction to the PartitionOps that it represents if
    // any.
    for (auto &instruction : *basicBlock) {
      LLVM_DEBUG(llvm::dbgs() << "Visiting: " << instruction);
      translateSILInstruction(&instruction);
      copy(builder.currentInstPartitionOps,
           std::back_inserter(foundPartitionOps));
    }
  }
};

TrackableValueID PartitionOpBuilder::lookupValueID(SILValue value) {
  return translator->lookupValueID(value);
}

bool PartitionOpBuilder::valueHasID(SILValue value, bool dumpIfHasNoID) {
  return translator->valueHasID(value, dumpIfHasNoID);
}

void PartitionOpBuilder::print(llvm::raw_ostream &os) const {
#ifndef NDEBUG
  // If we do not have anything to dump, just return.
  if (currentInstPartitionOps.empty())
    return;

  // First line.
  llvm::dbgs() << " ┌─┬─╼";
  currentInst->print(llvm::dbgs());

  // Second line.
  llvm::dbgs() << " │ └─╼  ";
  currentInst->getLoc().getSourceLoc().printLineAndColumn(
      llvm::dbgs(), currentInst->getFunction()->getASTContext().SourceMgr);

  auto ops = llvm::makeArrayRef(currentInstPartitionOps);

  // First op on its own line.
  llvm::dbgs() << "\n ├─────╼ ";
  ops.front().print(llvm::dbgs());

  // Rest of ops each on their own line.
  for (const PartitionOp &op : ops.drop_front()) {
    llvm::dbgs() << " │    └╼ ";
    op.print(llvm::dbgs());
  }

  // Now print out a translation from region to equivalence class value.
  llvm::dbgs() << " └─────╼ Used Values\n";
  llvm::SmallVector<unsigned, 8> opsToPrint;
  SWIFT_DEFER { opsToPrint.clear(); };
  for (const PartitionOp &op : ops) {
    // Now dump our the root value we map.
    for (unsigned opArg : op.getOpArgs()) {
      // If we didn't insert, skip this. We only emit this once.
      opsToPrint.push_back(opArg);
    }
  }
  sortUnique(opsToPrint);
  for (unsigned opArg : opsToPrint) {
    llvm::dbgs() << "          └╼ ";
    SILValue value = translator->stateIndexToEquivalenceClass[opArg];
    auto iter = translator->equivalenceClassValuesToState.find(value);
    assert(iter != translator->equivalenceClassValuesToState.end());
    llvm::dbgs() << "State: %%" << opArg << ". ";
    iter->getSecond().print(llvm::dbgs());
    llvm::dbgs() << "\n             Value: " << value;
  }
#endif
}

} // namespace

//===----------------------------------------------------------------------===//
//                          MARK: Block Level Model
//===----------------------------------------------------------------------===//

namespace {

/// Dataflow State associated with a specific SILBasicBlock.
class BlockPartitionState {
  friend class PartitionAnalysis;

  /// Set if this block in the next iteration needs to be visited.
  bool needsUpdate = false;

  /// Set if we have ever visited this block at all.
  bool reached = false;

  /// The partition of elements into regions at the top of the block.
  Partition entryPartition;

  /// The partition of elements into regions at the bottom of the block.
  Partition exitPartition;

  /// The basic block that this state belongs to.
  SILBasicBlock *basicBlock;

  /// The vector of PartitionOps that are used to perform the dataflow in this
  /// block.
  std::vector<PartitionOp> blockPartitionOps = {};

  TransferringOperandSetFactory &ptrSetFactory;

  BlockPartitionState(SILBasicBlock *basicBlock,
                      PartitionOpTranslator &translator,
                      TransferringOperandSetFactory &ptrSetFactory)
      : basicBlock(basicBlock), ptrSetFactory(ptrSetFactory) {
    translator.translateSILBasicBlock(basicBlock, blockPartitionOps);
  }

  /// Recomputes the exit partition from the entry partition, and returns
  /// whether this changed the exit partition.
  ///
  /// NOTE: This method ignored errors that arise. We process separately later
  /// to discover if an error occured.
  bool recomputeExitFromEntry(PartitionOpTranslator &translator) {
    Partition workingPartition = entryPartition;
    PartitionOpEvaluator eval(workingPartition, ptrSetFactory);
    eval.isClosureCapturedCallback = [&](Element element, Operand *op) -> bool {
      auto iter = translator.getValueForId(element);
      if (!iter)
        return false;
      return translator.isClosureCaptured(iter->getRepresentative(),
                                          op->getUser());
    };
    for (const auto &partitionOp : blockPartitionOps) {
      // By calling apply without providing a `handleFailure` closure, errors
      // will be suppressed
      eval.apply(partitionOp);
    }
    bool exitUpdated = !Partition::equals(exitPartition, workingPartition);
    exitPartition = workingPartition;
    return exitUpdated;
  }

public:
  ArrayRef<PartitionOp> getPartitionOps() const { return blockPartitionOps; }

  const Partition &getEntryPartition() const { return entryPartition; }

  const Partition &getExitPartition() const { return exitPartition; }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void print(llvm::raw_ostream &os) const {
    os << SEP_STR << "BlockPartitionState[reached=" << reached
       << ", needsUpdate=" << needsUpdate << "]\nid: ";
#ifndef NDEBUG
    auto printID = [&](SILBasicBlock *block) {
      block->printID(os);
    };
#else
    auto printID = [&](SILBasicBlock *) {
      os << "NOASSERTS. ";
    };
#endif
    printID(basicBlock);
    os << "entry partition: ";
    entryPartition.print(os);
    os << "exit partition: ";
    exitPartition.print(os);
    os << "instructions:\n┌──────────╼\n";
    for (const auto &op : blockPartitionOps) {
      os << "│ ";
      op.print(os, true /*extra space*/);
    }
    os << "└──────────╼\nSuccs:\n";
    for (auto succ : basicBlock->getSuccessorBlocks()) {
      os << "→";
      printID(succ);
    }
    os << "Preds:\n";
    for (auto pred : basicBlock->getPredecessorBlocks()) {
      os << "←";
      printID(pred);
    }
    os << SEP_STR;
  }
};

} // namespace

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
/// transfer instruction. We discover this by walking from user blocks to
struct RequireLiveness {
  unsigned generation;
  SILInstruction *transferInst;
  BasicBlockData<BlockLivenessInfo> &blockLivenessInfo;
  InstructionSet allRequires;
  InstructionSetWithSize finalRequires;

  /// If we have requires in the def block before our transfer, this is the
  /// first require.
  SILInstruction *firstRequireBeforeTransferInDefBlock = nullptr;

  RequireLiveness(unsigned generation, Operand *transferOp,
                  BasicBlockData<BlockLivenessInfo> &blockLivenessInfo)
      : generation(generation), transferInst(transferOp->getUser()),
        blockLivenessInfo(blockLivenessInfo),
        allRequires(transferOp->getParentFunction()),
        finalRequires(transferOp->getParentFunction()) {}

  template <typename Collection>
  void process(Collection collection);

  /// Attempt to process requireInst for our def block. Returns false if
  /// requireInst was before our def and we need to do interprocedural
  /// processing. Returns true if requireInst was after our transferInst and we
  /// were able to appropriately determine if we should emit it or not.
  void processDefBlock();

  /// Process all requires in block, updating blockLivenessInfo.
  void processNonDefBlock(SILBasicBlock *block);
};

} // namespace

void RequireLiveness::processDefBlock() {
  LLVM_DEBUG(llvm::dbgs() << "    Processing def block!\n");
  // First walk from the beginning of the block to the transfer instruction to
  // see if we have any requires before our def. Once we find one, we can skip
  // the traversal and jump straight to the transfer.
  for (auto ii = transferInst->getParent()->begin(),
            ie = transferInst->getIterator();
       ii != ie; ++ii) {
    if (allRequires.contains(&*ii) && !firstRequireBeforeTransferInDefBlock) {
      firstRequireBeforeTransferInDefBlock = &*ii;
      LLVM_DEBUG(llvm::dbgs() << "        Found transfer before def: "
                              << *firstRequireBeforeTransferInDefBlock);
      break;
    }
  }

  // Then walk from our transferInst to the end of the block looking for the
  // first require inst. Once we find it... return.
  for (auto ii = std::next(transferInst->getIterator()),
            ie = transferInst->getParent()->end();
       ii != ie; ++ii) {
    if (!allRequires.contains(&*ii))
      continue;

    finalRequires.insert(&*ii);
    LLVM_DEBUG(llvm::dbgs() << "        Found transfer after def: " << *ii);
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
  LLVM_DEBUG(llvm::dbgs() << "==> Performing Require Liveness for: "
                          << *transferInst);

  // Then put all of our requires into our allRequires set.
  BasicBlockWorklist initializingWorklist(transferInst->getFunction());
  for (auto *require : requireInstList) {
    LLVM_DEBUG(llvm::dbgs() << "        Require Inst: " << *require);
    allRequires.insert(require);
    initializingWorklist.pushIfNotVisited(require->getParent());
  }

  // Then process our def block to see if we have any requires before and after
  // the transferInst...
  processDefBlock();

  // If we found /any/ requries after the transferInst, we can bail early since
  // that is guaranteed to dominate all further requires.
  if (!finalRequires.empty()) {
    LLVM_DEBUG(
        llvm::dbgs()
        << "        Found transfer after def in def block! Exiting early!\n");
    return;
  }

  LLVM_DEBUG(llvm::dbgs() << "        Did not find transfer after def in def "
                             "block! Walking blocks!\n");

  // If we found a transfer in the def block before our def, add it to the block
  // state for the def.
  if (firstRequireBeforeTransferInDefBlock) {
    LLVM_DEBUG(
        llvm::dbgs()
        << "        Found a require before transfer! Adding to block state!\n");
    auto blockState = blockLivenessInfo.get(transferInst->getParent());
    blockState.get()->setInst(generation, firstRequireBeforeTransferInDefBlock);
  }

  // Then for each require block that isn't a def block transfer, find the
  // earliest transfer inst.
  while (auto *requireBlock = initializingWorklist.pop()) {
    auto blockState = blockLivenessInfo.get(requireBlock);
    for (auto &inst : *requireBlock) {
      if (!allRequires.contains(&inst))
        continue;
      LLVM_DEBUG(llvm::dbgs() << "        Mapping Block bb"
                              << requireBlock->getDebugID() << " to: " << inst);
      blockState.get()->setInst(generation, &inst);
      break;
    }
  }

  // Then walk from our def block looking for setInst blocks.
  auto *transferBlock = transferInst->getParent();
  BasicBlockWorklist worklist(transferInst->getFunction());
  for (auto *succBlock : transferBlock->getSuccessorBlocks())
    worklist.pushIfNotVisited(succBlock);

  while (auto *next = worklist.pop()) {
    // Check if we found an earliest requires... if so, add that to final
    // requires and continue. We don't want to visit successors.
    auto blockState = blockLivenessInfo.get(next);
    if (auto *inst = blockState.get()->getInst(generation)) {
      finalRequires.insert(inst);
      continue;
    }

    // Do not look at successors of the transfer block.
    if (next == transferBlock)
      continue;

    // Otherwise, we did not find a requires and need to search further
    // successors.
    for (auto *succBlock : next->getSuccessorBlocks())
      worklist.pushIfNotVisited(succBlock);
  }
}

//===----------------------------------------------------------------------===//
//                               MARK: Dataflow
//===----------------------------------------------------------------------===//

namespace {

/// The top level datastructure that we use to perform our dataflow. It
/// contains:
///
/// 1. State for each block.
/// 2. The translator that we use to translate block instructions.
/// 3. The raceTracer that we use to build up diagnostics.
///
/// It also has implemented upon it the main solve/diagnose routines.
class PartitionAnalysis {
  PartitionOpTranslator translator;

  BasicBlockData<BlockPartitionState> blockStates;

  llvm::BumpPtrAllocator allocator;
  TransferringOperandSetFactory ptrSetFactory;

  SILFunction *function;

  PostOrderFunctionInfo *pofi;

  bool solved;

  /// The constructor initializes each block in the function by compiling it to
  /// PartitionOps, then seeds the solve method by setting `needsUpdate` to true
  /// for the entry block
  PartitionAnalysis(SILFunction *fn, PostOrderFunctionInfo *pofi)
      : translator(fn, pofi),
        blockStates(fn,
                    [this](SILBasicBlock *block) {
                      return BlockPartitionState(block, translator,
                                                 ptrSetFactory);
                    }),
        allocator(), ptrSetFactory(allocator), function(fn), pofi(pofi),
        solved(false) {
    // Initialize the entry block as needing an update, and having a partition
    // that places all its non-sendable args in a single region
    blockStates[fn->getEntryBlock()].needsUpdate = true;
    blockStates[fn->getEntryBlock()].entryPartition =
        translator.getEntryPartition();
  }

  void solve() {
    assert(!solved && "solve should only be called once");
    solved = true;

    LLVM_DEBUG(llvm::dbgs() << SEP_STR << "Performing Dataflow!\n" << SEP_STR);
    LLVM_DEBUG(llvm::dbgs() << "Values!\n"; translator.dumpValues());

    bool anyNeedUpdate = true;
    while (anyNeedUpdate) {
      anyNeedUpdate = false;

      for (auto *block : pofi->getReversePostOrder()) {
        auto &blockState = blockStates[block];

        LLVM_DEBUG(llvm::dbgs() << "Block: bb" << block->getDebugID() << "\n");
        if (!blockState.needsUpdate) {
          LLVM_DEBUG(llvm::dbgs() << "    Doesn't need update! Skipping!\n");
          continue;
        }

        // mark this block as no longer needing an update
        blockState.needsUpdate = false;

        // mark this block as reached by the analysis
        blockState.reached = true;

        // compute the new entry partition to this block
        Partition newEntryPartition;
        bool firstPred = true;

        LLVM_DEBUG(llvm::dbgs() << "    Visiting Preds!\n");

        // This loop computes the join of the exit partitions of all
        // predecessors of this block
        for (SILBasicBlock *predBlock : block->getPredecessorBlocks()) {
          BlockPartitionState &predState = blockStates[predBlock];
          // ignore predecessors that haven't been reached by the analysis yet
          if (!predState.reached)
            continue;

          if (firstPred) {
            firstPred = false;
            newEntryPartition = predState.exitPartition;
            LLVM_DEBUG(llvm::dbgs() << "    First Pred. bb"
                                    << predBlock->getDebugID() << ": ";
                       newEntryPartition.print(llvm::dbgs()));
            continue;
          }

          LLVM_DEBUG(llvm::dbgs()
                         << "    Pred. bb" << predBlock->getDebugID() << ": ";
                     predState.exitPartition.print(llvm::dbgs()));
          newEntryPartition =
              Partition::join(newEntryPartition, predState.exitPartition);
        }

        // If we found predecessor blocks, then attempt to use them to update
        // the entry partition for this block, and abort this block's update if
        // the entry partition was not updated.
        if (!firstPred) {
          // if the recomputed entry partition is the same as the current one,
          // perform no update
          if (Partition::equals(newEntryPartition, blockState.entryPartition)) {
            LLVM_DEBUG(llvm::dbgs()
                       << "    Entry partition is the same... skipping!\n");
            continue;
          }

          // otherwise update the entry partition
          blockState.entryPartition = newEntryPartition;
        }

        // recompute this block's exit partition from its (updated) entry
        // partition, and if this changed the exit partition notify all
        // successor blocks that they need to update as well
        if (blockState.recomputeExitFromEntry(translator)) {
          for (SILBasicBlock *succBlock : block->getSuccessorBlocks()) {
            anyNeedUpdate = true;
            blockStates[succBlock].needsUpdate = true;
          }
        }
      }
    }

    // Now that we have finished processing, sort/unique our non transferred
    // array.
    translator.sortUniqueNeverTransferredValues();
  }

  /// Once we have reached a fixpoint, this routine runs over all blocks again
  /// reporting any failures by applying our ops to the converged dataflow
  /// state.
  void emitDiagnostics() {
    assert(solved && "diagnose should not be called before solve");

    LLVM_DEBUG(llvm::dbgs() << "Emitting diagnostics for function "
                            << function->getName() << "\n");

    SmallFrozenMultiMap<Operand *, SILInstruction *, 8>
        transferOpToRequireInstMultiMap;

    // Then for each block...
    LLVM_DEBUG(llvm::dbgs() << "Walking blocks for diagnostics.\n");
    for (auto [block, blockState] : blockStates) {
      LLVM_DEBUG(llvm::dbgs() << "|--> Block bb" << block.getDebugID() << "\n");

      // Grab its entry partition and setup an evaluator for the partition that
      // has callbacks that emit diagnsotics...
      Partition workingPartition = blockState.getEntryPartition();
      PartitionOpEvaluator eval(workingPartition, ptrSetFactory);
      eval.failureCallback = /*handleFailure=*/
          [&](const PartitionOp &partitionOp, TrackableValueID transferredVal,
              TransferringOperand transferringOp) {
            // Ignore this if we have a gep like instruction that is returning a
            // sendable type and transferringOp was not set with closure
            // capture.
            if (auto *svi = dyn_cast<SingleValueInstruction>(
                    partitionOp.getSourceInst())) {
              if (isa<TupleElementAddrInst, StructElementAddrInst>(svi) &&
                  !isNonSendableType(svi->getType(), svi->getFunction())) {
                bool isCapture = transferringOp.isClosureCaptured();
                if (!isCapture) {
                  return;
                }
              }
            }

            auto rep =
                translator.getValueForId(transferredVal)->getRepresentative();
            LLVM_DEBUG(
                llvm::dbgs()
                << "    Emitting Use After Transfer Error!\n"
                << "        ID:  %%" << transferredVal << "\n"
                << "        Rep: " << *rep
                << "        Require Inst: " << *partitionOp.getSourceInst()
                << "        Transferring Op Num: "
                << transferringOp.getOperand()->getOperandNumber() << '\n'
                << "        Transferring Inst: " << *transferringOp.getUser());
            transferOpToRequireInstMultiMap.insert(transferringOp.getOperand(),
                                                   partitionOp.getSourceInst());
          };
      eval.transferredNonTransferrableCallback =
          [&](const PartitionOp &partitionOp, TrackableValueID transferredVal) {
            LLVM_DEBUG(llvm::dbgs()
                       << "    Emitting TransferNonTransferrable Error!\n"
                       << "        ID:  %%" << transferredVal << "\n"
                       << "        Rep: "
                       << *translator.getValueForId(transferredVal)
                               ->getRepresentative());
            diagnose(partitionOp,
                     diag::regionbasedisolation_selforargtransferred);
          };
      eval.nonTransferrableElements = translator.getNeverTransferredValues();
      eval.isActorDerivedCallback = [&](Element element) -> bool {
        auto iter = translator.getValueForId(element);
        if (!iter)
          return false;
        return iter->isActorDerived();
      };
      eval.isClosureCapturedCallback = [&](Element element,
                                           Operand *op) -> bool {
        auto iter = translator.getValueForId(element);
        if (!iter)
          return false;
        return translator.isClosureCaptured(iter->getRepresentative(),
                                            op->getUser());
      };

      // And then evaluate all of our partition ops on the entry partition.
      for (auto &partitionOp : blockState.getPartitionOps()) {
        eval.apply(partitionOp);
      }
    }

    // Now that we have found all of our transferInsts/Requires emit errors.
    transferOpToRequireInstMultiMap.setFrozen();

    BasicBlockData<BlockLivenessInfo> blockLivenessInfo(function);
    // We use a generation counter so we can lazily reset blockLivenessInfo
    // since we cannot clear it without iterating over it.
    unsigned blockLivenessInfoGeneration = 0;

    if (transferOpToRequireInstMultiMap.empty())
      return;

    LLVM_DEBUG(llvm::dbgs()
               << "Visiting found transfer+[requireInsts] for diagnostics.\n");
    for (auto [transferOp, requireInsts] :
         transferOpToRequireInstMultiMap.getRange()) {

      LLVM_DEBUG(llvm::dbgs()
                 << "Transfer Op. Number: " << transferOp->getOperandNumber()
                 << ". User: " << *transferOp->getUser());

      RequireLiveness liveness(blockLivenessInfoGeneration, transferOp,
                               blockLivenessInfo);
      ++blockLivenessInfoGeneration;
      liveness.process(requireInsts);

      InferredCallerArgumentTypeInfo argTypeInfo;
      argTypeInfo.init(transferOp);

      // If we were supposed to emit an error and we failed to do so, emit a
      // hard error so that the user knows to file a bug.
      //
      // DISCUSSION: We do this rather than asserting since users often times do
      // not know what to do if the compiler crashes. This at least shows up in
      // editor UIs providing a more actionable error message.
      if (argTypeInfo.applyUses.empty()) {
        diagnose(transferOp, diag::regionbasedisolation_unknown_pattern);
        continue;
      }

      for (auto &info : argTypeInfo.applyUses) {
        if (auto isolation = info.second) {
          diagnose(
              transferOp,
              diag::regionbasedisolation_transfer_yields_race_with_isolation,
              info.first, isolation->getCallerIsolation(),
              isolation->getCalleeIsolation())
              .highlight(transferOp->get().getLoc().getSourceRange());
        } else {
          diagnose(transferOp,
                   diag::regionbasedisolation_transfer_yields_race_no_isolation,
                   info.first)
              .highlight(transferOp->get().getLoc().getSourceRange());
        }

        // Ok, we now have our requires... emit the errors.
        bool didEmitRequire = false;

        InstructionSet requireInstsUnique(function);
        for (auto *require : requireInsts) {
          // We can have multiple of the same require insts if we had a require
          // and an assign from the same instruction. Our liveness checking
          // above doesn't care about that, but we still need to make sure we do
          // not emit twice.
          if (!requireInstsUnique.insert(require))
            continue;

          // If this was not a last require, do not emit an error.
          if (!liveness.finalRequires.contains(require))
            continue;

          diagnose(require, diag::regionbasedisolation_maybe_race)
              .highlight(require->getLoc().getSourceRange());
          didEmitRequire = true;
        }

        assert(didEmitRequire && "Should have a require for all errors?!");
      }
    }
  }

public:
  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void print(llvm::raw_ostream &os) const {
    os << "\nPartitionAnalysis[fname=" << function->getName() << "]\n";

    for (auto [_, blockState] : blockStates) {
      blockState.print(os);
    }
  }

  static void performForFunction(SILFunction *function,
                                 PostOrderFunctionInfo *pofi) {
    auto analysis = PartitionAnalysis(function, pofi);
    analysis.solve();
    LLVM_DEBUG(llvm::dbgs() << "SOLVED: "; analysis.print(llvm::dbgs()););
    analysis.emitDiagnostics();
  }
};

} // namespace

//===----------------------------------------------------------------------===//
//                         MARK: Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class TransferNonSendable : public SILFunctionTransform {
  void run() override {
    SILFunction *function = getFunction();

    if (!function->getASTContext().LangOpts.hasFeature(
            Feature::RegionBasedIsolation))
      return;

    LLVM_DEBUG(llvm::dbgs()
               << "===> PROCESSING: " << function->getName() << '\n');

    // If this function does not correspond to a syntactic declContext and it
    // doesn't have a parent module, don't check it since we cannot check if a
    // type is sendable.
    if (!function->getDeclContext() && !function->getParentModule()) {
      LLVM_DEBUG(llvm::dbgs() << "No Decl Context! Skipping!\n");
      return;
    }

    // The sendable protocol should /always/ be available if TransferNonSendable
    // is enabled. If not, there is a major bug in the compiler and we should
    // fail loudly.
    if (!function->getASTContext().getProtocol(KnownProtocolKind::Sendable))
      llvm::report_fatal_error("Sendable protocol not available!");

    auto *pofi = this->getAnalysis<PostOrderAnalysis>()->get(function);
    PartitionAnalysis::performForFunction(function, pofi);
  }
};

} // end anonymous namespace

SILTransform *swift::createTransferNonSendable() {
  return new TransferNonSendable();
}
