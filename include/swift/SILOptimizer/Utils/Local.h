//===--- Local.h - Local SIL transformations. -------------------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_UTILS_LOCAL_H
#define SWIFT_SILOPTIMIZER_UTILS_LOCAL_H

#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/EpilogueARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/ClassHierarchyAnalysis.h"
#include "swift/SILOptimizer/Analysis/SimplifyInstruction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILCloner.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/Allocator.h"
#include <functional>
#include <utility>

namespace swift {

class DominanceInfo;
template <class T> class NullablePtr;

/// Transform a Use Range (Operand*) into a User Range (SILInstruction*)
using UserTransform = std::function<SILInstruction *(Operand *)>;
using ValueBaseUserRange =
  TransformRange<IteratorRange<ValueBase::use_iterator>, UserTransform>;

inline ValueBaseUserRange makeUserRange(
    iterator_range<ValueBase::use_iterator> R) {
  auto toUser = [](Operand *O) { return O->getUser(); };
  return makeTransformRange(makeIteratorRange(R.begin(), R.end()),
                            UserTransform(toUser));
}

using DeadInstructionSet = llvm::SmallSetVector<SILInstruction *, 8>;

/// \brief Create a retain of \p Ptr before the \p InsertPt.
NullablePtr<SILInstruction> createIncrementBefore(SILValue Ptr,
                                                  SILInstruction *InsertPt);

/// \brief Create a release of \p Ptr before the \p InsertPt.
NullablePtr<SILInstruction> createDecrementBefore(SILValue Ptr,
                                                  SILInstruction *InsertPt);

/// \brief For each of the given instructions, if they are dead delete them
/// along with their dead operands.
///
/// \param I The ArrayRef of instructions to be deleted.
/// \param Force If Force is set, don't check if the top level instructions
///        are considered dead - delete them regardless.
/// \param C a callback called whenever an instruction is deleted.
void
recursivelyDeleteTriviallyDeadInstructions(
  ArrayRef<SILInstruction*> I, bool Force = false,
  llvm::function_ref<void(SILInstruction *)> C = [](SILInstruction *){});

/// \brief For each of the given instructions, if they are dead delete them
/// along with their dead operands.
///
/// \param I The ArrayRef of instructions to be deleted.
/// \param InstIter is updated to the next valid instruction if it points to any
/// deleted instruction, including debug values.
/// \param Force If Force is set, don't check if the top level instructions
///        are considered dead - delete them regardless.
/// \param C a callback called whenever an instruction is deleted.
void recursivelyDeleteTriviallyDeadInstructions(
    ArrayRef<SILInstruction *> I, SILBasicBlock::iterator &InstIter,
    bool Force = false,
    llvm::function_ref<void(SILInstruction *)> C = [](SILInstruction *) {});

/// \brief If the given instruction is dead, delete it along with its dead
/// operands.
///
/// \param I The instruction to be deleted.
/// \param Force If Force is set, don't check if the top level instruction is
///        considered dead - delete it regardless.
/// \param C a callback called whenever an instruction is deleted.
///
/// Returns a valid instruction iterator to the next nondeleted instruction
/// after `I`.
SILBasicBlock::iterator recursivelyDeleteTriviallyDeadInstructions(
    SILInstruction *I, bool Force = false,
    llvm::function_ref<void(SILInstruction *)> C = [](SILInstruction *) {});

/// \brief Perform a fast local check to see if the instruction is dead.
///
/// This routine only examines the state of the instruction at hand.
bool isInstructionTriviallyDead(SILInstruction *I);

/// \brief Return true if this is a release instruction that's not going to
/// free the object.
bool isIntermediateRelease(SILInstruction *I, EpilogueARCFunctionInfo *ERFI);

/// \brief Recursively collect all the uses and transitive uses of the
/// instruction.
void
collectUsesOfValue(SILValue V, llvm::SmallPtrSetImpl<SILInstruction *> &Insts);

/// \brief Recursively erase all of the uses of the instruction (but not the
/// instruction itself)
void eraseUsesOfInstruction(
    SILInstruction *Inst,
    llvm::function_ref<void(SILInstruction *)> C = [](SILInstruction *){});

/// \brief Recursively erase all of the uses of the value (but not the
/// value itself)
void eraseUsesOfValue(SILValue V);

FullApplySite findApplyFromDevirtualizedResult(SILValue value);

/// Cast a value into the expected, ABI compatible type if necessary.
/// This may happen e.g. when:
/// - a type of the return value is a subclass of the expected return type.
/// - actual return type and expected return type differ in optionality.
/// - both types are tuple-types and some of the elements need to be casted.
SILValue castValueToABICompatibleType(SILBuilder *B, SILLocation Loc,
                                      SILValue Value,
                                      SILType SrcTy,
                                      SILType DestTy);

/// Returns a project_box if it is the next instruction after \p ABI and
/// and has \p ABI as operand. Otherwise it creates a new project_box right
/// after \p ABI and returns it.
ProjectBoxInst *getOrCreateProjectBox(AllocBoxInst *ABI, unsigned Index);

/// \brief Return true if any call inside the given function may bind dynamic
/// 'Self' to a generic argument of the callee.
bool mayBindDynamicSelf(SILFunction *F);

/// Check whether the \p addr is an address of a tail-allocated array element.
bool isAddressOfArrayElement(SILValue addr);

/// \brief Move an ApplyInst's FuncRef so that it dominates the call site.
void placeFuncRef(ApplyInst *AI, DominanceInfo *DT);

/// \brief Add an argument, \p val, to the branch-edge that is pointing into
/// block \p Dest. Return a new instruction and do not erase the old
/// instruction.
TermInst *addArgumentToBranch(SILValue Val, SILBasicBlock *Dest,
                              TermInst *Branch);

/// Handle the mechanical aspects of removing an unreachable block.
void removeDeadBlock(SILBasicBlock *BB);

/// Remove all instructions in the body of \p BB in safe manner by using
/// undef.
void clearBlockBody(SILBasicBlock *BB);

/// \brief Get the linkage to be used for specializations of a function with
/// the given linkage.
SILLinkage getSpecializedLinkage(SILFunction *F, SILLinkage L);

/// Tries to optimize a given apply instruction if it is a concatenation of
/// string literals. Returns a new instruction if optimization was possible.
SingleValueInstruction *tryToConcatenateStrings(ApplyInst *AI, SILBuilder &B);

/// Tries to perform jump-threading on all checked_cast_br instruction in
/// function \p Fn.
bool tryCheckedCastBrJumpThreading(SILFunction *Fn, DominanceInfo *DT,
                          SmallVectorImpl<SILBasicBlock *> &BlocksForWorklist);

/// A structure containing callbacks that are called when an instruction is
/// removed or added.
struct InstModCallbacks {
  using CallbackTy = std::function<void (SILInstruction *)>;
  CallbackTy DeleteInst = [](SILInstruction *I) {
    I->eraseFromParent();
  };
  CallbackTy CreatedNewInst = [](SILInstruction *){};

  InstModCallbacks(CallbackTy DeleteInst, CallbackTy CreatedNewInst)
    : DeleteInst(DeleteInst), CreatedNewInst(CreatedNewInst) {}
  InstModCallbacks() = default;
  ~InstModCallbacks() = default;
  InstModCallbacks(const InstModCallbacks &) = default;
  InstModCallbacks(InstModCallbacks &&) = default;
};

/// If Closure is a partial_apply or thin_to_thick_function with only local
/// ref count users and a set of post-dominating releases:
///
/// 1. Remove all ref count operations and the closure.
/// 2. Add each one of the last release locations insert releases for the
///    captured args if we have a partial_apply.
///
/// In the future this should be extended to be less conservative with users.
bool
tryDeleteDeadClosure(SingleValueInstruction *Closure,
                     InstModCallbacks Callbacks = InstModCallbacks());

/// Given a SILValue argument to a partial apply \p Arg and the associated
/// parameter info for that argument, perform the necessary cleanups to Arg when
/// one is attempting to delete the partial apply.
void releasePartialApplyCapturedArg(
    SILBuilder &Builder, SILLocation Loc, SILValue Arg, SILParameterInfo PInfo,
    InstModCallbacks Callbacks = InstModCallbacks());

/// This computes the lifetime of a single SILValue.
///
/// This does not compute a set of jointly postdominating use points. Instead it
/// assumes that the value's existing uses already jointly postdominate the
/// definition. This makes sense for values that are returned +1 from an
/// instruction, like partial_apply, and therefore must be released on all paths
/// via strong_release or apply.
class ValueLifetimeAnalysis {
public:

  /// The lifetime frontier for the value. It is the list of instructions
  /// following the last uses of the value. All the frontier instructions
  /// end the value's lifetime.
  typedef llvm::SmallVector<SILInstruction *, 4> Frontier;

  /// Constructor for the value \p Def with a specific set of users of Def's
  /// users.
  ValueLifetimeAnalysis(SILInstruction *Def, ArrayRef<SILInstruction*> UserList) :
      DefValue(Def), UserSet(UserList.begin(), UserList.end()) {
    propagateLiveness();
  }

  /// Constructor for the value \p Def considering all the value's uses.
  ValueLifetimeAnalysis(SILInstruction *Def) : DefValue(Def) {
    for (auto result : Def->getResults()) {
      for (Operand *op : result->getUses()) {
        UserSet.insert(op->getUser());
      }
    }
    propagateLiveness();
  }

  enum Mode {
    /// Don't split critical edges if the frontier instructions are located on
    /// a critical edges. Instead fail.
    DontModifyCFG,
    
    /// Split critical edges if the frontier instructions are located on
    /// a critical edges.
    AllowToModifyCFG,
    
    /// Require that all users must commonly post-dominate the definition. In
    /// other words: All paths from the definition to the function exit must
    /// contain at least one use. Fail if this is not the case.
    UsersMustPostDomDef
  };

  /// Computes and returns the lifetime frontier for the value in \p Fr.
  ///
  /// Returns true if all instructions in the frontier could be found in
  /// non-critical edges.
  /// Returns false if some frontier instructions are located on critical edges.
  /// In this case, if \p mode is AllowToModifyCFG, those critical edges are
  /// split, otherwise nothing is done and the returned \p Fr is not valid.
  ///
  /// If \p deadEndBlocks is provided, all dead-end blocks are ignored. This
  /// prevents unreachable-blocks to be included in the frontier.
  bool computeFrontier(Frontier &Fr, Mode mode,
                       DeadEndBlocks *DEBlocks = nullptr);

  /// Returns true if the instruction \p Inst is located within the value's
  /// lifetime.
  /// It is assumed that \p Inst is located after the value's definition.
  bool isWithinLifetime(SILInstruction *Inst);

  /// Returns true if the value is alive at the begin of block \p BB.
  bool isAliveAtBeginOfBlock(SILBasicBlock *BB) {
    return LiveBlocks.count(BB) && BB != DefValue->getParent();
  }

  /// For debug dumping.
  void dump() const;

private:

  /// The value.
  SILInstruction *DefValue;

  /// The set of blocks where the value is live.
  llvm::SmallSetVector<SILBasicBlock *, 16> LiveBlocks;

  /// The set of instructions where the value is used, or the users-list
  /// provided with the constructor.
  llvm::SmallPtrSet<SILInstruction*, 16> UserSet;

  /// Propagates the liveness information up the control flow graph.
  void propagateLiveness();

  /// Returns the last use of the value in the live block \p BB.
  SILInstruction *findLastUserInBlock(SILBasicBlock *BB);
};

/// Clone a single basic block and any required successor edges within the same
/// function.
class BasicBlockCloner : public SILCloner<BasicBlockCloner> {
  using SuperTy = SILCloner<BasicBlockCloner>;
  friend class SILCloner<BasicBlockCloner>;

protected:
  /// The original block to be cloned.
  SILBasicBlock *origBB;

public:
  /// An ordered list of old to new available value pairs.
  ///
  /// updateSSAAfterCloning() expects this public field to hold values that may
  /// be remapped in the cloned block and live out.
  SmallVector<std::pair<SILValue, SILValue>, 16> AvailVals;

  // Clone blocks starting at `origBB`, within the same function.
  BasicBlockCloner(SILBasicBlock *origBB)
      : SILCloner(*origBB->getParent()), origBB(origBB) {}

  void cloneBlock(SILBasicBlock *insertAfterBB = nullptr) {
    SmallVector<SILBasicBlock *, 4> successorBBs;
    successorBBs.reserve(origBB->getSuccessors().size());
    llvm::copy(origBB->getSuccessors(), std::back_inserter(successorBBs));
    cloneReachableBlocks(origBB, successorBBs, insertAfterBB);
  }

  /// Clone the given branch instruction's destination block, splitting
  /// its successors, and rewrite the branch instruction.
  void cloneBranchTarget(BranchInst *BI) {
    assert(origBB == BI->getDestBB());

    cloneBlock(/*insertAfter*/BI->getParent());

    SILBuilderWithScope(BI).createBranch(BI->getLoc(), getNewBB(),
                                         BI->getArgs());
    BI->eraseFromParent();
  }

  /// Get the newly cloned block corresponding to `origBB`.
  SILBasicBlock *getNewBB() {
    return remapBasicBlock(origBB);
  }

  /// Call this after processing all instructions to fix the control flow
  /// graph. The branch cloner may have left critical edges.
  bool splitCriticalEdges(DominanceInfo *DT, SILLoopInfo *LI);

protected:
  // MARK: CRTP overrides.

  /// Override getMappedValue to allow values defined outside the block to be
  /// cloned to be reused in the newly cloned block.
  SILValue getMappedValue(SILValue Value) {
    if (auto SI = Value->getDefiningInstruction()) {
      if (!isBlockCloned(SI->getParent()))
        return Value;
    } else if (auto BBArg = dyn_cast<SILArgument>(Value)) {
      if (!isBlockCloned(BBArg->getParent()))
        return Value;
    } else {
      assert(isa<SILUndef>(Value) && "Unexpected Value kind");
      return Value;
    }
    // `value` is not defined outside the cloned block, so consult the cloner's
    // map of cloned values.
    return SuperTy::getMappedValue(Value);
  }

  void mapValue(SILValue origValue, SILValue mappedValue) {
    SuperTy::mapValue(origValue, mappedValue);
    AvailVals.emplace_back(origValue, mappedValue);
  }
};

/// Helper function to perform SSA updates in case of jump threading.
void updateSSAAfterCloning(BasicBlockCloner &Cloner, SILBasicBlock *SrcBB,
                           SILBasicBlock *DestBB);

// Helper class that provides a callback that can be used in
// inliners/cloners for collecting new call sites.
class CloneCollector {
public:
  typedef std::pair<SILInstruction *, SILInstruction *> value_type;
  typedef std::function<void(SILInstruction *, SILInstruction *)> CallbackType;
  typedef std::function<bool (SILInstruction *)> FilterType;

private:
  FilterType Filter;

  // Pairs of collected instructions; (new, old)
  llvm::SmallVector<value_type, 4> InstructionPairs;

  void collect(SILInstruction *Old, SILInstruction *New) {
    if (Filter(New))
      InstructionPairs.push_back(std::make_pair(New, Old));
  }

public:
  CloneCollector(FilterType Filter) : Filter(Filter) {}

  CallbackType getCallback() {
    return std::bind(&CloneCollector::collect, this, std::placeholders::_1,
                     std::placeholders::_2);
  }

  llvm::SmallVectorImpl<value_type> &getInstructionPairs() {
    return InstructionPairs;
  }
};

/// This iterator 'looks through' one level of builtin expect users exposing all
/// users of the looked through builtin expect instruction i.e it presents a
/// view that shows all users as if there were no builtin expect instructions
/// interposed.
class IgnoreExpectUseIterator
    : public std::iterator<std::forward_iterator_tag, Operand *, ptrdiff_t> {
  ValueBaseUseIterator OrigUseChain;
  ValueBaseUseIterator CurrentIter;

  static BuiltinInst *isExpect(Operand *Use) {
    if (auto *BI = dyn_cast<BuiltinInst>(Use->getUser()))
      if (BI->getIntrinsicInfo().ID == llvm::Intrinsic::expect)
        return BI;
    return nullptr;
  }

  // Advance through expect users to their users until we encounter a user that
  // is not an expect.
  void advanceThroughExpects() {
    while (CurrentIter == OrigUseChain &&
           CurrentIter != ValueBaseUseIterator(nullptr)) {
      auto *Expect = isExpect(*CurrentIter);
      if (!Expect) return;
      CurrentIter = Expect->use_begin();
      // Expect with no users advance to next item in original use chain.
      if (CurrentIter == Expect->use_end())
        CurrentIter = ++OrigUseChain;
    }
  }

public:
  IgnoreExpectUseIterator(ValueBase *V)
      : OrigUseChain(V->use_begin()), CurrentIter(V->use_begin()) {
    advanceThroughExpects();
  }

  IgnoreExpectUseIterator() = default;

  Operand *operator*() const { return *CurrentIter; }
  Operand *operator->() const { return *CurrentIter; }
  SILInstruction *getUser() const { return CurrentIter->getUser(); }

  IgnoreExpectUseIterator &operator++() {
    assert(**this && "increment past end()!");
    if (OrigUseChain == CurrentIter) {
      // Use chain of the original value.
      ++OrigUseChain;
      ++CurrentIter;
      // Ignore expects.
      advanceThroughExpects();
      } else {
      // Use chain of an expect.
      ++CurrentIter;
      if (CurrentIter == ValueBaseUseIterator(nullptr)) {
        // At the end of the use chain of an expect.
        CurrentIter = ++OrigUseChain;
        advanceThroughExpects();
      }
    }
    return *this;
  }

  IgnoreExpectUseIterator operator++(int unused) {
    IgnoreExpectUseIterator Copy = *this;
    ++*this;
    return Copy;
  }
  friend bool operator==(IgnoreExpectUseIterator lhs,
                         IgnoreExpectUseIterator rhs) {
    return lhs.CurrentIter == rhs.CurrentIter;
  }
  friend bool operator!=(IgnoreExpectUseIterator lhs,
                         IgnoreExpectUseIterator rhs) {
    return !(lhs == rhs);
  }
};

inline iterator_range<IgnoreExpectUseIterator>
ignore_expect_uses(ValueBase *V) {
  return make_range(IgnoreExpectUseIterator(V),
                    IgnoreExpectUseIterator());
}

/// Run simplifyInstruction() on all of the instruction I's users if they only
/// have one result (since simplifyInstruction assumes that). Replace all uses
/// of the user with its simplification of we succeed. Returns true if we
/// succeed and false otherwise.
///
/// An example of how this is useful is in cases where one is splitting up an
/// aggregate and reforming it, the reformed aggregate may have extract
/// operations from it. These can be simplified and removed.
bool simplifyUsers(SingleValueInstruction *I);

///  True if a type can be expanded
/// without a significant increase to code size.
bool shouldExpand(SILModule &Module, SILType Ty);

/// Check if a given type is a simple type, i.e. a builtin
/// integer or floating point type or a struct/tuple whose members
/// are of simple types.
bool isSimpleType(SILType SILTy, SILModule& Module);

/// Check if the value of V is computed by means of a simple initialization.
/// Store the actual SILValue into \p Val and the reversed list of instructions
/// initializing it in \p Insns.
/// The check is performed by recursively walking the computation of the
/// SIL value being analyzed.
bool analyzeStaticInitializer(SILValue V,
                              SmallVectorImpl<SILInstruction *> &Insns);

/// Replace load sequence which may contain
/// a chain of struct_element_addr followed by a load.
/// The sequence is traversed inside out, i.e.
/// starting with the innermost struct_element_addr
void replaceLoadSequence(SILInstruction *I,
                         SILValue Value,
                         SILBuilder &B);


/// Do we have enough information to determine all callees that could
/// be reached by calling the function represented by Decl?
bool calleesAreStaticallyKnowable(SILModule &M, SILDeclRef Decl);

// Attempt to get the instance for S, whose static type is the same as
// its exact dynamic type, returning a null SILValue() if we cannot find it.
// The information that a static type is the same as the exact dynamic,
// can be derived e.g.:
// - from a constructor or
// - from a successful outcome of a checked_cast_br [exact] instruction.
SILValue getInstanceWithExactDynamicType(SILValue S, SILModule &M,
                                         ClassHierarchyAnalysis *CHA);

/// Try to determine the exact dynamic type of an object.
/// returns the exact dynamic type of the object, or an empty type if the exact
/// type could not be determined.
SILType getExactDynamicType(SILValue S, SILModule &M,
                            ClassHierarchyAnalysis *CHA,
                            bool ForUnderlyingObject = false);

/// Try to statically determine the exact dynamic type of the underlying object.
/// returns the exact dynamic type of the underlying object, or an empty SILType
/// if the exact type could not be determined.
SILType getExactDynamicTypeOfUnderlyingObject(SILValue S, SILModule &M,
                                              ClassHierarchyAnalysis *CHA);

/// Utility class for cloning init values into the static initializer of a
/// SILGlobalVariable.
class StaticInitCloner : public SILCloner<StaticInitCloner> {
  friend class SILInstructionVisitor<StaticInitCloner>;
  friend class SILCloner<StaticInitCloner>;

  /// The number of not yet cloned operands for each instruction.
  llvm::DenseMap<SILInstruction *, int> NumOpsToClone;

  /// List of instructions for which all operands are already cloned (or which
  /// don't have any operands).
  llvm::SmallVector<SILInstruction *, 8> ReadyToClone;

public:
  StaticInitCloner(SILGlobalVariable *GVar)
      : SILCloner<StaticInitCloner>(GVar) { }

  /// Add \p InitVal and all its operands (transitively) for cloning.
  ///
  /// Note: all init values must are added, before calling clone().
  void add(SILInstruction *InitVal);

  /// Clone \p InitVal and all its operands into the initializer of the
  /// SILGlobalVariable.
  ///
  /// \return Returns the cloned instruction in the SILGlobalVariable.
  SingleValueInstruction *clone(SingleValueInstruction *InitVal);

  /// Convenience function to clone a single \p InitVal.
  static void appendToInitializer(SILGlobalVariable *GVar,
                                  SingleValueInstruction *InitVal) {
    StaticInitCloner Cloner(GVar);
    Cloner.add(InitVal);
    Cloner.clone(InitVal);
  }

protected:
  SILLocation remapLocation(SILLocation Loc) {
    return ArtificialUnreachableLocation();
  }
};

/// Move only data structure that is the result of findLocalApplySite.
///
/// NOTE: Generally it is not suggested to have move only types that contain
/// small vectors. Since our small vectors contain one element or a std::vector
/// like data structure , this is ok since we will either just copy the single
/// element when we do the move or perform a move of the vector type.
struct LLVM_LIBRARY_VISIBILITY FindLocalApplySitesResult {
  /// Contains the list of local non fully applied partial apply sites that we
  /// found.
  SmallVector<ApplySite, 1> partialApplySites;

  /// Contains the list of full apply sites that we found.
  SmallVector<FullApplySite, 1> fullApplySites;

  /// Set to true if the function_ref escapes into a use that our analysis does
  /// not understand. Set to false if we found a use that had an actual
  /// escape. Set to None if we did not find any call sites, but also didn't
  /// find any "escaping uses" as well.
  ///
  /// The none case is so that we can distinguish in between saying that a value
  /// did escape and saying that we did not find any conservative information.
  bool escapes;

  FindLocalApplySitesResult() = default;
  FindLocalApplySitesResult(const FindLocalApplySitesResult &) = delete;
  FindLocalApplySitesResult &
  operator=(const FindLocalApplySitesResult &) = delete;
  FindLocalApplySitesResult(FindLocalApplySitesResult &&) = default;
  FindLocalApplySitesResult &operator=(FindLocalApplySitesResult &&) = default;
  ~FindLocalApplySitesResult() = default;

  /// Treat this function ref as escaping only if we found an actual user we
  /// didn't understand. Do not treat it as escaping if we did not find any
  /// users at all.
  bool isEscaping() const { return escapes; }
};

/// Returns .some(FindLocalApplySitesResult) if we found any interesting
/// information for the given function_ref. Otherwise, returns None.
///
/// We consider "interesting information" to mean inclusively that:
///
/// 1. We discovered that the function_ref never escapes.
/// 2. We were able to find either a partial apply or a full apply site.
Optional<FindLocalApplySitesResult> findLocalApplySites(FunctionRefInst *FRI);

} // end namespace swift

#endif
