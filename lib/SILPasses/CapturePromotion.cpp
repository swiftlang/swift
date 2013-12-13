//===--- CapturePromotion.cpp - Promotes closure captures -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "capture-promotion"
#include "swift/SILPasses/Passes.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "swift/SIL/SILCloner.h"
using namespace swift;

typedef llvm::SmallSet<unsigned, 4> IndicesSet;
typedef llvm::DenseMap<PartialApplyInst*, unsigned> PartialApplyIndexMap;
typedef llvm::DenseMap<PartialApplyInst*, IndicesSet> PartialApplyIndicesMap;

STATISTIC(NumCapturesPromoted, "Number of captures promoted");

namespace {
/// \brief Transient reference to a block set within ReachabilityInfo.
///
/// This is a bitset that conveniently flattens into a matrix allowing bit-wise
/// operations without masking.
///
/// TODO: If this sticks around, maybe we'll make a BitMatrix ADT.
class ReachingBlockSet {
public:
  enum { BITWORD_SIZE = (unsigned)sizeof(uint64_t) * CHAR_BIT };

  static size_t numBitWords(unsigned NumBlocks) {
    return (NumBlocks + BITWORD_SIZE - 1) / BITWORD_SIZE;
  }

  /// \brief Transient reference to a reaching block matrix.
  struct ReachingBlockMatrix {
    uint64_t *Bits;
    unsigned NumBitWords; // Words per row.

    ReachingBlockMatrix(): Bits(0), NumBitWords(0) {}

    bool empty() const { return !Bits; }
  };

  static ReachingBlockMatrix allocateMatrix(unsigned NumBlocks) {
    ReachingBlockMatrix M;
    M.NumBitWords = numBitWords(NumBlocks);
    M.Bits = new uint64_t[NumBlocks * M.NumBitWords];
    memset(M.Bits, 0, NumBlocks * M.NumBitWords * sizeof(uint64_t));
    return M;
  }
  static void deallocateMatrix(ReachingBlockMatrix &M) {
    delete [] M.Bits;
    M.Bits = 0;
    M.NumBitWords = 0;
  }
  static ReachingBlockSet allocateSet(unsigned NumBlocks) {
    ReachingBlockSet S;
    S.NumBitWords = numBitWords(NumBlocks);
    S.Bits = new uint64_t[S.NumBitWords];
    return S;
  }
  static void deallocateSet(ReachingBlockSet &S) {
    delete [] S.Bits;
    S.Bits = 0;
    S.NumBitWords = 0;
  }

private:
  uint64_t *Bits;
  unsigned NumBitWords;

public:
  ReachingBlockSet(): Bits(0), NumBitWords(0) {}

  ReachingBlockSet(unsigned BlockID, ReachingBlockMatrix &M)
    : Bits(&M.Bits[BlockID * M.NumBitWords]),
      NumBitWords(M.NumBitWords) {}

  bool test(unsigned ID) const {
    assert(ID / BITWORD_SIZE < NumBitWords && "block ID out-of-bounds");
    return Bits[ID / BITWORD_SIZE] & (1L << (ID % BITWORD_SIZE));
  }

  void set(unsigned ID) {
    assert(ID / BITWORD_SIZE < NumBitWords && "block ID out-of-bounds");
    Bits[ID / BITWORD_SIZE] |= 1L << (ID % BITWORD_SIZE);
  }

  ReachingBlockSet &operator|=(const ReachingBlockSet &RHS) {
    for (size_t i = 0, e = NumBitWords; i != e; ++i)
      Bits[i] |= RHS.Bits[i];
    return *this;
  }

  void clear() {
    memset(Bits, 0, NumBitWords * sizeof(uint64_t));
  }

  bool operator==(const ReachingBlockSet &RHS) const {
    assert(NumBitWords == RHS.NumBitWords && "mismatched sets");
    for (size_t i = 0, e = NumBitWords; i != e; ++i) {
      if (Bits[i] != RHS.Bits[i])
        return false;
    }
    return true;
  }

  bool operator!=(const ReachingBlockSet &RHS) const {
    return !(*this == RHS);
  }

  const ReachingBlockSet &operator=(const ReachingBlockSet &RHS) {
    assert(NumBitWords == RHS.NumBitWords && "mismatched sets");
    for (size_t i = 0, e = NumBitWords; i != e; ++i)
      Bits[i] = RHS.Bits[i];
    return *this;
  }
};

/// \brief Store the reachability matrix: ToBlock -> FromBlocks.
class ReachabilityInfo {
  SILFunction *F;
  llvm::DenseMap<SILBasicBlock*, unsigned> BlockMap;
  ReachingBlockSet::ReachingBlockMatrix Matrix;

public:
  ReachabilityInfo(SILFunction *f) : F(f) {}
  ~ReachabilityInfo() { ReachingBlockSet::deallocateMatrix(Matrix); }

  bool isComputed() const { return !Matrix.empty(); }

  bool isReachable(SILBasicBlock *From, SILBasicBlock *To);

private:
  void compute();
};

} // end anonymous namespace.


namespace {
/// \brief A SILCloner subclass which clones a closure function while converting
/// one or more captures from @inout (by-reference) to by-value.
class ClosureCloner : public SILCloner<ClosureCloner> {
public:
  friend class SILVisitor<ClosureCloner>;
  friend class SILCloner<ClosureCloner>;

  ClosureCloner(SILFunction *Orig, IndicesSet &PromotableIndices);

  void populateCloned();

  SILFunction *getCloned() { return &getBuilder().getFunction(); }

private:
  static SILFunction *initCloned(SILFunction *Orig,
                                 IndicesSet &PromotableIndices);

  void visitStrongReleaseInst(StrongReleaseInst *Inst);
  void visitStructElementAddrInst(StructElementAddrInst *Inst);
  void visitLoadInst(LoadInst *Inst);

  SILFunction *Orig;
  IndicesSet &PromotableIndices;
  llvm::DenseMap<SILArgument*, SILValue> BoxArgumentMap;
  llvm::DenseMap<SILArgument*, SILValue> AddrArgumentMap;
};
} // end anonymous namespace.

/// \brief Compute ReachabilityInfo so that it can answer queries about
/// whether a given basic block in a function is reachable from another basic
/// block in the function.
///
/// FIXME: Computing global reachability requires initializing an N^2
/// bitset. This could be avoided by computing reachability on-the-fly
/// for each alloc_box by walking backward from mutations.
void ReachabilityInfo::compute() {
  assert(!isComputed() && "already computed");

  unsigned N = 0;
  for (auto &BB : *F)
    BlockMap.insert({ &BB, N++ });
  Matrix = ReachingBlockSet::allocateMatrix(N);
  ReachingBlockSet NewSet = ReachingBlockSet::allocateSet(N);

  DEBUG(llvm::dbgs() << "Computing Reachability for " << F->getName()
        << " with " << N << " blocks.\n");

  // Iterate to a fix point, two times for a topological DAG.
  bool Changed;
  do {
    Changed = false;

    // Visit all blocks in a predictable order, hopefully close to topological.
    for (auto &BB : *F) {
      ReachingBlockSet CurSet(BlockMap[&BB], Matrix);
      if (!Changed) {
        // If we have not detected a change yet, then calculate new
        // reachabilities into a new bit vector so we can determine if any
        // change has occured.
        NewSet = CurSet;
        for (auto PI = BB.pred_begin(), PE = BB.pred_end(); PI != PE; ++PI) {
          unsigned PredID = BlockMap[*PI];
          ReachingBlockSet PredSet(PredID, Matrix);
          NewSet |= PredSet;
          NewSet.set(PredID);
        }
        if (NewSet != CurSet) {
          CurSet = NewSet;
          Changed = true;
        }
      } else {
        // Otherwise, just update the existing reachabilities in-place.
        for (auto PI = BB.pred_begin(), PE = BB.pred_end(); PI != PE; ++PI) {
          unsigned PredID = BlockMap[*PI];
          ReachingBlockSet PredSet(PredID, Matrix);
          CurSet |= PredSet;
          CurSet.set(PredID);
        }
      }
      DEBUG(llvm::dbgs() << "  Block " << BlockMap[&BB] << " reached by ";
            for (unsigned i = 0; i < N; ++i) {
              if (CurSet.test(i))
                llvm::dbgs() << i << " ";
            }
            llvm::dbgs() << "\n");
    }
  } while (Changed);

  ReachingBlockSet::deallocateSet(NewSet);
}

/// \brief Return true if the To basic block is reachable from the From basic
/// block. A block is considered reachable from itself only if its entry can be
/// recursively reached from its own exit.
bool
ReachabilityInfo::isReachable(SILBasicBlock *From, SILBasicBlock *To) {
  if (!isComputed())
    compute();

  auto FI = BlockMap.find(From), TI = BlockMap.find(To);
  assert(FI != BlockMap.end() && TI != BlockMap.end());
  ReachingBlockSet FromSet(TI->second, Matrix);
  return FromSet.test(FI->second);
}

ClosureCloner::ClosureCloner(SILFunction *Orig, IndicesSet &PromotableIndices)
  : SILCloner<ClosureCloner>(*initCloned(Orig, PromotableIndices)),
    Orig(Orig), PromotableIndices(PromotableIndices) {
}

/// \brief Create the function corresponding to the clone of the original
/// closure with the signature modified to reflect promotable captures (which
/// are givien by PromotableIndices, such that each entry in the set is the
/// index of the box containing the variable in the closure's argument list, and
/// the address of the box's contents is the argument immediately following each
/// box argument); does not actually clone the body of the function
SILFunction*
ClosureCloner::initCloned(SILFunction *Orig, IndicesSet &PromotableIndices) {
  SILModule &M = Orig->getModule();

  // Suffix the function name with "_promoteX", where X is the first integer
  // that does not result in a conflict
  unsigned Counter = 0;
  std::string ClonedName;
  do {
    ClonedName.clear();
    llvm::raw_string_ostream buffer(ClonedName);
    buffer << Orig->getName() << "_promote" << Counter++;
  } while (M.lookup(ClonedName));

  SmallVector<SILParameterInfo, 4> ClonedArgTys;

  SILFunctionType *OrigFTI = Orig->getLoweredFunctionType();
  auto OrigParams = OrigFTI->getParameters();

  // Iterate over the argument types of the original function, collapsing each
  // pair of a promotable box argument and the address of its contents into a
  // single argument of the object (rather than address) type of the box's
  // contents
  unsigned Index = 0;
  for (auto &param : OrigParams) {
    if (Index && PromotableIndices.count(Index - 1)) {
      assert(param.getConvention() == ParameterConvention::Indirect_Inout);
      auto &paramTL = M.Types.getTypeLowering(param.getSILType());
      ParameterConvention convention;
      if (paramTL.isPassedIndirectly()) {
        convention = ParameterConvention::Indirect_In;
      } else if (paramTL.isTrivial()) {
        convention = ParameterConvention::Direct_Unowned;
      } else {
        convention = ParameterConvention::Direct_Owned;
      }
      ClonedArgTys.push_back(SILParameterInfo(param.getType(), convention));
    } else if (!PromotableIndices.count(Index)) {
      ClonedArgTys.push_back(param);
    }
    ++Index;
  }

  // Create the thin function type for the cloned closure
  auto ClonedTy =
    SILFunctionType::get(OrigFTI->getGenericParams(),
                         OrigFTI->getExtInfo(),
                         OrigFTI->getCalleeConvention(),
                         ClonedArgTys,
                         OrigFTI->getResult(),
                         M.getASTContext());
  
  // This inserts the new cloned function before the original function.
  return new (M) SILFunction(M, SILLinkage::Internal, ClonedName, ClonedTy,
                             Orig->getLocation(), Orig->isBare(),
                             IsNotTransparent, Orig,
                             Orig->getDebugScope());
}

/// \brief Populate the body of the cloned closure, modifying instructions as
/// necessary to take into consideration the promoted capture(s)
void
ClosureCloner::populateCloned() {
  SILFunction *Cloned = getCloned();
  SILModule &M = Cloned->getModule();

  // Create arguments for the entry block
  SILBasicBlock *OrigEntryBB = Orig->begin();
  SILBasicBlock *ClonedEntryBB = new (M) SILBasicBlock(Cloned);
  unsigned ArgNo = 0;
  auto I = OrigEntryBB->bbarg_begin(), E = OrigEntryBB->bbarg_end();
  while (I != E) {
    if (PromotableIndices.count(ArgNo)) {
      // Handle the case of a promoted capture argument
      SILArgument *ReleaseArgument = *I++;
      SILValue MappedValue =
        new (M) SILArgument((*I)->getType().getObjectType(),
                            ClonedEntryBB, (*I)->getDecl());
      BoxArgumentMap.insert(std::make_pair(ReleaseArgument, MappedValue));
      AddrArgumentMap.insert(std::make_pair(*I, MappedValue));
      ++ArgNo;
    } else {
      // Otherwise, create a new argument which copies the original argument
      SILValue MappedValue =
        new (M) SILArgument((*I)->getType(), ClonedEntryBB, (*I)->getDecl());
      ValueMap.insert(std::make_pair(*I, MappedValue));
    }
    ++ArgNo;
    ++I;
  }

  getBuilder().setInsertionPoint(ClonedEntryBB);
  BBMap.insert(std::make_pair(OrigEntryBB, ClonedEntryBB));
  // Recursively visit original BBs in depth-first preorder, starting with the
  // entry block, cloning all instructions other than terminators.
  visitSILBasicBlock(OrigEntryBB);

  // Now iterate over the BBs and fix up the terminators.
  for (auto BI = BBMap.begin(), BE = BBMap.end(); BI != BE; ++BI) {
    getBuilder().setInsertionPoint(BI->second);
    visit(BI->first->getTerminator());
  }
}

/// \brief Handle a strong_release instruction during cloning of a closure; if
/// it is a strong release of a promoted box argument, then it is replaced wit
/// a destroyValue of the new object type argument, otherwise it is handled
/// normally.
void
ClosureCloner::visitStrongReleaseInst(StrongReleaseInst *Inst) {
  SILValue Operand = Inst->getOperand();
  if (SILArgument *A = dyn_cast<SILArgument>(Operand.getDef())) {
    assert(Operand.getResultNumber() == 0);
    auto I = BoxArgumentMap.find(A);
    if (I != BoxArgumentMap.end()) {
      // Releases of the box arguments get replaced with destroyValue of the new
      // object type argument.
      SILFunction &F = getBuilder().getFunction();
      auto &typeLowering = F.getModule().getTypeLowering(I->second.getType());
      typeLowering.emitDestroyValue(getBuilder(), Inst->getLoc(), I->second);
      return;
    }
  }

  SILCloner<ClosureCloner>::visitStrongReleaseInst(Inst);
}

/// \brief Handle a struct_element_addr instruction during cloning of a closure;
/// if its operand is the promoted address argument then ignore it, otherwise it
/// is handled normally.
void
ClosureCloner::visitStructElementAddrInst(StructElementAddrInst *Inst) {
  SILValue Operand = Inst->getOperand();
  if (SILArgument *A = dyn_cast<SILArgument>(Operand.getDef())) {
    assert(Operand.getResultNumber() == 0);
    auto I = AddrArgumentMap.find(A);
    if (I != AddrArgumentMap.end())
      return;
  }

  SILCloner<ClosureCloner>::visitStructElementAddrInst(Inst);
}

/// \brief Handle a load instruction during cloning of a closure; the two
/// relevant cases are a direct load from a promoted address argument or a load
/// of a struct_element_addr of a promoted address argument.
void
ClosureCloner::visitLoadInst(LoadInst *Inst) {
  SILValue Operand = Inst->getOperand();
  if (auto *A = dyn_cast<SILArgument>(Operand.getDef())) {
    assert(Operand.getResultNumber() == 0);
    auto I = AddrArgumentMap.find(A);
    if (I != AddrArgumentMap.end()) {
      // Loads of the address argument get eliminated completely; the uses of
      // the loads get mapped to uses of the new object type argument.
      ValueMap.insert(std::make_pair(Inst, I->second));
      return;
    }
  } else if (auto *SEAI = dyn_cast<StructElementAddrInst>(Operand.getDef())) {
    assert(Operand.getResultNumber() == 0);
    if (auto *A = dyn_cast<SILArgument>(SEAI->getOperand().getDef())) {
      assert(SEAI->getOperand().getResultNumber() == 0);
      auto I = AddrArgumentMap.find(A);
      if (I != AddrArgumentMap.end()) {
        // Loads of a struct_element_addr of an argument get replaced with
        // struct_extract of the new object type argument.
        SILValue V = getBuilder().emitStructExtract(Inst->getLoc(), I->second,
                                                    SEAI->getField(),
                                                    Inst->getType());
        ValueMap.insert(std::make_pair(Inst, V));
        return;
      }
    }
  }

  SILCloner<ClosureCloner>::visitLoadInst(Inst);
}

/// \brief Given a partial_apply instruction and the argument index into its
/// callee's argument list of a box argument (which is followed by an argument
/// for the address of the box's contents), return true if the closure is known
/// not to mutate the captured variable.
static bool
isNonmutatingCapture(PartialApplyInst *PAI, unsigned Index) {
  // Return false if the callee is not a function with accessible contents.
  auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee().getDef());
  if (!FRI)
    return false;
  assert(PAI->getCallee().getResultNumber() == 0);
  SILFunction *Orig = FRI->getReferencedFunction();
  if (Orig->empty())
    return false;

  // Obtain the arguments for the box and the address of its contents.
  SILBasicBlock *OrigEntryBB = Orig->begin();
  assert(Index + 1 < OrigEntryBB->bbarg_size() &&
         "Too few arguments to entry block of capturing closure");
  SILArgument *BoxArg = OrigEntryBB->getBBArgs()[Index];
  SILArgument *AddrArg = OrigEntryBB->getBBArgs()[Index + 1];

  // For now, return false is the address argument is an address-only type,
  // since we currently assume loadable types only.
  // TODO: handle address-only types
  SILModule &M = PAI->getModule();
  if (AddrArg->getType().isAddressOnly(M))
    return false;

  // Conservatively do not allow any use of the box argument other than a
  // strong_release, since this is the pattern expected from SILGen.
  for (auto *O : BoxArg->getUses())
    if (!isa<StrongReleaseInst>(O->getUser()))
      return false;

  // Only allow loads of the address argument, either directly or via
  // struct_element_addr instructions.
  //
  // TODO: This seems overly limited.  Why not projections of tuples and other
  // stuff?  Also, why not recursive struct elements?  This should be a helper
  // function that mirrors isNonEscapingUse.
  for (auto *O : AddrArg->getUses()) {
    if (auto *SEAI = dyn_cast<StructElementAddrInst>(O->getUser())) {
      for (auto *UO : SEAI->getUses())
        if (!isa<LoadInst>(UO->getUser()))
          return false;
      continue;
    }
    if (!isa<LoadInst>(O->getUser()))
      return false;
  }

  return true;
}

/// \brief Given a use of an alloc_box instruction, return true if the use
/// definitely does not allow the box to escape; also, if the use is an
/// instruction which possibly mutates the contents of the box, then add it to
/// the Mutations vector.
static bool
isNonescapingUse(Operand *O, SmallVectorImpl<SILInstruction*> &Mutations) {
  auto *U = O->getUser();
  // A store or assign is ok if the alloc_box is the destination.
  if (isa<StoreInst>(U) || isa<AssignInst>(U)) {
    if (O->getOperandNumber() != 1)
      return false;
    Mutations.push_back(cast<SILInstruction>(U));
    return true;
  }
  // copy_addr is ok, but counts as a mutation if the use is as the
  // destination or the copy_addr is a take.
  if (auto *CAI = dyn_cast<CopyAddrInst>(U)) {
    if (O->getOperandNumber() == 1 || CAI->isTakeOfSrc())
      Mutations.push_back(CAI);
    return true;
  }
  // Recursively see through struct_element_addr, tuple_element_addr, and
  // project_existential instructions.
  if (isa<StructElementAddrInst>(U) || isa<TupleElementAddrInst>(U) ||
      isa<EnumDataAddrInst>(U) || isa<ProjectExistentialInst>(U)) {
    for (auto *UO : U->getUses())
      if (!isNonescapingUse(UO, Mutations))
        return false;
    return true;
  }
  // An apply is ok if the argument is used as an @inout parameter or an
  // indirect return, but counts as a possible mutation in both cases.
  if (auto *AI = dyn_cast<ApplyInst>(U)) {
    if (AI->getSubstCalleeType()
          ->getParameters()[O->getOperandNumber()-1].isIndirect()) {
      Mutations.push_back(AI);
      return true;
    }
    return false;
  }
  // These instructions are ok but count as mutations.
  if (isa<DeallocBoxInst>(U)) {
    Mutations.push_back(cast<SILInstruction>(U));
    return true;
  }
  // These remaining instructions are ok and don't count as mutations.
  if (isa<StrongRetainInst>(U) || isa<StrongReleaseInst>(U) ||
      isa<LoadInst>(U) || isa<ProtocolMethodInst>(U))
    return true;
  return false;
}

/// \brief Examine an alloc_box instruction, returning true if at least one
/// capture of the boxed variable is promotable.  If so, then the pair of the
/// partial_apply instruction and the index of the box argument in the closure's
/// argument list is added to IM.
static bool
examineAllocBoxInst(AllocBoxInst *ABI, ReachabilityInfo &RI,
                    PartialApplyIndexMap &IM) {
  SmallVector<SILInstruction*, 32> Mutations;

  for (auto *O : ABI->getUses()) {
    if (auto *PAI = dyn_cast<PartialApplyInst>(O->getUser())) {
      unsigned OpNo = O->getOperandNumber();
      assert(OpNo != 0 && "Alloc box used as callee of partial apply?");
      if (O->get().getResultNumber() == 1) {
        if (OpNo < 2 ||
            PAI->getOperand(OpNo - 1) != SILValue(ABI, 0))
          return false;
        continue;
      }
      assert(O->get().getResultNumber() == 0 &&
             "Unexpected result number of alloc box instruction used?");

      // If we've already seen this partial apply, then it means the same alloc
      // box is being captured twice by the same closure, which is odd and
      // unexpected: bail instead of trying to handle this case.
      if (IM.count(PAI))
        return false;

      // Verify that the next operand of the partial apply is the second result
      // of the alloc_box.
      if (OpNo + 1 >= PAI->getNumOperands() ||
          PAI->getOperand(OpNo + 1) != SILValue(ABI, 1))
        return false;

      auto closureType = PAI->getType().castTo<SILFunctionType>();

      // TODO: We currently can only handle non-polymorphic closures.
      if (PAI->hasSubstitutions() || closureType->isPolymorphic())
        return false;

      // Calculate the index into the closure's argument list of the captured
      // box pointer (the captured address is always the immediately following
      // index so is not stored separately);
      unsigned Index = OpNo - 1 + closureType->getParameters().size();

      // Verify that this closure is known not to mutate the captured value; if
      // it does, then conservatively refuse to promote any captures of this
      // value.
      if (!isNonmutatingCapture(PAI, Index))
        return false;

      // Record the index and continue.
      IM.insert(std::make_pair(PAI, Index));
      continue;
    }

    // Verify that this this use does not otherwise allow the alloc_box to
    // escape.
    if (!isNonescapingUse(O, Mutations))
      return false;
  }

  // Helper lambda function to determine if instruction b is strictly after
  // instruction a, assuming both are in the same basic block.
  auto isAfter = [](SILInstruction *a, SILInstruction *b) {
    SILInstruction *f = b->getParent()->begin();
    while (b != f) {
      b = b->getPrevNode();
      if (a == b)
        return true;
    }
    return false;
  };

  // Loop over all mutations to possibly invalidate captures.
  for (auto *I : Mutations) {
    auto Iter = IM.begin();
    while (Iter != IM.end()) {
      auto *PAI = Iter->first;
      // The mutation invalidates a capture if it occurs in a block reachable
      // from the block the partial_apply is in, or if it is in the same
      // block is after the partial_apply.
      if (RI.isReachable(PAI->getParent(), I->getParent()) ||
          (PAI->getParent() == I->getParent() && isAfter(PAI, I))) {
        auto Prev = Iter++;
        IM.erase(Prev);
        continue;
      }
      ++Iter;
    }
    // If there are no valid captures left, then stop.
    if (IM.empty())
      return false;
  }

  return true;
}

/// \brief Given a partial_apply instruction and a set of promotable indices,
/// clone the closure with the promoted captures and replace the partial_apply
/// with a partial_apply of the new closure, fixing up reference counting as
/// necessary. Also, if the closure is cloned, the cloned function is added to
/// the worklist.
static void
processPartialApplyInst(PartialApplyInst *PAI, IndicesSet &PromotableIndices,
                        SmallVectorImpl<SILFunction*> &Worklist) {
  SILModule &M = PAI->getModule();

  auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee().getDef());
  assert(FRI && PAI->getCallee().getResultNumber() == 0);

  // Clone the closure with the given promoted captures.
  SILFunction *ClonedFn;
  {
    ClosureCloner cloner(FRI->getReferencedFunction(), PromotableIndices);
    cloner.populateCloned();
    ClonedFn = cloner.getCloned();
  }

  Worklist.push_back(ClonedFn);

  // Initialize a SILBuilder and create a function_ref referencing the cloned
  // closure.
  SILBuilder B(PAI);
  SILValue FnVal = B.createFunctionRef(PAI->getLoc(), ClonedFn);
  SILType FnTy = FnVal.getType();

  // Populate the argument list for a new partial_apply instruction, taking into
  // consideration any captures.
  unsigned FirstIndex =
    PAI->getType().castTo<SILFunctionType>()->getParameters().size();
  unsigned OpNo = 1, OpCount = PAI->getNumOperands();
  SmallVector<SILValue, 16> Args;
  while (OpNo != OpCount) {
    unsigned Index = OpNo - 1 + FirstIndex;
    if (PromotableIndices.count(Index)) {
      SILValue BoxValue = PAI->getOperand(OpNo);
      SILValue AddrValue = PAI->getOperand(OpNo + 1);
      assert(BoxValue.getDef() == AddrValue.getDef() &&
             BoxValue.getResultNumber() == 0 &&
             AddrValue.getResultNumber() == 1);

      // Emit a strong release, zapping a retain if we can.
      B.emitStrongRelease(PAI->getLoc(), BoxValue);

      // Load and copy from the address value, passing the result as an argument
      // to the new closure.
      auto &typeLowering = M.getTypeLowering(AddrValue.getType());
      Args.push_back(
        typeLowering.emitLoadOfCopy(B, PAI->getLoc(), AddrValue, IsNotTake));
      ++OpNo;
      ++NumCapturesPromoted;
    } else {
      Args.push_back(PAI->getOperand(OpNo));
    }
    ++OpNo;
  }

  // Create a new partial apply with the new arguments.
  auto *NewPAI = B.createPartialApply(PAI->getLoc(), FnVal, FnTy, {}, Args,
                                      PAI->getType());
  SILValue(PAI, 0).replaceAllUsesWith(NewPAI);
  PAI->eraseFromParent();
  if (FRI->use_empty()) {
    FRI->eraseFromParent();
    // TODO: If this is the last use of the closure, and if it has internal
    // linkage, we should remove it from the SILModule now.
  }
}

static void
runOnFunction(SILFunction *F, SmallVectorImpl<SILFunction*> &Worklist) {
  ReachabilityInfo RS(F);

  // This is a map from each partial apply to a set of indices of promotable
  // box variables.
  PartialApplyIndicesMap IndicesMap;

  // This is a map from each partial apply to a single index which is a
  // promotable box variable for the alloc_box currently being considered.
  PartialApplyIndexMap IndexMap;

  // Consider all alloc_box instructions in the function.
  for (auto &BB : *F)
    for (auto &I : BB)
      if (auto *ABI = dyn_cast<AllocBoxInst>(&I)) {
        IndexMap.clear();
        if (examineAllocBoxInst(ABI, RS, IndexMap))
          // If we are able to promote at least one capture of the alloc_box,
          // then add the promotable indices to the main map.
          for (auto &IndexPair : IndexMap)
            IndicesMap[IndexPair.first].insert(IndexPair.second);
      }

  // Do the actual promotions; all promotions on a single partial_apply are
  // handled together.
  for (auto &IndicesPair : IndicesMap)
    processPartialApplyInst(IndicesPair.first, IndicesPair.second, Worklist);
}

void
swift::performSILCapturePromotion(SILModule *M) {
  SmallVector<SILFunction*, 128> Worklist;
  for (auto &F : *M)
    runOnFunction(&F, Worklist);
  while (!Worklist.empty())
    runOnFunction(Worklist.pop_back_val(), Worklist);
}
