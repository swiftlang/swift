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
//
// Promotes captures from 'inout' (i.e. by-reference) to by-value
// ==============================================================
//
// Swift's closure model is that all local variables are capture by reference.
// This produces a very simple programming model which is great to use, but
// relies on the optimizer to promote by-ref captures to by-value (i.e. by-copy)
// captures for decent performance. Consider this simple example:
//
//   func foo(a : () -> ()) {} // assume this has an unknown body
//
//   func bar() {
//     var x = 42
//
//     foo({ print(x) })
//   }
//
// Since x is captured by-ref by the closure, x must live on the heap. By
// looking at bar without any knowledge of foo, we can know that it is safe to
// promote this to a by-value capture, allowing x to live on the stack under the
// following conditions:
//
// 1. If x is not modified in the closure body and is only loaded.
// 2. If we can prove that all mutations to x occur before the closure is
//    formed.
//
// Under these conditions if x is loadable then we can even load the given value
// and pass it as a scalar instead of an address.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-capture-promotion"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/Mangle.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include <tuple>

using namespace swift;

typedef llvm::SmallSet<unsigned, 4> IndicesSet;
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
/// one or more captures from 'inout' (by-reference) to by-value.
class ClosureCloner : public TypeSubstCloner<ClosureCloner> {
public:
  friend class SILVisitor<ClosureCloner>;
  friend class SILCloner<ClosureCloner>;

  ClosureCloner(SILFunction *Orig, StringRef ClonedName,
                TypeSubstitutionMap &InterfaceSubs,
                TypeSubstitutionMap &ContextSubs,
                ArrayRef<Substitution> ApplySubs,
                IndicesSet &PromotableIndices);

  void populateCloned();

  SILFunction *getCloned() { return &getBuilder().getFunction(); }

private:
  static SILFunction *initCloned(SILFunction *Orig, StringRef ClonedName,
                                 TypeSubstitutionMap &InterfaceSubs,
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

ClosureCloner::ClosureCloner(SILFunction *Orig, StringRef ClonedName,
                             TypeSubstitutionMap &InterfaceSubs,
                             TypeSubstitutionMap &ContextSubs,
                             ArrayRef<Substitution> ApplySubs,
                             IndicesSet &PromotableIndices)
  : TypeSubstCloner<ClosureCloner>(
                           *initCloned(Orig, ClonedName, InterfaceSubs,
                                       PromotableIndices),
                           *Orig, ContextSubs, ApplySubs),
    Orig(Orig), PromotableIndices(PromotableIndices) {
}

/// Compute the SILParameterInfo list for the new cloned closure.
///
/// SILGen always closes over boxes such that the container address is
/// first. Thus we know that:
///
/// 1. By assumption, all indices that is a box container value is in
///    PromotableIndices.
/// 2. All box address values must have the box container value previous to
///    it implying that PromotableIndices.count(ParamIndex - 1) will be true.
/// 3. The first parameter can *never* be a box address value since there
///    does not exist any previous box container that is able to be
///    associated with it.
///
/// Our goal as a result of this transformation is to:
///
/// 1. Let through all arguments not related to a promotable box.
/// 2. Do not add any container box value arguments to the cloned closure.
/// 3. Add the address box value argument to the cloned closure with the
///    appropriate transformations.
static void
computeNewArgInterfaceTypes(SILFunction *F,
                            IndicesSet &PromotableIndices,
                            SmallVectorImpl<SILParameterInfo> &OutTys) {
  auto Parameters = F->getLoweredFunctionType()->getParameters();

  DEBUG(llvm::dbgs() << "Preparing New Args!\n");

  // For each parameter in the old function...
  for (unsigned Index : indices(Parameters)) {
    auto &param = Parameters[Index];

    DEBUG(llvm::dbgs() << "Index: " << Index << "; PromotableIndices: "
          << (PromotableIndices.count(Index)?"yes":"no")
          << " Param: "; param.dump());

    // With that in mind, first check if we do not have a box address value...
    if (Index == 0 || !PromotableIndices.count(Index - 1)) {

      // If we do not have a box address value, if we have a box container
      // value, continue so we do not add it to the new closure's function type.
      if (PromotableIndices.count(Index))
        continue;

      // Otherwise, we have a function argument not related to a promotable
      // box. Just add it to the new signature and continue.
      OutTys.push_back(param);
      continue;
    }

    // Otherwise, we have an address value of the box. Perform the proper
    // conversions and then add it to the new parameter list for the type.
    assert(param.getConvention() == ParameterConvention::Indirect_Inout);
    auto &paramTL = F->getModule().Types.getTypeLowering(param.getSILType());
    ParameterConvention convention;
    if (paramTL.isPassedIndirectly()) {
      convention = ParameterConvention::Indirect_In;
    } else if (paramTL.isTrivial()) {
      convention = ParameterConvention::Direct_Unowned;
    } else {
      convention = ParameterConvention::Direct_Owned;
    }
    OutTys.push_back(SILParameterInfo(param.getType(), convention));
  }
}

static llvm::SmallString<64> getSpecializedName(SILFunction *F,
                                                IndicesSet &PromotableIndices) {
  llvm::SmallString<64> Name;

  {
    llvm::raw_svector_ostream buffer(Name);
    Mangle::Mangler M(buffer);
    auto P = Mangle::SpecializationPass::CapturePromotion;
    Mangle::FunctionSignatureSpecializationMangler FSSM(P, M, F);
    CanSILFunctionType FTy = F->getLoweredFunctionType();

    ArrayRef<SILParameterInfo> Parameters = FTy->getParameters();
    for (unsigned Index : indices(Parameters)) {
      if (Index == 0 || !PromotableIndices.count(Index - 1)) {
        if (!PromotableIndices.count(Index))
          continue;
        FSSM.setArgumentDead(Index);
        continue;
      }

      FSSM.setArgumentInOutToValue(Index);
    }

    FSSM.mangle();
  }

  return Name;
}

/// \brief Create the function corresponding to the clone of the original
/// closure with the signature modified to reflect promotable captures (which
/// are givien by PromotableIndices, such that each entry in the set is the
/// index of the box containing the variable in the closure's argument list, and
/// the address of the box's contents is the argument immediately following each
/// box argument); does not actually clone the body of the function
///
/// *NOTE* PromotableIndices only contains the container value of the box, not
/// the address value.
SILFunction*
ClosureCloner::initCloned(SILFunction *Orig, StringRef ClonedName,
                          TypeSubstitutionMap &InterfaceSubs,
                          IndicesSet &PromotableIndices) {
  SILModule &M = Orig->getModule();

  // Compute the arguments for our new function.
  SmallVector<SILParameterInfo, 4> ClonedInterfaceArgTys;
  computeNewArgInterfaceTypes(Orig, PromotableIndices, ClonedInterfaceArgTys);

  Module *SM = M.getSwiftModule();
  SILFunctionType *OrigFTI = Orig->getLoweredFunctionType();

  // Create the thin function type for the cloned closure.
  auto ClonedTy =
    SILFunctionType::get(OrigFTI->getGenericSignature(),
                         OrigFTI->getExtInfo(),
                         OrigFTI->getCalleeConvention(),
                         ClonedInterfaceArgTys,
                         OrigFTI->getResult(),
                         M.getASTContext());

  auto SubstTy = SILType::substFuncType(M, SM, InterfaceSubs, ClonedTy,
                                        /* dropGenerics = */ false);
  
  assert((Orig->isTransparent() || Orig->isBare() || Orig->getLocation())
         && "SILFunction missing location");
  assert((Orig->isTransparent() || Orig->isBare() || Orig->getDebugScope())
         && "SILFunction missing DebugScope");
  assert(!Orig->isGlobalInit() && "Global initializer cannot be cloned");

  auto Fn =
      SILFunction::create(M, Orig->getLinkage(), ClonedName, SubstTy,
                          Orig->getContextGenericParams(), Orig->getLocation(),
                          Orig->isBare(), IsNotTransparent, Orig->isFragile(),
                          Orig->getClassVisibility(), Orig->getInlineStrategy(),
                          Orig->getEffectsKind(), Orig, Orig->getDebugScope());
  Fn->setSemanticsAttr(Orig->getSemanticsAttr());
  return Fn;
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
        new (M) SILArgument(ClonedEntryBB,
                            (*I)->getType().getObjectType(),
                            (*I)->getDecl());
      BoxArgumentMap.insert(std::make_pair(ReleaseArgument, MappedValue));
      AddrArgumentMap.insert(std::make_pair(*I, MappedValue));
      ++ArgNo;
    } else {
      // Otherwise, create a new argument which copies the original argument
      SILValue MappedValue =
        new (M) SILArgument(ClonedEntryBB, (*I)->getType(), (*I)->getDecl());
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
/// a ReleaseValue of the new object type argument, otherwise it is handled
/// normally.
void
ClosureCloner::visitStrongReleaseInst(StrongReleaseInst *Inst) {
  SILValue Operand = Inst->getOperand();
  if (SILArgument *A = dyn_cast<SILArgument>(Operand)) {
    assert(Operand.getResultNumber() == 0);
    auto I = BoxArgumentMap.find(A);
    if (I != BoxArgumentMap.end()) {
      // Releases of the box arguments get replaced with ReleaseValue of the new
      // object type argument.
      SILFunction &F = getBuilder().getFunction();
      auto &typeLowering = F.getModule().getTypeLowering(I->second.getType());
      SILBuilderWithPostProcess<ClosureCloner, 1> B(this, Inst);
      typeLowering.emitReleaseValue(B, Inst->getLoc(), I->second);
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
  if (SILArgument *A = dyn_cast<SILArgument>(Operand)) {
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
  if (auto *A = dyn_cast<SILArgument>(Operand)) {
    assert(Operand.getResultNumber() == 0);
    auto I = AddrArgumentMap.find(A);
    if (I != AddrArgumentMap.end()) {
      // Loads of the address argument get eliminated completely; the uses of
      // the loads get mapped to uses of the new object type argument.
      ValueMap.insert(std::make_pair(Inst, I->second));
      return;
    }
  } else if (auto *SEAI = dyn_cast<StructElementAddrInst>(Operand)) {
    assert(Operand.getResultNumber() == 0);
    if (auto *A = dyn_cast<SILArgument>(SEAI->getOperand())) {
      assert(SEAI->getOperand().getResultNumber() == 0);
      auto I = AddrArgumentMap.find(A);
      if (I != AddrArgumentMap.end()) {
        // Loads of a struct_element_addr of an argument get replaced with
        // struct_extract of the new object type argument.
        SILBuilderWithPostProcess<ClosureCloner, 1> B(this, Inst);
        SILValue V = B.emitStructExtract(Inst->getLoc(), I->second,
                                         SEAI->getField(),
                                         Inst->getType());
        ValueMap.insert(std::make_pair(Inst, V));
        return;
      }
    }
  }

  SILCloner<ClosureCloner>::visitLoadInst(Inst);
}

static std::pair<SILArgument *, SILArgument *> getBoxAndAddrFromIndex(
                                                               SILFunction *F,
                                                               unsigned Index) {
  assert(F->isDefinition() && "Expected definition not external declaration!");
  auto &Entry = F->front();
  auto *Box = Entry.getBBArg(Index);
  auto *Addr = Entry.getBBArg(Index + 1);

  return std::make_pair(Box, Addr);
}

static SILFunction *getFunctionDefinition(SILValue FunctionValue) {
  auto *FRI = dyn_cast<FunctionRefInst>(FunctionValue);
  if (!FRI)
    return nullptr;

  auto *Fn = FRI->getReferencedFunction();
  if (!Fn->isDefinition())
    return nullptr;

  return Fn;
}

/// \brief Given a partial_apply instruction and the argument index into its
/// callee's argument list of a box argument (which is followed by an argument
/// for the address of the box's contents), return true if the closure is known
/// not to mutate the captured variable.
static bool
isNonmutatingCapture(SILArgument *BoxArg, SILArgument *AddrArg) {
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
  // open_existential instructions.
  if (isa<StructElementAddrInst>(U) || isa<TupleElementAddrInst>(U) ||
      isa<InitEnumDataAddrInst>(U) ||
      isa<OpenExistentialInst>(U) || isa<UncheckedTakeEnumDataAddrInst>(U)) {
    // UncheckedTakeEnumDataAddr is additionally a mutation.
    if (isa<UncheckedTakeEnumDataAddrInst>(U))
      Mutations.push_back(U);
    
    for (auto *UO : U->getUses())
      if (!isNonescapingUse(UO, Mutations))
        return false;
    return true;
  }
  // An apply is ok if the argument is used as an inout parameter or an
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
      isa<LoadInst>(U))
    return true;
  return false;
}

static bool signatureHasDependentTypes(SILFunctionType &CalleeTy) {
  if (CalleeTy.getSemanticResultSILType().isDependentType())
    return true;

  for (auto ParamTy : CalleeTy.getParameterSILTypesWithoutIndirectResult())
    if (ParamTy.isDependentType())
      return true;

  return false;
}

/// \brief Examine an alloc_box instruction, returning true if at least one
/// capture of the boxed variable is promotable.  If so, then the pair of the
/// partial_apply instruction and the index of the box argument in the closure's
/// argument list is added to IM.
static bool
examineAllocBoxInst(AllocBoxInst *ABI, ReachabilityInfo &RI,
                    llvm::DenseMap<PartialApplyInst*, unsigned> &IM) {
  SmallVector<SILInstruction*, 32> Mutations;
  
  // If the AllocBox is used by a mark_uninitialized, scan the MUI for
  // interesting uses.
  SILValue Addr = ABI->getAddressResult();
  if (Addr.hasOneUse())
    if (auto MUI = dyn_cast<MarkUninitializedInst>(Addr.use_begin()->getUser()))
      Addr = SILValue(MUI);
  
  for (Operand *O : Addr.getUses()) {
    if (auto *PAI = dyn_cast<PartialApplyInst>(O->getUser())) {
      unsigned OpNo = O->getOperandNumber();
      assert(OpNo != 0 && "Alloc box used as callee of partial apply?");

      // If we've already seen this partial apply, then it means the same alloc
      // box is being captured twice by the same closure, which is odd and
      // unexpected: bail instead of trying to handle this case.
      if (IM.count(PAI))
        return false;

      // Verify that the previous operand of the partial apply is the refcount
      // result of the alloc_box.
      if (PAI->getOperand(OpNo - 1) != SILValue(ABI))
        return false;

      auto Callee = PAI->getCallee();
      auto CalleeTy = Callee.getType().castTo<SILFunctionType>();

      // Bail if the signature has any dependent types as we do not
      // currently support these.
      if (signatureHasDependentTypes(*CalleeTy))
        return false;

      auto closureType = PAI->getType().castTo<SILFunctionType>();

      // Calculate the index into the closure's argument list of the captured
      // box pointer (the captured address is always the immediately following
      // index so is not stored separately);
      unsigned Index = OpNo - 2 + closureType->getParameters().size();

      auto *Fn = getFunctionDefinition(Callee);
      if (!Fn)
        return false;

      SILArgument *BoxArg;
      SILArgument *AddrArg;
      std::tie(BoxArg, AddrArg) = getBoxAndAddrFromIndex(Fn, Index);

      // For now, return false is the address argument is an address-only type,
      // since we currently assume loadable types only.
      // TODO: handle address-only types
      SILModule &M = PAI->getModule();
      if (AddrArg->getType().isAddressOnly(M))
        return false;

      // Verify that this closure is known not to mutate the captured value; if
      // it does, then conservatively refuse to promote any captures of this
      // value.
      if (!isNonmutatingCapture(BoxArg, AddrArg))
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

static SILFunction *
constructClonedFunction(PartialApplyInst *PAI, FunctionRefInst *FRI,
                        IndicesSet &PromotableIndices) {
  SILFunction *F = PAI->getFunction();

  // Create the substitution maps.
  TypeSubstitutionMap InterfaceSubs;
  TypeSubstitutionMap ContextSubs;

  ArrayRef<Substitution> ApplySubs = PAI->getSubstitutions();
  auto *genericSig = F->getLoweredFunctionType()->getGenericSignature();
  auto *genericParams = F->getContextGenericParams();

  if (ApplySubs.size()) {
    InterfaceSubs = genericSig->getSubstitutionMap(ApplySubs);
    ContextSubs = genericParams->getSubstitutionMap(ApplySubs);
  } else {
    assert(!genericSig && "Function type has Unexpected generic signature!");
    assert(!genericParams &&
           "Function definition has unexpected generic params!");
  }

  // Create the Cloned Name for the function.
  SILFunction *Orig = FRI->getReferencedFunction();
  auto ClonedName = getSpecializedName(Orig, PromotableIndices);

  // If we already have such a cloned function in the module then just use it.
  if (auto *PrevF = F->getModule().lookUpFunction(ClonedName))
    return PrevF;

  // Otherwise, create a new clone.
  ClosureCloner cloner(Orig, ClonedName, InterfaceSubs,
                       ContextSubs, ApplySubs, PromotableIndices);
  cloner.populateCloned();
  return cloner.getCloned();
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

  auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());
  assert(FRI && PAI->getCallee().getResultNumber() == 0);

  // Clone the closure with the given promoted captures.
  SILFunction *ClonedFn = constructClonedFunction(PAI, FRI, PromotableIndices);
  Worklist.push_back(ClonedFn);

  // Initialize a SILBuilder and create a function_ref referencing the cloned
  // closure.
  SILBuilderWithScope<8> B(PAI);
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
      SILValue UnderlyingAddrValue = AddrValue;
      if (auto *MUI = dyn_cast<MarkUninitializedInst>(AddrValue))
        UnderlyingAddrValue = MUI->getOperand();
      assert(BoxValue.getDef() == UnderlyingAddrValue.getDef() &&
             BoxValue.getResultNumber() == 0 &&
             UnderlyingAddrValue.getResultNumber() == 1);

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

  auto SubstFnTy = FnTy.substGenericArgs(M, PAI->getSubstitutions());

  // Create a new partial apply with the new arguments.
  auto *NewPAI = B.createPartialApply(PAI->getLoc(), FnVal, SubstFnTy,
                                      PAI->getSubstitutions(), Args,
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
constructMapFromPartialApplyToPromoteableIndices(SILFunction *F,
                                                 PartialApplyIndicesMap &Map) {
  ReachabilityInfo RS(F);

  // This is a map from each partial apply to a single index which is a
  // promotable box variable for the alloc_box currently being considered.
  llvm::DenseMap<PartialApplyInst*, unsigned> IndexMap;

  // Consider all alloc_box instructions in the function.
  for (auto &BB : *F) {
    for (auto &I : BB) {
      if (auto *ABI = dyn_cast<AllocBoxInst>(&I)) {
        IndexMap.clear();
        if (examineAllocBoxInst(ABI, RS, IndexMap)) {
          // If we are able to promote at least one capture of the alloc_box,
          // then add the promotable indices to the main map.
          for (auto &IndexPair : IndexMap)
            Map[IndexPair.first].insert(IndexPair.second);
        }
      }
    }
  }
}

static void
processFunction(SILFunction *F, SmallVectorImpl<SILFunction*> &Worklist) {
  // This is a map from each partial apply to a set of indices of promotable
  // box variables.
  PartialApplyIndicesMap IndicesMap;
  constructMapFromPartialApplyToPromoteableIndices(F, IndicesMap);

  // Do the actual promotions; all promotions on a single partial_apply are
  // handled together.
  for (auto &IndicesPair : IndicesMap)
    processPartialApplyInst(IndicesPair.first, IndicesPair.second, Worklist);
}

namespace {
class CapturePromotionPass : public SILModuleTransform {
  /// The entry point to the transformation.
  virtual void run() {
    SmallVector<SILFunction*, 128> Worklist;
    for (auto &F : *getModule())
      processFunction(&F, Worklist);

    if (!Worklist.empty())
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);

    while (!Worklist.empty())
      processFunction(Worklist.pop_back_val(), Worklist);
  }

  StringRef getName() override { return "Capture Promotion"; }
};
} // end anonymous namespace


SILTransform *swift::createCapturePromotion() {
  return new CapturePromotionPass();
}
