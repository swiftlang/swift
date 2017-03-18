//===--- CapturePromotion.cpp - Promotes closure captures -----------------===//
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
///
/// \file
///
/// Promotes captures from 'inout' (i.e. by-reference) to by-value
/// ==============================================================
///
/// Swift's closure model is that all local variables are capture by reference.
/// This produces a very simple programming model which is great to use, but
/// relies on the optimizer to promote by-ref captures to by-value (i.e. by-copy)
/// captures for decent performance. Consider this simple example:
///
///   func foo(a : () -> ()) {} // assume this has an unknown body
///
///   func bar() {
///     var x = 42
///
///     foo({ print(x) })
///   }
///
/// Since x is captured by-ref by the closure, x must live on the heap. By
/// looking at bar without any knowledge of foo, we can know that it is safe to
/// promote this to a by-value capture, allowing x to live on the stack under the
/// following conditions:
///
/// 1. If x is not modified in the closure body and is only loaded.
/// 2. If we can prove that all mutations to x occur before the closure is
///    formed.
///
/// Under these conditions if x is loadable then we can even load the given value
/// and pass it as a scalar instead of an address.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-capture-promotion"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/AST/GenericEnvironment.h"
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

    ReachingBlockMatrix() : Bits(nullptr), NumBitWords(0) {}

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
    M.Bits = nullptr;
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
    S.Bits = nullptr;
    S.NumBitWords = 0;
  }

private:
  uint64_t *Bits;
  unsigned NumBitWords;

public:
  ReachingBlockSet() : Bits(nullptr), NumBitWords(0) {}

  ReachingBlockSet(unsigned BlockID, ReachingBlockMatrix &M)
    : Bits(&M.Bits[BlockID * M.NumBitWords]),
      NumBitWords(M.NumBitWords) {}

  bool test(unsigned ID) const {
    assert(ID / BITWORD_SIZE < NumBitWords && "block ID out-of-bounds");
    unsigned int modulus = ID % BITWORD_SIZE;
    long shifted = 1L << modulus;
    return Bits[ID / BITWORD_SIZE] & shifted;
  }

  void set(unsigned ID) {
    unsigned int modulus = ID % BITWORD_SIZE;
    long shifted = 1L << modulus;
    assert(ID / BITWORD_SIZE < NumBitWords && "block ID out-of-bounds");
    Bits[ID / BITWORD_SIZE] |= shifted;
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

} // end anonymous namespace


namespace {
/// \brief A SILCloner subclass which clones a closure function while converting
/// one or more captures from 'inout' (by-reference) to by-value.
class ClosureCloner : public SILClonerWithScopes<ClosureCloner> {
public:
  friend class SILVisitor<ClosureCloner>;
  friend class SILCloner<ClosureCloner>;

  ClosureCloner(SILFunction *Orig, IsFragile_t Fragile,
                StringRef ClonedName,
                IndicesSet &PromotableIndices);

  void populateCloned();

  SILFunction *getCloned() { return &getBuilder().getFunction(); }

private:
  static SILFunction *initCloned(SILFunction *Orig, IsFragile_t Fragile,
                                 StringRef ClonedName,
                                 IndicesSet &PromotableIndices);

  void visitDebugValueAddrInst(DebugValueAddrInst *Inst);
  void visitStrongReleaseInst(StrongReleaseInst *Inst);
  void visitStructElementAddrInst(StructElementAddrInst *Inst);
  void visitLoadInst(LoadInst *Inst);
  void visitProjectBoxInst(ProjectBoxInst *Inst);

  SILFunction *Orig;
  IndicesSet &PromotableIndices;
  llvm::DenseMap<SILArgument*, SILValue> BoxArgumentMap;
  llvm::DenseMap<ProjectBoxInst*, SILValue> ProjectBoxArgumentMap;
};
} // end anonymous namespace

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
        // change has occurred.
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

ClosureCloner::ClosureCloner(SILFunction *Orig, IsFragile_t Fragile,
                             StringRef ClonedName,
                             IndicesSet &PromotableIndices)
  : SILClonerWithScopes<ClosureCloner>(
                           *initCloned(Orig, Fragile, ClonedName, PromotableIndices)),
    Orig(Orig), PromotableIndices(PromotableIndices) {
  assert(Orig->getDebugScope()->Parent != getCloned()->getDebugScope()->Parent);
}

/// Compute the SILParameterInfo list for the new cloned closure.
///
/// Our goal as a result of this transformation is to:
///
/// 1. Let through all arguments not related to a promotable box.
/// 2. Replace container box value arguments for the cloned closure with the
///    transformed address or value argument.
static void
computeNewArgInterfaceTypes(SILFunction *F,
                            IndicesSet &PromotableIndices,
                            SmallVectorImpl<SILParameterInfo> &OutTys) {
  auto fnConv = F->getConventions();
  auto Parameters = fnConv.funcTy->getParameters();

  DEBUG(llvm::dbgs() << "Preparing New Args!\n");

  // For each parameter in the old function...
  for (unsigned Index : indices(Parameters)) {
    auto &param = Parameters[Index];

    // The PromotableIndices index is expressed as the argument index (num
    // indirect result + param index). Add back the num indirect results to get
    // the arg index when working with PromotableIndices.
    unsigned ArgIndex = Index + fnConv.getSILArgIndexOfFirstParam();

    DEBUG(llvm::dbgs() << "Index: " << Index << "; PromotableIndices: "
          << (PromotableIndices.count(ArgIndex)?"yes":"no")
          << " Param: "; param.dump());

    if (!PromotableIndices.count(ArgIndex)) {
      OutTys.push_back(param);
      continue;
    }
    
    // Perform the proper conversions and then add it to the new parameter list
    // for the type.
    assert(!param.isFormalIndirect());
    auto paramTy = param.getSILStorageType();
    auto paramBoxTy = paramTy.castTo<SILBoxType>();
    assert(paramBoxTy->getLayout()->getFields().size() == 1
           && "promoting compound box not implemented yet");
    auto paramBoxedTy = paramBoxTy->getFieldType(F->getModule(), 0);
    auto &paramTL = F->getModule().Types.getTypeLowering(paramBoxedTy);

    ParameterConvention convention;
    if (paramTL.isFormallyPassedIndirectly()) {
      convention = ParameterConvention::Indirect_In;
    } else if (paramTL.isTrivial()) {
      convention = ParameterConvention::Direct_Unowned;
    } else {
      convention = ParameterConvention::Direct_Owned;
    }
    OutTys.push_back(SILParameterInfo(paramBoxedTy.getSwiftRValueType(),
                                      convention));
  }
}

static std::string getSpecializedName(SILFunction *F,
                                      IsFragile_t Fragile,
                                      IndicesSet &PromotableIndices) {
  auto P = Demangle::SpecializationPass::CapturePromotion;
  NewMangling::FunctionSignatureSpecializationMangler Mangler(P, Fragile, F);
  auto fnConv = F->getConventions();

  for (unsigned argIdx = 0, endIdx = fnConv.getNumSILArguments();
       argIdx < endIdx; ++argIdx) {
    if (!PromotableIndices.count(argIdx))
      continue;
    Mangler.setArgumentBoxToValue(argIdx);
  }
  return Mangler.mangle();
}

/// \brief Create the function corresponding to the clone of the original
/// closure with the signature modified to reflect promotable captures (which
/// are given by PromotableIndices, such that each entry in the set is the
/// index of the box containing the variable in the closure's argument list, and
/// the address of the box's contents is the argument immediately following each
/// box argument); does not actually clone the body of the function
///
/// *NOTE* PromotableIndices only contains the container value of the box, not
/// the address value.
SILFunction*
ClosureCloner::initCloned(SILFunction *Orig, IsFragile_t Fragile,
                          StringRef ClonedName,
                          IndicesSet &PromotableIndices) {
  SILModule &M = Orig->getModule();

  // Compute the arguments for our new function.
  SmallVector<SILParameterInfo, 4> ClonedInterfaceArgTys;
  computeNewArgInterfaceTypes(Orig, PromotableIndices, ClonedInterfaceArgTys);

  SILFunctionType *OrigFTI = Orig->getLoweredFunctionType();

  // Create the thin function type for the cloned closure.
  auto ClonedTy = SILFunctionType::get(
      OrigFTI->getGenericSignature(), OrigFTI->getExtInfo(),
      OrigFTI->getCalleeConvention(), ClonedInterfaceArgTys,
      OrigFTI->getResults(), OrigFTI->getOptionalErrorResult(),
      M.getASTContext());

  assert((Orig->isTransparent() || Orig->isBare() || Orig->getLocation())
         && "SILFunction missing location");
  assert((Orig->isTransparent() || Orig->isBare() || Orig->getDebugScope())
         && "SILFunction missing DebugScope");
  assert(!Orig->isGlobalInit() && "Global initializer cannot be cloned");

  auto *Fn = M.createFunction(
      Orig->getLinkage(), ClonedName, ClonedTy, Orig->getGenericEnvironment(),
      Orig->getLocation(), Orig->isBare(), IsNotTransparent, Fragile,
      Orig->isThunk(), Orig->getClassVisibility(), Orig->getInlineStrategy(),
      Orig->getEffectsKind(), Orig, Orig->getDebugScope());
  for (auto &Attr : Orig->getSemanticsAttrs())
    Fn->addSemanticsAttr(Attr);
  if (Orig->hasUnqualifiedOwnership()) {
    Fn->setUnqualifiedOwnership();
  }
  return Fn;
}

/// \brief Populate the body of the cloned closure, modifying instructions as
/// necessary to take into consideration the promoted capture(s)
void
ClosureCloner::populateCloned() {
  SILFunction *Cloned = getCloned();

  // Create arguments for the entry block
  SILBasicBlock *OrigEntryBB = &*Orig->begin();
  SILBasicBlock *ClonedEntryBB = Cloned->createBasicBlock();
  unsigned ArgNo = 0;
  auto I = OrigEntryBB->args_begin(), E = OrigEntryBB->args_end();
  while (I != E) {
    if (PromotableIndices.count(ArgNo)) {
      // Handle the case of a promoted capture argument.
      auto BoxTy = (*I)->getType().castTo<SILBoxType>();
      assert(BoxTy->getLayout()->getFields().size() == 1
             && "promoting compound box not implemented");
      auto BoxedTy = BoxTy->getFieldType(Cloned->getModule(),0).getObjectType();
      SILValue MappedValue =
          ClonedEntryBB->createFunctionArgument(BoxedTy, (*I)->getDecl());
      BoxArgumentMap.insert(std::make_pair(*I, MappedValue));
      
      // Track the projections of the box.
      for (auto *Use : (*I)->getUses()) {
        if (auto Proj = dyn_cast<ProjectBoxInst>(Use->getUser())) {
          ProjectBoxArgumentMap.insert(std::make_pair(Proj, MappedValue));
        }
      }
    } else {
      // Otherwise, create a new argument which copies the original argument
      SILValue MappedValue = ClonedEntryBB->createFunctionArgument(
          (*I)->getType(), (*I)->getDecl());
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

/// Handle a debug_value_addr instruction during cloning of a closure;
/// if its operand is the promoted address argument then lower it to a
/// debug_value, otherwise it is handled normally.
void ClosureCloner::visitDebugValueAddrInst(DebugValueAddrInst *Inst) {
  SILValue Operand = Inst->getOperand();
  if (auto *A = dyn_cast<ProjectBoxInst>(Operand)) {
    auto I = ProjectBoxArgumentMap.find(A);
    if (I != ProjectBoxArgumentMap.end()) {
      getBuilder().setCurrentDebugScope(getOpScope(Inst->getDebugScope()));
      getBuilder().createDebugValue(Inst->getLoc(), I->second,
                                    Inst->getVarInfo());
      return;
    }
  }

  SILCloner<ClosureCloner>::visitDebugValueAddrInst(Inst);
}

/// \brief Handle a strong_release instruction during cloning of a closure; if
/// it is a strong release of a promoted box argument, then it is replaced with
/// a ReleaseValue of the new object type argument, otherwise it is handled
/// normally.
void
ClosureCloner::visitStrongReleaseInst(StrongReleaseInst *Inst) {
  SILValue Operand = Inst->getOperand();
  if (SILArgument *A = dyn_cast<SILArgument>(Operand)) {
    auto I = BoxArgumentMap.find(A);
    if (I != BoxArgumentMap.end()) {
      // Releases of the box arguments get replaced with ReleaseValue of the new
      // object type argument.
      SILFunction &F = getBuilder().getFunction();
      auto &typeLowering = F.getModule().getTypeLowering(I->second->getType());
      SILBuilderWithPostProcess<ClosureCloner, 1> B(this, Inst);
      typeLowering.emitDestroyValue(B, Inst->getLoc(), I->second);
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
  if (auto *A = dyn_cast<ProjectBoxInst>(Operand)) {
    auto I = ProjectBoxArgumentMap.find(A);
    if (I != ProjectBoxArgumentMap.end())
      return;
  }

  SILCloner<ClosureCloner>::visitStructElementAddrInst(Inst);
}

/// project_box of captured boxes can be eliminated.
void
ClosureCloner::visitProjectBoxInst(ProjectBoxInst *I) {
  if (auto Arg = dyn_cast<SILArgument>(I->getOperand()))
    if (BoxArgumentMap.count(Arg))
      return;
  
  SILCloner<ClosureCloner>::visitProjectBoxInst(I);
}

/// \brief Handle a load instruction during cloning of a closure; the two
/// relevant cases are a direct load from a promoted address argument or a load
/// of a struct_element_addr of a promoted address argument.
void
ClosureCloner::visitLoadInst(LoadInst *Inst) {
  SILValue Operand = Inst->getOperand();
  if (auto *A = dyn_cast<ProjectBoxInst>(Operand)) {
    auto I = ProjectBoxArgumentMap.find(A);
    if (I != ProjectBoxArgumentMap.end()) {
      // Loads of the address argument get eliminated completely; the uses of
      // the loads get mapped to uses of the new object type argument.
      ValueMap.insert(std::make_pair(Inst, I->second));
      return;
    }
  } else if (auto *SEAI = dyn_cast<StructElementAddrInst>(Operand)) {
    if (auto *A = dyn_cast<ProjectBoxInst>(SEAI->getOperand())) {
      auto I = ProjectBoxArgumentMap.find(A);
      if (I != ProjectBoxArgumentMap.end()) {
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

static SILArgument *getBoxFromIndex(SILFunction *F, unsigned Index) {
  assert(F->isDefinition() && "Expected definition not external declaration!");
  auto &Entry = F->front();
  return Entry.getArgument(Index);
}

/// \brief Given a partial_apply instruction and the argument index into its
/// callee's argument list of a box argument (which is followed by an argument
/// for the address of the box's contents), return true if the closure is known
/// not to mutate the captured variable.
static bool
isNonmutatingCapture(SILArgument *BoxArg) {
  SmallVector<ProjectBoxInst*, 2> Projections;
  
  // Conservatively do not allow any use of the box argument other than a
  // strong_release or projection, since this is the pattern expected from
  // SILGen.
  for (auto *O : BoxArg->getUses()) {
    if (isa<StrongReleaseInst>(O->getUser()))
      continue;
    
    if (auto Projection = dyn_cast<ProjectBoxInst>(O->getUser())) {
      Projections.push_back(Projection);
      continue;
    }
    
    return false;
  }

  // Only allow loads of projections, either directly or via
  // struct_element_addr instructions.
  //
  // TODO: This seems overly limited.  Why not projections of tuples and other
  // stuff?  Also, why not recursive struct elements?  This should be a helper
  // function that mirrors isNonEscapingUse.
  for (auto *Projection : Projections) {
    for (auto *O : Projection->getUses()) {
      if (auto *SEAI = dyn_cast<StructElementAddrInst>(O->getUser())) {
        for (auto *UO : SEAI->getUses())
          if (!isa<LoadInst>(UO->getUser()))
            return false;
        continue;
      }
      if (!isa<LoadInst>(O->getUser())
          && !isa<DebugValueAddrInst>(O->getUser())
          && !isa<MarkFunctionEscapeInst>(O->getUser()))
        return false;
    }
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
  if (U->isTypeDependentOperand(*O))
    return true;
  // Marking the boxed value as escaping is OK. It's just a DI annotation.
  if (isa<MarkFunctionEscapeInst>(U))
    return true;
  
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
  // open_existential_addr instructions.
  if (isa<StructElementAddrInst>(U) || isa<TupleElementAddrInst>(U) ||
      isa<InitEnumDataAddrInst>(U) ||
      isa<OpenExistentialAddrInst>(U) || isa<UncheckedTakeEnumDataAddrInst>(U)) {
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
    auto argIndex = O->getOperandNumber()-1;
    SILFunctionConventions substConv(AI->getSubstCalleeType(), AI->getModule());
    auto convention = substConv.getSILArgumentConvention(argIndex);
    if (convention.isIndirectConvention()) {
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

/// \brief Examine an alloc_box instruction, returning true if at least one
/// capture of the boxed variable is promotable.  If so, then the pair of the
/// partial_apply instruction and the index of the box argument in the closure's
/// argument list is added to IM.
static bool
examineAllocBoxInst(AllocBoxInst *ABI, ReachabilityInfo &RI,
                    llvm::DenseMap<PartialApplyInst*, unsigned> &IM) {
  SmallVector<SILInstruction*, 32> Mutations;
  
  // Scan the box for interesting uses.
  for (Operand *O : ABI->getUses()) {
    if (auto *PAI = dyn_cast<PartialApplyInst>(O->getUser())) {
      unsigned OpNo = O->getOperandNumber();
      assert(OpNo != 0 && "Alloc box used as callee of partial apply?");

      // If we've already seen this partial apply, then it means the same alloc
      // box is being captured twice by the same closure, which is odd and
      // unexpected: bail instead of trying to handle this case.
      if (IM.count(PAI))
        return false;

      SILModule &M = PAI->getModule();
      auto closureType = PAI->getType().castTo<SILFunctionType>();
      SILFunctionConventions closureConv(closureType, M);

      // Calculate the index into the closure's argument list of the captured
      // box pointer (the captured address is always the immediately following
      // index so is not stored separately);
      unsigned Index = OpNo - 1 + closureConv.getNumSILArguments();

      auto *Fn = PAI->getReferencedFunction();
      if (!Fn || !Fn->isDefinition())
        return false;

      SILArgument *BoxArg = getBoxFromIndex(Fn, Index);

      // For now, return false is the address argument is an address-only type,
      // since we currently handle loadable types only.
      // TODO: handle address-only types
      auto BoxTy = BoxArg->getType().castTo<SILBoxType>();
      assert(BoxTy->getLayout()->getFields().size() == 1
             && "promoting compound box not implemented yet");
      if (BoxTy->getFieldType(M, 0).isAddressOnly(M))
        return false;

      // Verify that this closure is known not to mutate the captured value; if
      // it does, then conservatively refuse to promote any captures of this
      // value.
      if (!isNonmutatingCapture(BoxArg))
        return false;

      // Record the index and continue.
      IM.insert(std::make_pair(PAI, Index));
      continue;
    }
    if (auto *PBI = dyn_cast<ProjectBoxInst>(O->getUser())) {
      // Check for mutations of the address component.
      SILValue Addr = PBI;
      // If the AllocBox is used by a mark_uninitialized, scan the MUI for
      // interesting uses.
      if (Addr->hasOneUse()) {
        SILInstruction *SingleAddrUser = Addr->use_begin()->getUser();
        if (isa<MarkUninitializedInst>(SingleAddrUser))
          Addr = SILValue(SingleAddrUser);
      }

      for (Operand *AddrOp : Addr->getUses()) {
        if (!isNonescapingUse(AddrOp, Mutations))
          return false;
      }
      continue;
    }
    // Verify that this use does not otherwise allow the alloc_box to
    // escape.
    if (!isNonescapingUse(O, Mutations))
      return false;
  }

  // Helper lambda function to determine if instruction b is strictly after
  // instruction a, assuming both are in the same basic block.
  auto isAfter = [](SILInstruction *a, SILInstruction *b) {
    auto fIter = b->getParent()->begin();
    auto bIter = b->getIterator();
    auto aIter = a->getIterator();
    while (bIter != fIter) {
      --bIter;
      if (aIter == bIter)
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

  // Create the Cloned Name for the function.
  SILFunction *Orig = FRI->getReferencedFunction();

  IsFragile_t Fragile = IsNotFragile;
  if (F->isFragile() && Orig->isFragile())
    Fragile = IsFragile;

  auto ClonedName = getSpecializedName(Orig, Fragile, PromotableIndices);

  // If we already have such a cloned function in the module then just use it.
  if (auto *PrevF = F->getModule().lookUpFunction(ClonedName)) {
    assert(PrevF->isFragile() == Fragile);
    return PrevF;
  }

  // Otherwise, create a new clone.
  ClosureCloner cloner(Orig, Fragile, ClonedName, PromotableIndices);
  cloner.populateCloned();
  return cloner.getCloned();
}

/// \brief Given a partial_apply instruction and a set of promotable indices,
/// clone the closure with the promoted captures and replace the partial_apply
/// with a partial_apply of the new closure, fixing up reference counting as
/// necessary. Also, if the closure is cloned, the cloned function is added to
/// the worklist.
static SILFunction *
processPartialApplyInst(PartialApplyInst *PAI, IndicesSet &PromotableIndices,
                        SmallVectorImpl<SILFunction*> &Worklist) {
  SILModule &M = PAI->getModule();

  auto *FRI = dyn_cast<FunctionRefInst>(PAI->getCallee());

  // Clone the closure with the given promoted captures.
  SILFunction *ClonedFn = constructClonedFunction(PAI, FRI, PromotableIndices);
  Worklist.push_back(ClonedFn);

  // Initialize a SILBuilder and create a function_ref referencing the cloned
  // closure.
  SILBuilderWithScope B(PAI);
  SILValue FnVal = B.createFunctionRef(PAI->getLoc(), ClonedFn);
  SILType FnTy = FnVal->getType();

  // Populate the argument list for a new partial_apply instruction, taking into
  // consideration any captures.
  auto CalleeFunctionTy = PAI->getCallee()->getType().castTo<SILFunctionType>();
  SILFunctionConventions calleeConv(CalleeFunctionTy, M);
  auto CalleePInfo = CalleeFunctionTy->getParameters();
  SILFunctionConventions paConv(PAI->getType().castTo<SILFunctionType>(), M);
  unsigned FirstIndex = paConv.getNumSILArguments();
  unsigned OpNo = 1, OpCount = PAI->getNumOperands();
  SmallVector<SILValue, 16> Args;
  auto NumIndirectResults = calleeConv.getNumIndirectSILResults();
  while (OpNo != OpCount) {
    unsigned Index = OpNo - 1 + FirstIndex;
    if (PromotableIndices.count(Index)) {
      SILValue BoxValue = PAI->getOperand(OpNo);
      AllocBoxInst *ABI = cast<AllocBoxInst>(BoxValue);

      SILParameterInfo CPInfo = CalleePInfo[Index - NumIndirectResults];
      assert(calleeConv.getSILType(CPInfo) == BoxValue->getType()
             && "SILType of parameter info does not match type of parameter");
      // Cleanup the captured argument.
      releasePartialApplyCapturedArg(B, PAI->getLoc(), BoxValue,
                                     CPInfo);

      // Load and copy from the address value, passing the result as an argument
      // to the new closure.
      SILValue Addr;
      for (Operand *BoxUse : ABI->getUses()) {
        auto *PBI = dyn_cast<ProjectBoxInst>(BoxUse->getUser());
          // If the address is marked uninitialized, load through the mark, so
          // that DI can reason about it.
        if (PBI && PBI->hasOneUse()) {
          SILInstruction *PBIUser = PBI->use_begin()->getUser();
          if (isa<MarkUninitializedInst>(PBIUser))
            Addr = PBIUser;
          break;
        }
      }
      // We only reuse an existing project_box if it directly follows the
      // alloc_box. This makes sure that the project_box dominates the
      // partial_apply.
      if (!Addr)
        Addr = getOrCreateProjectBox(ABI, 0);

      auto &typeLowering = M.getTypeLowering(Addr->getType());
      Args.push_back(
        typeLowering.emitLoadOfCopy(B, PAI->getLoc(), Addr, IsNotTake));
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
  PAI->replaceAllUsesWith(NewPAI);
  PAI->eraseFromParent();
  if (FRI->use_empty()) {
    FRI->eraseFromParent();
    // TODO: If this is the last use of the closure, and if it has internal
    // linkage, we should remove it from the SILModule now.
  }
  return ClonedFn;
}

static void
constructMapFromPartialApplyToPromotableIndices(SILFunction *F,
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
          // then add the promotable index to the main map.
          for (auto &IndexPair : IndexMap)
            Map[IndexPair.first].insert(IndexPair.second);
        }
      }
    }
  }
}

namespace {
class CapturePromotionPass : public SILModuleTransform {
  /// The entry point to the transformation.
  void run() override {
    SmallVector<SILFunction*, 128> Worklist;
    for (auto &F : *getModule())
      processFunction(&F, Worklist);

    while (!Worklist.empty())
      processFunction(Worklist.pop_back_val(), Worklist);
  }

  void processFunction(SILFunction *F, SmallVectorImpl<SILFunction*> &Worklist);

  StringRef getName() override { return "Capture Promotion"; }
};
} // end anonymous namespace

void CapturePromotionPass::processFunction(SILFunction *F,
                                      SmallVectorImpl<SILFunction*> &Worklist) {
  // This is a map from each partial apply to a set of indices of promotable
  // box variables.
  PartialApplyIndicesMap IndicesMap;
  constructMapFromPartialApplyToPromotableIndices(F, IndicesMap);

  // Do the actual promotions; all promotions on a single partial_apply are
  // handled together.
  for (auto &IndicesPair : IndicesMap) {
    PartialApplyInst *PAI = IndicesPair.first;
    SILFunction *ClonedFn = processPartialApplyInst(PAI, IndicesPair.second,
                                                    Worklist);
    notifyAddFunction(ClonedFn);
  }
  invalidateAnalysis(F, SILAnalysis::InvalidationKind::Everything);
}

SILTransform *swift::createCapturePromotion() {
  return new CapturePromotionPass();
}
