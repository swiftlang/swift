//===-------------------------- SILCombine --------------------------------===//
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
// A port of LLVM's InstCombine pass to SIL. Its main purpose is for performing
// small combining operations/peepholes at the SIL level. It additionally
// performs dead code elimination when it initially adds instructions to the
// work queue in order to reduce compile time by not visiting trivially dead
// instructions.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-combine"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::PatternMatch;

STATISTIC(NumSimplified, "Number of instructions simplified");
STATISTIC(NumCombined, "Number of instructions combined");
STATISTIC(NumDeadInst, "Number of dead insts eliminated");
STATISTIC(NumDeadFunc, "Number of dead functions eliminated");
STATISTIC(NumFuncDevirt, "Number of functions devirtualized");

//===----------------------------------------------------------------------===//
//                             SILCombineWorklist
//===----------------------------------------------------------------------===//

namespace swift {

/// This is the worklist management logic for SILCombine.
class SILCombineWorklist {
  llvm::SmallVector<SILInstruction *, 256> Worklist;
  llvm::DenseMap<SILInstruction *, unsigned> WorklistMap;
  llvm::SmallVector<SILInstruction *, 8> TrackingList;

  void operator=(const SILCombineWorklist &RHS) = delete;
  SILCombineWorklist(const SILCombineWorklist &Worklist) = delete;
public:
  SILCombineWorklist() {}

  /// Returns true if the worklist is empty.
  bool isEmpty() const { return Worklist.empty(); }

  /// Add the specified instruction to the worklist if it isn't already in it.
  void add(SILInstruction *I) {
    if (WorklistMap.insert(std::make_pair(I, Worklist.size())).second) {
      DEBUG(llvm::dbgs() << "SC: ADD: " << *I << '\n');
      Worklist.push_back(I);
    }
  }

  /// If the given ValueBase is a SILInstruction add it to the worklist.
  void addValue(ValueBase *V) {
    if (SILInstruction *I = llvm::dyn_cast<SILInstruction>(V))
      add(I);
  }

  /// Add the given list of instructions in reverse order to the worklist. This
  /// routine assumes that the worklist is empty and the given list has no
  /// duplicates.
  void addInitialGroup(ArrayRef<SILInstruction *> List) {
    assert(Worklist.empty() && "Worklist must be empty to add initial group");
    Worklist.reserve(List.size()+16);
    WorklistMap.resize(List.size());
    DEBUG(llvm::dbgs() << "SC: ADDING: " << List.size()
                       << " instrs to worklist\n");
    while (!List.empty()) {
      SILInstruction *I = List.back();
      List = List.slice(0, List.size()-1);

      WorklistMap.insert(std::make_pair(I, Worklist.size()));
      Worklist.push_back(I);
    }
  }

  // If I is in the worklist, remove it.
  void remove(SILInstruction *I) {
    auto It = WorklistMap.find(I);
    if (It == WorklistMap.end()) return; // Not in worklist.

    // Don't bother moving everything down, just null out the slot. We will
    // check before we process any instruction if it is null.
    Worklist[It->second] = 0;

    WorklistMap.erase(It);
  }

  /// Remove the top element from the worklist.
  SILInstruction *removeOne() {
    SILInstruction *I = Worklist.pop_back_val();
    WorklistMap.erase(I);
    return I;
  }

  /// When an instruction has been simplified, add all of its users to the
  /// worklist since additional simplifications of its users may have been
  /// exposed.
  void addUsersToWorklist(ValueBase *I) {
    for (auto UI : I->getUses())
      add(UI->getUser());
  }

  /// If only one result of an instruction has been simplified, add all of the
  /// users of that result to the worklist since additional simplifications of
  /// its users may have been exposed.
  void addUsersToWorklist(ValueBase *I, unsigned Index) {
    for (auto UI : SILValue(I, Index).getUses())
      add(UI->getUser());
  }

  /// Check that the worklist is empty and nuke the backing store for the map if
  /// it is large.
  void zap() {
    assert(WorklistMap.empty() && "Worklist empty, but the map is not?");

    // Do an explicit clear, this shrinks the map if needed.
    WorklistMap.clear();
  }
};

} // end namespace swift

//===----------------------------------------------------------------------===//
//                                SILCombiner
//===----------------------------------------------------------------------===//

namespace swift {

/// This is a class which maintains the state of the combiner and simplifies
/// many operations such as removing/adding instructions and syncing them with
/// the worklist.
class SILCombiner :
    public SILInstructionVisitor<SILCombiner, SILInstruction *> {
public:
  SILCombiner(SILModule &M) : Module(M), Worklist(), MadeChange(false),
                              Iteration(0), Builder(0) { }

  void runOnFunction(SILFunction &F) {
    clear();

    // Create a SILBuilder for F and initialize the tracking list.
    SILBuilder B(F);
    B.setTrackingList(&TrackingList);
    Builder = &B;

    // Perform iterations until we do not make any changes.
    while (doOneIteration(F, Iteration)) {
      Iteration++;
    }

    // Cleanup the builder and return whether or not we made any changes.
    Builder = 0;
  }

  void clear() {
    Iteration = 0;
    Worklist.zap();
    MadeChange = false;
  }

  // Insert the instruction New before instruction Old in Old's parent BB. Add
  // New to the worklist.
  SILInstruction *insertNewInstBefore(SILInstruction *New,
                                      SILInstruction &Old) {
    assert(New && New->getParent() == 0 &&
           "New instruction already inserted into a basic block!");
    SILBasicBlock *BB = Old.getParent();
    BB->getInstList().insert(&Old, New);  // Insert inst
    Worklist.add(New);
    return New;
  }

  // This method is to be used when an instruction is found to be dead,
  // replacable with another preexisting expression. Here we add all uses of I
  // to the worklist, replace all uses of I with the new value, then return I,
  // so that the combiner will know that I was modified.
  SILInstruction *replaceInstUsesWith(SILInstruction &I, ValueBase *V) {
    Worklist.addUsersToWorklist(&I);   // Add all modified instrs to worklist.

    DEBUG(llvm::dbgs() << "SC: Replacing " << I << "\n"
          "    with " << *V << '\n');

    I.replaceAllUsesWith(V);

    return &I;
  }

  /// This is meant to be used when one is attempting to replace only one of the
  /// results of I with a result of V.
  SILInstruction *replaceInstUsesWith(SILInstruction &I, ValueBase *V,
                                      unsigned IIndex, unsigned VIndex=0) {
    assert(IIndex < I.getNumTypes() && "Can not have more results than "
           "types.");
    assert(VIndex < V->getNumTypes() && "Can not have more results than "
           "types.");

    // Add all modified instrs to worklist.
    Worklist.addUsersToWorklist(&I, IIndex);

    DEBUG(llvm::dbgs() << "SC: Replacing " << I << "\n"
          "    with " << *V << '\n');

    SILValue(&I, IIndex).replaceAllUsesWith(SILValue(V, VIndex));

    return &I;
  }

  // Some instructions can never be "trivially dead" due to side effects or
  // producing a void value. In those cases, since we can not rely on
  // SILCombines trivially dead instruction DCE in order to delete the
  // instruction, visit methods should use this method to delete the given
  // instruction and upon completion of their peephole return the value returned
  // by this method.
  SILInstruction *eraseInstFromFunction(SILInstruction &I) {
    DEBUG(llvm::dbgs() << "SC: ERASE " << I << '\n');

    assert(I.use_empty() && "Cannot erase instruction that is used!");
    // Make sure that we reprocess all operands now that we reduced their
    // use counts.
    if (I.getNumOperands() < 8) {
      for (auto &OpI : I.getAllOperands())
        if (SILInstruction *Op = llvm::dyn_cast<SILInstruction>(&*OpI.get()))
          Worklist.add(Op);
    }
    Worklist.remove(&I);
    I.eraseFromParent();
    MadeChange = true;
    return 0;  // Don't do anything with I
  }

  void addInitialGroup(ArrayRef<SILInstruction *> List) {
    Worklist.addInitialGroup(List);
  }

  /// Base visitor that does not do anything.
  SILInstruction *visitValueBase(ValueBase *V) { return nullptr; }
  SILInstruction *visitStructExtractInst(StructExtractInst *V);
  SILInstruction *visitTupleExtractInst(TupleExtractInst *TEI);
  SILInstruction *visitDestroyValueInst(DestroyValueInst *DI);
  SILInstruction *visitCopyValueInst(CopyValueInst *CI);
  SILInstruction *visitClassMethodInst(ClassMethodInst *CMI);
  SILInstruction *visitApplyInst(ApplyInst *AI);

private:
  /// Perform one SILCombine iteration.
  bool doOneIteration(SILFunction &F, unsigned Iteration);

  /// Module currently being processed.
  SILModule &Module;
  /// Worklist containing all of the instructions primed for simplification.
  SILCombineWorklist Worklist;
  /// Variable to track if the SILCombiner made any changes.
  bool MadeChange;
  /// The current iteration of the SILCombine.
  unsigned Iteration;
  /// Builder used to insert instructions.
  SILBuilder *Builder;
  /// A list that the builder inserts newly created instructions into. Its
  /// contents are added to the worklist after every iteration and then the list
  /// is cleared.
  llvm::SmallVector<SILInstruction *, 64> TrackingList;
};

} // end namespace swift

//===----------------------------------------------------------------------===//
//                         SILCombine Implementation
//===----------------------------------------------------------------------===//

/// addReachableCodeToWorklist - Walk the function in depth-first order, adding
/// all reachable code to the worklist.
///
/// This has a couple of tricks to make the code faster and more powerful.  In
/// particular, we DCE instructions as we go, to avoid adding them to the
/// worklist (this significantly speeds up SILCombine on code where many
/// instructions are dead or constant).
static void addReachableCodeToWorklist(SILBasicBlock *BB, SILCombiner &SC) {
  llvm::SmallVector<SILBasicBlock*, 256> Worklist;
  llvm::SmallVector<SILInstruction*, 128> InstrsForSILCombineWorklist;
  llvm::SmallPtrSet<SILBasicBlock*, 64> Visited;

  Worklist.push_back(BB);
  do {
    BB = Worklist.pop_back_val();

    // We have now visited this block!  If we've already been here, ignore it.
    if (!Visited.insert(BB)) continue;

    for (SILBasicBlock::iterator BBI = BB->begin(), E = BB->end(); BBI != E; ) {
      SILInstruction *Inst = BBI++;

      // DCE instruction if trivially dead.
      if (isInstructionTriviallyDead(Inst)) {
        ++NumDeadInst;
        DEBUG(llvm::dbgs() << "SC: DCE: " << *Inst << '\n');
        Inst->eraseFromParent();
        continue;
      }

      InstrsForSILCombineWorklist.push_back(Inst);
    }

    // Recursively visit successors.
    for (auto SI = BB->succ_begin(), SE = BB->succ_end(); SI != SE; ++SI)
      Worklist.push_back(*SI);
  } while (!Worklist.empty());

  // Once we've found all of the instructions to add to the worklist, add them
  // in reverse order. This way SILCombine will visit from the top of the
  // function down. This jives well with the way that it adds all uses of
  // instructions to the worklist after doing a transformation, thus avoiding
  // some N^2 behavior in pathological cases.
  SC.addInitialGroup(InstrsForSILCombineWorklist);
}

bool SILCombiner::doOneIteration(SILFunction &F, unsigned Iteration) {
  MadeChange = false;

  DEBUG(llvm::dbgs() << "\n\nSILCOMBINE ITERATION #" << Iteration << " on "
                     << F.getName() << "\n");

  // Add reachable instructions to our worklist.
  addReachableCodeToWorklist(F.begin(), *this);

  // Process until we run out of items in our worklist.
  while (!Worklist.isEmpty()) {
    SILInstruction *I = Worklist.removeOne();

    // When we erase an instruction, we use the map in the worklist to check if
    // the instruction is in the worklist. If it is, we replace it with null
    // instead of shifting all members of the worklist towards the front. This
    // check makes sure that if we run into any such residual null pointers, we
    // skip them.
    if (I == 0)
      continue;

    // Check to see if we can DCE the instruction.
    if (isInstructionTriviallyDead(I)) {
      DEBUG(llvm::dbgs() << "SC: DCE: " << *I << '\n');
      eraseInstFromFunction(*I);
      ++NumDeadInst;
      MadeChange = true;
      continue;
    }

    // Check to see if we can instsimplify the instruction.
    if (SILValue Result = simplifyInstruction(I)) {
      ++NumSimplified;

      DEBUG(llvm::dbgs() << "SC: Simplify Old = " << *I << '\n'
                         << "    New = " << *Result.getDef() << '\n');

      // Everything uses the new instruction now.
      replaceInstUsesWith(*I, Result.getDef(), 0, Result.getResultNumber());

      // Push the new instruction and any users onto the worklist.
      Worklist.addUsersToWorklist(Result.getDef());

      eraseInstFromFunction(*I);
      MadeChange = true;
      continue;
    }

    // If we have reached this point, all attempts to do simple simplifications
    // have failed. Prepare to SILCombine.
    Builder->setInsertionPoint(I->getParent(), I);

#ifndef NDEBUG
    std::string OrigI;
#endif
    DEBUG(llvm::raw_string_ostream SS(OrigI); I->print(SS); OrigI = SS.str(););
    DEBUG(llvm::dbgs() << "SC: Visiting: " << OrigI << '\n');

    if (SILInstruction *Result = visit(I)) {
      ++NumCombined;
      // Should we replace the old instruction with a new one?
      if (Result != I) {
        // Insert the new instruction into the basic block.
        SILBasicBlock *InstParent = I->getParent();
        SILBasicBlock::iterator InsertPos = I;
        InstParent->getInstList().insert(InsertPos, Result);

        DEBUG(llvm::dbgs() << "SC: Old = " << *I << '\n'
                           << "    New = " << *Result << '\n');

        // Everything uses the new instruction now.
        replaceInstUsesWith(*I, Result);

        // Push the new instruction and any users onto the worklist.
        Worklist.add(Result);
        Worklist.addUsersToWorklist(Result);


        eraseInstFromFunction(*I);
      } else {
        DEBUG(llvm::dbgs() << "SC: Mod = " << OrigI << '\n'
                     << "    New = " << *I << '\n');

        // If the instruction was modified, it's possible that it is now dead.
        // if so, remove it.
        if (isInstructionTriviallyDead(I)) {
          eraseInstFromFunction(*I);
        } else {
          Worklist.add(I);
          Worklist.addUsersToWorklist(I);
        }
      }
      MadeChange = true;
    }

    // Our tracking list has been accumulating instructions created by the
    // SILBuilder during this iteration. Go through the tracking list and add
    // its contents to the worklist and then clear said list in preparation for
    // the next iteration.
    for (SILInstruction *I : TrackingList) {
      Worklist.add(I);
    }
    TrackingList.clear();
  }

  Worklist.zap();
  return MadeChange;
}

//===----------------------------------------------------------------------===//
//                                  Visitors
//===----------------------------------------------------------------------===//

SILInstruction *SILCombiner::visitStructExtractInst(StructExtractInst *SEI) {
  // (struct_extract (load %x) #vardecl)
  //   ->
  // (load (struct_element_addr %x), #vardecl)
  LoadInst *LI;
  if (match(SEI->getOperand(), m_LoadInst(LI))) {
    // Move our insertion point to the load so we insert the new
    // struct_element_addr and load there.
    //
    // This is to ensure that in a situation like the following:
    //
    // %y = (load %x)
    // (do_stuff)
    // (struct_extract %y #vardecl)
    //
    // if (do_stuff) modifies the memory at %x, we get the original value.
    Builder->setInsertionPoint(LI);
    StructElementAddrInst *SEA =
      Builder->createStructElementAddr(SEI->getLoc(), LI->getOperand(),
                                       SEI->getField());
    LoadInst *Result = Builder->createLoad(SEI->getLoc(), SEA);

    return replaceInstUsesWith(*SEI, Result, 0);
  }

  return nullptr;
}

SILInstruction *SILCombiner::visitTupleExtractInst(TupleExtractInst *TEI) {
  // (tuple_extract (load %x) 0)
  //   ->
  // (load (tuple_element_addr %x) 0)
  LoadInst *LI;
  if (match(TEI->getOperand(), m_LoadInst(LI))) {
    // Move our insertion point to the load so we insert the new
    // tuple_element_addr and load there.
    //
    // This is to ensure that in a situation like the following:
    //
    // %y = (load %x)
    // (do_stuff)
    // (tuple_extract %y 0)
    //
    // if (do_stuff) modifies the memory at %x, we get the original value.
    Builder->setInsertionPoint(LI);
    TupleElementAddrInst *TEA =
      Builder->createTupleElementAddr(TEI->getLoc(), LI->getOperand(),
                                      TEI->getFieldNo());
    LoadInst *Result = Builder->createLoad(TEI->getLoc(), TEA);

    return replaceInstUsesWith(*TEI, Result, 0);
  }

  return nullptr;
}

SILInstruction *SILCombiner::visitDestroyValueInst(DestroyValueInst *DI) {
  SILValue Operand = DI->getOperand();
  SILType OperandTy = Operand.getType();

  // Destroy value of an enum with a trivial payload or no-payload is a no-op.
  if (auto *EI = dyn_cast<EnumInst>(Operand.getDef()))
    if (!EI->hasOperand() || EI->getOperand().getType().isTrivial(Module))
      return eraseInstFromFunction(*DI);

  // DestroyValueInst of a reference type is a strong_release.
  if (OperandTy.hasReferenceSemantics()) {
    return new (Module) StrongReleaseInst(DI->getLoc(), Operand);
  }

  // DestroyValueInst of a trivial type is a no-op.
  if (OperandTy.isTrivial(Module)) {
    return eraseInstFromFunction(*DI);
  }

  // Do nothing for non-trivial non-reference types.
  return nullptr;
}

SILInstruction *SILCombiner::visitCopyValueInst(CopyValueInst *CI) {
  SILValue Operand = CI->getOperand();

  // If the copy_value is right before a destroy_value that uses it, remove
  // both and forward the copy_value's operand to its uses.
  //
  // Since the copy_value is right before the destroy_value, there are no flow
  // issues here. This occurs often with enum values.
  SILBasicBlock::iterator I = CI;
  ++I;
  if (auto *DI = dyn_cast<DestroyValueInst>(&*I))
    if (DI->getOperand().getDef() == CI ||
        DI->getOperand() == Operand) {
      eraseInstFromFunction(*DI);
      replaceInstUsesWith(*CI, CI->getOperand().getDef(), 0);
      return eraseInstFromFunction(*CI);
    }

  SILType OperandTy = Operand.getType();

  // copy_value of an enum with a trivial payload or no-payload is a no-op +
  // RAUW.
  if (auto *EI = dyn_cast<EnumInst>(Operand.getDef()))
    if (!EI->hasOperand() || EI->getOperand().getType().isTrivial(Module)) {
      // We need to use eraseInstFromFunction + RAUW here since a copy value can
      // never be trivially dead since it touches reference counts.
      replaceInstUsesWith(*CI, EI, 0);
      return eraseInstFromFunction(*CI);
    }

  // CopyValueInst of a reference type is a strong_release.
  if (OperandTy.hasReferenceSemantics()) {
    Builder->createStrongRetain(CI->getLoc(), Operand);
    // We need to use eraseInstFromFunction + RAUW here since a copy value can
    // never be trivially dead since it touches reference counts.
    replaceInstUsesWith(*CI, Operand.getDef(), 0);
    return eraseInstFromFunction(*CI);
  }

  // CopyValueInst of a trivial type is a no-op + use propogation.
  if (OperandTy.isTrivial(Module)) {
    // We need to use eraseInstFromFunction + RAUW here since a copy value can
    // never be trivially dead since it touches reference counts.
    replaceInstUsesWith(*CI, Operand.getDef(), 0);
    return eraseInstFromFunction(*CI);
  }

  // Do nothing for non-trivial non-reference types.
  return nullptr;
}

/// \brief Scan the use-def chain and skip cast instructions that don't change
/// the value of the class. Stop on classes that define a class type.
SILInstruction *findMetaType(SILValue S) {
  SILInstruction *Inst = dyn_cast<SILInstruction>(S);
  if (!Inst)
    return nullptr;

  switch (Inst->getKind()) {
    case ValueKind::AllocRefInst:
    case ValueKind::MetatypeInst:
      return Inst;
    case ValueKind::UpcastInst:
    case ValueKind::UnconditionalCheckedCastInst:
      return findMetaType(Inst->getOperand(0));
    default:
      return nullptr;
  }
}

/// \brief Replaces a virtual ApplyInst instruction with a new ApplyInst
/// instruction that does not use a project_existencial \p PEI and calls \p F
/// directly. See visitApplyInst.
static SILInstruction *
replaceDynApplyWithStaticApply(ApplyInst *AI,
                               SILFunction *F,
                               InitExistentialInst *In,
                               ProjectExistentialInst *PEI) {
  // Creates a new FunctionRef Inst and inserts it to the basic block.
  FunctionRefInst *FRI = new (AI->getModule()) FunctionRefInst(AI->getLoc(), F);
  AI->getParent()->getInstList().insert(AI, FRI);
  SmallVector<SILValue, 4> Args;

  // Push all of the args and replace uses of PEI with the InitExistentional.
  MutableArrayRef<Operand> OrigArgs = AI->getArgumentOperands();
  for (unsigned i = 0; i < OrigArgs.size(); i++) {
    SILValue A = OrigArgs[i].get();
    Args.push_back(A.getDef() == PEI ? In : A);
  }

  // Create a new non-virtual ApplyInst.
  SILType FnTy = FRI->getType();
  return ApplyInst::create(AI->getLoc(), FRI, FnTy,
                           FnTy.castTo<SILFunctionType>()
                           ->getInterfaceResult().getSILType(),
                           ArrayRef<Substitution>(), Args, false, *F);
}

/// \brief Scan the uses of the protocol object and return the initialization
/// instruction, which can be copy_addr or init_existential.
/// There needs to be only one initialization instruction and the
/// object must not be captured by any instruction that may re-initialize it.
static SILInstruction *
findSingleInitNoCaptureProtocol(SILValue ProtocolObject) {
  SILInstruction *Init = 0;
  for (auto UI = ProtocolObject->use_begin(), E = ProtocolObject->use_end();
       UI != E; UI++) {
    switch (UI.getUser()->getKind()) {
      case ValueKind::CopyAddrInst: {
        // If we are reading the content of the protocol (to initialize
        // something else) then its okay.
        if (cast<CopyAddrInst>(UI.getUser())->getSrc() == ProtocolObject)
          continue;

        // fallthrough: ...
      }
      case ValueKind::InitExistentialInst: {
        // Make sure there is a single initialization:
        if (Init) {
          DEBUG(llvm::dbgs() << " *** Multiple Protocol initializers: " <<
                *UI.getUser() << " and " << *Init);
          return nullptr;
        }
        // This is the first initialization.
        Init = UI.getUser();
        continue;
      }
      case ValueKind::ProjectExistentialInst:
      case ValueKind::ProtocolMethodInst:
      case ValueKind::DeallocBoxInst:
      case ValueKind::DeallocRefInst:
      case ValueKind::DeallocStackInst:
      case ValueKind::StrongReleaseInst:
      case ValueKind::DestroyAddrInst:
      case ValueKind::DestroyValueInst:
        continue;

      default: {
        DEBUG(llvm::dbgs() << " *** Protocol captured by: " << *UI.getUser());
        return nullptr;
      }
    }
  }
  return Init;
}

SILInstruction *SILCombiner::visitApplyInst(ApplyInst *AI) {
  // Devirtualize protocol_method + project_existential + init_existential
  // instructions.  For example:
  //
  // %0 = alloc_box $Pingable
  // %1 = init_existential %0#1 : $*Pingable, $*Foo  <-- Foo is the static type!
  // %4 = project_existential %0#1 : $*Pingable to $*@sil_self Pingable
  // %5 = protocol_method %0#1 : $*Pingable, #Pingable.ping!1 :
  // %8 = apply %5(ARGUMENTS ... , %4) :

  // Find the protocol_method instruction.
  ProtocolMethodInst *PMI = dyn_cast<ProtocolMethodInst>(AI->getCallee());
  if (!PMI)
    return nullptr;

  // Find the last argument, which is the Self argument, which may be a
  // project_existential instruction.
  MutableArrayRef<Operand> Args = AI->getArgumentOperands();
  if (Args.size() < 1)
    return nullptr;

  SILValue LastArg = Args[Args.size() - 1].get();
  ProjectExistentialInst *PEI = dyn_cast<ProjectExistentialInst>(LastArg);
  if (!PEI)
    return nullptr;

  // Make sure that the project_existential and protocol_method instructions
  // use the same protocol.
  SILValue ProtocolObject = PMI->getOperand();
  if (PEI->getOperand().getDef() != ProtocolObject.getDef())
    return nullptr;

  DEBUG(llvm::dbgs() << " *** Protocol to devirtualize : " <<
        *ProtocolObject.getDef());

  // Find a single initialization point, and make sure the protocol is not
  // captured. We also handle the case where the initializer is the copy_addr
  // instruction by looking at the source object.
  SILInstruction *InitInst = findSingleInitNoCaptureProtocol(ProtocolObject);
  if (CopyAddrInst *CAI = dyn_cast_or_null<CopyAddrInst>(InitInst)) {
    if (!CAI->isInitializationOfDest() || !CAI->isTakeOfSrc())
      return nullptr;
    InitInst = findSingleInitNoCaptureProtocol(CAI->getSrc());
  }

  InitExistentialInst *Init = dyn_cast_or_null<InitExistentialInst>(InitInst);
  if (!Init)
    return nullptr;

  // Strip the @InOut qualifier.
  CanType ConcreteTy = Init->getConcreteType().getSwiftType();
  if (InOutType *IOT = dyn_cast<InOutType>(ConcreteTy)) {
    ConcreteTy = IOT->getObjectType()->getCanonicalType();
  }

  SILDeclRef Member = PMI->getMember();
  // For each protocol that our type conforms to:
  for (auto &Conf : Init->getConformances()) {
    // Scan all of the witness tables in search of a matching method.
    for (SILWitnessTable &Witness : AI->getModule().getWitnessTableList()) {
      ProtocolDecl *WitnessProtocol = Witness.getConformance()->getProtocol();
      // Is this the correct protocol?

      if (WitnessProtocol != Conf->getProtocol() ||
          !ConcreteTy.getPointer()->isEqual(Witness.getConformance()->getType()))
        continue;

      DEBUG(llvm::dbgs() << " *** Found witness table for : " << *Init);

      // Okay, we found the right witness table. Now look for the method.
      for (auto &Entry : Witness.getEntries()) {
        // Look at method entries only.
        if (Entry.getKind() != SILWitnessTable::WitnessKind::Method)
          continue;

        SILWitnessTable::MethodWitness MethodEntry = Entry.getMethodWitness();
        // Check if this is the member we were looking for.
        if (MethodEntry.Requirement != Member)
          continue;

        // We found the correct witness function. Devirtualize this Apply.
        DEBUG(llvm::dbgs() << " *** Devirtualized : " << *AI);
        SILFunction *StaticRef = MethodEntry.Witness;
        NumFuncDevirt++;
        return replaceDynApplyWithStaticApply(AI, StaticRef, Init, PEI);
      }
    }
  }
  DEBUG(llvm::dbgs() << " *** Could not find a witness table for: " << *PMI);

  return nullptr;
}

SILInstruction *SILCombiner::visitClassMethodInst(ClassMethodInst *CMI) {
  // Optimize a class_method and alloc_ref pair into a direct function
  // reference:
  //
  // %XX = alloc_ref $Foo
  // %YY = class_method %XX : $Foo, #Foo.get!1 : $@cc(method) @thin ...
  //
  //  or
  //
  //  %XX = metatype $...
  //  %YY = class_method %XX : ...
  //
  //  into
  //
  //  %YY = function_ref @...

  // Look for an instruction that defines a class type.
  SILInstruction *Meta = findMetaType(CMI->getOperand());
  if (!Meta)
    return nullptr;

  ClassDecl *Class = nullptr;

  // Look for a a static ClassTypes in AllocRefInst or MetatypeInst.
  if (AllocRefInst *ARI = dyn_cast<AllocRefInst>(Meta)) {
    Class = ARI->getType().getClassOrBoundGenericClass();
  } else if (MetatypeInst *MTI = dyn_cast<MetatypeInst>(Meta)) {
    CanType MetaTy = MTI->getType().getSwiftRValueType();
    TypeBase *T = cast<MetatypeType>(MetaTy)->getInstanceType().getPointer();
    Class = T->getClassOrBoundGenericClass();
  } else {
    return nullptr;
  }

  // Walk up the class hierarchy and scan all members.
  // TODO: There has to be a faster way of doing this scan.
  SILDeclRef Member = CMI->getMember();
  while (Class) {
    // Search all of the vtables in the module.
    for (auto &Vtbl : CMI->getModule().getVTableList()) {
      if (Vtbl.getClass() != Class)
        continue;

      // Find the requested method.
      if (SILFunction *F = Vtbl.getImplementation(CMI->getModule(), Member)) {
        // Create a direct reference to the method.
         NumFuncDevirt++;
        return new (Module) FunctionRefInst(CMI->getLoc(), F);
      }
    }

    // We could not find the member in our class. Moving to our superclass.
    if (Type T = Class->getSuperclass())
      Class = T->getClassOrBoundGenericClass();
    else
      break;
  }

  return nullptr;
}

bool tryToRemoveFunction(SILFunction *F) {
  // Remove internal functions that are not referenced by anything.
  // TODO: top_level_code is currently marked as internal so we explicitly check
  // for functions with this name and keep them around.
  if (F->getLinkage() != SILLinkage::Internal || F->getRefCount() ||
      F->getName() == SWIFT_ENTRY_POINT_FUNCTION)
    return false;

  DEBUG(llvm::dbgs() << "SC: Erasing:" << F->getName() << "\n");
  F->getModule().eraseFunction(F);
  NumDeadFunc++;
  return true;
}

/// Removes internal functions that no other function calls.
void deleteDeadFunctions(SILModule *M) {
  // Erase trivially dead functions that may not be a part of the call graph.
  for (auto FI = M->begin(), EI = M->end(); FI != EI;) {
    SILFunction *F = FI++;
    tryToRemoveFunction(F);
  }

  std::vector<SILFunction*> Order;
  // returns a bottom-up list of functions, leafs first.
  bottomUpCallGraphOrder(M, Order);

  // Scan the call graph top-down (caller first) because eliminating functions
  // can generate more opportunities.
  for (int i = Order.size() - 1; i >= 0; i--)
    tryToRemoveFunction(Order[i]);
 }

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILCombine(SILModule *M) {
  SILCombiner Combiner(*M);

  // Process each function in M.
  for (SILFunction &F : *M) {
    // If F is just a declaration without any basic blocks, skip it.
    if (F.empty())
      continue;

    // Combine instructions in F.
    Combiner.runOnFunction(F);
  }

  deleteDeadFunctions(M);
}
