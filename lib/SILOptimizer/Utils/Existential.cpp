//===--- Existential.cpp - Functions analyzing existentials.  -------------===//
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

#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/SILOptimizer/Utils/Existential.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "llvm/ADT/SmallPtrSet.h"

using namespace swift;

/// Determine InitExistential from global_addr.
/// %3 = global_addr @$P : $*SomeP
/// %4 = init_existential_addr %3 : $*SomeP, $SomeC
/// %5 = alloc_ref $SomeC
/// store %5 to %4 : $*SomeC
/// %8 = alloc_stack $SomeP
/// copy_addr %3 to [initialization] %8 : $*SomeP
/// %10 = apply %9(%3) : $@convention(thin) (@in_guaranteed SomeP)
/// Assumptions: Insn is a direct user of GAI (e.g., copy_addr or 
/// apply pattern shown above) and that a valid init_existential_addr 
/// value is returned only if it can prove that the value it 
/// initializes is the same value at the use point.
static SILValue findInitExistentialFromGlobalAddr(GlobalAddrInst *GAI,
                                                  SILInstruction *Insn) {
  /// Check for a single InitExistential usage for GAI and
  /// a simple dominance check: both InitExistential and Insn are in
  /// the same basic block and only one InitExistential
  /// occurs between GAI and Insn.
  llvm::SmallPtrSet<SILInstruction *, 8> IEUses;
  for (auto *Use : GAI->getUses()) {
    if (auto *InitExistential =
            dyn_cast<InitExistentialAddrInst>(Use->getUser())) {
      IEUses.insert(InitExistential);
    }
  }

  /// No InitExistential found in the basic block.
  if (IEUses.empty())
    return SILValue();

  /// Walk backwards from Insn instruction till the begining of the basic block
  /// looking for an InitExistential.
  SILValue SingleIE;
  for (auto II = Insn->getIterator().getReverse(),
            IE = Insn->getParent()->rend();
       II != IE; ++II) {
    if (!IEUses.count(&*II))
      continue;
    if (SingleIE)
      return SILValue();

    SingleIE = cast<InitExistentialAddrInst>(&*II);
  }
  return SingleIE;
}

/// Determine InitExistential from global_addr and copy_addr.
/// %3 = global_addr @$P : $*SomeP
/// %4 = init_existential_addr %3 : $*SomeP, $SomeC
/// %5 = alloc_ref $SomeC
/// store %5 to %4 : $*SomeC
/// %8 = alloc_stack $SomeP
/// copy_addr %3 to [initialization] %8 : $*SomeP
SILValue
swift::findInitExistentialFromGlobalAddrAndCopyAddr(GlobalAddrInst *GAI,
                                                    CopyAddrInst *CAI) {
  assert(CAI->getSrc() == SILValue(GAI) &&
         "Broken Assumption! Global Addr is not the source of the passed in "
         "copy_addr?!");
  return findInitExistentialFromGlobalAddr(GAI, cast<SILInstruction>(CAI));
}

/// Determine InitExistential from global_addr and an apply argument.
/// Pattern 1
/// %3 = global_addr @$P : $*SomeP
/// %4 = init_existential_addr %3 : $*SomeP, $SomeC
/// %5 = alloc_ref $SomeC
/// store %5 to %4 : $*SomeC
/// %10 = apply %9(%3) : $@convention(thin) (@in_guaranteed SomeP)
/// Pattern 2
/// %3 = global_addr @$P : $*SomeP
/// %9 = open_existential_addr mutable_access %3 : $*SomeP to $*@opened SomeP
/// %15 = apply %11(%9) : $@convention(thin) (@in_guaranteed SomeP)
SILValue swift::findInitExistentialFromGlobalAddrAndApply(GlobalAddrInst *GAI,
                                                          ApplySite Apply,
                                                          int ArgIdx) {
  /// Code to ensure that we are calling only in two pattern matching scenarios.
  bool isArg = false;
  auto Arg = Apply.getArgument(ArgIdx);
  if (auto *ApplyGAI = dyn_cast<GlobalAddrInst>(Arg)) {
    if (ApplyGAI->isIdenticalTo(GAI)) {
      isArg = true;
    }
  } else if (auto Open = dyn_cast<OpenExistentialAddrInst>(Arg)) {
    auto Op = Open->getOperand();
    if (auto *OpGAI = dyn_cast<GlobalAddrInst>(Op)) {
      if (OpGAI->isIdenticalTo(GAI)) {
        isArg = true;
      }
    }
  }
  assert(isArg && "Broken Assumption! Global Addr is not an argument to "
                  "apply?!");
  return findInitExistentialFromGlobalAddr(GAI, Apply.getInstruction());
}

/// Returns the address of an object with which the stack location \p ASI is
/// initialized. This is either a init_existential_addr or the destination of a
/// copy_addr. Returns a null value if the address does not dominate the
/// alloc_stack user \p ASIUser.
/// If the value is copied from another stack location, \p isCopied is set to
/// true.
SILValue swift::getAddressOfStackInit(AllocStackInst *ASI,
                                      SILInstruction *ASIUser, bool &isCopied) {
  SILInstruction *SingleWrite = nullptr;
  // Check that this alloc_stack is initialized only once.
  for (auto Use : ASI->getUses()) {
    auto *User = Use->getUser();

    // Ignore instructions which don't write to the stack location.
    // Also ignore ASIUser (only kicks in if ASIUser is the original apply).
    if (isa<DeallocStackInst>(User) || isa<DebugValueAddrInst>(User) ||
        isa<DestroyAddrInst>(User) || isa<WitnessMethodInst>(User) ||
        isa<DeinitExistentialAddrInst>(User) ||
        isa<OpenExistentialAddrInst>(User) || User == ASIUser) {
      continue;
    }
    if (auto *CAI = dyn_cast<CopyAddrInst>(User)) {
      if (CAI->getDest() == ASI) {
        if (SingleWrite)
          return SILValue();
        SingleWrite = CAI;
        isCopied = true;
      }
      continue;
    }
    if (isa<InitExistentialAddrInst>(User)) {
      if (SingleWrite)
        return SILValue();
      SingleWrite = User;
      continue;
    }
    if (isa<ApplyInst>(User) || isa<TryApplyInst>(User)) {
      // Ignore function calls which do not write to the stack location.
      auto Conv = FullApplySite(User).getArgumentConvention(*Use);
      if (Conv != SILArgumentConvention::Indirect_In &&
          Conv != SILArgumentConvention::Indirect_In_Guaranteed)
        return SILValue();
      continue;
    }
    // Bail if there is any unknown (and potentially writing) instruction.
    return SILValue();
  }
  if (!SingleWrite)
    return SILValue();

  // A very simple dominance check. As ASI is an operand of ASIUser,
  // SingleWrite dominates ASIUser if it is in the same block as ASI or ASIUser.
  SILBasicBlock *BB = SingleWrite->getParent();
  if (BB != ASI->getParent() && BB != ASIUser->getParent())
    return SILValue();

  if (auto *CAI = dyn_cast<CopyAddrInst>(SingleWrite)) {
    // Try to derive the type from the copy_addr that was used to
    // initialize the alloc_stack.
    assert(isCopied && "isCopied not set for a copy_addr");
    SILValue CAISrc = CAI->getSrc();
    if (auto *ASI = dyn_cast<AllocStackInst>(CAISrc))
      return getAddressOfStackInit(ASI, CAI, isCopied);
    // Check if the CAISrc is a global_addr.
    if (auto *GAI = dyn_cast<GlobalAddrInst>(CAISrc)) {
      return findInitExistentialFromGlobalAddrAndCopyAddr(GAI, CAI);
    }
    return CAISrc;
  }
  return cast<InitExistentialAddrInst>(SingleWrite);
}

/// Find the init_existential, which could be used to determine a concrete
/// type of the \p Self.
/// If the value is copied from another stack location, \p isCopied is set to
/// true.
SILInstruction *swift::findInitExistential(Operand &openedUse,
                                           ArchetypeType *&OpenedArchetype,
                                           SILValue &OpenedArchetypeDef,
                                           bool &isCopied) {
  SILValue Self = openedUse.get();
  SILInstruction *User = openedUse.getUser();
  isCopied = false;
  if (auto *Instance = dyn_cast<AllocStackInst>(Self)) {
    // In case the Self operand is an alloc_stack where a copy_addr copies the
    // result of an open_existential_addr to this stack location.
    if (SILValue Src = getAddressOfStackInit(Instance, User, isCopied))
      Self = Src;
  }

  if (auto *Open = dyn_cast<OpenExistentialAddrInst>(Self)) {
    auto Op = Open->getOperand();
    auto *ASI = dyn_cast<AllocStackInst>(Op);
    if (!ASI)
      return nullptr;

    SILValue StackWrite = getAddressOfStackInit(ASI, Open, isCopied);
    if (!StackWrite)
      return nullptr;

    auto *IE = dyn_cast<InitExistentialAddrInst>(StackWrite);
    if (!IE)
      return nullptr;

    OpenedArchetype = Open->getType().castTo<ArchetypeType>();
    OpenedArchetypeDef = Open;
    return IE;
  }

  if (auto *Open = dyn_cast<OpenExistentialRefInst>(Self)) {
    if (auto *IE = dyn_cast<InitExistentialRefInst>(Open->getOperand())) {
      OpenedArchetype = Open->getType().castTo<ArchetypeType>();
      OpenedArchetypeDef = Open;
      return IE;
    }
    return nullptr;
  }

  if (auto *Open = dyn_cast<OpenExistentialMetatypeInst>(Self)) {
    if (auto *IE = dyn_cast<InitExistentialMetatypeInst>(Open->getOperand())) {
      auto Ty = Open->getType().getASTType();
      while (auto Metatype = dyn_cast<MetatypeType>(Ty))
        Ty = Metatype.getInstanceType();
      OpenedArchetype = cast<ArchetypeType>(Ty);
      OpenedArchetypeDef = Open;
      return IE;
    }
    return nullptr;
  }
  return nullptr;
}

/// Derive a concrete type of self and conformance from the init_existential
/// instruction.
/// If successful, initializes a valid ConformanceAndConcreteType.
ConcreteExistentialInfo::ConcreteExistentialInfo(Operand &openedUse) {
  // Try to find the init_existential, which could be used to
  // determine a concrete type of the self.
  // Returns: InitExistential, OpenedArchetype, OpenedArchetypeDef, isCopied.
  InitExistential = findInitExistential(openedUse, OpenedArchetype,
                                        OpenedArchetypeDef, isCopied);
  if (!InitExistential)
    return;

  ArrayRef<ProtocolConformanceRef> ExistentialConformances;

  if (auto IE = dyn_cast<InitExistentialAddrInst>(InitExistential)) {
    ExistentialType = IE->getOperand()->getType().getASTType();
    ExistentialConformances = IE->getConformances();
    ConcreteType = IE->getFormalConcreteType();
    ConcreteValue = IE;
  } else if (auto IER = dyn_cast<InitExistentialRefInst>(InitExistential)) {
    ExistentialType = IER->getType().getASTType();
    ExistentialConformances = IER->getConformances();
    ConcreteType = IER->getFormalConcreteType();
    ConcreteValue = IER->getOperand();
  } else if (auto IEM =
                 dyn_cast<InitExistentialMetatypeInst>(InitExistential)) {
    ExistentialType = IEM->getType().getASTType();
    ExistentialConformances = IEM->getConformances();
    ConcreteValue = IEM->getOperand();
    ConcreteType = ConcreteValue->getType().getASTType();
    while (auto InstanceType =
               dyn_cast<ExistentialMetatypeType>(ExistentialType)) {
      ExistentialType = InstanceType.getInstanceType();
      ConcreteType = cast<MetatypeType>(ConcreteType).getInstanceType();
    }
  } else {
    assert(!isValid());
    return;
  }
  // Construct a single-generic-parameter substitution map directly to the
  // ConcreteType with this existential's full list of conformances.
  SILModule &M = InitExistential->getModule();
  CanGenericSignature ExistentialSig =
      M.getASTContext().getExistentialSignature(ExistentialType,
                                                M.getSwiftModule());
  ExistentialSubs = SubstitutionMap::get(ExistentialSig, {ConcreteType},
                                         ExistentialConformances);
  // If the concrete type is another existential, we're "forwarding" an
  // opened existential type, so we must keep track of the original
  // defining instruction.
  if (ConcreteType->isOpenedExistential()) {
    if (InitExistential->getTypeDependentOperands().empty()) {
      auto op = InitExistential->getOperand(0);
      assert(op->getType().hasOpenedExistential()
             && "init_existential is supposed to have a typedef operand");
      ConcreteTypeDef = cast<SingleValueInstruction>(op);
    } else {
      ConcreteTypeDef = cast<SingleValueInstruction>(
          InitExistential->getTypeDependentOperands()[0].get());
    }
  }
  assert(isValid());
}

/// Initialize a ConcreteExistentialInfo based on the already computed concrete
/// type and protocol declaration. It determines the OpenedArchetypeDef
/// and SubstituionMap for the ArgOperand argument.
/// We need the OpenedArchetypeDef to be able to cast it to the concrete type.
/// findInitExistential helps us determine this OpenedArchetypeDef. For cases
/// where OpenedArchetypeDef can not be found from findInitExistential (because
/// there was no InitExistential), then we do
/// extra effort in trying to find an OpenedArchetypeDef for AllocStackInst
/// using getAddressOfStackInit.
ConcreteExistentialInfo::ConcreteExistentialInfo(Operand &ArgOperand,
                                                 CanType ConcreteTy,
                                                 ProtocolDecl *Protocol)
    : ConcreteExistentialInfo(ArgOperand) {

  // If we found an InitExistential, assert that ConcreteType we determined is
  // same as ConcreteTy argument.
  if (InitExistential) {
    assert(ConcreteType == ConcreteTy);
    assert(isValid());
    return;
  }

  ConcreteType = ConcreteTy;
  OpenedArchetypeDef = ArgOperand.get();

  // If findInitExistential call from the other constructor did not update the
  // OpenedArchetypeDef (because it did not find an InitExistential) and that
  // the original Arg is an alloc_stack instruction, then we determine the
  // OpenedArchetypeDef using getAddressOfStackInit. Please keep in mind that an
  // alloc_stack can be an argument to apply (which could have no associated
  // InitExistential), thus we need to determine the OpenedArchetypeDef for it.
  // This is the extra effort.
  SILInstruction *User = ArgOperand.getUser();
  SILModule &M = User->getModule();
  if (auto *ASI = dyn_cast<AllocStackInst>(OpenedArchetypeDef)) {
    bool copied = false;
    if (SILValue Src = getAddressOfStackInit(ASI, User, copied)) {
      OpenedArchetypeDef = Src;
    }
    isCopied = copied;
  }

  // Bail, if we did not find an opened existential.
  if (!(isa<OpenExistentialRefInst>(OpenedArchetypeDef) ||
        isa<OpenExistentialAddrInst>(OpenedArchetypeDef)))
    return;

  // We have the open_existential; we still need the conformance.
  auto ConformanceRef =
      M.getSwiftModule()->lookupConformance(ConcreteType, Protocol);
  if (!ConformanceRef)
    return;

  // Assert that the conformance is complete.
  auto *ConcreteConformance = ConformanceRef.getValue().getConcrete();
  assert(ConcreteConformance->isComplete());

  /// Determine the ExistentialConformances and SubstitutionMap.
  ExistentialType = Protocol->getDeclaredType()->getCanonicalType();
  auto ExistentialSig = M.getASTContext().getExistentialSignature(
      ExistentialType, M.getSwiftModule());
  ExistentialSubs = SubstitutionMap::get(
      ExistentialSig, {ConcreteType},
      llvm::makeArrayRef(ProtocolConformanceRef(ConcreteConformance)));

  /// Determine the OpenedArchetype.
  OpenedArchetype =
      OpenedArchetypeDef->getType().castTo<ArchetypeType>();

  /// Check validity.
  assert(isValid());
}
