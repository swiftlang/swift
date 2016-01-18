//===--- LetPropertiesOpts.cpp - Optimize let properties ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// Promote values of non-static let properties initialized by means
// of constant values of simple types into their uses.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "let-properties-opt"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
using namespace swift;

namespace {
/// Promote values of non-static let properties initialized by means
/// of constant values of simple types into their uses.
///
/// TODO: Don't occupy any storage for such let properties with constant
/// initializers.
///
/// Note: Storage from a let property can only be removed if this
/// property can never be referenced from another module.

class LetPropertiesOpt {
  SILModule *Module;
  bool HasChanged = false;

  typedef SmallVector<SILInstruction *, 8> Instructions;

  // Map each let property to a set of instructions accessing it.
  llvm::MapVector<VarDecl *, Instructions> AccessMap;
  // Map each let property to the instruction sequence which initializes it.
  llvm::MapVector<VarDecl *, Instructions> InitMap;
  // Properties in this set should not be processed by this pass
  // anymore.
  llvm::SmallPtrSet<VarDecl *, 16> SkipProcessing;
  // Properties in this set cannot be removed.
  llvm::SmallPtrSet<VarDecl *, 16> CannotRemove;

public:
  LetPropertiesOpt(SILModule *M): Module(M) {}

  bool run();

protected:
  bool isConstantLetProperty(VarDecl *Property);
  void collectPropertyAccess(SILInstruction *I, VarDecl *Property, bool NonRemovable);
  void optimizeLetPropertyAccess(VarDecl *SILG,
                                 SmallVectorImpl<SILInstruction *> &Init);
  bool getInitializer(NominalTypeDecl *NTD, VarDecl *Property,
                      SmallVectorImpl<SILInstruction *> &Init);
};

/// Helper class to copy only a set of SIL instructions providing in the
/// constructor.
class InstructionsCloner : public SILClonerWithScopes<InstructionsCloner> {
  friend class SILVisitor<InstructionsCloner>;
  friend class SILCloner<InstructionsCloner>;

  ArrayRef<SILInstruction *> Insns;

  protected:
  SILBasicBlock *FromBB;
  SILInstruction *Dest;

  public:
  // A map of old to new available values.
  SmallVector<std::pair<ValueBase *, SILValue>, 16> AvailVals;

  InstructionsCloner(SILFunction &F, ArrayRef<SILInstruction *> Insns,
                     SILInstruction *Dest = nullptr)
      : SILClonerWithScopes(Dest ? *Dest->getFunction() : F), Insns(Insns),
        FromBB(nullptr), Dest(Dest) {}

  void process(SILInstruction *I) { visit(I); }

  SILBasicBlock *remapBasicBlock(SILBasicBlock *BB) { return BB; }

  SILValue remapValue(SILValue Value) {
    return SILCloner<InstructionsCloner>::remapValue(Value);
  }

  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    Dest->getParent()->push_front(Cloned);
    Cloned->moveBefore(Dest);
    SILClonerWithScopes<InstructionsCloner>::postProcess(Orig, Cloned);
    AvailVals.push_back(std::make_pair(Orig, SILValue(Cloned, 0)));
  }

  // Clone all instructions from Insns into DestBB
  void clone() {
    for (auto I : Insns)
      process(I);
  }
};

} // namespace

/// Optimize access to the let property, which is known
/// to have a constant value. Replace all loads from the
/// property by its constant value.
void LetPropertiesOpt::optimizeLetPropertyAccess(VarDecl *Property,
    SmallVectorImpl<SILInstruction *> &Init) {

  if (SkipProcessing.count(Property))
    return;

  auto *Ty = dyn_cast<NominalTypeDecl>(Property->getDeclContext());
  DEBUG(llvm::dbgs() << "Replacing access to property "
                     << Ty->getName() << "::" << Property->getName()
                     << " by its constant initializer\n");

  auto PropertyAccess = Property->getEffectiveAccess();
  auto TypeAccess = Ty->getEffectiveAccess();
  auto CanRemove = false;

  // Check if a given let property can be removed, because it
  // is not accessible elsewhere. This can happen if this property
  // is private or if it is internal and WMO mode is used.
  if (TypeAccess == Accessibility::Private ||
      PropertyAccess == Accessibility::Private
      || ((TypeAccess == Accessibility::Internal ||
          PropertyAccess == Accessibility::Internal) &&
          Module->isWholeModule())) {
    CanRemove = true;
    DEBUG(llvm::dbgs() << "Storage for property "
                       << Ty->getName() << "::" << Property->getName()
                       << " can be eliminated\n");
  }

  if (CannotRemove.count(Property))
    CanRemove = false;

  if (!AccessMap.count(Property)) {
    if (CanRemove) {
      // TODO: Remove the let property, because it is never accessed.
    }
    return;
  }

  auto &Loads = AccessMap[Property];

  unsigned NumReplaced = 0;

  for (auto Load: Loads) {
    // Look for any instructions accessing let properties.
    if (isa<RefElementAddrInst>(Load)) {
      // Copy the initializer into the function
      // Replace the access to a let property by the value
      // computed by this initializer.
      InstructionsCloner Cloner(*Load->getFunction(), Init, Load);
      Cloner.clone();
      SILInstruction *I = &*std::prev(Load->getIterator());
      SILBuilderWithScope B(Load);
      for (auto UI = Load->use_begin(), E = Load->use_end(); UI != E;) {
        auto *User = UI->getUser();
        ++UI;
        if (isa<StoreInst>(User))
          continue;

        replaceLoadSequence(User, I, B);
        eraseUsesOfInstruction(User);
        User->eraseFromParent();
        ++NumReplaced;
      }
      HasChanged = true;
    } else if (isa<StructExtractInst>(Load)) {
      // Copy the initializer into the function
      // Replace the access to a let property by the value
      // computed by this initializer.
      InstructionsCloner Cloner(*Load->getFunction(), Init, Load);
      Cloner.clone();
      SILInstruction *I = &*std::prev(Load->getIterator());
      Load->replaceAllUsesWith(I);
      Load->eraseFromParent();
      ++NumReplaced;
      HasChanged = true;
    }  else if (isa<StructElementAddrInst>(Load)) {
      // Copy the initializer into the function
      // Replace the access to a let property by the value
      // computed by this initializer.
      InstructionsCloner Cloner(*Load->getFunction(), Init, Load);
      Cloner.clone();
      SILInstruction *I = &*std::prev(Load->getIterator());
      SILBuilderWithScope B(Load);
      for (auto UI = Load->use_begin(), E = Load->use_end(); UI != E;) {
        auto *User = UI->getUser();
        ++UI;
        if (isa<StoreInst>(User))
          continue;
        replaceLoadSequence(User, I, B);
        eraseUsesOfInstruction(User);
        User->eraseFromParent();
        ++NumReplaced;
      }
      HasChanged = true;
    }
  }

  DEBUG(llvm::dbgs() << "Access to "
                     << Ty->getName() << "::" << Property->getName()
                     << " was replaced " << NumReplaced << " time(s)\n");

  if (CanRemove) {
    // TODO: Remove the let property, because it is never accessed.
  }
}

/// Compare to SILValues structurally.
static bool CmpSILValues(SILValue LHS, SILValue RHS) {
  if (LHS.getResultNumber() != RHS.getResultNumber() ||
      LHS.getType() != RHS.getType())
    return false;

  auto L = dyn_cast<SILInstruction>(LHS.getDef());
  return L->isIdenticalTo(dyn_cast<SILInstruction>(RHS.getDef()), CmpSILValues);
};

/// Compare two sequences of SIL instructions. They should be structurally equivalent.
static bool compareInsnSequences(SmallVectorImpl<SILInstruction *> &LHS,
                                 SmallVectorImpl<SILInstruction *> &RHS) {
  if (LHS.size() != RHS.size())
    return false;

  for (int i=0, e=LHS.size(); i < e; ++i)
    if (!LHS[i]->isIdenticalTo(RHS[i], CmpSILValues))
      return false;
  return true;
}

/// Find a set of instructions computing the constant value
/// initializing a given property.
SILValue findStoredValue(SILFunction *Init, VarDecl *Property,
                         SmallVectorImpl<SILInstruction *> &Insns) {
  for (auto &BB: *Init) {
    for (auto &I: BB) {
      if (auto *RI = dyn_cast<ReturnInst>(&I)) {
        if (auto *SI = dyn_cast<StructInst>(RI->getOperand())) {
          auto Value = SI->getFieldValue(Property);
          SmallVector<SILInstruction *, 8> ReverseInsns;
          if (!analyzeStaticInitializer(Value, ReverseInsns))
            return SILValue();
          // Produce a correct order of instructions.
          while (!ReverseInsns.empty()) {
            Insns.push_back(ReverseInsns.pop_back_val());
          }
          return Value;
        }
      }

      if (auto *SI = dyn_cast<StoreInst>(&I)) {
        auto Dest = SI->getDest();

        auto REAI = dyn_cast<RefElementAddrInst>(Dest);
        if (!REAI || REAI->getField() != Property)
            continue;

        auto Value = SI->getSrc();
        SmallVector<SILInstruction *, 8> ReverseInsns;
        // ReverseInsns.push_back(SI);
        if (!analyzeStaticInitializer(Value, ReverseInsns))
          return SILValue();
        // Produce a correct order of instructions.
        while (!ReverseInsns.empty()) {
          Insns.push_back(ReverseInsns.pop_back_val());
        }
        return Value;
      }
    }
  }

  return SILValue();
}

/// Try to find a sequence of instructions which initializes
/// a given property \p Property with a constant value.
/// If all initializers of a type enclosing this property
/// initialize it with the same constant value, then this
/// functions returns true and copies this sequence of
/// instructions into \p Insns.
/// Otherwise, false is returned, indicating a failure.
bool
LetPropertiesOpt::getInitializer(NominalTypeDecl *NTD, VarDecl *Property,
                                 SmallVectorImpl<SILInstruction *> &Insns) {
  // Iterate over all initializers of this struct and check
  // if a given property is initialized in the same way.

  SmallVector<SmallVector<SILInstruction *, 8>, 4> ConstrPropertyInit;

  SILFunction *Init = nullptr;

  SILDeclRef::Kind InitializerKind = isa<StructDecl>(NTD) ?
                                       SILDeclRef::Kind::Allocator:
                                       SILDeclRef::Kind::Initializer;

  for (auto *Member: NTD->getMembers()) {
    if (auto *FD = dyn_cast<ConstructorDecl>(Member)) {
        // Find the SIL body of this initializer.
        auto SDR = SILDeclRef(FD, InitializerKind);
        Init = Module->lookUpFunction(SDR);
        if (!Init)
          continue;
        // Analyze the body of the constructor.
        SmallVector<SILInstruction *, 8> Insns;
        if (!findStoredValue(Init, Property, Insns))
          return false;
        // Remember the set of instructions initializing
        // this Property inside this constructor.
        DEBUG(llvm::dbgs() << "Found initializer insns: \n";
              for (auto I: Insns) {
                I->dump();
              });
        ConstrPropertyInit.push_back(Insns);
    }
  }

  // Check that all collected instruction sequences are equivalent.
  for(int i = 1, e = ConstrPropertyInit.size(); i < e; i++) {
    if (!compareInsnSequences(ConstrPropertyInit[i-1], ConstrPropertyInit[i])) {
      DEBUG(llvm::dbgs() << "Not all initializers are the same for "
                         << Property->getNameStr() << "\n");
      return false;
    }
  }

  if (ConstrPropertyInit.empty())
    return false;

  DEBUG(llvm::dbgs() << "All initializers are the same for "
                     << Property->getNameStr() << "\n");
  Insns = ConstrPropertyInit[0];

  return true;
}

/// Check if a given property is a non-static let property
/// with known constant value.
bool LetPropertiesOpt::isConstantLetProperty(VarDecl *Property) {
  // Process only non-static let properties here.
  if (!Property->isLet() || Property->isStatic())
    return false;

  // Do not re-process already known properties.
  if (InitMap.count(Property))
    return true;

  if (SkipProcessing.count(Property))
    return false;

  // Only properties of simple types can be optimized.
  if(!isSimpleType(Module->Types.getLoweredType(Property->getType()), *Module)) {
    SkipProcessing.insert(Property);
    return false;
  }

  auto Ty = dyn_cast<NominalTypeDecl>(Property->getDeclContext());

  bool Init = false;

  if (Ty) {
    if (StructDecl *SD = dyn_cast<StructDecl>(Ty)) {
      Init = getInitializer(SD, Property, InitMap[Property]);
    }

    if (auto *CD = dyn_cast<ClassDecl>(Ty)) {
      Init = getInitializer(CD, Property, InitMap[Property]);
    }
  }

  if (!Init) {
    // Remember that this property does not have a constant initializer.
    SkipProcessing.insert(Property);
    return false;
  }

  return true;
}

/// Remember where this property is accessed.
void LetPropertiesOpt::collectPropertyAccess(SILInstruction *I,
                                             VarDecl *Property,
                                             bool NonRemovable) {
  AccessMap[Property].push_back(I);
  // If any property is marked as non-removable, their initialization
  // and storage cannot be completely removed. But their constant
  // values can still be propagated into their uses whenever possible.
  if (NonRemovable)
    CannotRemove.insert(Property);
}

bool LetPropertiesOpt::run() {
  // Collect property access information for the whole module.
  for (auto &F : *Module) {
    // Take into account even those functions that should not be
    // optimized, because they may contain access to the let
    // properties.
    bool NonRemovable = !F.shouldOptimize();

    for (auto &BB : F) {
      for (auto &I : BB)
        // Look for any instructions accessing let properties.
        if (auto *REAI = dyn_cast<RefElementAddrInst>(&I)) {
          if (!isConstantLetProperty(REAI->getField()))
            continue;
          collectPropertyAccess(REAI, REAI->getField(), NonRemovable);
        } else if (auto *SEI = dyn_cast<StructExtractInst>(&I)) {
          if (!isConstantLetProperty(SEI->getField()))
            continue;
          collectPropertyAccess(SEI, SEI->getField(), NonRemovable);
        }  else if (auto *SEAI = dyn_cast<StructElementAddrInst>(&I)) {
          if (!isConstantLetProperty(SEAI->getField()))
            continue;
          collectPropertyAccess(SEAI, SEAI->getField(), NonRemovable);
        }
    }
  }

  for (auto &Init: InitMap) {
    optimizeLetPropertyAccess(Init.first, Init.second);
  }

  return HasChanged;
}

namespace {
class LetPropertiesOptPass : public SILModuleTransform
{
  void run() override {
    if (LetPropertiesOpt(getModule()).run()) {
      // Program flow is not changed by this pass.
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }

  StringRef getName() override {
    return "SIL Let Properties Optimization";
  }
};
} // anonymous

SILTransform *swift::createLetPropertiesOpt() {
  return new LetPropertiesOptPass();
}
