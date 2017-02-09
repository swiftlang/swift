//===--- LetPropertiesOpts.cpp - Optimize let properties ------------------===//
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
// Promote values of non-static let properties initialized by means
// of constant values of simple types into their uses.
//
// For any given non-static let property this optimization is only possible
// if this pass can prove that it has analyzed all assignments of an initial
// value to this property and all those assignments assign the same value
// to this property.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "let-properties-opt"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILLinkage.h"
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
  typedef SmallVector<VarDecl *, 4> Properties;

  // Map each let property to a set of instructions accessing it.
  llvm::MapVector<VarDecl *, Instructions> AccessMap;
  // Map each let property to the instruction sequence which initializes it.
  llvm::MapVector<VarDecl *, Instructions> InitMap;
  // Properties in this set should not be processed by this pass
  // anymore.
  llvm::SmallPtrSet<VarDecl *, 16> SkipProcessing;
  // Types in this set should not be processed by this pass
  // anymore.
  llvm::SmallPtrSet<NominalTypeDecl *, 16> SkipTypeProcessing;
  // Properties in this set cannot be removed.
  llvm::SmallPtrSet<VarDecl *, 16> CannotRemove;
  // Set of let properties in a given nominal type.
  llvm::MapVector<NominalTypeDecl *, Properties> NominalTypeLetProperties;
  // Set of properties which already fulfill all conditions, except
  // the available of constant, statically known initializer.
  llvm::SmallPtrSet<VarDecl *, 16> PotentialConstantLetProperty;

public:
  LetPropertiesOpt(SILModule *M): Module(M) {}

  bool run();

protected:
  bool isConstantLetProperty(VarDecl *Property);
  void collectPropertyAccess(SILInstruction *I, VarDecl *Property, bool NonRemovable);
  void collectStructPropertiesAccess(StructInst *SI, bool NonRemovable);
  void optimizeLetPropertyAccess(VarDecl *SILG,
                                 SmallVectorImpl<SILInstruction *> &Init);
  bool analyzeInitValue(SILInstruction *I, VarDecl *Prop);
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
    AvailVals.push_back(std::make_pair(Orig, Cloned));
  }

  // Clone all instructions from Insns into DestBB
  void clone() {
    for (auto I : Insns)
      process(I);
  }
};

} // end anonymous namespace

#ifndef NDEBUG
// For debugging only.
static raw_ostream &operator<<(raw_ostream &OS, const VarDecl &decl) {
  auto *Ty = dyn_cast<NominalTypeDecl>(decl.getDeclContext());
  if (Ty)
    OS << Ty->getName() << "::";
  OS << decl.getName();
  return OS;
}
#endif

/// Optimize access to the let property, which is known
/// to have a constant value. Replace all loads from the
/// property by its constant value.
void LetPropertiesOpt::optimizeLetPropertyAccess(VarDecl *Property,
    SmallVectorImpl<SILInstruction *> &Init) {

  if (SkipProcessing.count(Property))
    return;

  if (Init.empty())
    return;

  auto *Ty = dyn_cast<NominalTypeDecl>(Property->getDeclContext());
  if (SkipTypeProcessing.count(Ty))
    return;

  DEBUG(llvm::dbgs() << "Replacing access to property '" << *Property
                     << "' by its constant initializer\n");

  auto PropertyAccess = Property->getEffectiveAccess();
  auto TypeAccess = Ty->getEffectiveAccess();
  auto CanRemove = false;

  // Check if a given let property can be removed, because it
  // is not accessible elsewhere. This can happen if this property
  // is private or if it is internal and WMO mode is used.
  if (TypeAccess <= Accessibility::FilePrivate ||
      PropertyAccess <= Accessibility::FilePrivate
      || ((TypeAccess <= Accessibility::Internal ||
          PropertyAccess <= Accessibility::Internal) &&
          Module->isWholeModule())) {
    CanRemove = true;
    DEBUG(llvm::dbgs() << "Storage for property '" << *Property
                       << "' can be eliminated\n");
  }

  if (CannotRemove.count(Property))
    CanRemove = false;

  if (!AccessMap.count(Property)) {
    DEBUG(llvm::dbgs() << "Property '" << *Property << "' is never read\n");
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
      DEBUG(llvm::dbgs() << "Access to " << *Property << " was replaced:\n";
            I->dumpInContext());

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

  DEBUG(llvm::dbgs() << "Access to " << *Property << " was replaced "
                     << NumReplaced << " time(s)\n");

  if (CanRemove) {
    // TODO: Remove the let property, because it is never accessed.
  }
}

/// Compare to SILValues structurally.
static bool CmpSILValues(SILValue LHS, SILValue RHS) {
  if (LHS->getType() != RHS->getType())
    return false;

  auto L = dyn_cast<SILInstruction>(LHS);
  return L->isIdenticalTo(dyn_cast<SILInstruction>(RHS), CmpSILValues);
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

/// Check if a given let property can be assigned externally.
static bool isAssignableExternally(VarDecl *Property, SILModule *Module) {
  Accessibility accessibility = Property->getEffectiveAccess();
  SILLinkage linkage;
  switch (accessibility) {
  case Accessibility::Private:
  case Accessibility::FilePrivate:
    linkage = SILLinkage::Private;
    DEBUG(llvm::dbgs() << "Property " << *Property << " has private access\n");
    break;
  case Accessibility::Internal:
    linkage = SILLinkage::Hidden;
    DEBUG(llvm::dbgs() << "Property " << *Property << " has internal access\n");
    break;
  case Accessibility::Public:
  case Accessibility::Open:
    linkage = SILLinkage::Public;
    DEBUG(llvm::dbgs() << "Property " << *Property << " has public access\n");
    break;
  }

  DEBUG(llvm::dbgs() << "Module of " << *Property << " WMO mode is: " << Module->isWholeModule() << "\n");

  if (isPossiblyUsedExternally(linkage, Module->isWholeModule())) {
    // If at least one of the properties of the enclosing type cannot be
    // used externally, then no initializer can be implemented externally as
    // it wouldn't be able to initialize such a property.
    // More over, for classes, only the class itself can initialize its
    // let properties. Subclasses and extensions cannot do it.
    // For structs, external extensions may initialize let properties. But to do
    // that they need to be able to initialize all properties, i.e. all
    // properties should be accessible by the extension.

    auto *Ty = dyn_cast<NominalTypeDecl>(Property->getDeclContext());

    // Initializer for a let property of a class cannot exist externally.
    // It cannot be defined by an extension or a derived class.
    if (isa<ClassDecl>(Ty))
      return false;

    // Check if there are any private properties or any internal properties and
    // it is a whole module compilation. In this case, no external initializer
    // may exist.
    for (auto SP : Ty->getStoredProperties()) {
      auto storedPropertyAccessibility = SP->getEffectiveAccess();
      if (storedPropertyAccessibility <= Accessibility::FilePrivate ||
          (storedPropertyAccessibility <= Accessibility::Internal &&
           Module->isWholeModule())) {
       DEBUG(llvm::dbgs() << "Property " << *Property
                       << " cannot be set externally\n");
       return false;
      }
    }

    DEBUG(llvm::dbgs() << "Property " << *Property
                       << " can be used externally\n");
    return true;
  }

  return false;
}

// Checks if a given property may have any unknown uses which cannot
// be analyzed by this pass.
static bool mayHaveUnknownUses(VarDecl *Property, SILModule *Module) {
  if (Property->getDeclContext()->getParentModule() !=
      Module->getSwiftModule()) {
    DEBUG(llvm::dbgs() << "Property " << *Property
                       << " is defined in a different module\n");
    // We don't see the bodies of initializers from a different module
    // unless all of them are fragile.
    // TODO: Support fragile initializers.
    return true;
  }

  // If let properties can be assigned externally, we don't know
  // the values they may get.
  if (isAssignableExternally(Property, Module)) {
    return true;
  }

  return false;
}


/// Check if a given property is a non-static let property
/// with known constant value.
bool LetPropertiesOpt::isConstantLetProperty(VarDecl *Property) {
  // Process only non-static let properties here.
  if (!Property->isLet() || Property->isStatic())
    return false;

  // Do not re-process already known properties.
  if (SkipProcessing.count(Property))
    return false;

  // If these checks were performed already, no need to
  // repeat them.
  if (PotentialConstantLetProperty.count(Property))
    return true;

  // Check the visibility of this property. If its visibility
  // implies that this optimization pass cannot analyze all uses,
  // don't process it.
  if (mayHaveUnknownUses(Property, Module)) {
    DEBUG(llvm::dbgs() << "Property '" << *Property
                       << "' may have unknown uses\n");
    SkipProcessing.insert(Property);
    return false;
  }

  DEBUG(llvm::dbgs() << "Property '" << *Property
                      << "' has no unknown uses\n");

  // Only properties of simple types can be optimized.
  if (!isSimpleType(Module->Types.getLoweredType(Property->getType()), *Module)) {
     DEBUG(llvm::dbgs() << "Property '" << *Property
                       << "' is not of trivial type\n");
    SkipProcessing.insert(Property);
    return false;
  }

  PotentialConstantLetProperty.insert(Property);

  return true;
}

// Analyze the init value being stored by the instruction into a property.
bool
LetPropertiesOpt::analyzeInitValue(SILInstruction *I, VarDecl *Property) {
  SmallVector<SILInstruction *, 8> Insns;
  SmallVector<SILInstruction *, 8> ReverseInsns;
  SILValue ValueOfProperty;
  if (auto SI = dyn_cast<StructInst>(I)) {
    ValueOfProperty = SI->getFieldValue(Property);
  } else if (auto SI = dyn_cast<StoreInst>(I)) {
    auto Dest = SI->getDest();

    assert(((isa<RefElementAddrInst>(Dest) &&
             cast<RefElementAddrInst>(Dest)->getField() == Property) ||
            (isa<StructElementAddrInst>(Dest) &&
             cast<StructElementAddrInst>(Dest)->getField() == Property)) &&
           "Store instruction should store into a proper let property");
    (void) Dest;
    ValueOfProperty = SI->getSrc();
  }

  // Bail if a value of a property is not a statically known constant init.
  if (!analyzeStaticInitializer(ValueOfProperty, ReverseInsns))
    return false;

  // Produce a correct order of instructions.
  while (!ReverseInsns.empty()) {
    Insns.push_back(ReverseInsns.pop_back_val());
  }

  auto &InitInsns = InitMap[Property];
  if (!InitInsns.empty() && !compareInsnSequences(InitInsns, Insns)) {
    // The found init value is different from the already seen init value.
    return false;
  } else {
    DEBUG(llvm::dbgs() << "The value of property '" << *Property
                       << "' is statically known so far\n");
    // Remember the statically known value.
    InitInsns = Insns;
    return true;
  }
}

// Analyze the 'struct' instruction and check if it initializes
// any let properties by statically known constant initializers.
void LetPropertiesOpt::collectStructPropertiesAccess(StructInst *SI,
                                                     bool NonRemovable) {
  auto structDecl = SI->getStructDecl();
  // Check if this struct has any let properties.

  // Bail, if this struct is known to contain nothing interesting.
  if (SkipTypeProcessing.count(structDecl))
    return;

  // Get the set of let properties defined by this struct.
  if (!NominalTypeLetProperties.count(structDecl)) {
    // Compute the let properties of this struct.
    SmallVector<VarDecl *, 4> LetProps;

    for (auto Prop : structDecl->getStoredProperties()) {
      if (!isConstantLetProperty(Prop))
        continue;
      LetProps.push_back(Prop);
    }

    if (LetProps.empty()) {
      // No interesting let properties in this struct.
      SkipTypeProcessing.insert(structDecl);
      return;
    }

    NominalTypeLetProperties[structDecl] = LetProps;
    DEBUG(llvm::dbgs() << "Computed set of let properties for struct '"
                       << structDecl->getName() << "'\n");
  }

  auto &Props = NominalTypeLetProperties[structDecl];

  DEBUG(llvm::dbgs()
            << "Found a struct instruction initializing some let properties: ";
        SI->dumpInContext());
  // Figure out the initializing sequence for each
  // of the properties.
  for (auto Prop : Props) {
    if (SkipProcessing.count(Prop))
      continue;
    SILValue PropValue = SI->getOperandForField(Prop)->get();
    DEBUG(llvm::dbgs() << "Check the value of property '" << *Prop
                       << "' :" << PropValue << "\n");
    if (!analyzeInitValue(SI, Prop)) {
      SkipProcessing.insert(Prop);
      DEBUG(llvm::dbgs() << "The value of a let property '" << *Prop
                         << "' is not statically known\n");
    }
    (void) PropValue;
  }
}

/// Check if I is a sequence of projections followed by a load.
/// Since it is supposed to be a load from a let property with
/// statically known constant initializer, only struct_element_addr
/// and tuple_element_addr projections are considered.
static bool isValidPropertyLoad(SILInstruction *I) {
  if (isa<LoadInst>(I))
    return true;

  if (isa<StructElementAddrInst>(I) || isa<TupleElementAddrInst>(I)) {
    for (auto Use : getNonDebugUses(I))
      if (!isValidPropertyLoad(Use->getUser()))
        return false;
    return true;
  }

  return false;
}


/// Remember where this property is accessed.
void LetPropertiesOpt::collectPropertyAccess(SILInstruction *I,
                                             VarDecl *Property,
                                             bool NonRemovable) {
  if (!isConstantLetProperty(Property))
    return;

  DEBUG(llvm::dbgs() << "Collecting property access for property '" << *Property
                     << "':\n";
        llvm::dbgs() << "The instructions are:\n"; I->dumpInContext());

  if (isa<RefElementAddrInst>(I) || isa<StructElementAddrInst>(I)) {
    // Check if there is a store to this property.
    for (auto Use : getNonDebugUses(I)) {
      auto *User = Use->getUser();

      if (auto *SI = dyn_cast<StoreInst>(User)) {
        // There is a store into this property.
        // Analyze the assigned value and check if it is a constant
        // statically known initializer.
        if (SI->getDest() != I || !analyzeInitValue(SI, Property)) {
          SkipProcessing.insert(Property);
          return;
        }
        continue;
      }

      // Follow the chain of projections and check if it ends up with a load.
      // If this is not the case, it is potentially a store into sub-property
      // of a property.
      // We cannot handle such cases yet, so bail.
      if (!isValidPropertyLoad(User)) {
        SkipProcessing.insert(Property);
        return;
      }
    }
  }

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
        // It includes referencing this specific property (both reads and
        // stores), as well as implicit stores by means of e.g.
        // a struct instruction.
        if (auto *REAI = dyn_cast<RefElementAddrInst>(&I)) {
          collectPropertyAccess(REAI, REAI->getField(), NonRemovable);
        } else if (auto *SEI = dyn_cast<StructExtractInst>(&I)) {
          collectPropertyAccess(SEI, SEI->getField(), NonRemovable);
        }  else if (auto *SEAI = dyn_cast<StructElementAddrInst>(&I)) {
          collectPropertyAccess(SEAI, SEAI->getField(), NonRemovable);
        } else if (auto *SI = dyn_cast<StructInst>(&I)) {
          collectStructPropertiesAccess(SI, NonRemovable);
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
} // end anonymous namespace

SILTransform *swift::createLetPropertiesOpt() {
  return new LetPropertiesOptPass();
}
