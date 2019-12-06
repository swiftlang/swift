//===--- UsePrespecialized.cpp - use pre-specialized functions ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
/// An optimization which marks functions and types as inlinable or usable
/// from inline. This lets such functions be serialized (later in the pipeline),
/// which makes them available for other modules.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "cross-module-serialization-setup"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILCloner.h"
#include "swift/AST/Module.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

/// Scans a whole module and marks functions and types as inlinable or usable
/// from inline.
class CrossModuleSerializationSetup {
  friend class InstructionVisitor;

  // The worklist of function which should be serialized.
  llvm::SmallVector<SILFunction *, 16> workList;
  llvm::SmallPtrSet<SILFunction *, 16> functionsHandled;

  llvm::SmallPtrSet<TypeBase *, 16> typesHandled;

  SILModule &M;
  
  void addToWorklistIfNotHandled(SILFunction *F) {
    if (functionsHandled.count(F) == 0) {
      workList.push_back(F);
      functionsHandled.insert(F);
    }
  }

  bool setUpForSerialization(SILFunction *F);

  void prepareInstructionForSerialization(SILInstruction *inst);

  void handleReferencedFunction(SILFunction *F);

  void handleReferencedMethod(SILDeclRef method);

  void makeTypeUsableFromInline(CanType type);

  void makeSubstUsableFromInline(const SubstitutionMap &substs);

public:
  CrossModuleSerializationSetup(SILModule &M) : M(M) { }

  void scanModule();
};

static bool canUseFromInline(SILFunction *F) {
  if (!F)
    return false;

  switch (F->getLinkage()) {
  case SILLinkage::PublicNonABI:
  case SILLinkage::Shared:
    return F->isSerialized() != IsNotSerialized;
  case SILLinkage::Public:
  case SILLinkage::Hidden:
  case SILLinkage::Private:
  case SILLinkage::PublicExternal:
  case SILLinkage::SharedExternal:
  case SILLinkage::PrivateExternal:
  case SILLinkage::HiddenExternal:
    break;
  }

  return true;
}

/// Visitor for making used types of an intruction inlinable.
///
/// We use the SILCloner for visiting types, though it sucks that we allocate
/// instructions just to delete them immediately. But it's better than to
/// reimplement the logic.
/// TODO: separate the type visiting logic in SILCloner from the instruction
/// creation.
class InstructionVisitor : public SILCloner<InstructionVisitor> {
  friend class SILCloner<InstructionVisitor>;
  friend class SILInstructionVisitor<InstructionVisitor>;

private:
  CrossModuleSerializationSetup &CMS;
  SILInstruction *result = nullptr;

public:
  InstructionVisitor(SILFunction *F, CrossModuleSerializationSetup &CMS) :
    SILCloner(*F), CMS(CMS) {}

  SILType remapType(SILType Ty) {
    CMS.makeTypeUsableFromInline(Ty.getASTType());
    return Ty;
  }

  CanType remapASTType(CanType Ty) {
    CMS.makeTypeUsableFromInline(Ty);
    return Ty;
  }

  SubstitutionMap remapSubstitutionMap(SubstitutionMap Subs) {
    CMS.makeSubstUsableFromInline(Subs);
    return Subs;
  }

  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    result = Cloned;
    SILCloner<InstructionVisitor>::postProcess(Orig, Cloned);
  }

  SILValue getMappedValue(SILValue Value) { return Value; }

  SILBasicBlock *remapBasicBlock(SILBasicBlock *BB) { return BB; }

  static void visitInst(SILInstruction *I, CrossModuleSerializationSetup &CMS) {
    InstructionVisitor visitor(I->getFunction(), CMS);
    visitor.visit(I);

    SILInstruction::destroy(visitor.result);
    CMS.M.deallocateInst(visitor.result);
  }
};

/// Make a nominal type, including it's context, usable from inline.
static void makeDeclUsableFromInline(ValueDecl *decl, SILModule &M) {
  if (decl->getEffectiveAccess() >= AccessLevel::Public)
    return;

  if (!decl->isUsableFromInline()) {
    // Mark the nominal type as "usableFromInline".
    // TODO: find a way to do this without modifying the AST. The AST should be
    // immutable at this point.
    auto &ctx = decl->getASTContext();
    auto *attr = new (ctx) UsableFromInlineAttr(/*implicit=*/true);
    decl->getAttrs().add(attr);
  }
  if (auto *nominalCtx = dyn_cast<NominalTypeDecl>(decl->getDeclContext())) {
    makeDeclUsableFromInline(nominalCtx, M);
  } else if (auto *extCtx = dyn_cast<ExtensionDecl>(decl->getDeclContext())) {
    if (auto *extendedNominal = extCtx->getExtendedNominal()) {
      makeDeclUsableFromInline(extendedNominal, M);
    }
  } else if (decl->getDeclContext()->isLocalContext()) {
    // TODO
  }
}

/// Ensure that the \p type is usable from serialized functions.
void CrossModuleSerializationSetup::makeTypeUsableFromInline(CanType type) {
  if (!typesHandled.insert(type.getPointer()).second)
    return;

  if (NominalTypeDecl *NT = type->getNominalOrBoundGenericNominal()) {
    makeDeclUsableFromInline(NT, M);
  }

  // Also make all sub-types usable from inline.
  type.visit([this](Type rawSubType) {
    CanType subType = rawSubType->getCanonicalType();
    if (typesHandled.insert(subType.getPointer()).second) {
      if (NominalTypeDecl *subNT = subType->getNominalOrBoundGenericNominal()) {
        makeDeclUsableFromInline(subNT, M);
      }
    }
  });
}

/// Ensure that all replacement types of \p substs are usable from serialized
/// functions.
void CrossModuleSerializationSetup::
makeSubstUsableFromInline(const SubstitutionMap &substs) {
  for (Type replType : substs.getReplacementTypes()) {
    makeTypeUsableFromInline(replType->getCanonicalType());
  }
}

/// Decide whether to serialize a function.
static bool shouldSerialize(SILFunction *F) {
  // The basic heursitic: serialize all generic functions, because it makes a
  // huge difference if generic functions can be specialized or not.
  if (!F->getLoweredFunctionType()->isPolymorphic())
    return false;

  // Check if we already handled this function before.
  if (F->isSerialized() == IsSerialized)
    return false;

  if (F->hasSemanticsAttr("optimize.no.crossmodule"))
    return false;

  return true;
}

static void makeFunctionUsableFromInline(SILFunction *F) {
  if (!isAvailableExternally(F->getLinkage()))
    F->setLinkage(SILLinkage::Public);
}

/// Prepare \p inst for serialization and in case it's a function_ref, put the
/// referenced function onto the worklist.
void CrossModuleSerializationSetup::
prepareInstructionForSerialization(SILInstruction *inst) {
  // Make all types of the instruction usable from inline.
  InstructionVisitor::visitInst(inst, *this);

  // Put callees onto the worklist if they should be serialized as well.
  if (auto *FRI = dyn_cast<FunctionRefBaseInst>(inst)) {
    SILFunction *callee = FRI->getReferencedFunctionOrNull();
    assert(callee);
    handleReferencedFunction(callee);
    return;
  }
  if (auto *MI = dyn_cast<MethodInst>(inst)) {
    handleReferencedMethod(MI->getMember());
    return;
  }
  if (auto *KPI = dyn_cast<KeyPathInst>(inst)) {
    KPI->getPattern()->visitReferencedFunctionsAndMethods(
        [this](SILFunction *func) { handleReferencedFunction(func); },
        [this](SILDeclRef method) { handleReferencedMethod(method); });
    return;
  }
  if (auto *REAI = dyn_cast<RefElementAddrInst>(inst)) {
    makeDeclUsableFromInline(REAI->getField(), M);
  }
}

void CrossModuleSerializationSetup::handleReferencedFunction(SILFunction *func) {
  if (!func->isDefinition() || func->isAvailableExternally())
    return;
  if (shouldSerialize(func)) {
    addToWorklistIfNotHandled(func);
  } else {
    makeFunctionUsableFromInline(func);
  }
}

void CrossModuleSerializationSetup::handleReferencedMethod(SILDeclRef method) {
  if (method.isForeign)
    return;
  // Prevent the method from dead-method elimination.
  auto *methodDecl = cast<AbstractFunctionDecl>(method.getDecl());
  M.addExternallyVisibleDecl(getBaseMethod(methodDecl));
}

/// Setup the function \p param F for serialization and put callees onto the
/// worklist for further processing.
///
/// Returns false in case this is not possible for some reason.
bool CrossModuleSerializationSetup::setUpForSerialization(SILFunction *F) {
  // First step: check if serializing F is even possible.
  for (SILBasicBlock &block : *F) {
    for (SILInstruction &inst : block) {
      if (auto *FRI = dyn_cast<FunctionRefBaseInst>(&inst)) {
        SILFunction *callee = FRI->getReferencedFunctionOrNull();
        if (!canUseFromInline(callee))
          return false;
      } else if (auto *KPI = dyn_cast<KeyPathInst>(&inst)) {
        bool canUse = true;
        KPI->getPattern()->visitReferencedFunctionsAndMethods(
            [&](SILFunction *func) {
              if (!canUseFromInline(func))
                canUse = false;
            },
            [](SILDeclRef method) { });
        if (!canUse)
          return false;
      }
    }
  }

  // Second step: go through all instructions and prepare them for
  // for serialization.
  for (SILBasicBlock &block : *F) {
    for (SILInstruction &inst : block) {
      prepareInstructionForSerialization(&inst);
    }
  }
  return true;
}

/// Select functions in the module which should be serialized.
void CrossModuleSerializationSetup::scanModule() {

  // Start with public functions.
  for (SILFunction &F : M) {
    if (F.getLinkage() == SILLinkage::Public)
      addToWorklistIfNotHandled(&F);
  }

  // Continue with called functions.
  while (!workList.empty()) {
    SILFunction *F = workList.pop_back_val();
    // Decide whether we want to serialize the function.
    if (shouldSerialize(F)) {
      // Try to serialize.
      if (setUpForSerialization(F)) {
        F->setSerialized(IsSerialized);

        // As a code size optimization, make serialized functions
        // @alwaysEmitIntoClient.
        F->setLinkage(SILLinkage::PublicNonABI);
      } else {
        // If for some reason the function cannot be serialized, we mark it as
        // usable-from-inline.
        makeFunctionUsableFromInline(F);
      }
    }
  }
}

class CrossModuleSerializationSetupPass: public SILModuleTransform {
  void run() override {

    auto &M = *getModule();
    if (M.getSwiftModule()->isResilient())
      return;
    if (!M.isWholeModule())
      return;
    if (!M.getOptions().CrossModuleOptimization)
      return;

    CrossModuleSerializationSetup CMSS(M);
    CMSS.scanModule();
  }
};

} // end anonymous namespace

SILTransform *swift::createCrossModuleSerializationSetup() {
  return new CrossModuleSerializationSetupPass();
}
