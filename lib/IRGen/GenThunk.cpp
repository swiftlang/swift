//===--- GenThunk.cpp - IR Generation for Method Dispatch Thunks ----------===//
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
//
//  This file implements IR generation for class and protocol method dispatch
//  thunks, which are used in resilient builds to hide vtable and witness table
//  offsets from clients.
//
//===----------------------------------------------------------------------===//

#include "Callee.h"
#include "ClassMetadataVisitor.h"
#include "ConstantBuilder.h"
#include "Explosion.h"
#include "GenClass.h"
#include "GenDecl.h"
#include "GenHeap.h"
#include "GenOpaque.h"
#include "GenPointerAuth.h"
#include "GenProto.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "MetadataLayout.h"
#include "ProtocolInfo.h"
#include "Signature.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/SILDeclRef.h"
#include "llvm/IR/Function.h"

using namespace swift;
using namespace irgen;

/// Find the entry point for a method dispatch thunk.
llvm::Function *
IRGenModule::getAddrOfDispatchThunk(SILDeclRef declRef,
                                    ForDefinition_t forDefinition) {
  LinkEntity entity = LinkEntity::forDispatchThunk(declRef);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(*this, entry, entity);
    return entry;
  }

  auto fnType = getSILModule().Types.getConstantFunctionType(
      getMaximalTypeExpansionContext(), declRef);
  Signature signature = getSignature(fnType);
  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);

  entry = createFunction(*this, link, signature);
  return entry;
}

static FunctionPointer lookupMethod(IRGenFunction &IGF, SILDeclRef declRef) {
  auto expansionContext = IGF.IGM.getMaximalTypeExpansionContext();
  auto *decl = cast<AbstractFunctionDecl>(declRef.getDecl());

  // Protocol case.
  if (isa<ProtocolDecl>(decl->getDeclContext())) {
    // Find the witness table.
    llvm::Value *wtable = (IGF.CurFn->arg_end() - 1);

    // Find the witness we're interested in.
    return emitWitnessMethodValue(IGF, wtable, declRef);
  }

  // Class case.
  auto funcTy = IGF.IGM.getSILModule().Types.getConstantFunctionType(
      expansionContext, declRef);

  // Load the metadata, or use the 'self' value if we have a static method.
  llvm::Value *self;

  // Non-throwing class methods always have the 'self' parameter at the end.
  // Throwing class methods have 'self' right before the error parameter.
  //
  // FIXME: Should find a better way of expressing this.
  if (funcTy->hasErrorResult())
    self = (IGF.CurFn->arg_end() - 2);
  else
    self = (IGF.CurFn->arg_end() - 1);

  auto selfTy = funcTy->getSelfParameter().getSILStorageType(
      IGF.IGM.getSILModule(), funcTy, IGF.IGM.getMaximalTypeExpansionContext());

  llvm::Value *metadata;
  if (selfTy.is<MetatypeType>()) {
    metadata = self;
  } else {
    metadata = emitHeapMetadataRefForHeapObject(IGF, self, selfTy,
                                                /*suppress cast*/ true);
  }

  return emitVirtualMethodValue(IGF, metadata, declRef, funcTy);
}

void IRGenModule::emitDispatchThunk(SILDeclRef declRef) {
  auto *f = getAddrOfDispatchThunk(declRef, ForDefinition);
  if (!f->isDeclaration()) {
    return;
  }

  IRGenFunction IGF(*this, f);

  // Look up the method.
  auto fn = lookupMethod(IGF, declRef);

  // Call the witness, forwarding all of the parameters.
  auto params = IGF.collectParameters();
  auto result = IGF.Builder.CreateCall(fn, params.claimAll());

  // Return the result, if we have one.
  if (result->getType()->isVoidTy())
    IGF.Builder.CreateRetVoid();
  else
    IGF.Builder.CreateRet(result);
}

llvm::Constant *
IRGenModule::getAddrOfAsyncFunctionPointer(SILFunction *function) {
  (void)getAddrOfSILFunction(function, NotForDefinition);
  auto entity = LinkEntity::forAsyncFunctionPointer(function);
  return getAddrOfLLVMVariable(entity, NotForDefinition, DebugTypeInfo());
}

llvm::Constant *IRGenModule::defineAsyncFunctionPointer(SILFunction *function,
                                                        ConstantInit init) {
  auto entity = LinkEntity::forAsyncFunctionPointer(function);
  auto *var = cast<llvm::GlobalVariable>(
      getAddrOfLLVMVariable(entity, init, DebugTypeInfo()));
  setTrueConstGlobal(var);
  return var;
}

SILFunction *
IRGenModule::getSILFunctionForAsyncFunctionPointer(llvm::Constant *afp) {
  for (auto &entry : GlobalVars) {
    if (entry.getSecond() == afp) {
      auto entity = entry.getFirst();
      return entity.getSILFunction();
    }
  }
  return nullptr;
}

llvm::GlobalValue *IRGenModule::defineMethodDescriptor(SILDeclRef declRef,
                                                       NominalTypeDecl *nominalDecl,
                                                       llvm::Constant *definition) {
  auto entity = LinkEntity::forMethodDescriptor(declRef);
  return defineAlias(entity, definition);
}

/// Get or create a method descriptor variable.
llvm::Constant *
IRGenModule::getAddrOfMethodDescriptor(SILDeclRef declRef,
                                       ForDefinition_t forDefinition) {
  assert(forDefinition == NotForDefinition);
  assert(declRef.getOverriddenWitnessTableEntry() == declRef &&
         "Overriding protocol requirements do not have method descriptors");
  LinkEntity entity = LinkEntity::forMethodDescriptor(declRef);
  return getAddrOfLLVMVariable(entity, forDefinition, DebugTypeInfo());
}

/// Fetch the method lookup function for a resilient class.
llvm::Function *
IRGenModule::getAddrOfMethodLookupFunction(ClassDecl *classDecl,
                                           ForDefinition_t forDefinition) {
  IRGen.noteUseOfTypeMetadata(classDecl);

  LinkEntity entity = LinkEntity::forMethodLookupFunction(classDecl);
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(*this, entry, entity);
    return entry;
  }

  llvm::Type *params[] = {
    TypeMetadataPtrTy,
    MethodDescriptorStructTy->getPointerTo()
  };
  auto fnType = llvm::FunctionType::get(Int8PtrTy, params, false);
  Signature signature(fnType, llvm::AttributeList(), SwiftCC);
  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);
  entry = createFunction(*this, link, signature);
  return entry;
}

void IRGenModule::emitMethodLookupFunction(ClassDecl *classDecl) {
  auto *f = getAddrOfMethodLookupFunction(classDecl, ForDefinition);
  if (!f->isDeclaration()) {
    assert(IRGen.isLazilyReemittingNominalTypeDescriptor(classDecl));
    return;
  }

  IRGenFunction IGF(*this, f);

  auto params = IGF.collectParameters();
  auto *metadata = params.claimNext();
  auto *method = params.claimNext();

  auto *description = getAddrOfTypeContextDescriptor(classDecl,
                                                     RequireMetadata);

  // Check for lookups of nonoverridden methods first.
  class LookUpNonoverriddenMethods
    : public ClassMetadataScanner<LookUpNonoverriddenMethods> {
  
    IRGenFunction &IGF;
    llvm::Value *methodArg;
      
  public:
    LookUpNonoverriddenMethods(IRGenFunction &IGF,
                               ClassDecl *classDecl,
                               llvm::Value *methodArg)
      : ClassMetadataScanner(IGF.IGM, classDecl), IGF(IGF),
        methodArg(methodArg) {}
      
    void noteNonoverriddenMethod(SILDeclRef method) {
      // The method lookup function would be used only for `super.` calls
      // from other modules, so we only need to look at public-visibility
      // methods here.
      if (!hasPublicVisibility(method.getLinkage(NotForDefinition))) {
        return;
      }
      
      auto methodDesc = IGM.getAddrOfMethodDescriptor(method, NotForDefinition);
      
      auto isMethod = IGF.Builder.CreateICmpEQ(methodArg, methodDesc);
      
      auto falseBB = IGF.createBasicBlock("");
      auto trueBB = IGF.createBasicBlock("");

      IGF.Builder.CreateCondBr(isMethod, trueBB, falseBB);
      
      IGF.Builder.emitBlock(trueBB);
      // Since this method is nonoverridden, we can produce a static result.
      auto entry = VTable->getEntry(IGM.getSILModule(), method);
      llvm::Value *impl = IGM.getAddrOfSILFunction(entry->getImplementation(),
                                                   NotForDefinition);
      // Sign using the discriminator we would include in the method
      // descriptor.
      if (auto &schema = IGM.getOptions().PointerAuth.SwiftClassMethods) {
        auto discriminator =
          PointerAuthInfo::getOtherDiscriminator(IGM, schema, method);
        
        impl = emitPointerAuthSign(IGF, impl,
                            PointerAuthInfo(schema.getKey(), discriminator));
      }
      impl = IGF.Builder.CreateBitCast(impl, IGM.Int8PtrTy);
      IGF.Builder.CreateRet(impl);
      
      IGF.Builder.emitBlock(falseBB);
      // Continue emission on the false branch.
    }
      
    void noteResilientSuperclass() {}
    void noteStartOfImmediateMembers(ClassDecl *clas) {}
  };
  
  LookUpNonoverriddenMethods(IGF, classDecl, method).layout();
  
  // Use the runtime to look up vtable entries.
  auto *result = IGF.Builder.CreateCall(getLookUpClassMethodFn(),
                                        {metadata, method, description});
  IGF.Builder.CreateRet(result);
}
