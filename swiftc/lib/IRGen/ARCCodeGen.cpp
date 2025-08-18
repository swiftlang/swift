//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/IRGen/ARCCodeGen.h"
#include "swiftc/AST/Expr.h"
#include "swiftc/AST/Decl.h"
#include "swiftc/AST/Type.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/Support/Casting.h"
#include <algorithm>
#include <iostream>

namespace swiftc {
namespace irgen {

//===----------------------------------------------------------------------===//
// MARK: - ARCCodeGen Implementation
//===----------------------------------------------------------------------===//

ARCCodeGen::ARCCodeGen(llvm::IRBuilder<>& builder, llvm::Module& module)
  : Builder(builder), Module(module), Context(module.getContext()) {
  initializeRuntimeFunctions();
}

void ARCCodeGen::initializeRuntimeFunctions() {
  // Get pointer type for objects
  llvm::Type* objectPtrType = getObjectPtrType();
  llvm::Type* voidType = llvm::Type::getVoidTy(Context);
  llvm::Type* boolType = llvm::Type::getInt1Ty(Context);
  llvm::Type* sizeType = llvm::Type::getInt64Ty(Context);
  
  // swift_retain(object) -> void
  llvm::FunctionType* retainType = createRuntimeFunctionType(voidType, {objectPtrType});
  swiftRetainFunc = getOrCreateRuntimeFunction("swift_retain", retainType);
  
  // swift_release(object) -> void
  llvm::FunctionType* releaseType = createRuntimeFunctionType(voidType, {objectPtrType});
  swiftReleaseFunc = getOrCreateRuntimeFunction("swift_release", releaseType);
  
  // swift_weakRetain(object) -> void
  llvm::FunctionType* weakRetainType = createRuntimeFunctionType(voidType, {objectPtrType});
  swiftWeakRetainFunc = getOrCreateRuntimeFunction("swift_weakRetain", weakRetainType);
  
  // swift_weakRelease(object) -> void
  llvm::FunctionType* weakReleaseType = createRuntimeFunctionType(voidType, {objectPtrType});
  swiftWeakReleaseFunc = getOrCreateRuntimeFunction("swift_weakRelease", weakReleaseType);
  
  // swift_tryRetain(object) -> bool
  llvm::FunctionType* tryRetainType = createRuntimeFunctionType(boolType, {objectPtrType});
  swiftTryRetainFunc = getOrCreateRuntimeFunction("swift_tryRetain", tryRetainType);
  
  // swift_allocObject(size, alignment) -> object*
  llvm::FunctionType* allocType = createRuntimeFunctionType(objectPtrType, {sizeType, sizeType});
  swiftAllocObjectFunc = getOrCreateRuntimeFunction("swift_allocObject", allocType);
  
  // swift_deallocObject(object, size) -> void
  llvm::FunctionType* deallocType = createRuntimeFunctionType(voidType, {objectPtrType, sizeType});
  swiftDeallocObjectFunc = getOrCreateRuntimeFunction("swift_deallocObject", deallocType);
  
  // swift_isBeingDeallocated(object) -> bool
  llvm::FunctionType* isBeingDeallocatedType = createRuntimeFunctionType(boolType, {objectPtrType});
  swiftIsBeingDeallocatedFunc = getOrCreateRuntimeFunction("swift_isBeingDeallocated", isBeingDeallocatedType);
}

llvm::Value* ARCCodeGen::emitRetain(llvm::Value* object) {
  if (!object || !needsARCManagement(nullptr)) {
    return object;
  }
  
  // Cast to object pointer type if needed
  llvm::Value* objectPtr = Builder.CreateBitCast(object, getObjectPtrType());
  
  // Generate retain call
  llvm::CallInst* retainCall = Builder.CreateCall(swiftRetainFunc, {objectPtr});
  
  // Track for optimization
  if (arcState.optimizationEnabled) {
    trackRetain(object, retainCall);
  }
  
  return object;
}

llvm::Value* ARCCodeGen::emitRelease(llvm::Value* object) {
  if (!object || !needsARCManagement(nullptr)) {
    return object;
  }
  
  // Cast to object pointer type if needed
  llvm::Value* objectPtr = Builder.CreateBitCast(object, getObjectPtrType());
  
  // Generate release call
  llvm::CallInst* releaseCall = Builder.CreateCall(swiftReleaseFunc, {objectPtr});
  
  // Track for optimization
  if (arcState.optimizationEnabled) {
    trackRelease(object, releaseCall);
  }
  
  return object;
}

llvm::Value* ARCCodeGen::emitWeakRetain(llvm::Value* object) {
  if (!object) return object;
  
  llvm::Value* objectPtr = Builder.CreateBitCast(object, getObjectPtrType());
  Builder.CreateCall(swiftWeakRetainFunc, {objectPtr});
  
  return object;
}

llvm::Value* ARCCodeGen::emitWeakRelease(llvm::Value* object) {
  if (!object) return object;
  
  llvm::Value* objectPtr = Builder.CreateBitCast(object, getObjectPtrType());
  Builder.CreateCall(swiftWeakReleaseFunc, {objectPtr});
  
  return object;
}

llvm::Value* ARCCodeGen::emitTryRetain(llvm::Value* object) {
  if (!object) {
    return llvm::ConstantInt::getFalse(Context);
  }
  
  llvm::Value* objectPtr = Builder.CreateBitCast(object, getObjectPtrType());
  return Builder.CreateCall(swiftTryRetainFunc, {objectPtr});
}

llvm::Value* ARCCodeGen::emitAllocObject(llvm::Type* objectType) {
  // Calculate object size and alignment
  llvm::DataLayout dataLayout = Module.getDataLayout();
  uint64_t size = dataLayout.getTypeAllocSize(objectType);
  uint64_t alignment = dataLayout.getABITypeAlignment(objectType);
  
  llvm::Value* sizeValue = llvm::ConstantInt::get(llvm::Type::getInt64Ty(Context), size);
  llvm::Value* alignmentValue = llvm::ConstantInt::get(llvm::Type::getInt64Ty(Context), alignment);
  
  // Call allocation function
  llvm::Value* object = Builder.CreateCall(swiftAllocObjectFunc, {sizeValue, alignmentValue});
  
  // Cast to appropriate type
  return Builder.CreateBitCast(object, llvm::PointerType::getUnqual(objectType));
}

void ARCCodeGen::emitDeallocObject(llvm::Value* object, llvm::Type* objectType) {
  if (!object) return;
  
  // Calculate object size
  llvm::DataLayout dataLayout = Module.getDataLayout();
  uint64_t size = dataLayout.getTypeAllocSize(objectType);
  llvm::Value* sizeValue = llvm::ConstantInt::get(llvm::Type::getInt64Ty(Context), size);
  
  llvm::Value* objectPtr = Builder.CreateBitCast(object, getObjectPtrType());
  Builder.CreateCall(swiftDeallocObjectFunc, {objectPtr, sizeValue});
}

llvm::Value* ARCCodeGen::emitIsBeingDeallocated(llvm::Value* object) {
  if (!object) {
    return llvm::ConstantInt::getTrue(Context);
  }
  
  llvm::Value* objectPtr = Builder.CreateBitCast(object, getObjectPtrType());
  return Builder.CreateCall(swiftIsBeingDeallocatedFunc, {objectPtr});
}

void ARCCodeGen::emitAssignment(llvm::Value* destination, llvm::Value* source, Type* type) {
  if (!needsARCManagement(type)) {
    return;
  }
  
  // Retain the new value
  if (source) {
    emitRetain(source);
  }
  
  // Load the old value (if any) and release it
  if (destination) {
    llvm::Value* oldValue = Builder.CreateLoad(destination->getType()->getPointerElementType(), destination);
    emitRelease(oldValue);
  }
  
  // Store the new value
  if (destination && source) {
    Builder.CreateStore(source, destination);
  }
}

void ARCCodeGen::emitParameterPassing(llvm::Value* argument, Type* type, bool isConsuming) {
  if (!needsARCManagement(type) || !argument) {
    return;
  }
  
  if (isConsuming) {
    // Transfer ownership - no additional retain needed
    return;
  } else {
    // Borrow parameter - retain for the duration of the call
    emitRetain(argument);
  }
}

llvm::Value* ARCCodeGen::emitReturn(llvm::Value* value, Type* type) {
  if (!needsARCManagement(type) || !value) {
    return value;
  }
  
  // Return transfers ownership - retain the value
  emitRetain(value);
  return value;
}

void ARCCodeGen::emitVariableInit(VarDecl* varDecl, llvm::Value* value) {
  if (!varDecl || !value) return;
  
  // For variable initialization, we typically transfer ownership
  // The value should already be retained by the expression that produced it
  // So no additional ARC operations needed here in most cases
}

void ARCCodeGen::emitVariableDestroy(VarDecl* varDecl, llvm::Value* value) {
  if (!varDecl || !value) return;
  
  // Release the variable's value when it goes out of scope
  emitRelease(value);
}

llvm::Value* ARCCodeGen::emitExpressionWithARC(Expr* expr) {
  if (!expr) return nullptr;
  
  // This would integrate with the main expression generation
  // For now, return a placeholder
  return nullptr;
}

llvm::Value* ARCCodeGen::emitNullCheckedRetain(llvm::Value* object) {
  if (!object) return object;
  
  // Create null check
  llvm::Value* isNull = Builder.CreateICmpEQ(object, 
    llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(object->getType())));
  
  return emitConditionalARC(Builder.CreateNot(isNull), object, 
    [this](llvm::Value* obj) { return emitRetain(obj); });
}

llvm::Value* ARCCodeGen::emitNullCheckedRelease(llvm::Value* object) {
  if (!object) return object;
  
  // Create null check
  llvm::Value* isNull = Builder.CreateICmpEQ(object,
    llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(object->getType())));
  
  return emitConditionalARC(Builder.CreateNot(isNull), object,
    [this](llvm::Value* obj) { return emitRelease(obj); });
}

void ARCCodeGen::optimizeARC(llvm::Function* function) {
  if (!arcState.optimizationEnabled) return;
  
  // Run optimization passes
  ARCOptimizer::runOptimizationPasses(function, *this);
}

//===----------------------------------------------------------------------===//
// MARK: - Helper Methods
//===----------------------------------------------------------------------===//

llvm::Function* ARCCodeGen::getOrCreateRuntimeFunction(const std::string& name, 
                                                      llvm::FunctionType* type) {
  llvm::Function* func = Module.getFunction(name);
  if (!func) {
    func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, name, Module);
  }
  return func;
}

llvm::FunctionType* ARCCodeGen::createRuntimeFunctionType(llvm::Type* returnType,
                                                         llvm::ArrayRef<llvm::Type*> paramTypes) {
  return llvm::FunctionType::get(returnType, paramTypes, false);
}

bool ARCCodeGen::needsARCManagement(Type* type) {
  // Simplified check - in a real implementation, this would check if the type
  // is a reference type (class, closure, etc.)
  return true; // For now, assume all types need ARC
}

llvm::Type* ARCCodeGen::getObjectPtrType() {
  return llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(Context));
}

llvm::Value* ARCCodeGen::emitConditionalARC(llvm::Value* condition, 
                                           llvm::Value* object,
                                           std::function<llvm::Value*(llvm::Value*)> operation) {
  llvm::Function* currentFunction = Builder.GetInsertBlock()->getParent();
  
  // Create basic blocks
  llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(Context, "arc_then", currentFunction);
  llvm::BasicBlock* endBlock = llvm::BasicBlock::Create(Context, "arc_end", currentFunction);
  
  // Branch based on condition
  Builder.CreateCondBr(condition, thenBlock, endBlock);
  
  // Generate ARC operation in then block
  Builder.SetInsertPoint(thenBlock);
  llvm::Value* result = operation(object);
  Builder.CreateBr(endBlock);
  
  // Continue in end block
  Builder.SetInsertPoint(endBlock);
  
  return result;
}

void ARCCodeGen::trackRetain(llvm::Value* object, llvm::Instruction* retainInst) {
  arcState.retainCounts[object]++;
  arcState.retainInstructions.push_back(retainInst);
}

void ARCCodeGen::trackRelease(llvm::Value* object, llvm::Instruction* releaseInst) {
  arcState.retainCounts[object]--;
  arcState.releaseInstructions.push_back(releaseInst);
}

//===----------------------------------------------------------------------===//
// MARK: - ARCOptimizer Implementation
//===----------------------------------------------------------------------===//

void ARCOptimizer::runOptimizationPasses(llvm::Function* function, ARCCodeGen& arcGen) {
  bool changed = true;
  int iterations = 0;
  const int maxIterations = 10;
  
  while (changed && iterations < maxIterations) {
    changed = false;
    
    // Run optimization passes
    changed |= eliminateRedundantPairs(function);
    changed |= optimizeARCPlacement(function);
    changed |= combineARCOperations(function);
    changed |= removeLocalObjectARC(function);
    
    iterations++;
  }
  
  std::cout << "[ARC] Optimization completed in " << iterations << " iterations" << std::endl;
}

bool ARCOptimizer::eliminateRedundantPairs(llvm::Function* function) {
  bool changed = false;
  
  // Find retain/release pairs that can be eliminated
  std::vector<std::pair<llvm::Instruction*, llvm::Instruction*>> pairs = 
    findRetainReleasePairs(function);
  
  for (auto& pair : pairs) {
    if (canEliminateRetainRelease(pair.first, pair.second)) {
      // Remove both instructions
      pair.first->eraseFromParent();
      pair.second->eraseFromParent();
      changed = true;
    }
  }
  
  return changed;
}

bool ARCOptimizer::optimizeARCPlacement(llvm::Function* function) {
  // Move ARC operations to optimal positions
  // This is a simplified implementation
  return false;
}

bool ARCOptimizer::combineARCOperations(llvm::Function* function) {
  // Combine multiple consecutive ARC operations
  // This is a simplified implementation
  return false;
}

bool ARCOptimizer::removeLocalObjectARC(llvm::Function* function) {
  // Remove ARC operations for objects that don't escape the function
  // This is a simplified implementation
  return false;
}

std::vector<std::pair<llvm::Instruction*, llvm::Instruction*>> 
ARCOptimizer::findRetainReleasePairs(llvm::Function* function) {
  std::vector<std::pair<llvm::Instruction*, llvm::Instruction*>> pairs;
  
  // Simplified implementation - find retain/release pairs
  std::vector<llvm::Instruction*> retains;
  std::vector<llvm::Instruction*> releases;
  
  for (auto& block : *function) {
    for (auto& inst : block) {
      if (auto* call = llvm::dyn_cast<llvm::CallInst>(&inst)) {
        if (call->getCalledFunction() && 
            call->getCalledFunction()->getName() == "swift_retain") {
          retains.push_back(call);
        } else if (call->getCalledFunction() && 
                   call->getCalledFunction()->getName() == "swift_release") {
          releases.push_back(call);
        }
      }
    }
  }
  
  // Match retains with releases (simplified)
  for (auto* retain : retains) {
    for (auto* release : releases) {
      if (retain->getOperand(0) == release->getOperand(0)) {
        pairs.emplace_back(retain, release);
        break;
      }
    }
  }
  
  return pairs;
}

bool ARCOptimizer::canEliminateRetainRelease(llvm::Instruction* retain, 
                                           llvm::Instruction* release) {
  // Check if the retain/release pair can be safely eliminated
  // This is a simplified check
  return retain->getParent() == release->getParent() && 
         retain->comesBefore(release);
}

//===----------------------------------------------------------------------===//
// MARK: - WeakRefCodeGen Implementation
//===----------------------------------------------------------------------===//

llvm::Value* WeakRefCodeGen::emitWeakRefCreate(llvm::Value* object) {
  if (!object) return nullptr;
  
  // Allocate weak reference structure
  llvm::Type* weakRefType = llvm::StructType::get(Module.getContext(), {
    llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(Module.getContext()))
  });
  
  llvm::Value* weakRef = Builder.CreateAlloca(weakRefType);
  
  // Store object pointer and retain weakly
  llvm::Value* objectPtr = Builder.CreateBitCast(object, 
    llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(Module.getContext())));
  llvm::Value* objectField = Builder.CreateStructGEP(weakRefType, weakRef, 0);
  Builder.CreateStore(objectPtr, objectField);
  
  arcGen.emitWeakRetain(object);
  
  return weakRef;
}

llvm::Value* WeakRefCodeGen::emitWeakRefLoad(llvm::Value* weakRef) {
  if (!weakRef) return nullptr;
  
  // Load object pointer from weak reference
  llvm::Type* weakRefType = weakRef->getType()->getPointerElementType();
  llvm::Value* objectField = Builder.CreateStructGEP(weakRefType, weakRef, 0);
  llvm::Value* objectPtr = Builder.CreateLoad(
    llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(Module.getContext())), objectField);
  
  // Try to retain the object
  llvm::Value* retained = arcGen.emitTryRetain(objectPtr);
  
  // Return object if successfully retained, null otherwise
  return Builder.CreateSelect(retained, objectPtr, 
    llvm::ConstantPointerNull::get(
      llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(Module.getContext()))));
}

void WeakRefCodeGen::emitWeakRefAssign(llvm::Value* weakRef, llvm::Value* object) {
  if (!weakRef) return;
  
  // Load old object and release it weakly
  llvm::Type* weakRefType = weakRef->getType()->getPointerElementType();
  llvm::Value* objectField = Builder.CreateStructGEP(weakRefType, weakRef, 0);
  llvm::Value* oldObject = Builder.CreateLoad(
    llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(Module.getContext())), objectField);
  
  if (oldObject) {
    arcGen.emitWeakRelease(oldObject);
  }
  
  // Store new object and retain it weakly
  if (object) {
    llvm::Value* objectPtr = Builder.CreateBitCast(object,
      llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(Module.getContext())));
    Builder.CreateStore(objectPtr, objectField);
    arcGen.emitWeakRetain(object);
  } else {
    Builder.CreateStore(
      llvm::ConstantPointerNull::get(
        llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(Module.getContext()))), 
      objectField);
  }
}

void WeakRefCodeGen::emitWeakRefDestroy(llvm::Value* weakRef) {
  if (!weakRef) return;
  
  // Load object and release it weakly
  llvm::Type* weakRefType = weakRef->getType()->getPointerElementType();
  llvm::Value* objectField = Builder.CreateStructGEP(weakRefType, weakRef, 0);
  llvm::Value* object = Builder.CreateLoad(
    llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(Module.getContext())), objectField);
  
  if (object) {
    arcGen.emitWeakRelease(object);
  }
}

llvm::Value* WeakRefCodeGen::emitUnownedRefAccess(llvm::Value* unownedRef) {
  if (!unownedRef) return nullptr;
  
  // Load object from unowned reference
  llvm::Value* object = Builder.CreateLoad(unownedRef->getType()->getPointerElementType(), unownedRef);
  
  // Check if object is being deallocated
  llvm::Value* isBeingDeallocated = arcGen.emitIsBeingDeallocated(object);
  
  // Create error block for deallocated access
  llvm::Function* currentFunction = Builder.GetInsertBlock()->getParent();
  llvm::BasicBlock* errorBlock = llvm::BasicBlock::Create(Module.getContext(), "unowned_error", currentFunction);
  llvm::BasicBlock* continueBlock = llvm::BasicBlock::Create(Module.getContext(), "unowned_continue", currentFunction);
  
  Builder.CreateCondBr(isBeingDeallocated, errorBlock, continueBlock);
  
  // Error block - would call runtime error function
  Builder.SetInsertPoint(errorBlock);
  // In a real implementation, this would call a runtime error function
  Builder.CreateUnreachable();
  
  // Continue block
  Builder.SetInsertPoint(continueBlock);
  return object;
}

llvm::Value* WeakRefCodeGen::emitUnownedRefValidation(llvm::Value* unownedRef) {
  if (!unownedRef) return llvm::ConstantInt::getFalse(Module.getContext());
  
  llvm::Value* object = Builder.CreateLoad(unownedRef->getType()->getPointerElementType(), unownedRef);
  llvm::Value* isBeingDeallocated = arcGen.emitIsBeingDeallocated(object);
  
  return Builder.CreateNot(isBeingDeallocated);
}

} // namespace irgen
} // namespace swiftc