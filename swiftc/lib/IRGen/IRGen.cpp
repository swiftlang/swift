#include "swiftc/IRGen/IRGen.h"
#include "swiftc/SIL/SILType.h"
#include "swiftc/AST/Type.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Type.h>

using namespace swiftc;

IRGen::IRGen(DiagnosticEngine& diags, llvm::LLVMContext& context, StringRef moduleName)
    : Diags(diags), Context(context), 
      Module(std::make_unique<llvm::Module>(moduleName, context)),
      Builder(context) {}

std::unique_ptr<llvm::Module> IRGen::generateIR(SILModule& silModule) {
  // Generate IR for all functions
  for (const auto& silFunc : silModule.getFunctions()) {
    generateFunction(*silFunc);
  }
  
  return std::move(Module);
}

void IRGen::generateFunction(SILFunction& silFunc) {
  // Create LLVM function type
  std::vector<llvm::Type*> paramTypes;
  for (const auto& paramType : silFunc.getParameterTypes()) {
    paramTypes.push_back(getLLVMType(paramType.get()));
  }
  
  llvm::Type* returnType = silFunc.getReturnType() ? 
                          getLLVMType(silFunc.getReturnType()) :
                          llvm::Type::getVoidTy(Context);
  
  llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
  
  // Create LLVM function
  llvm::Function* llvmFunc = llvm::Function::Create(
      funcType, llvm::Function::ExternalLinkage, silFunc.getName(), Module.get());
  
  // Generate basic blocks
  for (const auto& silBlock : silFunc.getBasicBlocks()) {
    generateBasicBlock(*silBlock, llvmFunc);
  }
}

void IRGen::generateBasicBlock(SILBasicBlock& silBlock, llvm::Function* llvmFunc) {
  // Create LLVM basic block
  llvm::BasicBlock* llvmBlock = llvm::BasicBlock::Create(
      Context, silBlock.getLabel(), llvmFunc);
  
  // Set insertion point
  Builder.SetInsertPoint(llvmBlock);
  
  // Generate instructions
  for (const auto& silInst : silBlock.getInstructions()) {
    llvm::Value* value = generateInstruction(*silInst);
    if (value) {
      mapValue(silInst.get(), value);
    }
  }
}

llvm::Value* IRGen::generateInstruction(SILInstruction& inst) {
  switch (inst.getKind()) {
  case SILInstructionKind::IntegerLiteral: {
    auto intLit = static_cast<IntegerLiteralInst*>(&inst);
    return llvm::ConstantInt::get(llvm::Type::getInt64Ty(Context), 
                                  intLit->getValue(), true);
  }
  
  case SILInstructionKind::Load: {
    auto loadInst = static_cast<LoadInst*>(&inst);
    llvm::Value* address = getLLVMValue(loadInst->getAddress());
    if (address) {
      // For LLVM 20+, we need to specify the type explicitly
      llvm::Type* elementType = llvm::Type::getInt64Ty(Context); // Simplified for now
      return Builder.CreateLoad(elementType, address);
    }
    return nullptr;
  }
  
  case SILInstructionKind::Store: {
    auto storeInst = static_cast<StoreInst*>(&inst);
    llvm::Value* value = getLLVMValue(storeInst->getValue());
    llvm::Value* address = getLLVMValue(storeInst->getAddress());
    if (value && address) {
      Builder.CreateStore(value, address);
    }
    return nullptr;
  }
  
  case SILInstructionKind::Return: {
    auto returnInst = static_cast<ReturnInst*>(&inst);
    llvm::Value* returnValue = getLLVMValue(returnInst->getReturnValue());
    if (returnValue) {
      return Builder.CreateRet(returnValue);
    } else {
      return Builder.CreateRetVoid();
    }
  }
  
  case SILInstructionKind::AllocStack: {
    auto allocInst = static_cast<AllocStackInst*>(&inst);
    llvm::Type* allocType = getLLVMType(allocInst->getAllocatedType());
    return Builder.CreateAlloca(allocType);
  }
  
  default:
    // Unhandled instruction
    return nullptr;
  }
}

llvm::Type* IRGen::getLLVMType(SILType* silType) {
  if (!silType)
    return llvm::Type::getVoidTy(Context);
  
  // For now, just return some basic types
  // In a real implementation, we'd need to properly map SIL types to LLVM types
  return llvm::Type::getInt64Ty(Context);
}

llvm::Value* IRGen::getLLVMValue(SILValue silValue) {
  if (!silValue.isValid())
    return nullptr;
  
  auto it = ValueMap.find(silValue.getDefiningInstruction());
  return it != ValueMap.end() ? it->second : nullptr;
}

void IRGen::mapValue(SILInstruction* inst, llvm::Value* value) {
  ValueMap[inst] = value;
}