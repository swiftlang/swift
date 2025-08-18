#ifndef SWIFTC_IRGEN_IRGEN_H
#define SWIFTC_IRGEN_IRGEN_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/SIL/SILFunction.h"
#include "swiftc/SIL/SILInstruction.h"
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <memory>
#include <unordered_map>

namespace swiftc {

/// LLVM IR generation from SIL.
class IRGen {
  DiagnosticEngine& Diags;
  llvm::LLVMContext& Context;
  std::unique_ptr<llvm::Module> Module;
  llvm::IRBuilder<> Builder;
  
  // Mapping from SIL values to LLVM values
  std::unordered_map<SILInstruction*, llvm::Value*> ValueMap;

public:
  IRGen(DiagnosticEngine& diags, llvm::LLVMContext& context, StringRef moduleName);

  /// Generate LLVM IR for a SIL module.
  std::unique_ptr<llvm::Module> generateIR(SILModule& silModule);

  /// Get the generated LLVM module.
  llvm::Module* getLLVMModule() const { return Module.get(); }

private:
  /// Generate LLVM IR for a SIL function.
  void generateFunction(SILFunction& silFunc);

  /// Generate LLVM IR for a SIL basic block.
  void generateBasicBlock(SILBasicBlock& silBlock, llvm::Function* llvmFunc);

  /// Generate LLVM IR for a SIL instruction.
  llvm::Value* generateInstruction(SILInstruction& inst);

  /// Get LLVM type for a SIL type.
  llvm::Type* getLLVMType(SILType* silType);

  /// Get LLVM value for a SIL value.
  llvm::Value* getLLVMValue(SILValue silValue);

  /// Map a SIL instruction to an LLVM value.
  void mapValue(SILInstruction* inst, llvm::Value* value);
};

} // namespace swiftc

#endif // SWIFTC_IRGEN_IRGEN_H