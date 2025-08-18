//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#ifndef SWIFTC_IRGEN_ARCCODEGEN_H
#define SWIFTC_IRGEN_ARCCODEGEN_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/AST/ASTNode.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include <unordered_map>
#include <vector>

namespace swiftc {

class Expr;
class VarDecl;
class Type;

namespace irgen {

//===----------------------------------------------------------------------===//
// MARK: - ARC Code Generation
//===----------------------------------------------------------------------===//

/// ARC code generation for automatic memory management
class ARCCodeGen {
private:
  llvm::IRBuilder<>& Builder;
  llvm::Module& Module;
  llvm::LLVMContext& Context;
  
  // Runtime function declarations
  llvm::Function* swiftRetainFunc = nullptr;
  llvm::Function* swiftReleaseFunc = nullptr;
  llvm::Function* swiftWeakRetainFunc = nullptr;
  llvm::Function* swiftWeakReleaseFunc = nullptr;
  llvm::Function* swiftTryRetainFunc = nullptr;
  llvm::Function* swiftAllocObjectFunc = nullptr;
  llvm::Function* swiftDeallocObjectFunc = nullptr;
  llvm::Function* swiftIsBeingDeallocatedFunc = nullptr;
  
  // ARC optimization state
  struct ARCState {
    std::unordered_map<llvm::Value*, int> retainCounts;
    std::vector<llvm::Instruction*> retainInstructions;
    std::vector<llvm::Instruction*> releaseInstructions;
    bool optimizationEnabled = true;
  };
  
  ARCState arcState;
  
public:
  ARCCodeGen(llvm::IRBuilder<>& builder, llvm::Module& module);
  
  /// Initialize ARC runtime function declarations
  void initializeRuntimeFunctions();
  
  /// Generate retain call for a value
  llvm::Value* emitRetain(llvm::Value* object);
  
  /// Generate release call for a value
  llvm::Value* emitRelease(llvm::Value* object);
  
  /// Generate weak retain call for a value
  llvm::Value* emitWeakRetain(llvm::Value* object);
  
  /// Generate weak release call for a value
  llvm::Value* emitWeakRelease(llvm::Value* object);
  
  /// Generate try retain call for weak-to-strong conversion
  llvm::Value* emitTryRetain(llvm::Value* object);
  
  /// Generate object allocation
  llvm::Value* emitAllocObject(llvm::Type* objectType);
  
  /// Generate object deallocation
  void emitDeallocObject(llvm::Value* object, llvm::Type* objectType);
  
  /// Check if object is being deallocated
  llvm::Value* emitIsBeingDeallocated(llvm::Value* object);
  
  /// Generate ARC operations for variable assignment
  void emitAssignment(llvm::Value* destination, llvm::Value* source, Type* type);
  
  /// Generate ARC operations for function parameter passing
  void emitParameterPassing(llvm::Value* argument, Type* type, bool isConsuming);
  
  /// Generate ARC operations for function return
  llvm::Value* emitReturn(llvm::Value* value, Type* type);
  
  /// Generate ARC operations for variable initialization
  void emitVariableInit(VarDecl* varDecl, llvm::Value* value);
  
  /// Generate ARC operations for variable destruction
  void emitVariableDestroy(VarDecl* varDecl, llvm::Value* value);
  
  /// Generate ARC operations for expression evaluation
  llvm::Value* emitExpressionWithARC(Expr* expr);
  
  /// Enable/disable ARC optimizations
  void setOptimizationEnabled(bool enabled) { arcState.optimizationEnabled = enabled; }
  
  /// Perform ARC optimization pass
  void optimizeARC(llvm::Function* function);
  
  /// Insert null checks before ARC operations
  llvm::Value* emitNullCheckedRetain(llvm::Value* object);
  llvm::Value* emitNullCheckedRelease(llvm::Value* object);
  
  /// Generate balanced retain/release pairs
  void balanceRetainRelease(llvm::BasicBlock* block);
  
  /// Eliminate redundant ARC operations
  void eliminateRedundantARC(llvm::Function* function);
  
private:
  /// Helper: Get or create runtime function
  llvm::Function* getOrCreateRuntimeFunction(const std::string& name, 
                                           llvm::FunctionType* type);
  
  /// Helper: Create function type for runtime functions
  llvm::FunctionType* createRuntimeFunctionType(llvm::Type* returnType,
                                               llvm::ArrayRef<llvm::Type*> paramTypes);
  
  /// Helper: Check if type needs ARC management
  bool needsARCManagement(Type* type);
  
  /// Helper: Get LLVM type for object pointers
  llvm::Type* getObjectPtrType();
  
  /// Helper: Generate conditional ARC operation
  llvm::Value* emitConditionalARC(llvm::Value* condition, 
                                 llvm::Value* object,
                                 std::function<llvm::Value*(llvm::Value*)> operation);
  
  /// Helper: Track retain/release for optimization
  void trackRetain(llvm::Value* object, llvm::Instruction* retainInst);
  void trackRelease(llvm::Value* object, llvm::Instruction* releaseInst);
  
  /// Helper: Find matching retain/release pairs
  std::vector<std::pair<llvm::Instruction*, llvm::Instruction*>> 
  findRetainReleasePairs(llvm::Function* function);
  
  /// Helper: Check if value escapes the current scope
  bool valueEscapes(llvm::Value* value, llvm::BasicBlock* scope);
};

//===----------------------------------------------------------------------===//
// MARK: - ARC Optimization Passes
//===----------------------------------------------------------------------===//

/// ARC optimization pass manager
class ARCOptimizer {
public:
  /// Run all ARC optimization passes on a function
  static void runOptimizationPasses(llvm::Function* function, ARCCodeGen& arcGen);
  
  /// Eliminate redundant retain/release pairs
  static bool eliminateRedundantPairs(llvm::Function* function);
  
  /// Move ARC operations to optimal positions
  static bool optimizeARCPlacement(llvm::Function* function);
  
  /// Combine multiple ARC operations
  static bool combineARCOperations(llvm::Function* function);
  
  /// Remove ARC operations for local objects
  static bool removeLocalObjectARC(llvm::Function* function);
  
private:
  /// Helper: Analyze object lifetime
  struct LifetimeInfo {
    llvm::Instruction* firstUse = nullptr;
    llvm::Instruction* lastUse = nullptr;
    bool escapesToOtherFunctions = false;
    bool hasWeakReferences = false;
  };
  
  static LifetimeInfo analyzeObjectLifetime(llvm::Value* object, llvm::Function* function);
  
  /// Helper: Check if retain/release can be eliminated
  static bool canEliminateRetainRelease(llvm::Instruction* retain, 
                                       llvm::Instruction* release);
};

//===----------------------------------------------------------------------===//
// MARK: - Weak Reference Code Generation
//===----------------------------------------------------------------------===//

/// Code generation for weak and unowned references
class WeakRefCodeGen {
private:
  ARCCodeGen& arcGen;
  llvm::IRBuilder<>& Builder;
  llvm::Module& Module;
  
public:
  WeakRefCodeGen(ARCCodeGen& arcCodeGen, llvm::IRBuilder<>& builder, llvm::Module& module)
    : arcGen(arcCodeGen), Builder(builder), Module(module) {}
  
  /// Generate code for weak reference creation
  llvm::Value* emitWeakRefCreate(llvm::Value* object);
  
  /// Generate code for weak reference loading
  llvm::Value* emitWeakRefLoad(llvm::Value* weakRef);
  
  /// Generate code for weak reference assignment
  void emitWeakRefAssign(llvm::Value* weakRef, llvm::Value* object);
  
  /// Generate code for weak reference destruction
  void emitWeakRefDestroy(llvm::Value* weakRef);
  
  /// Generate code for unowned reference access
  llvm::Value* emitUnownedRefAccess(llvm::Value* unownedRef);
  
  /// Generate code for unowned reference validation
  llvm::Value* emitUnownedRefValidation(llvm::Value* unownedRef);
};

//===----------------------------------------------------------------------===//
// MARK: - ARC Debugging Support
//===----------------------------------------------------------------------===//

/// ARC debugging and instrumentation
class ARCDebugger {
private:
  llvm::IRBuilder<>& Builder;
  llvm::Module& Module;
  
  // Debug function declarations
  llvm::Function* debugRetainFunc = nullptr;
  llvm::Function* debugReleaseFunc = nullptr;
  llvm::Function* debugObjectAllocFunc = nullptr;
  llvm::Function* debugObjectDeallocFunc = nullptr;
  
public:
  ARCDebugger(llvm::IRBuilder<>& builder, llvm::Module& module)
    : Builder(builder), Module(module) {}
  
  /// Initialize debug function declarations
  void initializeDebugFunctions();
  
  /// Instrument retain operations for debugging
  void instrumentRetain(llvm::Value* object, const std::string& location);
  
  /// Instrument release operations for debugging
  void instrumentRelease(llvm::Value* object, const std::string& location);
  
  /// Instrument object allocation for debugging
  void instrumentAllocation(llvm::Value* object, llvm::Type* type, const std::string& location);
  
  /// Instrument object deallocation for debugging
  void instrumentDeallocation(llvm::Value* object, const std::string& location);
  
  /// Generate reference count assertions
  void emitReferenceCountAssertion(llvm::Value* object, int expectedCount);
  
  /// Generate cycle detection hooks
  void emitCycleDetectionHook(llvm::Value* object);
};

} // namespace irgen
} // namespace swiftc

#endif // SWIFTC_IRGEN_ARCCODEGEN_H