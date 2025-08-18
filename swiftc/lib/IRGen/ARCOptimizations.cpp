//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/IRGen/ARCCodeGen.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Support/Casting.h"
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <algorithm>

namespace swiftc {
namespace irgen {

//===----------------------------------------------------------------------===//
// MARK: - ARC Optimization Analysis
//===----------------------------------------------------------------------===//

/// Analyzes ARC operations for optimization opportunities
class ARCAnalysis {
public:
  struct ObjectLifetime {
    llvm::Instruction* firstRetain = nullptr;
    llvm::Instruction* lastRelease = nullptr;
    std::vector<llvm::Instruction*> retains;
    std::vector<llvm::Instruction*> releases;
    bool escapesToOtherFunctions = false;
    bool hasWeakReferences = false;
    int netRetainCount = 0;
  };
  
  struct FunctionAnalysis {
    std::unordered_map<llvm::Value*, ObjectLifetime> objectLifetimes;
    std::vector<std::pair<llvm::Instruction*, llvm::Instruction*>> redundantPairs;
    std::vector<llvm::Instruction*> eliminableRetains;
    std::vector<llvm::Instruction*> eliminableReleases;
    int totalARCOperations = 0;
    int optimizableOperations = 0;
  };
  
  static FunctionAnalysis analyzeFunctionARC(llvm::Function* function);
  
private:
  static bool isARCOperation(llvm::Instruction* inst);
  static llvm::Value* getARCOperationObject(llvm::Instruction* inst);
  static bool isRetainOperation(llvm::Instruction* inst);
  static bool isReleaseOperation(llvm::Instruction* inst);
  static bool objectEscapesFunction(llvm::Value* object, llvm::Function* function);
};

ARCAnalysis::FunctionAnalysis ARCAnalysis::analyzeFunctionARC(llvm::Function* function) {
  FunctionAnalysis analysis;
  
  // First pass: identify all ARC operations
  for (auto& block : *function) {
    for (auto& inst : block) {
      if (isARCOperation(&inst)) {
        analysis.totalARCOperations++;
        
        llvm::Value* object = getARCOperationObject(&inst);
        if (!object) continue;
        
        ObjectLifetime& lifetime = analysis.objectLifetimes[object];
        
        if (isRetainOperation(&inst)) {
          lifetime.retains.push_back(&inst);
          if (!lifetime.firstRetain) {
            lifetime.firstRetain = &inst;
          }
          lifetime.netRetainCount++;
        } else if (isReleaseOperation(&inst)) {
          lifetime.releases.push_back(&inst);
          lifetime.lastRelease = &inst;
          lifetime.netRetainCount--;
        }
      }
    }
  }
  
  // Second pass: analyze optimization opportunities
  for (auto& [object, lifetime] : analysis.objectLifetimes) {
    // Check if object escapes the function
    lifetime.escapesToOtherFunctions = objectEscapesFunction(object, function);
    
    // Find redundant retain/release pairs
    for (auto* retain : lifetime.retains) {
      for (auto* release : lifetime.releases) {
        // Check if retain and release are in the same basic block
        if (retain->getParent() == release->getParent() && 
            retain->comesBefore(release)) {
          
          // Check if there are no other uses between retain and release
          bool hasIntermediateUse = false;
          auto* current = retain->getNextNode();
          while (current && current != release) {
            if (current->getNumUses() > 0) {
              for (auto& use : current->uses()) {
                if (use.get() == object) {
                  hasIntermediateUse = true;
                  break;
                }
              }
            }
            if (hasIntermediateUse) break;
            current = current->getNextNode();
          }
          
          if (!hasIntermediateUse) {
            analysis.redundantPairs.emplace_back(retain, release);
            analysis.optimizableOperations += 2;
          }
        }
      }
    }
    
    // Identify eliminable operations for local objects
    if (!lifetime.escapesToOtherFunctions && lifetime.netRetainCount == 0) {
      analysis.eliminableRetains.insert(analysis.eliminableRetains.end(),
                                       lifetime.retains.begin(), lifetime.retains.end());
      analysis.eliminableReleases.insert(analysis.eliminableReleases.end(),
                                        lifetime.releases.begin(), lifetime.releases.end());
      analysis.optimizableOperations += lifetime.retains.size() + lifetime.releases.size();
    }
  }
  
  return analysis;
}

bool ARCAnalysis::isARCOperation(llvm::Instruction* inst) {
  if (auto* call = llvm::dyn_cast<llvm::CallInst>(inst)) {
    if (auto* func = call->getCalledFunction()) {
      llvm::StringRef name = func->getName();
      return name == "swift_retain" || name == "swift_release" ||
             name == "swift_weakRetain" || name == "swift_weakRelease" ||
             name == "swift_tryRetain";
    }
  }
  return false;
}

llvm::Value* ARCAnalysis::getARCOperationObject(llvm::Instruction* inst) {
  if (auto* call = llvm::dyn_cast<llvm::CallInst>(inst)) {
    if (call->getNumArgOperands() > 0) {
      return call->getArgOperand(0);
    }
  }
  return nullptr;
}

bool ARCAnalysis::isRetainOperation(llvm::Instruction* inst) {
  if (auto* call = llvm::dyn_cast<llvm::CallInst>(inst)) {
    if (auto* func = call->getCalledFunction()) {
      llvm::StringRef name = func->getName();
      return name == "swift_retain" || name == "swift_weakRetain" || name == "swift_tryRetain";
    }
  }
  return false;
}

bool ARCAnalysis::isReleaseOperation(llvm::Instruction* inst) {
  if (auto* call = llvm::dyn_cast<llvm::CallInst>(inst)) {
    if (auto* func = call->getCalledFunction()) {
      llvm::StringRef name = func->getName();
      return name == "swift_release" || name == "swift_weakRelease";
    }
  }
  return false;
}

bool ARCAnalysis::objectEscapesFunction(llvm::Value* object, llvm::Function* function) {
  // Check if the object is passed to other functions or stored in global state
  for (auto& use : object->uses()) {
    llvm::Instruction* useInst = llvm::dyn_cast<llvm::Instruction>(use.getUser());
    if (!useInst) continue;
    
    // Check if used in a call to another function
    if (auto* call = llvm::dyn_cast<llvm::CallInst>(useInst)) {
      if (auto* calledFunc = call->getCalledFunction()) {
        if (calledFunc != function && !calledFunc->getName().startswith("swift_")) {
          return true;
        }
      }
    }
    
    // Check if stored to memory that outlives the function
    if (llvm::isa<llvm::StoreInst>(useInst)) {
      return true; // Simplified - assume all stores escape
    }
    
    // Check if returned from function
    if (llvm::isa<llvm::ReturnInst>(useInst)) {
      return true;
    }
  }
  
  return false;
}

//===----------------------------------------------------------------------===//
// MARK: - ARC Optimization Passes
//===----------------------------------------------------------------------===//

/// Comprehensive ARC optimization pass
class ARCOptimizationPass {
public:
  static bool runOptimizations(llvm::Function* function) {
    bool changed = false;
    
    // Analyze the function
    auto analysis = ARCAnalysis::analyzeFunctionARC(function);
    
    std::cout << "[ARC] Function: " << function->getName().str() 
              << " - " << analysis.totalARCOperations << " ARC operations, "
              << analysis.optimizableOperations << " optimizable" << std::endl;
    
    // Apply optimizations
    changed |= eliminateRedundantPairs(analysis.redundantPairs);
    changed |= eliminateLocalObjectARC(analysis.eliminableRetains, analysis.eliminableReleases);
    changed |= combineConsecutiveOperations(function);
    changed |= moveARCOperationsToOptimalPositions(function);
    
    if (changed) {
      std::cout << "[ARC] Optimizations applied to " << function->getName().str() << std::endl;
    }
    
    return changed;
  }
  
private:
  static bool eliminateRedundantPairs(
    const std::vector<std::pair<llvm::Instruction*, llvm::Instruction*>>& pairs) {
    
    bool changed = false;
    
    for (auto& [retain, release] : pairs) {
      std::cout << "[ARC] Eliminating redundant retain/release pair" << std::endl;
      
      // Remove both instructions
      retain->eraseFromParent();
      release->eraseFromParent();
      changed = true;
    }
    
    return changed;
  }
  
  static bool eliminateLocalObjectARC(
    const std::vector<llvm::Instruction*>& retains,
    const std::vector<llvm::Instruction*>& releases) {
    
    bool changed = false;
    
    // Remove ARC operations for objects that don't escape
    for (auto* retain : retains) {
      std::cout << "[ARC] Eliminating local object retain" << std::endl;
      retain->eraseFromParent();
      changed = true;
    }
    
    for (auto* release : releases) {
      std::cout << "[ARC] Eliminating local object release" << std::endl;
      release->eraseFromParent();
      changed = true;
    }
    
    return changed;
  }
  
  static bool combineConsecutiveOperations(llvm::Function* function) {
    bool changed = false;
    
    // Look for consecutive retain/release operations on the same object
    for (auto& block : *function) {
      llvm::Instruction* prevInst = nullptr;
      
      for (auto it = block.begin(); it != block.end(); ) {
        llvm::Instruction* inst = &*it;
        ++it; // Increment before potential deletion
        
        if (ARCAnalysis::isARCOperation(inst) && 
            ARCAnalysis::isARCOperation(prevInst)) {
          
          llvm::Value* obj1 = ARCAnalysis::getARCOperationObject(inst);
          llvm::Value* obj2 = ARCAnalysis::getARCOperationObject(prevInst);
          
          if (obj1 == obj2) {
            // Check for retain followed by release (or vice versa)
            bool isRetainRelease = ARCAnalysis::isRetainOperation(prevInst) && 
                                  ARCAnalysis::isReleaseOperation(inst);
            bool isReleaseRetain = ARCAnalysis::isReleaseOperation(prevInst) && 
                                  ARCAnalysis::isRetainOperation(inst);
            
            if (isRetainRelease || isReleaseRetain) {
              std::cout << "[ARC] Combining consecutive operations" << std::endl;
              
              // Remove both operations
              prevInst->eraseFromParent();
              inst->eraseFromParent();
              changed = true;
              prevInst = nullptr;
              continue;
            }
          }
        }
        
        prevInst = inst;
      }
    }
    
    return changed;
  }
  
  static bool moveARCOperationsToOptimalPositions(llvm::Function* function) {
    // Move retain operations as late as possible
    // Move release operations as early as possible
    // This is a simplified implementation
    
    bool changed = false;
    
    for (auto& block : *function) {
      std::vector<llvm::Instruction*> retains;
      std::vector<llvm::Instruction*> releases;
      
      // Collect ARC operations
      for (auto& inst : block) {
        if (ARCAnalysis::isRetainOperation(&inst)) {
          retains.push_back(&inst);
        } else if (ARCAnalysis::isReleaseOperation(&inst)) {
          releases.push_back(&inst);
        }
      }
      
      // Move retains later in the block (closer to first use)
      for (auto* retain : retains) {
        llvm::Value* object = ARCAnalysis::getARCOperationObject(retain);
        if (!object) continue;
        
        // Find first use of the object after the retain
        llvm::Instruction* firstUse = nullptr;
        auto* current = retain->getNextNode();
        while (current) {
          for (auto& use : current->uses()) {
            if (use.get() == object) {
              firstUse = current;
              break;
            }
          }
          if (firstUse) break;
          current = current->getNextNode();
        }
        
        // Move retain closer to first use
        if (firstUse && firstUse != retain->getNextNode()) {
          retain->moveBefore(firstUse);
          changed = true;
        }
      }
      
      // Move releases earlier in the block (closer to last use)
      for (auto* release : releases) {
        llvm::Value* object = ARCAnalysis::getARCOperationObject(release);
        if (!object) continue;
        
        // Find last use of the object before the release
        llvm::Instruction* lastUse = nullptr;
        for (auto it = block.rbegin(); it != block.rend(); ++it) {
          llvm::Instruction* inst = &*it;
          if (inst == release) continue;
          
          for (auto& use : inst->uses()) {
            if (use.get() == object) {
              lastUse = inst;
              break;
            }
          }
          if (lastUse) break;
        }
        
        // Move release closer to last use
        if (lastUse && release != lastUse->getNextNode()) {
          release->moveAfter(lastUse);
          changed = true;
        }
      }
    }
    
    return changed;
  }
};

//===----------------------------------------------------------------------===//
// MARK: - Advanced ARC Optimizations
//===----------------------------------------------------------------------===//

/// Advanced ARC optimization techniques
class AdvancedARCOptimizer {
public:
  /// Perform retain/release coalescing across basic blocks
  static bool performGlobalCoalescing(llvm::Function* function) {
    bool changed = false;
    
    // Build dominator tree for analysis
    llvm::DominatorTree domTree(*function);
    
    // Find retain/release operations that can be moved across blocks
    std::unordered_map<llvm::Value*, std::vector<llvm::Instruction*>> objectRetains;
    std::unordered_map<llvm::Value*, std::vector<llvm::Instruction*>> objectReleases;
    
    // Collect all ARC operations
    for (auto& block : *function) {
      for (auto& inst : block) {
        if (ARCAnalysis::isARCOperation(&inst)) {
          llvm::Value* object = ARCAnalysis::getARCOperationObject(&inst);
          if (!object) continue;
          
          if (ARCAnalysis::isRetainOperation(&inst)) {
            objectRetains[object].push_back(&inst);
          } else if (ARCAnalysis::isReleaseOperation(&inst)) {
            objectReleases[object].push_back(&inst);
          }
        }
      }
    }
    
    // Optimize each object's ARC operations
    for (auto& [object, retains] : objectRetains) {
      auto& releases = objectReleases[object];
      
      if (retains.size() > 1 || releases.size() > 1) {
        changed |= optimizeObjectARCOperations(object, retains, releases, domTree);
      }
    }
    
    return changed;
  }
  
  /// Eliminate ARC operations for stack-allocated objects
  static bool eliminateStackObjectARC(llvm::Function* function) {
    bool changed = false;
    
    // Find alloca instructions (stack allocations)
    std::unordered_set<llvm::Value*> stackObjects;
    for (auto& block : *function) {
      for (auto& inst : block) {
        if (llvm::isa<llvm::AllocaInst>(&inst)) {
          stackObjects.insert(&inst);
        }
      }
    }
    
    // Remove ARC operations for stack objects
    for (auto& block : *function) {
      for (auto it = block.begin(); it != block.end(); ) {
        llvm::Instruction* inst = &*it;
        ++it;
        
        if (ARCAnalysis::isARCOperation(inst)) {
          llvm::Value* object = ARCAnalysis::getARCOperationObject(inst);
          if (stackObjects.count(object)) {
            std::cout << "[ARC] Eliminating ARC operation for stack object" << std::endl;
            inst->eraseFromParent();
            changed = true;
          }
        }
      }
    }
    
    return changed;
  }
  
  /// Optimize ARC operations using dataflow analysis
  static bool performDataflowOptimization(llvm::Function* function) {
    // This would implement sophisticated dataflow analysis
    // to track object lifetimes and optimize ARC operations
    
    // Simplified implementation for now
    return false;
  }
  
  /// Inline simple ARC operations
  static bool inlineSimpleARCOperations(llvm::Function* function) {
    bool changed = false;
    
    // Look for simple retain/release patterns that can be inlined
    for (auto& block : *function) {
      for (auto& inst : block) {
        if (auto* call = llvm::dyn_cast<llvm::CallInst>(&inst)) {
          if (auto* func = call->getCalledFunction()) {
            llvm::StringRef name = func->getName();
            
            // Inline simple retain operations
            if (name == "swift_retain" && call->getNumArgOperands() == 1) {
              // In a real implementation, this would inline the retain operation
              // For now, just mark as optimizable
              std::cout << "[ARC] Found inlinable retain operation" << std::endl;
            }
          }
        }
      }
    }
    
    return changed;
  }
  
private:
  static bool optimizeObjectARCOperations(
    llvm::Value* object,
    std::vector<llvm::Instruction*>& retains,
    std::vector<llvm::Instruction*>& releases,
    llvm::DominatorTree& domTree) {
    
    bool changed = false;
    
    // Sort operations by dominance order
    std::sort(retains.begin(), retains.end(), 
      [&domTree](llvm::Instruction* a, llvm::Instruction* b) {
        return domTree.dominates(a, b);
      });
    
    std::sort(releases.begin(), releases.end(),
      [&domTree](llvm::Instruction* a, llvm::Instruction* b) {
        return domTree.dominates(a, b);
      });
    
    // Look for optimization opportunities
    if (retains.size() == releases.size()) {
      // Balanced retains and releases - look for coalescing opportunities
      for (size_t i = 0; i < retains.size() - 1; ++i) {
        if (retains[i]->getParent() == retains[i + 1]->getParent()) {
          // Two retains in the same block - can potentially combine
          std::cout << "[ARC] Found combinable retains" << std::endl;
          // In a real implementation, would combine the operations
        }
      }
    }
    
    return changed;
  }
};

//===----------------------------------------------------------------------===//
// MARK: - ARC Optimization Entry Points
//===----------------------------------------------------------------------===//

/// Main entry point for ARC optimizations
bool ARCOptimizer::eliminateRedundantPairs(llvm::Function* function) {
  auto analysis = ARCAnalysis::analyzeFunctionARC(function);
  return ARCOptimizationPass::runOptimizations(function);
}

bool ARCOptimizer::optimizeARCPlacement(llvm::Function* function) {
  return AdvancedARCOptimizer::performGlobalCoalescing(function);
}

bool ARCOptimizer::combineARCOperations(llvm::Function* function) {
  return AdvancedARCOptimizer::inlineSimpleARCOperations(function);
}

bool ARCOptimizer::removeLocalObjectARC(llvm::Function* function) {
  return AdvancedARCOptimizer::eliminateStackObjectARC(function);
}

ARCOptimizer::LifetimeInfo ARCOptimizer::analyzeObjectLifetime(llvm::Value* object, llvm::Function* function) {
  LifetimeInfo info;
  
  // Find first and last use of the object
  for (auto& block : *function) {
    for (auto& inst : block) {
      for (auto& use : inst.uses()) {
        if (use.get() == object) {
          if (!info.firstUse) {
            info.firstUse = &inst;
          }
          info.lastUse = &inst;
        }
      }
    }
  }
  
  // Check if object escapes
  info.escapesToOtherFunctions = ARCAnalysis::objectEscapesFunction(object, function);
  
  return info;
}

bool ARCOptimizer::canEliminateRetainRelease(llvm::Instruction* retain, llvm::Instruction* release) {
  // Check basic conditions for elimination
  if (!retain || !release) return false;
  
  // Must be in the same function
  if (retain->getFunction() != release->getFunction()) return false;
  
  // Retain must come before release
  if (!retain->comesBefore(release)) return false;
  
  // Check if the object is used between retain and release
  llvm::Value* object = ARCAnalysis::getARCOperationObject(retain);
  if (!object) return false;
  
  // Simple check - if in the same basic block with no intermediate uses
  if (retain->getParent() == release->getParent()) {
    auto* current = retain->getNextNode();
    while (current && current != release) {
      for (auto& use : current->uses()) {
        if (use.get() == object) {
          return false; // Object is used between retain and release
        }
      }
      current = current->getNextNode();
    }
    return true;
  }
  
  return false;
}

std::vector<std::pair<llvm::Instruction*, llvm::Instruction*>> 
ARCOptimizer::findRetainReleasePairs(llvm::Function* function) {
  auto analysis = ARCAnalysis::analyzeFunctionARC(function);
  return analysis.redundantPairs;
}

bool ARCOptimizer::valueEscapes(llvm::Value* value, llvm::BasicBlock* scope) {
  return ARCAnalysis::objectEscapesFunction(value, scope->getParent());
}

} // namespace irgen
} // namespace swiftc