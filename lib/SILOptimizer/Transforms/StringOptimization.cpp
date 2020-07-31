//===--- StringOptimization.cpp - Optimize string operations --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This pass performs several optimizations on String operations.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "string-optimization"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/AST/SemanticAttrs.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ASTMangler.h"
#include "swift/Demangling/Demangle.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

/// Optimizes String operations with constant operands.

/// Specifically:
///   * Replaces x.append(y) with x = y if x is empty.
///   * Removes x.append("")
///   * Replaces x.append(y) with x = x + y if x and y are constant strings.
///   * Replaces _typeName(T.self) with a constant string if T is statically
///     known.
///
/// This pass must run on high-level SIL, where semantic calls are still in
/// place.
///
/// The optimization is implemented in a simple way. Therfore it cannot handle
/// complicated patterns, e.g. the dataflow analysis for the String.append self
/// argument is only done within a single block.
/// But this is totally sufficient to be able to constant propagate strings in
/// string interpolations.
///
/// If we want to make this optimization more powerful it's best done by using
/// the ConstExprStepEvaluator (which is currently lacking a few features to be
/// used for this optimization).
class StringOptimization {

  struct StringInfo {
    /// The string
    StringRef str;
    
    /// Negative means: not constant
    int numCodeUnits = -1;
    
    /// Not 0 for the empty-string initializer which reserves a capacity.
    int reservedCapacity = 0;
    
    bool isConstant() const { return numCodeUnits >= 0; }
    bool isEmpty() const { return isConstant() && str.empty(); }
  };

  /// The stdlib's String type.
  SILType stringType;
  
  /// The String initializer which takes an UTF8 string literal as argument.
  SILFunction *makeUTF8Func = nullptr;
  
  /// Caches the analysis result for an alloc_stack or an inout function
  /// argument, whether it is an "identifyable" object.
  /// See mayWriteToIdentifyableObject().
  llvm::DenseMap<SILValue, bool> identifyableObjectsCache;

public:
  bool run(SILFunction *F);

private:

  bool optimizeBlock(SILBasicBlock &block);
  
  bool optimizeStringAppend(ApplyInst *appendCall,
                            llvm::DenseMap<SILValue, SILValue> &storedStrings);

  bool optimizeTypeName(ApplyInst *typeNameCall);

  static ApplyInst *isSemanticCall(SILInstruction *inst, StringRef attr,
                                   unsigned numArgs);
  StoreInst *isStringStoreToIdentifyableObject(SILInstruction *inst);
  static void invalidateModifiedObjects(SILInstruction *inst,
                            llvm::DenseMap<SILValue, SILValue> &storedStrings);
  static StringInfo getStringInfo(SILValue value);
  static Optional<int> getIntConstant(SILValue value);
  static void replaceAppendWith(ApplyInst *appendCall, SILValue newValue,
                                bool copyNewValue);
  ApplyInst *createStringInit(StringRef str, SILInstruction *beforeInst);
};

/// The main entry point of the optimization.
bool StringOptimization::run(SILFunction *F) {
  NominalTypeDecl *stringDecl = F->getModule().getASTContext().getStringDecl();
  if (!stringDecl)
    return false;
  stringType = SILType::getPrimitiveObjectType(
                 stringDecl->getDeclaredInterfaceType()->getCanonicalType());

  bool changed = false;
  
  for (SILBasicBlock &block : *F) {
    changed |= optimizeBlock(block);
  }
  return changed;
}

/// Run the optimization on a basic block.
bool StringOptimization::optimizeBlock(SILBasicBlock &block) {
  bool changed = false;
  
  /// Maps identifyable objects (alloc_stack, inout parameters) to string values
  /// which are stored in those objects.
  llvm::DenseMap<SILValue, SILValue> storedStrings;
  
  for (auto iter = block.begin(); iter != block.end();) {
    SILInstruction *inst = &*iter++;

    if (StoreInst *store = isStringStoreToIdentifyableObject(inst)) {
      storedStrings[store->getDest()] = store->getSrc();
      continue;
    }
    if (ApplyInst *append = isSemanticCall(inst, semantics::STRING_APPEND, 2)) {
      if (optimizeStringAppend(append, storedStrings)) {
        changed = true;
        continue;
      }
    }
    if (ApplyInst *typeName = isSemanticCall(inst, semantics::TYPENAME, 2)) {
      if (optimizeTypeName(typeName)) {
        changed = true;
        continue;
      }
    }
    // Remove items from storedStrings if inst overwrites (or potentially
    // overwrites) a stored String in an identifyable object.
    invalidateModifiedObjects(inst, storedStrings);
  }
  return changed;
}

/// Optimize String.append in case anything is known about the parameters.
bool StringOptimization::optimizeStringAppend(ApplyInst *appendCall,
                            llvm::DenseMap<SILValue, SILValue> &storedStrings) {
  SILValue rhs = appendCall->getArgument(0);
  StringInfo rhsString = getStringInfo(rhs);
  
  // Remove lhs.append(rhs) if rhs is empty.
  if (rhsString.isEmpty()) {
    appendCall->eraseFromParent();
    return true;
  }
  
  SILValue lhsAddr = appendCall->getArgument(1);
  StringInfo lhsString = getStringInfo(storedStrings[lhsAddr]);

  // The following two optimizations are a trade-off: Performance-wise it may be
  // benefitial to initialize an empty string with reserved capacity and then
  // append multiple other string components.
  // Removing the empty string (with the reserved capacity) might result in more
  // allocations.
  // So we just do this optimization up to a certain capacity limit (found by
  // experiment).
  if (lhsString.reservedCapacity > 50)
    return false;

  // Replace lhs.append(rhs) with 'lhs = rhs' if lhs is empty.
  if (lhsString.isEmpty()) {
    replaceAppendWith(appendCall, rhs, /*copyNewValue*/ true);
    storedStrings[lhsAddr] = rhs;
    return true;
  }
  
  // Replace lhs.append(rhs) with "lhs = lhs + rhs" if both lhs and rhs are
  // constant.
  if (lhsString.isConstant() && rhsString.isConstant()) {
    std::string concat = lhsString.str;
    concat += rhsString.str;
    if (ApplyInst *stringInit = createStringInit(concat, appendCall)) {
      replaceAppendWith(appendCall, stringInit, /*copyNewValue*/ false);
      storedStrings[lhsAddr] = stringInit;
      return true;
    }
  }
  
  return false;
}

/// Checks if the demangling tree contains any node which prevents constant
/// folding of the type name.
static bool containsProblematicNode(Demangle::Node *node, bool qualified) {
  switch (node->getKind()) {
    case Demangle::Node::Kind::LocalDeclName:
      // The printing of contexts for local types is completely different
      // in the runtime. Don't constant fold if we need to print the context.
      if (qualified)
        return true;
      break;
    case Demangle::Node::Kind::Class:
      // ObjC class names are not derived from the mangling but from the
      // ObjC runtime. We cannot constant fold this.
      if (node->getChild(0)->getText() == "__C")
        return true;
      break;
    default:
      break;
  }
  for (Demangle::Node *child : *node) {
    if (containsProblematicNode(child, qualified))
      return true;
  }
  return false;
}

/// Try to replace a _typeName() call with a constant string if the type is
/// statically known.
bool StringOptimization::optimizeTypeName(ApplyInst *typeNameCall) {
  // Check, if the type is statically known.
  auto *anyType =
    dyn_cast<InitExistentialMetatypeInst>(typeNameCall->getArgument(0));
  if (!anyType)
    return false;
  auto *metatypeInst = dyn_cast<MetatypeInst>(anyType->getOperand());
  if (!metatypeInst)
    return false;

  auto metatype = metatypeInst->getType().getAs<MetatypeType>();
  Type ty = metatype->getInstanceType();
  if (ty->hasArchetype())
    return false;
  
  // Usually the "qualified" parameter of _typeName() is a constant boolean.
  Optional<int> isQualifiedOpt = getIntConstant(typeNameCall->getArgument(1));
  if (!isQualifiedOpt)
    return false;
  bool isQualified = isQualifiedOpt.getValue();

  // Create the constant type string by mangling + demangling.
  Mangle::ASTMangler mangler;
  std::string mangledTypeName = mangler.mangleTypeForTypeName(ty);

  Demangle::DemangleOptions options;
  options.PrintForTypeName = true;
  options.DisplayLocalNameContexts = false;
  options.QualifyEntities = isQualified;

  Demangle::Context ctx;
  Demangle::NodePointer root = ctx.demangleTypeAsNode(mangledTypeName);
  if (!root || containsProblematicNode(root, isQualified))
    return false;

  std::string typeStr = nodeToString(root, options);
  if (typeStr.empty())
    return false;

  ApplyInst *stringInit = createStringInit(typeStr, typeNameCall);
  if (!stringInit)
    return false;

  typeNameCall->replaceAllUsesWith(stringInit);
  typeNameCall->eraseFromParent();

  return true;
}


/// Returns the apply instruction if \p inst is a call of a function which has
/// a semantic attribute \p attr and exactly \p numArgs arguments.
ApplyInst *StringOptimization::isSemanticCall(SILInstruction *inst,
                                              StringRef attr, unsigned numArgs) {
  auto *apply = dyn_cast<ApplyInst>(inst);
  if (!apply || apply->getNumArguments() != numArgs)
    return nullptr;
    
  SILFunction *callee = apply->getReferencedFunctionOrNull();
  if (callee && callee->hasSemanticsAttr(attr))
    return apply;

  return nullptr;
}

/// Returns true for all instructions which we can safely analyze as a potential
/// write to an identifyable objects.
///
/// If we see any other kind of object user, which may write to an object, or
/// let the object address escape in some unexpected way (like address
/// projections), we'll just ignore that object and will not treat it as
/// "identifyable" object.
static bool mayWriteToIdentifyableObject(SILInstruction *inst) {
  // For simplicity, only handle store and apply. This is sufficient for most
  // case, especially for string interpolation.
  return isa<StoreInst>(inst) || isa<ApplyInst>(inst);
}

/// Returns the store intstruction if \p inst is a store of a String to an
/// identifyable object.
StoreInst *StringOptimization::
isStringStoreToIdentifyableObject(SILInstruction *inst) {
  auto *store = dyn_cast<StoreInst>(inst);
  if (!store)
    return nullptr;
  if (store->getSrc()->getType() != stringType)
    return nullptr;

  SILValue destAddr = store->getDest();
  // We only handle alloc_stack an indirect function arguments. For those we can
  // be sure that they are not aliased, just by checking all users.
  if (!isa<AllocStackInst>(destAddr) && !isExclusiveArgument(destAddr))
    return nullptr;

  if (identifyableObjectsCache.count(destAddr) != 0) {
    return identifyableObjectsCache[destAddr] ? store : nullptr;
  }

  // Check if it's an "identifyable" object. This is the case if it only has
  // users which we are able to track in a simple way: stores and applies.
  for (Operand *use : destAddr->getUses()) {
    SILInstruction *user = use->getUser();
    switch (user->getKind()) {
      // Those instructions do not write to destAddr nor let they destAddr
      // escape.
      case SILInstructionKind::DebugValueAddrInst:
      case SILInstructionKind::DeallocStackInst:
      case SILInstructionKind::LoadInst:
        break;
      default:
        if (!mayWriteToIdentifyableObject(user)) {
          // We don't handle user. It is some instruction which may write to
          // destAddr or let destAddr "escape" (like an address projection).
          identifyableObjectsCache[destAddr] = false;
          return nullptr;
        }
        break;
    }
  }
  identifyableObjectsCache[destAddr] = true;
  return store;
}

/// Removes all objects from \p storedStrings which \p inst (potentially)
/// modifies.
void StringOptimization::invalidateModifiedObjects(SILInstruction *inst,
                            llvm::DenseMap<SILValue, SILValue> &storedStrings) {
  // Ignore non-writing instructions, like "load", "dealloc_stack".
  // Note that identifyable objects (= keys in storedStrings) can only have
  // certain kind of instructions as users: all instruction which we handle in
  // isStringStoreToIdentifyableObject().
  if (!mayWriteToIdentifyableObject(inst))
    return;

  for (Operand &op : inst->getAllOperands()) {
    storedStrings.erase(op.get());
  }
}

/// Returns information about value if it's a constant string.
StringOptimization::StringInfo StringOptimization::getStringInfo(SILValue value) {
  // Start with a non-constant result.
  StringInfo result;
  
  auto *apply = dyn_cast_or_null<ApplyInst>(value);
  if (!apply)
    return result;

  SILFunction *callee = apply->getReferencedFunctionOrNull();
  if (!callee)
    return result;
    
  if (callee->hasSemanticsAttr(semantics::STRING_INIT_EMPTY)) {
    // An empty string initializer.
    result.numCodeUnits = 0;
    return result;
  }
    
  if (callee->hasSemanticsAttr(semantics::STRING_INIT_EMPTY_WITH_CAPACITY)) {
    // An empty string initializer with initial capacity.
    result.numCodeUnits = 0;
    result.reservedCapacity = std::numeric_limits<int>::max();
    if (apply->getNumArguments() > 0) {
      if (Optional<int> capacity = getIntConstant(apply->getArgument(0)))
        result.reservedCapacity = capacity.getValue();
    }
    return result;
  }
    
  if (callee->hasSemanticsAttr(semantics::STRING_MAKE_UTF8)) {
    // A string literal initializer.
    SILValue stringVal = apply->getArgument(0);
    auto *stringLiteral = dyn_cast<StringLiteralInst>(stringVal);
    SILValue lengthVal = apply->getArgument(1);
    auto *intLiteral = dyn_cast<IntegerLiteralInst>(lengthVal);
    if (intLiteral && stringLiteral &&
        // For simplicity, we only support UTF8 string literals.
        stringLiteral->getEncoding() == StringLiteralInst::Encoding::UTF8) {
      result.str = stringLiteral->getValue();
      result.numCodeUnits = intLiteral->getValue().getSExtValue();
      return result;
    }
  }
  return result;
}

/// Returns the constant integer value if \a value is an Int or Bool struct with
/// an integer_literal as operand.
Optional<int> StringOptimization::getIntConstant(SILValue value) {
  auto *boolOrIntStruct = dyn_cast<StructInst>(value);
  if (!boolOrIntStruct || boolOrIntStruct->getNumOperands() != 1)
    return None;
    
  auto *literal = dyn_cast<IntegerLiteralInst>(boolOrIntStruct->getOperand(0));
  if (!literal || literal->getValue().getActiveBits() > 64)
    return None;

  return literal->getValue().getSExtValue();
}

/// Replace a String.append() with a store of \p newValue to the destination.
void StringOptimization::replaceAppendWith(ApplyInst *appendCall,
                                      SILValue newValue, bool copyNewValue) {
  SILBuilder builder(appendCall);
  SILLocation loc = appendCall->getLoc();
  SILValue destAddr = appendCall->getArgument(1);
  if (appendCall->getFunction()->hasOwnership()) {
    if (copyNewValue)
      newValue = builder.createCopyValue(loc, newValue);
    builder.createStore(loc, newValue, destAddr,
                        StoreOwnershipQualifier::Assign);
  } else {
    if (copyNewValue)
      builder.createRetainValue(loc, newValue, builder.getDefaultAtomicity());
    builder.createDestroyAddr(loc, destAddr);
    builder.createStore(loc, newValue, destAddr,
                        StoreOwnershipQualifier::Unqualified);
  }
  appendCall->eraseFromParent();
}

/// Creates a call to a string initializer.
ApplyInst *StringOptimization::createStringInit(StringRef str,
                                                SILInstruction *beforeInst) {
  SILBuilder builder(beforeInst);
  SILLocation loc = beforeInst->getLoc();
  SILModule &module = beforeInst->getFunction()->getModule();
  ASTContext &ctxt = module.getASTContext();

  if (!makeUTF8Func) {
    // Find the String initializer which takes a string_literal as argument.
    ConstructorDecl *makeUTF8Decl = ctxt.getMakeUTF8StringDecl();
    if (!makeUTF8Decl)
      return nullptr;
    
    auto Mangled = SILDeclRef(makeUTF8Decl, SILDeclRef::Kind::Allocator).mangle();
    makeUTF8Func = module.findFunction(Mangled, SILLinkage::PublicExternal);
    if (!makeUTF8Func)
      return nullptr;
  }

  auto *literal = builder.createStringLiteral(loc, str,
                    StringLiteralInst::Encoding::UTF8);

  auto *length = builder.createIntegerLiteral(loc,
                    SILType::getBuiltinWordType(ctxt),
                    literal->getCodeUnitCount());

  auto *isAscii = builder.createIntegerLiteral(loc,
                    SILType::getBuiltinIntegerType(1, ctxt),
                    intmax_t(ctxt.isASCIIString(str)));

  SILType stringMetaType = SILType::getPrimitiveObjectType(
    CanType(MetatypeType::get(stringType.getASTType(),
      MetatypeRepresentation::Thin)));

  auto *metaTypeInst = builder.createMetatype(loc, stringMetaType);

  auto *functionRef = builder.createFunctionRefFor(loc, makeUTF8Func);

  return builder.createApply(loc, functionRef, SubstitutionMap(),
                             { literal, length, isAscii, metaTypeInst });
}

/// The StringOptimization function pass.
class StringOptimizationPass : public SILFunctionTransform {
public:

  void run() override {
    SILFunction *F = getFunction();
    if (!F->shouldOptimize())
      return;

    LLVM_DEBUG(llvm::dbgs() << "*** StringOptimization on function: "
                            << F->getName() << " ***\n");

    StringOptimization stringOptimization;
    bool changed = stringOptimization.run(F);

    if (changed) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createStringOptimization() {
  return new StringOptimizationPass();
}
