//===--- OSLogOptimizer.cpp - Optimizes calls to OS Log ===//
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
///
/// This pass implements SIL-level optimizations and diagnostics for the
/// os log APIs based on string interpolations. The APIs are implemented
/// in the files: OSLogMessage.swift, OSLog.swift. This pass constant evaluates
/// the log calls along with the auto-generated calls to the custom string
/// interpolation methods, which processes the string interpolation
/// passed to the log calls, and folds the constants found during the
/// evaluation. The constants that are folded include the C format string that
/// is constructed by the custom string interpolation methods from the string
/// interpolation, and the size and headers of the byte buffer into which
/// arguments are packed. This pass is closely tied to the implementation of
/// the log APIs.
///
/// Pass Dependencies:  This pass depends on MandatoryInlining and Mandatory
/// Linking happening before this pass and ConstantPropagation happening after
/// this pass. This pass also uses `ConstExprStepEvaluator` defined in
/// `Utils/ConstExpr.cpp`.
///
/// Algorithm Overview:
///
/// This pass implements a function-level transformation that collects calls
/// to the initializer of the custom string interpolation type: OSLogMessage,
/// which are annotated with an @_semantics attribute, and performs the
/// following steps on each such call.
///
///  1. Determines the range of instructions to constant evaluate.
///     The range starts from the first SIL instruction that begins the
///     construction of the custom string interpolation type: OSLogMessage to
///     the last transitive users of OSLogMessage. The log call which is marked
///     as @_transparent will be inlined into the caller before this pass
///     begins.
///
///  2. Constant evaluates the range of instruction identified in Step 1 and
///     collects string and integer-valued instructions who values were found
///     to be constants. The evaluation uses 'ConsExprStepEvaluator' utility.
///
///  3. After constant evaluation, the string and integer-value properties
///     of the custom string interpolation type: `OSLogInterpolation` must be
///     constants. This property is checked and any violations are diagnosed.
///     The errors discovered here may arise from the implementation of the
///     log APIs in the  overlay or could be because of wrong usage of the
///     log APIs.
///     TODO: these errors will be diagnosed by a separate, dedicated pass.
///
///  4. The constant instructions that were found in step 2 are folded by
///     generating SIL code that produces the constants. This also removes
///     instructions that are dead after folding.
///
/// Code Overview:
///
/// The function 'OSLogOptimization::run' implements the overall driver for
/// steps 1 to 4. The function 'beginOfInterpolation' identifies the begining of
/// interpolation (step 1) and the function 'getEndPointsOfDataDependentChain'
/// identifies the last transitive users of the OSLogMessage instance (step 1).
/// The function 'constantFold' is a driver for the steps 2 to 4. Step 2 is
/// implemented by the function 'collectConstants', step 3 by
/// 'detectAndDiagnoseErrors' and 'checkOSLogMessageIsConstant', and step 4 by
/// 'substituteConstants' and 'emitCodeForSymbolicValue'. The remaining
/// functions in the file implement the subtasks and utilities needed by the
/// above functions.

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/OptimizationMode.h"
#include "swift/AST/SemanticAttrs.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/Demangler.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/ConstExpr.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"
#include "llvm/ADT/BreadthFirstIterator.h"
#include "llvm/ADT/MapVector.h"

using namespace swift;
using namespace Lowering;

template <typename... T, typename... U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&... args) {
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

namespace {

/// If the given instruction is a call to the compiler-intrinsic initializer
/// of String that accepts string literals, return the called function.
/// Otherwise, return nullptr.
static SILFunction *getStringMakeUTF8Init(SILInstruction *inst) {
  auto *apply = dyn_cast<ApplyInst>(inst);
  if (!apply)
    return nullptr;

  SILFunction *callee = apply->getCalleeFunction();
  if (!callee || !callee->hasSemanticsAttr(semantics::STRING_MAKE_UTF8))
    return nullptr;
  return callee;
}

// A cache of string-related, SIL information that is needed to create and
// initalize strings from raw string literals. This information is
// extracted from instructions while they are constant evaluated. Though the
// information contained here can be constructed from scratch, extracting it
// from existing instructions is more efficient.
class StringSILInfo {
  /// SILFunction corresponding to an intrinsic string initializer that
  /// constructs a Swift String from a string literal.
  SILFunction *stringInitIntrinsic = nullptr;

  /// SIL metatype of String.
  SILType stringMetatype = SILType();

public:
  /// Extract and cache the required string-related information from the
  /// given instruction, if possible.
  void extractStringInfoFromInstruction(SILInstruction *inst) {
    // If the cache is already initialized do nothing.
    if (stringInitIntrinsic)
      return;

    SILFunction *callee = getStringMakeUTF8Init(inst);
    if (!callee)
      return;

    this->stringInitIntrinsic = callee;

    MetatypeInst *stringMetatypeInst =
        dyn_cast<MetatypeInst>(inst->getOperand(4)->getDefiningInstruction());
    this->stringMetatype = stringMetatypeInst->getType();
  }

  SILFunction *getStringInitIntrinsic() const {
    assert(stringInitIntrinsic);
    return stringInitIntrinsic;
  }

  SILType getStringMetatype() const {
    assert(stringMetatype);
    return stringMetatype;
  }
};

/// State needed for constant folding.
class FoldState {
public:
  /// Storage for symbolic values constructed during interpretation.
  SymbolicValueBumpAllocator allocator;

  /// Evaluator for evaluating instructions one by one.
  ConstExprStepEvaluator constantEvaluator;

  /// Information needed for folding strings.
  StringSILInfo stringInfo;

  /// Instruction from where folding must begin.
  SILInstruction *beginInstruction;

  /// Instructions that mark the end points of constant evaluation.
  SmallSetVector<SILInstruction *, 2> endInstructions;

private:
  /// SIL values that were found to be constants during
  /// constant evaluation.
  SmallVector<SILValue, 4> constantSILValues;

public:
  FoldState(SILFunction *fun, unsigned assertConfig, SILInstruction *beginInst,
            ArrayRef<SILInstruction *> endInsts)
      : constantEvaluator(allocator, fun, assertConfig),
        beginInstruction(beginInst),
        endInstructions(endInsts.begin(), endInsts.end()) {}

  void addConstantSILValue(SILValue value) {
    constantSILValues.push_back(value);
  }

  ArrayRef<SILValue> getConstantSILValues() {
    return ArrayRef<SILValue>(constantSILValues);
  }
};

/// Return true if and only if the given nominal type declaration is that of
/// a stdlib Int or stdlib Bool.
static bool isStdlibIntegerOrBoolDecl(NominalTypeDecl *numberDecl,
                                      ASTContext &astCtx) {
  return (numberDecl == astCtx.getIntDecl() ||
          numberDecl == astCtx.getInt8Decl() ||
          numberDecl == astCtx.getInt16Decl() ||
          numberDecl == astCtx.getInt32Decl() ||
          numberDecl == astCtx.getInt64Decl() ||
          numberDecl == astCtx.getUIntDecl() ||
          numberDecl == astCtx.getUInt8Decl() ||
          numberDecl == astCtx.getUInt16Decl() ||
          numberDecl == astCtx.getUInt32Decl() ||
          numberDecl == astCtx.getUInt64Decl() ||
          numberDecl == astCtx.getBoolDecl());
}

/// Return true if and only if the given SIL type represents a Stdlib or builtin
/// integer type or a Bool type.
static bool isIntegerOrBoolType(SILType silType, ASTContext &astContext) {
  if (silType.is<BuiltinIntegerType>()) {
    return true;
  }
  NominalTypeDecl *nominalDecl = silType.getNominalOrBoundGenericNominal();
  return nominalDecl && isStdlibIntegerOrBoolDecl(nominalDecl, astContext);
}

/// Return true if and only if the given SIL type represents a String type.
static bool isStringType(SILType silType, ASTContext &astContext) {
  NominalTypeDecl *nominalDecl = silType.getNominalOrBoundGenericNominal();
  return nominalDecl && nominalDecl == astContext.getStringDecl();
}

/// Return true if and only if the given SIL type represents an Array type.
static bool isArrayType(SILType silType, ASTContext &astContext) {
  NominalTypeDecl *nominalDecl = silType.getNominalOrBoundGenericNominal();
  return nominalDecl && nominalDecl == astContext.getArrayDecl();
}

/// Decide if the given instruction (which could possibly be a call) should
/// be constant evaluated.
///
/// \returns true iff the given instruction is not a call or if it is, it calls
/// a known constant-evaluable function such as string append etc., or calls
/// a function annotate as "constant_evaluable".
static bool shouldAttemptEvaluation(SILInstruction *inst) {
  auto *apply = dyn_cast<ApplyInst>(inst);
  if (!apply)
    return true;
  SILFunction *calleeFun = apply->getCalleeFunction();
  if (!calleeFun)
    return false;
  return isConstantEvaluable(calleeFun);
}

/// Skip or evaluate the given instruction based on the evaluation policy and
/// handle errors. The policy is to evaluate all non-apply instructions as well
/// as apply instructions that are marked as "constant_evaluable".
static std::pair<Optional<SILBasicBlock::iterator>, Optional<SymbolicValue>>
evaluateOrSkip(ConstExprStepEvaluator &stepEval,
               SILBasicBlock::iterator instI) {
  SILInstruction *inst = &(*instI);

  // Note that skipping a call conservatively approximates its effects on the
  // interpreter state.
  if (shouldAttemptEvaluation(inst)) {
    return stepEval.tryEvaluateOrElseMakeEffectsNonConstant(instI);
  }
  return stepEval.skipByMakingEffectsNonConstant(instI);
}

/// Return true iff the given value is a stdlib Int or Bool and it not a direct
/// construction of Int or Bool.
static bool isFoldableIntOrBool(SILValue value, ASTContext &astContext) {
  return isIntegerOrBoolType(value->getType(), astContext) &&
         !isa<StructInst>(value);
}

/// Return true iff the given value is a string and is not an initialization
/// of an string from a string literal.
static bool isFoldableString(SILValue value, ASTContext &astContext) {
  return isStringType(value->getType(), astContext) &&
         (!isa<ApplyInst>(value) ||
          !getStringMakeUTF8Init(cast<ApplyInst>(value)));
}

/// Return true iff the given value is an array and is not an initialization
/// of an array from an array literal.
static bool isFoldableArray(SILValue value, ASTContext &astContext) {
  if (!isArrayType(value->getType(), astContext))
    return false;
  // If value is an initialization of an array from a literal or an empty array
  // initializer, it need not be folded. Arrays constructed from literals use a
  // function with semantics: "array.uninitialized_intrinsic" that returns
  // a pair, where the first element of the pair is the array.
  SILInstruction *definingInst = value->getDefiningInstruction();
  if (!definingInst)
    return true;
  SILInstruction *constructorInst = definingInst;
  if (isa<DestructureTupleInst>(definingInst) ||
      isa<TupleExtractInst>(definingInst)) {
    constructorInst = definingInst->getOperand(0)->getDefiningInstruction();
  }
  if (!constructorInst || !isa<ApplyInst>(constructorInst))
    return true;
  SILFunction *callee = cast<ApplyInst>(constructorInst)->getCalleeFunction();
  return !callee ||
         (!callee->hasSemanticsAttr("array.init.empty") &&
          !callee->hasSemanticsAttr("array.uninitialized_intrinsic"));
}

/// Return true iff the given value is a closure but is not a creation of a
/// closure e.g., through partial_apply or thin_to_thick_function or
/// convert_function.
static bool isFoldableClosure(SILValue value) {
  return value->getType().is<SILFunctionType>() &&
         (!isa<FunctionRefInst>(value) && !isa<PartialApplyInst>(value) &&
          !isa<ThinToThickFunctionInst>(value) &&
          !isa<ConvertFunctionInst>(value));
}

/// Check whether a SILValue is foldable. String, integer, array and
/// function values are foldable with the following exceptions:
///  - Addresses cannot be folded.
///  - Literals need not be folded.
///  - Results of ownership instructions like load_borrow/copy_value need not
///  be folded
///  - Constructors such as \c struct Int or \c string.init() need not be folded.
static bool isSILValueFoldable(SILValue value) {
  SILInstruction *definingInst = value->getDefiningInstruction();
  if (!definingInst)
    return false;
  ASTContext &astContext = definingInst->getFunction()->getASTContext();
  SILType silType = value->getType();
  return (!silType.isAddress() && !isa<LiteralInst>(definingInst) &&
          !isa<LoadBorrowInst>(definingInst) &&
          !isa<BeginBorrowInst>(definingInst) &&
          !isa<CopyValueInst>(definingInst) &&
          (isFoldableIntOrBool(value, astContext) ||
           isFoldableString(value, astContext) ||
           isFoldableArray(value, astContext) || isFoldableClosure(value)));
}

/// Diagnose failure during evaluation of a call to a constant-evaluable
/// function. Note that all auto-generated 'appendInterpolation' calls are
/// constant evaluable. This function detects and specially handles such
/// functions to present better diagnostic messages.
static void diagnoseErrorInConstantEvaluableFunction(ApplyInst *call,
                                                     SymbolicValue errorInfo) {
  SILNode *unknownNode = errorInfo.getUnknownNode();
  UnknownReason unknownReason = errorInfo.getUnknownReason();

  SILFunction *callee = call->getCalleeFunction();
  assert(callee);
  SILLocation loc = call->getLoc();
  SourceLoc sourceLoc = loc.getSourceLoc();
  ASTContext &astContext = callee->getASTContext();

  std::string demangledCalleeName = Demangle::demangleSymbolAsString(
      callee->getName(),
      Demangle::DemangleOptions::SimplifiedUIDemangleOptions());

  // If an 'appendInterpolation' evaluation failed, it is probably due to
  // invalid privacy or format specifiers. These are the only possible errors
  // that the users of the log API could make. The rest are for library authors
  // or users who extend the log APIs.
  if (unknownReason.getKind() == UnknownReason::CallArgumentUnknown &&
      dyn_cast<ApplyInst>(unknownNode) == call) {
    if (StringRef(demangledCalleeName)
            .contains(astContext.Id_appendInterpolation.str())) {
      // TODO: extract and report the label of the parameter that is not a
      // constant.
      diagnose(astContext, sourceLoc,
               diag::oslog_non_const_interpolation_options);
      return;
    }
  }
  diagnose(astContext, sourceLoc, diag::oslog_const_evaluable_fun_error,
           demangledCalleeName);
  errorInfo.emitUnknownDiagnosticNotes(loc);
  return;
}

/// Detect and emit diagnostics for errors found during evaluation. Errors
/// can happen due to incorrect implementation of the os log API in the
/// overlay or due to incorrect use of the os log API.
/// TODO: errors due to incorrect use of the API should be diagnosed by a
/// dedicated diagnostics pass that will happen before this optimization starts.
static bool detectAndDiagnoseErrors(SymbolicValue errorInfo,
                                    SILInstruction *unevaluableInst) {
  SILFunction *parentFun = unevaluableInst->getFunction();
  ASTContext &astContext = parentFun->getASTContext();

  // If evaluation of any other constant_evaluable function call fails, point
  // to that failed function along with a reason: such as that a parameter is
  // non-constant parameter or that body is not constant evaluable.
  ApplyInst *call = dyn_cast<ApplyInst>(unevaluableInst);
  if (call) {
    SILFunction *callee = call->getCalleeFunction();
    if (callee && isConstantEvaluable(callee)) {
      diagnoseErrorInConstantEvaluableFunction(call, errorInfo);
      return true; // abort evaluation.
    }
  }

  // Every other error must happen in the body of the os_log function which
  // is inlined in the 'parentFun' before this pass. In this case, if we have a
  // fail-stop error, point to the error and abort evaluation. Otherwise, just
  // ignore the error and continue evaluation as this error might not affect the
  // constant value of the OSLogMessage instance.
  if (isFailStopError(errorInfo)) {
    assert(errorInfo.getKind() == SymbolicValue::Unknown);
    SILLocation loc = unevaluableInst->getLoc();
    SourceLoc sourceLoc = loc.getSourceLoc();
    diagnose(astContext, sourceLoc, diag::oslog_fail_stop_error);
    errorInfo.emitUnknownDiagnosticNotes(loc);
    return true;
  }
  return false;
}

/// Given a 'foldState', constant evaluate instructions from
/// 'foldState.beginInstruction' until an instruction in
/// 'foldState.endInstructions' is seen. Add foldable, constant-valued
/// instructions discovered during the evaluation to
/// 'foldState.constantSILValues'.
/// \returns error information if the evaluation failed.
static Optional<SymbolicValue> collectConstants(FoldState &foldState) {

  ConstExprStepEvaluator &constantEvaluator = foldState.constantEvaluator;
  SILBasicBlock::iterator currI = foldState.beginInstruction->getIterator();
  auto &endInstructions = foldState.endInstructions;

  // The loop will break when it sees a return instruction or an instruction in
  // endInstructions.
  while (true) {
    SILInstruction *currInst = &(*currI);
    if (endInstructions.count(currInst))
      break;

    // Initialize string info from this instruction if possible.
    foldState.stringInfo.extractStringInfoFromInstruction(currInst);

    Optional<SymbolicValue> errorInfo = None;
    Optional<SILBasicBlock::iterator> nextI = None;

    std::tie(nextI, errorInfo) = evaluateOrSkip(constantEvaluator, currI);

    // If the evaluation of this instruction failed, check whether it should be
    // diagnosed and reported. If so, abort evaluation. Otherwise, continue
    // evaluation if possible as this error could be due to an instruction that
    // doesn't affect the OSLogMessage value.
    if (errorInfo && detectAndDiagnoseErrors(errorInfo.getValue(), currInst)) {
      return errorInfo;
    }

    if (!nextI) {
      // We cannnot find the next instruction to continue evaluation, and we
      // haven't seen any reportable errors during evaluation. Therefore,
      // consider this the end point of evaluation.
      return None; // No error.
    }

    // Set the next instruction to continue evaluation from.
    currI = nextI.getValue();

    // If the instruction results are foldable and if we found a constant value
    // for the results, record it.
    for (SILValue instructionResult : currInst->getResults()) {
      if (!isSILValueFoldable(instructionResult))
        continue;

      Optional<SymbolicValue> constantVal =
          constantEvaluator.lookupConstValue(instructionResult);
      if (constantVal.hasValue()) {
        foldState.addConstantSILValue(instructionResult);
      }
    }
  }
  return None; // No error.
}

/// Generate SIL code to create an array of constant size from the given
/// SILValues \p elements. This function creates the same sequence of SIL
/// instructions that would be generated for initializing an array from an array
/// literal of the form [element1, element2, ..., elementn].
///
/// \param elements SILValues that the array should contain
/// \param arrayType the type of the array that must be created.
/// \param builder SILBuilder that provides the context for emitting the code
/// for the array.
/// \param loc SILLocation to use in the emitted instructions.
/// \return the SILValue of the array that is created with the given \c
/// elements.
static SILValue emitCodeForConstantArray(ArrayRef<SILValue> elements,
                                         CanType arrayType, SILBuilder &builder,
                                         SILLocation loc) {
  ASTContext &astContext = builder.getASTContext();
  assert(astContext.getArrayDecl() ==
         arrayType->getNominalOrBoundGenericNominal());
  SILModule &module = builder.getModule();

  // Create a SILValue for the number of elements.
  unsigned numElements = elements.size();
  SILValue numElementsSIL = builder.createIntegerLiteral(
      loc, SILType::getBuiltinWordType(astContext), numElements);

  // Find the SILFunction that corresponds to _allocateUninitializedArray.
  FuncDecl *arrayAllocateDecl = astContext.getAllocateUninitializedArray();
  assert(arrayAllocateDecl);
  std::string allocatorMangledName =
      SILDeclRef(arrayAllocateDecl, SILDeclRef::Kind::Func).mangle();
  SILFunction *arrayAllocateFun =
      module.findFunction(allocatorMangledName, SILLinkage::PublicExternal);
  assert(arrayAllocateFun);

  // Call the _allocateUninitializedArray function with numElementsSIL. The
  // call returns a two-element tuple, where the first element is the newly
  // created array and the second element is a pointer to the internal storage
  // of the array.
  SubstitutionMap subMap = arrayType->getContextSubstitutionMap(
      module.getSwiftModule(), astContext.getArrayDecl());
  FunctionRefInst *arrayAllocateRef =
      builder.createFunctionRef(loc, arrayAllocateFun);
  ApplyInst *applyInst = builder.createApply(
      loc, arrayAllocateRef, subMap, ArrayRef<SILValue>(numElementsSIL), false);

  // Extract the elements of the tuple returned by the call to the allocator.
  DestructureTupleInst *destructureInst =
      builder.createDestructureTuple(loc, applyInst);
  SILValue arraySIL = destructureInst->getResults()[0];
  SILValue storagePointerSIL = destructureInst->getResults()[1];

  if (elements.empty()) {
    // Nothing more to be done if we are creating an empty array.
    return arraySIL;
  }

  // Convert the pointer to the storage to an address. The elements will be
  // stored into offsets from this address.
  SILType elementSILType = elements[0]->getType();
  PointerToAddressInst *storageAddr = builder.createPointerToAddress(
      loc, storagePointerSIL, elementSILType.getAddressType(),
      /*isStrict*/ true,
      /*isInvariant*/ false);

  // Iterate over the elements and store them into the storage address
  // after offsetting it appropriately.

  // Create a TypeLowering for emitting stores. Note that TypeLowering
  // provides a utility for emitting stores for storing trivial and
  // non-trivial values, and also handles OSSA and non-OSSA.
  const TypeLowering &elementTypeLowering =
      builder.getTypeLowering(elementSILType);

  unsigned elementIndex = 0;
  for (SILValue elementSIL : elements) {
    // Compute the address where the element must be stored.
    SILValue currentStorageAddr;
    if (elementIndex != 0) {
      SILValue indexSIL = builder.createIntegerLiteral(
          loc, SILType::getBuiltinWordType(astContext), elementIndex);
      currentStorageAddr = builder.createIndexAddr(loc, storageAddr, indexSIL);
    } else {
      currentStorageAddr = storageAddr;
    }
    // Store the generated element into the currentStorageAddr. This is an
    // initializing store and therefore there is no need to free any existing
    // element.
    elementTypeLowering.emitStore(builder, loc, elementSIL, currentStorageAddr,
                                  StoreOwnershipQualifier::Init);
    elementIndex++;
  }
  return arraySIL;
}

/// Given a SILValue \p value, return the instruction immediately following the
/// definition of the value. That is, if the value is defined by an
/// instruction, return the instruction following the definition. Otherwise, if
/// the value is a basic block parameter, return the first instruction of the
/// basic block.
SILInstruction *getInstructionFollowingValueDefinition(SILValue value) {
  SILInstruction *definingInst = value->getDefiningInstruction();
  if (definingInst) {
    return &*std::next(definingInst->getIterator());
  }
  // Here value must be a basic block argument.
  SILBasicBlock *bb = value->getParentBlock();
  return &*bb->begin();
}

/// Given a SILValue \p value, create a copy of the value using copy_value in
/// OSSA or retain in non-OSSA, if \p value is a non-trivial type. Otherwise, if
/// \p value is a trivial type, return the value itself.
SILValue makeOwnedCopyOfSILValue(SILValue value, SILFunction &fun) {
  SILType type = value->getType();
  if (type.isTrivial(fun))
    return value;
  assert(!type.isAddress() && "cannot make owned copy of addresses");

  SILInstruction *instAfterValueDefinition =
      getInstructionFollowingValueDefinition(value);
  SILLocation copyLoc = instAfterValueDefinition->getLoc();
  SILBuilderWithScope builder(instAfterValueDefinition);
  const TypeLowering &typeLowering = builder.getTypeLowering(type);
  SILValue copy = typeLowering.emitCopyValue(builder, copyLoc, value);
  return copy;
}

/// Generate SIL code that computes the constant given by the symbolic value
/// `symVal`. Note that strings and struct-typed constant values will require
/// multiple instructions to be emitted.
/// \param symVal symbolic value for which SIL code needs to be emitted.
/// \param expectedType the expected type of the instruction that would be
/// computing the symbolic value `symVal`. The type is accepted as a
/// parameter as some symbolic values like integer constants can inhabit more
/// than one type.
/// \param builder SILBuilder that provides the context for emitting the code
/// for the symbolic value
/// \param loc SILLocation to use in the emitted instructions.
/// \param stringInfo String.init and metatype information for generating code
/// for string literals.
static SILValue emitCodeForSymbolicValue(SymbolicValue symVal,
                                         Type expectedType, SILBuilder &builder,
                                         SILLocation &loc,
                                         StringSILInfo &stringInfo) {
  ASTContext &astContext = expectedType->getASTContext();

  switch (symVal.getKind()) {
  case SymbolicValue::String: {
    assert(astContext.getStringDecl() ==
           expectedType->getNominalOrBoundGenericNominal());

    StringRef stringVal = symVal.getStringValue();
    StringLiteralInst *stringLitInst = builder.createStringLiteral(
        loc, stringVal, StringLiteralInst::Encoding::UTF8);

    // Create a builtin word for the size of the string
    IntegerLiteralInst *sizeInst = builder.createIntegerLiteral(
        loc, SILType::getBuiltinWordType(astContext), stringVal.size());
    // Set isAscii to false.
    IntegerLiteralInst *isAscii = builder.createIntegerLiteral(
        loc, SILType::getBuiltinIntegerType(1, astContext), 0);
    // Create a metatype inst.
    MetatypeInst *metatypeInst =
        builder.createMetatype(loc, stringInfo.getStringMetatype());

    auto args = SmallVector<SILValue, 4>();
    args.push_back(stringLitInst);
    args.push_back(sizeInst);
    args.push_back(isAscii);
    args.push_back(metatypeInst);

    FunctionRefInst *stringInitRef =
        builder.createFunctionRef(loc, stringInfo.getStringInitIntrinsic());
    ApplyInst *applyInst = builder.createApply(
        loc, stringInitRef, SubstitutionMap(), ArrayRef<SILValue>(args), false);
    return applyInst;
  }
  case SymbolicValue::Integer: { // Builtin integer types.
    APInt resInt = symVal.getIntegerValue();
    assert(expectedType->is<BuiltinIntegerType>());

    SILType builtinIntType =
        SILType::getPrimitiveObjectType(expectedType->getCanonicalType());
    IntegerLiteralInst *intLiteralInst =
        builder.createIntegerLiteral(loc, builtinIntType, resInt);
    return intLiteralInst;
  }
  case SymbolicValue::Aggregate: {
    // Support only stdlib integer or bool structs.
    StructDecl *structDecl = expectedType->getStructOrBoundGenericStruct();
    assert(structDecl);
    assert(isStdlibIntegerOrBoolDecl(structDecl, astContext));
    assert(symVal.getAggregateType()->isEqual(expectedType) &&
           "aggregate symbolic value's type and expected type do not match");

    VarDecl *propertyDecl = structDecl->getStoredProperties().front();
    Type propertyType = expectedType->getTypeOfMember(
        propertyDecl->getModuleContext(), propertyDecl);
    SymbolicValue propertyVal = symVal.lookThroughSingleElementAggregates();
    SILValue newPropertySIL = emitCodeForSymbolicValue(
        propertyVal, propertyType, builder, loc, stringInfo);
    // The lowered SIL type of an integer/bool type is just the primitive
    // object type containing the Swift type.
    SILType aggregateType =
        SILType::getPrimitiveObjectType(expectedType->getCanonicalType());
    StructInst *newStructInst = builder.createStruct(
        loc, aggregateType, ArrayRef<SILValue>(newPropertySIL));
    return newStructInst;
  }
  case SymbolicValue::Array: {
    assert(expectedType->isEqual(symVal.getArrayType()));
    CanType elementType;
    ArrayRef<SymbolicValue> arrayElements =
        symVal.getStorageOfArray().getStoredElements(elementType);

    // Emit code for the symbolic values corresponding to the array elements.
    SmallVector<SILValue, 8> elementSILValues;
    for (SymbolicValue elementSymVal : arrayElements) {
      SILValue elementSIL = emitCodeForSymbolicValue(elementSymVal, elementType,
                                                     builder, loc, stringInfo);
      elementSILValues.push_back(elementSIL);
    }
    SILValue arraySIL = emitCodeForConstantArray(
        elementSILValues, expectedType->getCanonicalType(), builder, loc);
    return arraySIL;
  }
  case SymbolicValue::Closure: {
    assert(expectedType->is<AnyFunctionType>() ||
           expectedType->is<SILFunctionType>());

    SymbolicClosure *closure = symVal.getClosure();
    SubstitutionMap callSubstMap = closure->getCallSubstitutionMap();
    SILModule &module = builder.getModule();
    ArrayRef<SymbolicClosureArgument> captures = closure->getCaptures();

    // Recursively emit code for all captured values that are mapped to a
    // symbolic value. If there is a captured value that is not mapped
    // to a symbolic value, use the captured value as such (after possibly
    // copying non-trivial captures).
    SmallVector<SILValue, 4> capturedSILVals;
    for (SymbolicClosureArgument capture : captures) {
      SILValue captureOperand = capture.first;
      Optional<SymbolicValue> captureSymVal = capture.second;
      if (!captureSymVal) {
        SILFunction &fun = builder.getFunction();
        assert(captureOperand->getFunction() == &fun &&
               "non-constant captured arugment not defined in this function");
        // If the captureOperand is a non-trivial value, it should be copied
        // as it now used in a new folded closure.
        SILValue captureCopy = makeOwnedCopyOfSILValue(captureOperand, fun);
        capturedSILVals.push_back(captureCopy);
        continue;
      }
      // Here, we have a symbolic value for the capture. Therefore, use it to
      // create a new constant at this point. Note that the captured operand
      // type may have generic parameters which has to be substituted with the
      // substitution map that was inferred by the constant evaluator at the
      // partial-apply site.
      SILType operandType = captureOperand->getType();
      SILType captureType = operandType.subst(module, callSubstMap);
      SILValue captureSILVal = emitCodeForSymbolicValue(
          captureSymVal.getValue(), captureType.getASTType(), builder, loc,
          stringInfo);
      capturedSILVals.push_back(captureSILVal);
    }

    FunctionRefInst *functionRef =
        builder.createFunctionRef(loc, closure->getTarget());
    SILType closureType = closure->getClosureType();
    ParameterConvention convention =
        closureType.getAs<SILFunctionType>()->getCalleeConvention();
    PartialApplyInst *papply = builder.createPartialApply(
        loc, functionRef, callSubstMap, capturedSILVals, convention);
    // The type of the created closure must be a lowering of the expected type.
    SILType resultType = papply->getType();
    CanType expectedCanType = expectedType->getCanonicalType();
    assert(expectedType->is<SILFunctionType>()
               ? resultType.getASTType() == expectedCanType
               : resultType.is<SILFunctionType>());
    return papply;
  }
  default: {
    llvm_unreachable("Symbolic value kind is not supported");
  }
  }
}

/// Given a SILValue \p value, compute the set of transitive users of the value
/// (excluding value itself) by following the use-def chain starting at value.
/// Note that this function does not follow use-def chains though branches.
static void getTransitiveUsers(SILValue value,
                               SmallVectorImpl<SILInstruction *> &users) {
  // Collect the instructions that are data dependent on the value using a
  // fix point iteration.
  SmallPtrSet<SILInstruction *, 16> visitedUsers;
  SmallVector<SILValue, 16> worklist;
  worklist.push_back(value);

  while (!worklist.empty()) {
    SILValue currVal = worklist.pop_back_val();
    for (Operand *use : currVal->getUses()) {
      SILInstruction *user = use->getUser();
      if (visitedUsers.count(user))
        continue;
      visitedUsers.insert(user);
      llvm::copy(user->getResults(), std::back_inserter(worklist));
    }
  }
  // At this point, visitedUsers have all the transitive, data-dependent uses.
  users.append(visitedUsers.begin(), visitedUsers.end());
}

/// Collect the end points of the instructions that are data dependent on \c
/// value. A instruction is data dependent on \c value if its result may
/// transitively depends on \c value. Note that data dependencies through
/// addresses are not tracked by this function.
///
/// \param value SILValue that is not an address.
/// \param fun SILFunction that defines \c value.
/// \param endUsers buffer for storing the found end points of the data
/// dependence chain.
static void
getEndPointsOfDataDependentChain(SILValue value, SILFunction *fun,
                                 SmallVectorImpl<SILInstruction *> &endUsers) {
  assert(!value->getType().isAddress());

  SmallVector<SILInstruction *, 16> transitiveUsers;
  // Get transitive users of value, ignoring use-def chain going through
  // branches. These transitive users define the end points of the constant
  // evaluation. Igoring use-def chains through branches causes constant
  // evaluation to miss some constant folding opportunities. This can be
  // relaxed in the future, if necessary.
  getTransitiveUsers(value, transitiveUsers);

  // Compute the lifetime frontier of all the transitive uses which are the
  // instructions following the last uses. Every exit from the last uses will
  // have a lifetime frontier.
  SILInstruction *valueDefinition = value->getDefiningInstruction();
  SILInstruction *def =
      valueDefinition ? valueDefinition : &(value->getParentBlock()->front());
  ValueLifetimeAnalysis lifetimeAnalysis =
      ValueLifetimeAnalysis(def, transitiveUsers);
  ValueLifetimeAnalysis::Frontier frontier;
  bool hasCriticlEdges = lifetimeAnalysis.computeFrontier(
      frontier, ValueLifetimeAnalysis::DontModifyCFG);
  endUsers.append(frontier.begin(), frontier.end());
  if (!hasCriticlEdges)
    return;
  // If there are some lifetime frontiers on the critical edges, take the
  // first instruction of the target of the critical edge as the frontier. This
  // will suffice as every exit from the visitedUsers must go through one of
  // them.
  for (auto edgeIndexPair : lifetimeAnalysis.getCriticalEdges()) {
    SILBasicBlock *targetBB =
        edgeIndexPair.first->getSuccessors()[edgeIndexPair.second];
    endUsers.push_back(&targetBB->front());
  }
}

/// Given a guaranteed SILValue \p value, return a borrow-scope introducing
/// value, if there is exactly one such introducing value. Otherwise, return
/// None. There can be multiple borrow scopes for a SILValue iff it is derived
/// from a guaranteed basic block parameter representing a phi node.
static Optional<BorrowScopeIntroducingValue>
getUniqueBorrowScopeIntroducingValue(SILValue value) {
  assert(value.getOwnershipKind() == ValueOwnershipKind::Guaranteed &&
         "parameter must be a guarenteed value");
  SmallVector<BorrowScopeIntroducingValue, 4> borrowIntroducers;
  getUnderlyingBorrowIntroducingValues(value, borrowIntroducers);
  assert(borrowIntroducers.size() > 0 &&
         "folding guaranteed value with no borrow introducer");
  if (borrowIntroducers.size() > 1)
    return None;
  return borrowIntroducers[0];
}

/// Replace all uses of \c originalVal by \c foldedVal and adjust lifetimes of
/// original and folded values by emitting required destory/release instructions
/// at the right places. Note that this function does not remove any
/// instruction.
///
/// \param originalVal the SIL value that is replaced.
/// \param foldedVal the SIL value that replaces the \c originalVal.
/// \param fun the SIL function containing the \c foldedVal and \c originalVal
static void replaceAllUsesAndFixLifetimes(SILValue foldedVal,
                                          SILValue originalVal,
                                          SILFunction *fun) {
  SILInstruction *originalInst = originalVal->getDefiningInstruction();
  SILInstruction *foldedInst = foldedVal->getDefiningInstruction();
  assert(originalInst &&
         "cannot constant fold function or basic block parameter");
  assert(!isa<TermInst>(originalInst) &&
         "cannot constant fold a terminator instruction");
  assert(foldedInst && "constant value does not have a defining instruction");

  if (originalVal->getType().isTrivial(*fun)) {
    assert(foldedVal->getType().isTrivial(*fun));
    // Just replace originalVal by foldedVal.
    originalVal->replaceAllUsesWith(foldedVal);
    return;
  }
  assert(!foldedVal->getType().isTrivial(*fun));
  assert(fun->hasOwnership());
  assert(foldedVal.getOwnershipKind() == ValueOwnershipKind::Owned &&
         "constant value must have owned ownership kind");

  if (originalVal.getOwnershipKind() == ValueOwnershipKind::Owned) {
    originalVal->replaceAllUsesWith(foldedVal);
    // Destroy originalVal, which is now unused, immediately after its
    // definition. Note that originalVal's destorys are now transferred to
    // foldedVal.
    SILInstruction *insertionPoint = &(*std::next(originalInst->getIterator()));
    SILBuilderWithScope builder(insertionPoint);
    SILLocation loc = insertionPoint->getLoc();
    builder.emitDestroyValueOperation(loc, originalVal);
    return;
  }

  // Here, originalVal is guaranteed. It must belong to a borrow scope that
  // begins at a scope introducing instruction e.g. begin_borrow or load_borrow.
  // The foldedVal should also have been inserted at the beginning of the scope.
  // Therefore, create a borrow of foldedVal at the beginning of the scope and
  // use the borrow in place of the originalVal. Also, end the borrow and
  // destroy foldedVal at the end of the borrow scope.
  assert(originalVal.getOwnershipKind() == ValueOwnershipKind::Guaranteed);

  Optional<BorrowScopeIntroducingValue> originalScopeBegin =
      getUniqueBorrowScopeIntroducingValue(originalVal);
  assert(originalScopeBegin &&
         "value without a unique borrow scope should not have been folded");
  SILInstruction *scopeBeginInst =
      originalScopeBegin->value->getDefiningInstruction();
  assert(scopeBeginInst);

  SILBuilderWithScope builder(scopeBeginInst);
  SILValue borrow =
      builder.emitBeginBorrowOperation(scopeBeginInst->getLoc(), foldedVal);

  originalVal->replaceAllUsesWith(borrow);

  SmallVector<SILInstruction *, 4> scopeEndingInsts;
  originalScopeBegin->getLocalScopeEndingInstructions(scopeEndingInsts);

  for (SILInstruction *scopeEndingInst : scopeEndingInsts) {
    SILBuilderWithScope builder(scopeEndingInst);
    builder.emitEndBorrowOperation(scopeEndingInst->getLoc(), borrow);
    builder.emitDestroyValueOperation(scopeEndingInst->getLoc(), foldedVal);
  }
  return;
}

/// Given a fold state with constant-valued instructions, substitute the
/// instructions with the constant values. The constant values could be strings
/// or Stdlib integer-struct values or builtin integers.
static void substituteConstants(FoldState &foldState) {
  ConstExprStepEvaluator &evaluator = foldState.constantEvaluator;
  // Instructions that are possibly dead since their results are folded.
  SmallVector<SILInstruction *, 8> possiblyDeadInsts;

  for (SILValue constantSILValue : foldState.getConstantSILValues()) {
    SymbolicValue constantSymbolicVal =
        evaluator.lookupConstValue(constantSILValue).getValue();

    SILInstruction *definingInst = constantSILValue->getDefiningInstruction();
    assert(definingInst);
    SILFunction *fun = definingInst->getFunction();

    // Find an insertion point for inserting the new constant value. If we are
    // folding a value like struct_extract within a borrow scope, we need to
    // insert the constant value at the beginning of the borrow scope. This
    // is because the borrowed value is expected to be alive during its entire
    // borrow scope and could be stored into memory and accessed indirectly
    // without a copy e.g. using store_borrow within the borrow scope. On the
    // other hand, if we are folding an owned value, we can insert the constant
    // value at the point where the owned value is defined.
    SILInstruction *insertionPoint = definingInst;
    if (constantSILValue.getOwnershipKind() == ValueOwnershipKind::Guaranteed) {
      Optional<BorrowScopeIntroducingValue> borrowIntroducer =
          getUniqueBorrowScopeIntroducingValue(constantSILValue);
      if (!borrowIntroducer) {
        // This case happens only if constantSILValue is derived from a
        // guaranteed basic block parameter. This is unlikley because the values
        // that have to be folded should just be a struct-extract of an owned
        // instance of OSLogMessage.
        continue;
      }
      insertionPoint = borrowIntroducer->value->getDefiningInstruction();
      assert(insertionPoint && "borrow scope begnning is a parameter");
    }

    SILBuilderWithScope builder(insertionPoint);
    SILLocation loc = insertionPoint->getLoc();
    CanType instType = constantSILValue->getType().getASTType();
    SILValue foldedSILVal = emitCodeForSymbolicValue(
        constantSymbolicVal, instType, builder, loc, foldState.stringInfo);

    // Replace constantSILValue with foldedSILVal and adjust the lifetime and
    // ownership of the values appropriately.
    replaceAllUsesAndFixLifetimes(foldedSILVal, constantSILValue, fun);
    possiblyDeadInsts.push_back(definingInst);
  }
}

/// Check whether OSLogMessage and OSLogInterpolation instances and all their
/// stored properties are constants. If not, it indicates errors that are due to
/// incorrect implementation of OSLogMessage either in the overlay or in the
/// extensions created by users. Detect and emit diagnostics for such errors.
/// The diagnostics here are for os log library authors.
static bool checkOSLogMessageIsConstant(SingleValueInstruction *osLogMessage,
                                        FoldState &foldState) {
  ConstExprStepEvaluator &constantEvaluator = foldState.constantEvaluator;
  SILLocation loc = osLogMessage->getLoc();
  SourceLoc sourceLoc = loc.getSourceLoc();
  SILFunction *fn = osLogMessage->getFunction();
  SILModule &module = fn->getModule();
  ASTContext &astContext = fn->getASTContext();

  Optional<SymbolicValue> osLogMessageValueOpt =
      constantEvaluator.lookupConstValue(osLogMessage);
  if (!osLogMessageValueOpt ||
      osLogMessageValueOpt->getKind() != SymbolicValue::Aggregate) {
    diagnose(astContext, sourceLoc, diag::oslog_non_constant_message);
    return true;
  }

  // The first (and only) property of OSLogMessage is the OSLogInterpolation
  // instance.
  SymbolicValue osLogInterpolationValue =
      osLogMessageValueOpt->getAggregateMembers()[0];
  if (!osLogInterpolationValue.isConstant()) {
    diagnose(astContext, sourceLoc, diag::oslog_non_constant_interpolation);
    return true;
  }

  // Check if every proprety of the OSLogInterpolation instance has a constant
  // value.
  SILType osLogMessageType = osLogMessage->getType();
  StructDecl *structDecl = osLogMessageType.getStructOrBoundGenericStruct();
  assert(structDecl);

  auto typeExpansionContext =
      TypeExpansionContext(*osLogMessage->getFunction());
  VarDecl *interpolationPropDecl = structDecl->getStoredProperties().front();
  SILType osLogInterpolationType = osLogMessageType.getFieldType(
      interpolationPropDecl, module, typeExpansionContext);
  StructDecl *interpolationStruct =
      osLogInterpolationType.getStructOrBoundGenericStruct();
  assert(interpolationStruct);

  auto propertyDecls = interpolationStruct->getStoredProperties();
  ArrayRef<SymbolicValue> propertyValues =
      osLogInterpolationValue.getAggregateMembers();
  auto propValueI = propertyValues.begin();
  bool errorDetected = false;

  for (auto *propDecl : propertyDecls) {
    SymbolicValue propertyValue = *(propValueI++);
    if (!propertyValue.isConstant()) {
      diagnose(astContext, sourceLoc, diag::oslog_property_not_constant,
               propDecl->getNameStr());
      errorDetected = true;
      break;
    }
  }
  return errorDetected;
}

/// Try to dead-code eliminate the OSLogMessage instance \c oslogMessage passed
/// to the os log call and clean up its dependencies. If the instance cannot be
/// eliminated, it implies that either the instance is not auto-generated or the
/// implementation of the os log overlay is incorrect. Therefore emit
/// diagnostics in such cases.
static void tryEliminateOSLogMessage(SingleValueInstruction *oslogMessage) {
  // Collect the set of root instructions that could be dead due to constant
  // folding. These include the oslogMessage initialzer call and its transitive
  // users.
  SmallVector<SILInstruction *, 8> oslogMessageUsers;
  getTransitiveUsers(oslogMessage, oslogMessageUsers);

  InstructionDeleter deleter;
  for (SILInstruction *user : oslogMessageUsers)
    deleter.trackIfDead(user);
  deleter.trackIfDead(oslogMessage);

  bool isOSLogMessageDead = false;
  deleter.cleanUpDeadInstructions([&](SILInstruction *deadInst) {
    if (deadInst == oslogMessage)
      isOSLogMessageDead = true;
  });
  // At this point, the OSLogMessage instance must be deleted if
  // the overlay implementation (or its extensions by users) is correct.
  if (!isOSLogMessageDead) {
    SILFunction *fun = oslogMessage->getFunction();
    diagnose(fun->getASTContext(), oslogMessage->getLoc().getSourceLoc(),
             diag::oslog_message_alive_after_opts);
  }
}

/// Constant evaluate instructions starting from 'start' and fold the uses
/// of the value 'oslogMessage'. Stop when oslogMessageValue is released.
static bool constantFold(SILInstruction *start,
                         SingleValueInstruction *oslogMessage,
                         unsigned assertConfig) {
  SILFunction *fun = start->getFunction();
  assert(fun->hasOwnership() && "function not in ownership SIL");

  // Initialize fold state.
  SmallVector<SILInstruction *, 2> endUsersOfOSLogMessage;
  getEndPointsOfDataDependentChain(oslogMessage, fun, endUsersOfOSLogMessage);
  assert(!endUsersOfOSLogMessage.empty());

  FoldState state(fun, assertConfig, start, endUsersOfOSLogMessage);

  auto errorInfo = collectConstants(state);
  if (errorInfo) // Evaluation failed with diagnostics.
    return false;

  // At this point, the `OSLogMessage` instance should be mapped to a constant
  // value in the interpreter state. If this is not the case, it means the
  // overlay implementation of OSLogMessage (or its extensions by users) are
  // incorrect. Detect and diagnose this scenario.
  bool errorDetected = checkOSLogMessageIsConstant(oslogMessage, state);
  if (errorDetected)
    return false;

  substituteConstants(state);

  tryEliminateOSLogMessage(oslogMessage);
  return true;
}

/// Given a call to the initializer of OSLogMessage, which conforms to
/// 'ExpressibleByStringInterpolation', find the first instruction, if any, that
/// marks the begining of the string interpolation that is used to create an
/// OSLogMessage instance. This function traverses the backward data-dependence
/// chain of the given OSLogMessage initializer: \p oslogInit. As a special case
/// it avoids chasing the data-dependencies from the captured values of
/// partial-apply instructions, as a partial apply instruction is considered as
/// a constant regardless of the constantness of its captures.
static SILInstruction *beginOfInterpolation(ApplyInst *oslogInit) {
  auto oslogInitCallSite = FullApplySite(oslogInit);
  SILFunction *callee = oslogInitCallSite.getCalleeFunction();

  assert (callee->hasSemanticsAttrThatStartsWith("oslog.message.init"));
  // The initializer must return the OSLogMessage instance directly.
  assert(oslogInitCallSite.getNumArguments() >= 1 &&
         oslogInitCallSite.getNumIndirectSILResults() == 0);

  // List of backward dependencies that needs to be analyzed.
  SmallVector<SILInstruction *, 4> worklist = { oslogInit };
  SmallPtrSet<SILInstruction *, 4> seenInstructions = { oslogInit };
  // List of instructions that could potentially mark the beginning of the
  // interpolation.
  SmallPtrSet<SILInstruction *, 4> candidateStartInstructions;

  unsigned i = 0;
  while (i < worklist.size()) {
    SILInstruction *inst = worklist[i++];

    if (isa<PartialApplyInst>(inst)) {
      // Partial applies are used to capture the dynamic arguments passed to
      // the string interpolation. Their arguments are not required to be
      // known at compile time and they need not be constant evaluated.
      // Therefore, follow only the dependency chain along function ref operand.
      SILInstruction *definingInstruction =
          inst->getOperand(0)->getDefiningInstruction();
      assert(definingInstruction && "no function-ref operand in partial-apply");
      if (seenInstructions.insert(definingInstruction).second) {
        worklist.push_back(definingInstruction);
        candidateStartInstructions.insert(definingInstruction);
      }
      continue;
    }

    for (Operand &operand : inst->getAllOperands()) {
      if (SILInstruction *definingInstruction =
            operand.get()->getDefiningInstruction()) {
        if (seenInstructions.count(definingInstruction))
          continue;
        worklist.push_back(definingInstruction);
        seenInstructions.insert(definingInstruction);
        candidateStartInstructions.insert(definingInstruction);
      }
      // If there is no definining instruction for this operand, it could be a
      // basic block or function parameter. Such operands are not considered
      // in the backward slice. Dependencies through them are safe to ignore
      // in this context.
    }

    // If the instruction: `inst` has an operand, its definition should precede
    // `inst` in the control-flow order. Therefore, remove `inst` from the
    // candidate start instructions.
    if (inst->getNumOperands() > 0) {
      candidateStartInstructions.erase(inst);
    }

    if (!isa<AllocStackInst>(inst)) {
      continue;
    }

    // If we have an alloc_stack instruction, include stores into it into the
    // backward dependency list. However, whether alloc_stack precedes the
    // definitions of values stored into the location in the control-flow order
    // can only be determined by traversing the instrutions in the control-flow
    // order.
    AllocStackInst *allocStackInst = cast<AllocStackInst>(inst);
    for (StoreInst *storeInst : allocStackInst->getUsersOfType<StoreInst>()) {
      worklist.push_back(storeInst);
      candidateStartInstructions.insert(storeInst);
    }
  }

  // Find the first basic block in the control-flow order. Typically, if
  // formatting and privacy options are literals, all candidate instructions
  // must be in the same basic block. But, this code doesn't rely on that
  // assumption.
  SmallPtrSet<SILBasicBlock *, 4> candidateBBs;
  for (auto *candidate: candidateStartInstructions) {
    SILBasicBlock *candidateBB = candidate->getParent();
    candidateBBs.insert(candidateBB);
  }

  SILBasicBlock *firstBB = nullptr;
  SILBasicBlock *entryBB = oslogInit->getFunction()->getEntryBlock();
  for (SILBasicBlock *bb: llvm::breadth_first<SILBasicBlock *>(entryBB)) {
    if (candidateBBs.count(bb)) {
      firstBB = bb;
      break;
    }
  }
  assert(firstBB);

  // Iterate over the instructions in the firstBB and find the instruction that
  // starts the interpolation.
  SILInstruction *startInst = nullptr;
  for (SILInstruction &inst : *firstBB) {
    if (candidateStartInstructions.count(&inst)) {
      startInst = &inst;
      break;
    }
  }
  assert(startInst);
  return startInst;
}

/// If the SILInstruction is an initialization of OSLogMessage, return the
/// initialization call as an ApplyInst. Otherwise, return nullptr.
static ApplyInst *getAsOSLogMessageInit(SILInstruction *inst) {
  auto *applyInst = dyn_cast<ApplyInst>(inst);
  if (!applyInst) {
    return nullptr;
  }

  SILFunction *callee = applyInst->getCalleeFunction();
  if (!callee ||
      !callee->hasSemanticsAttrThatStartsWith("oslog.message.init")) {
    return nullptr;
  }

  // Default argument generators created for a function also inherit
  // the semantics attribute of the function. Therefore, check that there are
  // at least two operands for this apply instruction.
  if (applyInst->getNumOperands() > 1) {
    return applyInst;
  }
  return nullptr;
}

/// Return true iff the SIL function \c fun is a method of the \c OSLogMessage
/// type.
bool isMethodOfOSLogMessage(SILFunction &fun) {
  DeclContext *declContext = fun.getDeclContext();
  if (!declContext)
    return false;
  Decl *decl = declContext->getAsDecl();
  if (!decl)
    return false;
  ConstructorDecl *ctor = dyn_cast<ConstructorDecl>(decl);
  if (!ctor)
    return false;
  DeclContext *parentContext = ctor->getParent();
  if (!parentContext)
    return false;
  NominalTypeDecl *typeDecl = parentContext->getSelfNominalTypeDecl();
  if (!typeDecl)
    return false;
  return typeDecl->getName() == fun.getASTContext().Id_OSLogMessage;
}

class OSLogOptimization : public SILFunctionTransform {

  ~OSLogOptimization() override {}

  /// The entry point to the transformation.
  void run() override {
    auto &fun = *getFunction();
    unsigned assertConfig = getOptions().AssertConfig;

    // Don't rerun optimization on deserialized functions or stdlib functions.
    if (fun.wasDeserializedCanonical()) {
      return;
    }

    // Skip methods of OSLogMessage type. This avoid unnecessary work and also
    // avoids falsely diagnosing the auto-generated (transparent) witness method
    // of OSLogMessage, which ends up invoking the OSLogMessage initializer:
    // "oslog.message.init_interpolation" without an interpolated string
    // literal that is expected by this pass.
    if (isMethodOfOSLogMessage(fun)) {
      return;
    }

    // Collect all 'OSLogMessage.init' in the function. 'OSLogMessage' is a
    // custom string interpolation type used by the new OS log APIs.
    SmallVector<ApplyInst *, 4> oslogMessageInits;
    for (auto &bb : fun) {
      for (auto &inst : bb) {
        auto init = getAsOSLogMessageInit(&inst);
        if (!init)
          continue;
        oslogMessageInits.push_back(init);
      }
    }

    bool madeChange = false;

    // Constant fold the uses of properties of OSLogMessage instance. Note that
    // the function body will change due to constant folding, after each
    // iteration.
    for (auto *oslogInit : oslogMessageInits) {
      SILInstruction *interpolationStart = beginOfInterpolation(oslogInit);
      assert(interpolationStart);
      madeChange |= constantFold(interpolationStart, oslogInit, assertConfig);
    }

    // TODO: Can we be more conservative here with our invalidation?
    if (madeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createOSLogOptimization() {
  return new OSLogOptimization();
}
