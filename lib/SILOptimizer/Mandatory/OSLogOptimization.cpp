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
///     The range starts from the first SIL instruction that corresponds to the
///     construction of the custom string interpolation type: OSLogMessage to
///     its destruction. The log call is also inlined into the
///     caller.
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
/// steps 1 to 4. The function 'beginOfInterpolation' implements step 1. It
/// computes the instruction from which the evaluation must start. The function
/// 'constantFold' is a driver for the  steps 2 to 4. Step 2 is implemented by
/// the function 'collectConstants', step 3 by 'detectAndDiagnoseErrors', and
/// step 4 by 'substituteConstants' and 'emitCodeForSymbolicValue'.
/// The remaining functions in the file implement the subtasks and utilities
/// needed by the above functions.

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/OptimizationMode.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/Demangler.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/ConstExpr.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/ADT/MapVector.h"

using namespace swift;

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
  if (!callee || !callee->hasSemanticsAttr("string.makeUTF8"))
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

  /// Instructions that mark the end points of folding. No folded SIL value must
  /// be usable beyond these instructions (in the control-flow order). These
  /// instructions are also used to emit destory instructions for non-trivial,
  /// SIL values emitted during folding.
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

/// Return true if and only if the given SIL type represents a String or
/// a Stdlib or builtin integer type.
static bool isIntegerOrStringType(SILType silType, ASTContext &astContext) {
  if (silType.is<BuiltinIntegerType>()) {
    return true;
  }

  NominalTypeDecl *nominalDecl = silType.getNominalOrBoundGenericNominal();
  if (!nominalDecl) {
    return false;
  }

  return (nominalDecl == astContext.getStringDecl()) ||
         isStdlibIntegerOrBoolDecl(nominalDecl, astContext);
}

/// Decide if the given instruction (which could possibly be a call) should
/// be constant evaluated.
///
/// \returns true iff the given instruction is not a call or if it is, it calls
/// a known string operation, such as concat/append etc., or calls an os log
/// overlay function annotated with a semantics attribute.
static bool shouldAttemptEvaluation(SILInstruction *inst) {
  auto *apply = dyn_cast<ApplyInst>(inst);
  if (!apply)
    return true;

  SILFunction *calleeFun = apply->getCalleeFunction();
  if (!calleeFun)
    return false;

  return calleeFun->hasSemanticsAttrThatStartsWith("string.") ||
         calleeFun->hasSemanticsAttr("constant_evaluable");
}

/// Skip or evaluate the given instruction based on the evaluation policy and
/// handle errors. The policy is to evaluate all non-apply instructions as well
/// as apply instructions that either invoke a known string operation or an os
/// log specific function that constructs compile-time constants
/// (like format string). Every other function call is skipped.
/// This includes calls that manipulate runtime values such as the arguments
/// (i.e, interpolated expressions) or the raw byte buffer.
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

/// Check whether a single-valued instruction is foldable. String or integer
/// valued instructions are foldable with the exceptions:
///   - Addresses-valued instructions cannot be folded.
///   - Literal instruction need not be folded.
///   - "String.makeUTF8" instrinsic initializer need not be folded as it is
///     used only on string literals.
///   - StructInst cannot be folded. We can only fold its arguments and not the
///     instruction itself.
static bool isSILValueFoldable(SILValue value) {
  SILInstruction *definingInst = value->getDefiningInstruction();
  if (!definingInst)
    return false;

  ASTContext &astContext = definingInst->getFunction()->getASTContext();
  SILType silType = value->getType();

  return (!silType.isAddress() && !isa<LiteralInst>(definingInst) &&
          !isa<StructInst>(definingInst) &&
          !getStringMakeUTF8Init(definingInst) &&
          isIntegerOrStringType(silType, astContext));
}

/// Given a 'foldState', constant evaluate instructions from
/// 'foldState.beginInstruction' until an instruction in
/// 'foldState.endInstructions' is seen. Add foldable, constant-valued
/// instructions discovered during the evaluation to
/// 'foldState.constantSILValues'.
/// \returns error information for emitting diagnostics if the evaluation
/// failed.
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
    if (!nextI) {
      return errorInfo;
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
                                         SILType &expectedType,
                                         SILBuilder &builder, SILLocation &loc,
                                         StringSILInfo &stringInfo) {
  ASTContext &astContext = expectedType.getASTContext();

  switch (symVal.getKind()) {
  case SymbolicValue::String: {
    assert(astContext.getStringDecl() ==
           expectedType.getNominalOrBoundGenericNominal());

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
    assert(expectedType.is<BuiltinIntegerType>());

    IntegerLiteralInst *intLiteralInst =
        builder.createIntegerLiteral(loc, expectedType, resInt);
    return intLiteralInst;
  }
  case SymbolicValue::Aggregate: {
    // Support only stdlib integer or bool structs.
    StructDecl *structDecl = expectedType.getStructOrBoundGenericStruct();
    assert(structDecl);
    assert(isStdlibIntegerOrBoolDecl(structDecl, astContext));

    VarDecl *propertyDecl = structDecl->getStoredProperties().front();
    SILType propertyType =
        expectedType.getFieldType(propertyDecl, builder.getModule());
    SymbolicValue propertyVal = symVal.lookThroughSingleElementAggregates();
    SILValue newPropertySIL = emitCodeForSymbolicValue(
        propertyVal, propertyType, builder, loc, stringInfo);
    StructInst *newStructInst = builder.createStruct(
        loc, expectedType, ArrayRef<SILValue>(newPropertySIL));
    return newStructInst;
  }
  default: {
    llvm_unreachable("Symbolic value kind is not supported");
  }
  }
}

/// Collect the end-of-lifetime instructions of the given SILValue. These are
/// either release_value or destroy_value instructions.
/// \param value SIL value whose end-of-lifetime instructions must be collected.
/// \param lifetimeEndInsts buffer for storing the found end-of-lifetime
/// instructions of 'value'.
static void getLifetimeEndInstructionsOfSILValue(
    SILValue value, SmallVectorImpl<SILInstruction *> &lifetimeEndInsts) {

  bool continueLifetimeEndInstructionSearch = true;
  SILValue currValue = value;

  while (continueLifetimeEndInstructionSearch) {
    continueLifetimeEndInstructionSearch = false;

    for (Operand *use : currValue->getUses()) {
      SILInstruction *user = use->getUser();

      if (isa<ReleaseValueInst>(user) || isa<DestroyValueInst>(user)) {
        lifetimeEndInsts.push_back(user);
        continue;
      }

      if (isa<CopyValueInst>(user)) {
        auto *copyValueInst = cast<CopyValueInst>(user);
        // Continue looking for the end-of-lifetime instruction for the
        // result of copy_value.
        currValue = copyValueInst;
        continueLifetimeEndInstructionSearch = true;
      }
    }
  }
}

/// Emit instructions to destroy the folded value at the end of its use, if
/// required. Since this pass folds only integers or strings and since the
/// former is a trivial type, we only have to destroy strings that are folded.
/// For strings, a release_value (or a destory_value instruction in ownership
/// SIL) has to be emitted if it is not already present.
static void
destroyFoldedValueAtEndOfUse(SILValue foldedVal, SILValue originalVal,
                             ArrayRef<SILInstruction *> endOfUseInsts,
                             SILFunction *fun) {
  // Folded value should have either trivial or owned ownership as it is an
  // integer or string constant.
  assert(foldedVal.getOwnershipKind() == ValueOwnershipKind::Any ||
         foldedVal.getOwnershipKind() == ValueOwnershipKind::Owned);

  // If the ownership kinds of folded and original values are both either
  // owned or trivial, there is nothing to do.
  if (foldedVal.getOwnershipKind() == originalVal.getOwnershipKind()) {
    return;
  }
  assert(originalVal.getOwnershipKind() == ValueOwnershipKind::Guaranteed);

  // Here, the original value may be at +0 and hence may not be released.
  // However, the folded value should always be released.
  SmallVector<SILInstruction *, 2> lifeTimeEndInstsOfOriginal;
  getLifetimeEndInstructionsOfSILValue(originalVal, lifeTimeEndInstsOfOriginal);

  if (!lifeTimeEndInstsOfOriginal.empty()) {
    // Here, the original value is released, and so would be the folded value.
    return;
  }

  // Here, the original value is not released. Release the folded value at the
  // 'endOfUse' instructions passed as parameter.
  bool hasOwnership = fun->hasOwnership();
  for (SILInstruction *endInst : endOfUseInsts) {
    SILBuilderWithScope builder(endInst);
    if (hasOwnership) {
      builder.createDestroyValue(endInst->getLoc(), foldedVal);
    } else {
      builder.createReleaseValue(endInst->getLoc(), foldedVal,
                                 builder.getDefaultAtomicity());
    }
  }
}

/// Given a fold state with constant-valued instructions, substitute the
/// instructions with the constant values. The constant values could be strings
/// or Stdlib integer-struct values or builtin integers.
static void substituteConstants(FoldState &foldState) {

  ConstExprStepEvaluator &evaluator = foldState.constantEvaluator;
  SmallVector<SILInstruction *, 4> deletedInsts;
  auto endOfUseInsts = ArrayRef<SILInstruction *>(
      foldState.endInstructions.begin(), foldState.endInstructions.end());

  for (SILValue constantSILValue : foldState.getConstantSILValues()) {
    SymbolicValue constantSymbolicVal =
        evaluator.lookupConstValue(constantSILValue).getValue();

    SILInstruction *definingInst = constantSILValue->getDefiningInstruction();
    assert(definingInst);

    SILBuilderWithScope builder(definingInst);
    SILLocation loc = definingInst->getLoc();
    SILType instType = constantSILValue->getType();
    SILValue foldedSILVal = emitCodeForSymbolicValue(
        constantSymbolicVal, instType, builder, loc, foldState.stringInfo);

    // Add an instruction to end the lifetime of the foldedSILVal, if necessary.
    destroyFoldedValueAtEndOfUse(foldedSILVal, constantSILValue, endOfUseInsts,
                                 definingInst->getFunction());

    constantSILValue->replaceAllUsesWith(foldedSILVal);

    if (isa<SingleValueInstruction>(definingInst)) {
      deletedInsts.push_back(definingInst);
    } // Otherwise, be conservative and do not delete the instruction as other
    // results of the instruction could be used.
  }

  recursivelyDeleteTriviallyDeadInstructions(deletedInsts, true,
                                             [&](SILInstruction *DeadI) {});
}

/// Detect and emit diagnostics for errors found during evaluation. Errors
/// can happen due to incorrect implementation of the os log API in the
/// overlay or due to incorrect use of the os log API.
/// TODO: some of the checks here would be made redundant by a dedicated
/// diagnostics check that will happen before the optimization starts.
static bool detectAndDiagnoseErrors(Optional<SymbolicValue> errorInfo,
                                    SingleValueInstruction *osLogMessage,
                                    FoldState &foldState) {
  ConstExprStepEvaluator &constantEvaluator = foldState.constantEvaluator;
  SILLocation loc = osLogMessage->getLoc();
  SourceLoc sourceLoc = loc.getSourceLoc();
  SILFunction *fn = osLogMessage->getFunction();
  SILModule &module = fn->getModule();
  ASTContext &astContext = fn->getASTContext();
  bool errorDetected = false;

  // If we have errorInfo that indicates a fail-stop error, diagnose it.
  if (errorInfo && constantEvaluator.isFailStopError(*errorInfo)) {
    assert(errorInfo->getKind() == SymbolicValue::Unknown);
    diagnose(astContext, sourceLoc, diag::oslog_const_evaluation_error);
    errorInfo->emitUnknownDiagnosticNotes(loc);
    errorDetected = true;
  }

  // Check if the OSLogMessage and OSLogInterpolation instances are correctly
  // inferred as constants. If not, it implies incorrect implementation
  // of the os log API in the overlay. Diagnostics here are for os log
  // library authors.
  Optional<SymbolicValue> osLogMessageValueOpt =
      constantEvaluator.lookupConstValue(osLogMessage);
  if (!osLogMessageValueOpt ||
      osLogMessageValueOpt->getKind() != SymbolicValue::Aggregate) {
    diagnose(astContext, sourceLoc, diag::oslog_non_constant_message);
    return true;
  }

  SymbolicValue osLogInterpolationValue =
      osLogMessageValueOpt->lookThroughSingleElementAggregates();
  if (!osLogInterpolationValue.isConstant()) {
    diagnose(astContext, sourceLoc, diag::oslog_non_constant_interpolation);
    return true;
  }

  // Check if every proprety of the OSLogInterpolation instance that is a
  // string or integer has a constant value. If this is violated this could
  // be an indication of an error in the usage of the API. Diagnostics emitted
  // here are for the users of the os log APIs.
  SILType osLogMessageType = osLogMessage->getType();
  StructDecl *structDecl = osLogMessageType.getStructOrBoundGenericStruct();
  assert(structDecl);

  VarDecl *interpolationPropDecl = structDecl->getStoredProperties().front();
  SILType osLogInterpolationType =
      osLogMessageType.getFieldType(interpolationPropDecl, module);
  StructDecl *interpolationStruct =
      osLogInterpolationType.getStructOrBoundGenericStruct();
  assert(interpolationStruct);

  auto propertyDecls = interpolationStruct->getStoredProperties();
  ArrayRef<SymbolicValue> propertyValues =
      osLogInterpolationValue.getAggregateValue();
  auto propValueI = propertyValues.begin();

  for (auto *propDecl : propertyDecls) {
    SymbolicValue propertyValue = *(propValueI++);
    if (propertyValue.isConstant()) {
      continue;
    }

    if (!isIntegerOrStringType(
            osLogInterpolationType.getFieldType(propDecl, module),
            astContext)) {
      continue;
    }

    diagnose(astContext, sourceLoc, diag::oslog_property_not_constant,
             propDecl->getNameStr());
    errorDetected = true;
    break;
  }
  return errorDetected;
}

/// Constant evaluate instructions starting from 'start' and fold the uses
/// of the value 'oslogMessage'. Stop when oslogMessageValue is released.
static void constantFold(SILInstruction *start,
                         SingleValueInstruction *oslogMessage,
                         unsigned assertConfig) {

  // Initialize fold state.
  SmallVector<SILInstruction *, 2> lifetimeEndInsts;
  getLifetimeEndInstructionsOfSILValue(oslogMessage, lifetimeEndInsts);

  FoldState state(start->getFunction(), assertConfig, start, lifetimeEndInsts);

  auto errorInfo = collectConstants(state);

  // At this point, the `OSLogMessage` instance should be mapped to a symbolic
  // value in the interpreter state. Furthermore, its format string and
  // interger-valued fields (other than `OSLogArguments`) must be constants.
  // If this is not the case, it means the formatting options or privacy
  // qualifiers provided by the user were not inferred as compile-time
  // constants. Detect and diagnose this scenario.
  bool errorDetected = detectAndDiagnoseErrors(errorInfo, oslogMessage, state);
  if (errorDetected)
    return;

  substituteConstants(state);
}

/// Given a call to the initializer of OSLogMessage, which conforms to
/// 'ExpressibleByStringInterpolation', find the first instruction, if any,
/// that marks the begining of the string interpolation that is used to
/// create an OSLogMessage instance. Normally, this instruction is the
/// alloc_stack of the string interpolation type: 'OSLogInterpolation'.
/// Constant evaluation and folding must begin from this instruction.
static SILInstruction *beginOfInterpolation(ApplyInst *oslogInit) {
  auto oslogInitCallSite = FullApplySite(oslogInit);
  SILFunction *callee = oslogInitCallSite.getCalleeFunction();
  auto &astContext = oslogInit->getFunction()->getASTContext();

  // The initializer must return the OSLogMessage instance directly.
  assert(oslogInitCallSite.getNumArguments() >= 1 &&
         oslogInitCallSite.getNumIndirectSILResults() == 0);

  SILInstruction *firstArgumentInst =
      oslogInitCallSite.getArgument(0)->getDefiningInstruction();
  if (!firstArgumentInst) {
    // oslogInit call does not correspond to  an auto-generated initialization
    // done by the compiler on seeing a string interpolation. Ignore this.
    return nullptr;
  }

  SILInstruction *startInst = nullptr;

  // If this is an initialization from string interpolation, the first argument
  // to the  initializer is a load of an auto-generated alloc-stack of
  // OSLogInterpolation:
  //  'alloc_stack $OSLogInterpolation, var, name $interpolation'
  // If no such instruction exists, ignore this call as this is not a
  // OSLogMessage instantiation through string interpolation.
  if (callee->hasSemanticsAttr("oslog.message.init_interpolation")) {
    auto *loadInst = dyn_cast<LoadInst>(firstArgumentInst);
    if (!loadInst)
      return nullptr;

    auto *allocStackInst = dyn_cast<AllocStackInst>(loadInst->getOperand());
    if (!allocStackInst)
      return nullptr;

    Optional<SILDebugVariable> varInfo = allocStackInst->getVarInfo();
    if (!varInfo && varInfo->Name != astContext.Id_dollarInterpolation.str())
      return nullptr;

    startInst = allocStackInst;
  }

  // If this is an initialization from a string literal, the first argument
  // should be the creation of the string literal.
  if (callee->hasSemanticsAttr("oslog.message.init_stringliteral")) {
    if (!getStringMakeUTF8Init(firstArgumentInst))
      return nullptr;
    startInst = firstArgumentInst;
  }

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

    // Constant fold the uses of properties of OSLogMessage instance. Note that
    // the function body will change due to constant folding, after each
    // iteration.
    for (auto *oslogInit : oslogMessageInits) {

      // Find the first instruction from where constant evaluation and folding
      // must begin. The first instruction should precede (in the control-flow
      // order) the instructions that are generated by the compiler for
      // the string-interpolation literal that is used to instantiate
      // OSLogMessage instance.
      SILInstruction *interpolationStart = beginOfInterpolation(oslogInit);
      if (!interpolationStart) {
        // This scenario indicates an explicit initialization of OSLogMessage
        // that doesn't use a string inteprolation literal.
        // However, this is not always an error as explicit initialization is
        // used by thunk initializers auto-generated for protocol conformances.
        // TODO: the log APIs uses must be diagnosed by a separate pass
        // (possibly before mandatory inlining). The current pass should not
        // emit diagnostics but only perform optimization.
        continue;
      }

      constantFold(interpolationStart, oslogInit, assertConfig);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createOSLogOptimization() {
  return new OSLogOptimization();
}
