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
/// Pass Dependencies: MandatoryInlining. This pass also uses
/// `ConstExprStepEvaluator` defined in `Utils/ConstExpr.cpp`.
///
/// Algorithm Overview:
///
/// This pass implements a function-level transformation that collects calls
/// to the os log APIs, which are annotated with an @_semantics attribute,
/// and performs the following steps on each such call.
///
///  1. Determines the range of instructions to constant evaluate.
///    The range starts from the first instruction that corresponds to the
///     construction of the custom string interpolation to the last instruction
///     of the body of the log call. The log call is also inlined into the
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
/// step 1 to 4. The functions 'OSLogOptimization::inlineLogCallAndGetLast' and
/// 'beginOfInterpolation' implement step 1 i.e., they compute the range of
///  instructions to evaluate. The functions 'constantFold' is a driver for the
/// steps 2 to 4. Step 2 is implemented by the function 'collectConstants',
/// step 3 by 'detectAndDiagnoseErrors', and step 4 by 'substituteConstants' and
/// 'emitCodeForSymbolicValue'. The remaining functions in the file implement
/// the subtasks and utilities needed by the above functions.

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/SubstitutionMap.h"
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

private:
  /// Single-valued instructions that were found to be constants during
  /// constant evaluation.
  SmallVector<SingleValueInstruction *, 4> constantValuedInstructions;

public:
  FoldState(SILFunction *fun) : constantEvaluator(allocator, fun) {}

  void addConstantInstruction(SingleValueInstruction *value) {
    constantValuedInstructions.push_back(value);
  }

  ArrayRef<SingleValueInstruction *> getConstantInstructions() {
    return ArrayRef<SingleValueInstruction *>(constantValuedInstructions);
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
         calleeFun->hasSemanticsAttrThatStartsWith("oslog.");
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
static bool isInstructionFoldable(SingleValueInstruction *inst) {
  ASTContext &astContext = inst->getFunction()->getASTContext();
  ;
  SILType silType = inst->getType();

  return (!silType.isAddress() && !isa<LiteralInst>(inst) &&
          !isa<StructInst>(inst) && !getStringMakeUTF8Init(inst) &&
          isIntegerOrStringType(silType, astContext));
}

/// Constant evaluate the instructions in the range 'first' to 'last'.
/// Add foldable, constant-valued instructions discovered during the evaluation
/// to the 'foldState' passed in.
/// \returns error information for emitting diagnostics if the evaluation
/// failed.
static Optional<SymbolicValue> collectConstants(SILBasicBlock::iterator first,
                                                SILBasicBlock::iterator last,
                                                FoldState &foldState) {

  ConstExprStepEvaluator &constantEvaluator = foldState.constantEvaluator;

  for (auto currI = first; currI != last;) {
    SILInstruction *currInst = &(*currI);

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

    // If the instruction is foldable and if we found a constant value for
    // the result of the instruction, record it.
    auto *singleValInst = dyn_cast<SingleValueInstruction>(currInst);
    if (!singleValInst || !isInstructionFoldable(singleValInst)) {
      continue;
    }

    Optional<SymbolicValue> constantVal =
        constantEvaluator.lookupConstValue(singleValInst);
    if (constantVal.hasValue()) {
      foldState.addConstantInstruction(singleValInst);
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

/// Given a fold state with constant-valued instructions, substitute the
/// instructions with the constant values. The constant values could be strings
/// or Stdlib integer-struct values or builtin integers.
static void substituteConstants(FoldState &foldState) {

  ConstExprStepEvaluator &evaluator = foldState.constantEvaluator;

  SmallVector<SILInstruction *, 4> deletedInsts;
  for (SingleValueInstruction *constantInst :
       foldState.getConstantInstructions()) {
    SymbolicValue constantVal =
        evaluator.lookupConstValue(constantInst).getValue();

    SILBuilderWithScope builder(constantInst);
    SILLocation loc = constantInst->getLoc();
    SILType instType = constantInst->getType();
    SILValue foldedSILVal = emitCodeForSymbolicValue(
        constantVal, instType, builder, loc, foldState.stringInfo);

    constantInst->replaceAllUsesWith(foldedSILVal);
    deletedInsts.push_back(constantInst);
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
                                    SingleValueInstruction *osLogMessageAddr,
                                    FoldState &foldState) {
  ConstExprStepEvaluator &constantEvaluator = foldState.constantEvaluator;
  SILLocation loc = osLogMessageAddr->getLoc();
  SourceLoc sourceLoc = loc.getSourceLoc();
  SILFunction *fn = osLogMessageAddr->getFunction();
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
  Optional<SymbolicValue> osLogMessageAddrValueOpt =
      constantEvaluator.lookupConstValue(osLogMessageAddr);
  assert(osLogMessageAddrValueOpt.hasValue() &&
         osLogMessageAddrValueOpt->getKind() == SymbolicValue::Address);

  SmallVector<unsigned, 2> accessPath;
  SymbolicValue osLogMessageValue =
      osLogMessageAddrValueOpt->getAddressValue(accessPath)->getValue();
  if (!osLogMessageValue.isConstant()) {
    diagnose(astContext, sourceLoc, diag::oslog_non_constant_message);
    return true;
  }

  SymbolicValue osLogInterpolationValue =
      osLogMessageValue.lookThroughSingleElementAggregates();
  if (!osLogInterpolationValue.isConstant()) {
    diagnose(astContext, sourceLoc, diag::oslog_non_constant_interpolation);
    return true;
  }

  // Check if every proprety of the OSLogInterpolation instance that is a
  // string or integer has a constant value. If this is violated this could
  // be an indication of an error in the usage of the API. Diagnostics emitted
  // here are for the users of the os log APIs.
  SILType osLogMessageType = osLogMessageAddr->getType();
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

/// Given a range of instructions from `first` to `last`, fold the constants
/// that could be discovered by constant evaluating the instructions.
static void constantFold(SILBasicBlock::iterator first,
                         SILBasicBlock::iterator last) {

  SILInstruction *firstInst = &(*first);

  // Initialize fold state.
  FoldState state(firstInst->getFunction());

  auto errorInfo = collectConstants(first, last, state);

  // At this point, the `OSLogMessage` instance should be mapped to a symbolic
  // value in the interpreter state. Furthermore, its format string and
  // interger-valued fields (other than `OSLogArguments`) must be constants.
  // If this is not the case, it means the formatting options or privacy
  // qualifiers provided by the user were not inferred as compile-time
  // constants. Detect and diagnose this scenario.
  assert(isa<SingleValueInstruction>(firstInst));
  bool errorDetected = detectAndDiagnoseErrors(
      errorInfo, dyn_cast<SingleValueInstruction>(firstInst), state);
  if (errorDetected)
    return;

  substituteConstants(state);
}

/// Find an argument of type OSLogMessage from the given os log call.
static SILValue findOSLogMessageArgument(ApplyInst *oslogCall) {
  ASTContext &astContext = oslogCall->getFunction()->getASTContext();

  for (auto argument : oslogCall->getArguments()) {
    SILType argumentType = argument->getType();
    auto *structDecl = argumentType.getStructOrBoundGenericStruct();
    if (!structDecl)
      continue;

    if (structDecl->getName() == astContext.Id_OSLogMessage) {
      return argument;
    }
  }
  return SILValue();
}

/// Given an os log call that is passed a string interpolation,
/// find the first instruction that marks the begining of the string
/// interpolation. Normally, this instruction is the alloc_stack of the
/// string interpolation type, which in this case if 'OSLogMessage'.
/// Constant evaluation and folding must begin from this instruction.
static Optional<SILBasicBlock::iterator>
beginOfInterpolation(ApplyInst *oslogCall) {
  SILFunction *caller = oslogCall->getFunction();

  // Find an argument to the call with name 'OSLogMessage'. Since the log
  // function definition may change/shuffle its arguments and also since mock
  // test helpers for log functions do exist, do not rely on the position of
  // the argument and instead look for an argument with a name.
  auto &astContext = caller->getASTContext();
  auto oslogMessageArgument = findOSLogMessageArgument(oslogCall);
  assert(oslogMessageArgument && "No argument of type 'OSLogMessage' in the "
                                 "os log call");

  auto *stringInterpolAllocInst =
      oslogMessageArgument->getDefiningInstruction();
  if (!stringInterpolAllocInst ||
      !isa<AllocStackInst>(stringInterpolAllocInst)) {
    diagnose(astContext, oslogCall->getLoc().getSourceLoc(),
             diag::oslog_dynamic_message);
    return None;
  }

  // Assumption: the alloc_stack of the string interpolation type 'OSLogMessage'
  // must precede every other instruction relevant to the string interpolation.
  return stringInterpolAllocInst->getIterator();
}

/// If the SILInstruction is a call to an os log function, return the call
/// as an ApplyInst. Otherwise, return nullptr.
static ApplyInst *getAsOSLogCall(SILInstruction *inst) {
  auto *applyInst = dyn_cast<ApplyInst>(inst);
  if (!applyInst) {
    return nullptr;
  }

  SILFunction *callee = applyInst->getCalleeFunction();
  if (!callee || !callee->hasSemanticsAttrThatStartsWith("oslog.log")) {
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

  /// Inline the given os log Call, by loading and linking the callee if it has
  /// not been loaded yet, and return the last inlined instruction.
  /// \param oslogCall log call instruction to inline.
  /// \returns last instruction of the callee after inlining.
  SILBasicBlock::iterator inlineLogCallAndGetLast(ApplyInst *oslogCall) {
    SILFunction *callee = oslogCall->getReferencedFunctionOrNull();
    assert(callee);

    // Load and link the called os log function before inlining. This is needed
    // to link shared functions that are used in the callee body.
    if (callee->isExternalDeclaration()) {
      callee->getModule().loadFunction(callee);
      assert(!callee->isExternalDeclaration());
      oslogCall->getFunction()->getModule().linkFunction(
          callee, SILModule::LinkingMode::LinkNormal);
    }

    // Inline the log call into the caller and find the last instruction of the
    // log call.
    SILOptFunctionBuilder funcBuilder(*this);
    SILBasicBlock *lastBB =
        SILInliner::inlineFullApply(
            oslogCall, SILInliner::InlineKind::PerformanceInline, funcBuilder)
            .second;
    return lastBB->end();
  }

  /// The entry point to the transformation.
  void run() override {
    // Don't rerun optimization on deserialized functions or stdlib functions.
    if (getFunction()->wasDeserializedCanonical()) {
      return;
    }

    auto &fun = *getFunction();

    // Collect all os log calls in the function.
    SmallVector<ApplyInst *, 4> oslogCalls;
    for (auto &bb : fun) {
      for (auto &inst : bb) {
        auto oslogCall = getAsOSLogCall(&inst);
        if (!oslogCall)
          continue;
        oslogCalls.push_back(oslogCall);
      }
    }

    // Optimize each os log call found. Optimizing a call will change the
    // function body by inlining functions and folding constants.
    for (auto *oslogCall : oslogCalls) {

      // Find the range of instructions that have to be constant evaluated
      // and folded. The relevant instructions start at the begining of the
      // string interpolation passed to the log call and end at the last
      // instruction of the body of the called os log function (which will be
      // inlined).
      Optional<SILBasicBlock::iterator> firstI =
          beginOfInterpolation(oslogCall);
      if (!firstI)
        continue;

      SILBasicBlock::iterator lastI = inlineLogCallAndGetLast(oslogCall);
      constantFold(firstI.getValue(), lastI);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createOSLogOptimization() {
  return new OSLogOptimization();
}
