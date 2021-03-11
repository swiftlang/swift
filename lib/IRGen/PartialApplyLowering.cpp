//===---- PartialApplyLowering.cpp - Prepare partial_applies for IRGen ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "partial-apply-lowerer"

#include "swift/AST/ExtInfo.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/IRGen/IRGenSILPasses.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILInstructionWorklist.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"

#include "IRGenModule.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                        PartialApplyLowerer Interface
//===----------------------------------------------------------------------===//

class PartialApplyLowerer
    : public SILInstructionVisitor<PartialApplyLowerer, SILInstruction *> {

  SILFunction &function;
  irgen::IRGenModule &IGM;
  SILOptFunctionBuilder &thunkBuilder;

  SmallSILInstructionWorklist<256> worklist;
  bool madeChange;

  InstModCallbacks instModCallbacks;
  SmallVectorImpl<SILFunction *> &thunks;
  SmallVector<SILInstruction *, 16> instructionsPendingDeletion;

  SILFunction *createThunk(PartialApplyInst *instruction);

public:
  PartialApplyLowerer(SILFunction &function, irgen::IRGenModule &IGM,
                      SILOptFunctionBuilder &thunkBuilder,
                      SmallVectorImpl<SILFunction *> &thunks)
      : function(function), IGM(IGM), thunkBuilder(thunkBuilder),
        worklist("APAF"), madeChange(false),
        instModCallbacks(
            [&](SILInstruction *instruction) {
              worklist.erase(instruction);
              instructionsPendingDeletion.push_back(instruction);
            },
            [&](SILInstruction *instruction) { worklist.add(instruction); },
            [this](Operand *use, SILValue newValue) {
              use->set(newValue);
              worklist.add(use->getUser());
            }),
        thunks(thunks) {}

  bool run();

  SILInstruction *visitSILInstruction(SILInstruction *) { return nullptr; }
  SILInstruction *visitPartialApplyInst(PartialApplyInst *instruction);
};

//===----------------------------------------------------------------------===//
//                PartialApplyLowerer Utility Methods
//===----------------------------------------------------------------------===//

bool PartialApplyLowerer::run() {
  madeChange = false;

  for (auto &block : function) {
    for (auto &instruction : block) {
      worklist.add(&instruction);
    }
  }

  while (!worklist.isEmpty()) {
    auto *instruction = worklist.pop_back_val();
    if (instruction == nullptr) {
      continue;
    }

#ifndef NDEBUG
    std::string instructionDescription;
#endif
    LLVM_DEBUG(llvm::raw_string_ostream SS(instructionDescription);
               instruction->print(SS); instructionDescription = SS.str(););
    LLVM_DEBUG(llvm::dbgs()
               << "APAF: Visiting: " << instructionDescription << '\n');

    if (auto replacement = visit(instruction)) {
      worklist.replaceInstructionWithInstruction(instruction, replacement
#ifndef NDEBUG
                                                 ,
                                                 instructionDescription
#endif
      );
      madeChange = true;
    }
  }
  for (SILInstruction *instruction : instructionsPendingDeletion) {
    worklist.eraseInstFromFunction(*instruction);
    madeChange = true;
  }
  instructionsPendingDeletion.clear();

  return madeChange;
}

SILFunction *PartialApplyLowerer::createThunk(PartialApplyInst *instruction) {
  auto calleeType = instruction->getSubstCalleeType();

  llvm::SmallVector<TupleTypeElt, 2> results;
  for (auto result : calleeType->getResults()) {
    results.push_back(
        result
            .getReturnValueType(function.getModule(), calleeType,
                                function.getTypeExpansionContext())
            ->mapTypeOutOfContext());
  }
  auto resultType =
      TupleType::get(results, function.getModule().getASTContext());

  llvm::SmallVector<AnyFunctionType::Param, 8> parameters;
  for (auto parameter : calleeType->getParameters()) {
    parameters.push_back(AnyFunctionType::Param(
        parameter
            .getArgumentType(function.getModule(), calleeType,
                             function.getTypeExpansionContext())
            ->mapTypeOutOfContext(),
        Identifier()));
  }

  CanGenericSignature signature;
  switch (calleeType->getRepresentation()) {
  case swift::SILFunctionTypeRepresentation::Method:
  case swift::SILFunctionTypeRepresentation::WitnessMethod:
    signature = instruction->getOrigCalleeType()->getSubstGenericSignature();
    parameters.push_back(
        AnyFunctionType::Param(instruction->getOrigCalleeType()));
    break;
  case swift::SILFunctionTypeRepresentation::Thin:
  case swift::SILFunctionTypeRepresentation::Thick:
  case swift::SILFunctionTypeRepresentation::Block:
  case swift::SILFunctionTypeRepresentation::Closure:
  case swift::SILFunctionTypeRepresentation::ObjCMethod:
  case swift::SILFunctionTypeRepresentation::CFunctionPointer:
    if (auto *env = function.getGenericEnvironment()) {
      signature = env->getGenericSignature()->getCanonicalSignature();
    } else {
      signature = CanGenericSignature();
    }
    parameters.push_back(
        AnyFunctionType::Param(calleeType->mapTypeOutOfContext()));
    break;
  }

  AnyFunctionType *ty;
  if (signature) {
    ty = GenericFunctionType::get(signature, parameters, resultType);
  } else {
    ty = FunctionType::get(parameters, resultType);
  }
  auto thunkTypeOriginal =
      IGM.getLoweredType(ty).castTo<SILFunctionType>()->getWithExtInfo(
          SILExtInfoBuilder()
              .withAsync(true)
              .withRepresentation(SILFunctionTypeRepresentation::Thin)
              .build());

  llvm::SmallVector<SILParameterInfo, 2> parameterInfos;
  auto originalThunkParameters = thunkTypeOriginal->getParameters();
  auto calleeParameters = calleeType->getParameters();
  for (auto index : indices(originalThunkParameters)) {
    auto originalThunkParameter = originalThunkParameters[index];
    if (index < calleeParameters.size()) {
      auto calleeParameter = calleeParameters[index];
      SILParameterInfo info =
          SILParameterInfo(originalThunkParameter.getInterfaceType(),
                           calleeParameter.getConvention(),
                           originalThunkParameter.getDifferentiability());
      parameterInfos.push_back(info);
    } else {
      parameterInfos.push_back(originalThunkParameter);
    }
  }

  // FIXME_NOW: Find a way to combine these two definitions of thunkType and
  //            eliminate thunkTypeOriginal.
  auto thunkType = SILFunctionType::get(
      thunkTypeOriginal->getInvocationGenericSignature().getPointer(),
      thunkTypeOriginal->getExtInfo(), thunkTypeOriginal->getCoroutineKind(),
      thunkTypeOriginal->getCalleeConvention(), parameterInfos,
      thunkTypeOriginal->getYields(), thunkTypeOriginal->getResults(),
      thunkTypeOriginal->hasErrorResult()
          ? Optional<SILResultInfo>(thunkTypeOriginal->getErrorResult())
          : llvm::None,
      thunkTypeOriginal->getPatternSubstitutions(),
      thunkTypeOriginal->getInvocationSubstitutions(), function.getASTContext(),
      thunkTypeOriginal->getWitnessMethodConformanceOrInvalid());

  std::string name(function.getName());
  name.append("Tw");
  if (thunks.size() > 0) {
    name.append(".");
    name.append(std::to_string(thunks.size()));
  }

  SILLocation location = RegularLocation::getAutoGeneratedLocation();
  auto *thunk = thunkBuilder.createFunction(
      /*linkage=*/SILLinkage::Private, /*name=*/name,
      /*type=*/thunkType,
      // FIXME_NOW: This is not correct for methods.
      /*genericEnv=*/function.getGenericEnvironment(), /*loc*/ llvm::None,
      /*isBareSILFunction=*/IsNotBare, /*isTransparent=*/IsNotTransparent,
      /*isSerialized=*/IsNotSerialized, /*isDynamic=*/IsNotDynamic,
      ProfileCounter(),
      /*isThunk=*/IsThunk, /*subclassScope=*/SubclassScope::NotApplicable,
      /*inlineStrategy=*/InlineDefault,
      /*effectsKind=*/EffectsKind::Unspecified,
      /*InsertBefore=*/nullptr, function.getDebugScope());

  thunk->setDebugScope(new (function.getModule())
                           SILDebugScope(function.getLocation(), thunk));

  auto *body = thunk->createBasicBlock();
  SILBuilder builder(body);
  builder.setCurrentDebugScope(body->getParent()->getDebugScope());

  SILFunctionConventions fnConv(thunkType, IGM.getSILModule());
  for (auto indirectResult : thunkType->getIndirectFormalResults()) {
    auto outTy =
        fnConv.getSILType(indirectResult, function.getTypeExpansionContext());
    outTy = thunk->mapTypeIntoContext(outTy);
    body->createFunctionArgument(outTy, nullptr);
  }

  for (auto parameter : thunkType->getParameters()) {
    if (auto *genericEnvironment = thunk->getGenericEnvironment()) {
      auto argTy =
          genericEnvironment->mapTypeIntoContext(parameter.getInterfaceType())
              ->getCanonicalType();
      parameter = SILParameterInfo(argTy, parameter.getConvention(),
                                   parameter.getDifferentiability());
    }
    auto argTy =
        fnConv.getSILType(parameter, function.getTypeExpansionContext());
    body->createFunctionArgument(argTy, nullptr);
  }

  auto args = body->getSILFunctionArguments();
  auto size = args.size();
  SILValue callee = args[size - 1];

  llvm::SmallVector<SILValue, 8> forwardedArguments;
  for (unsigned index = 0, end = size - 1; index < end; ++index) {
    forwardedArguments.push_back(body->getArgument(index));
  }

  if (isConsumedParameter(calleeType->getCalleeConvention())) {
    callee = builder.createCopyValue(location, callee);
  }

  auto returnValue = builder.createApply(
      location, callee, instruction->getSubstitutionMap(), forwardedArguments);
  builder.createReturn(location, returnValue);

  assert(function.getDebugScope()->Parent != thunk->getDebugScope()->Parent);

// FIXME_NOW: Remove this last just to be safe.
#ifndef NDEBUG
  thunk->verify(true);
#endif

  return thunk;
}

//===----------------------------------------------------------------------===//
//                PartialApplyLowerer Visitor Methods
//===----------------------------------------------------------------------===//

SILInstruction *
PartialApplyLowerer::visitPartialApplyInst(PartialApplyInst *instruction) {
  if (!instruction->getFunctionType()->isAsync()) {
    return nullptr;
  }

  // If we cannot get the referenced function, then we have a non-constant
  // function.  IRGen cannot create an AsyncFunctionPointer for the partial
  // apply forwarder it will generate for this partial_apply instruction because
  // the AsyncFunctionPointers it forms for partial apply forwarders contain
  // the same async context size as the function which is partially applied*.
  // Without a fixed function which is partially applied, there is no fixed size
  // to use for the AsyncFunctionPointer.
  //
  // * In fact, it emits the @llvm.coro.async.size.replace intrinsic into the
  //   partial apply forwarder's body which is then processed by LLVM's
  //   CoroCleanup to rewrite the AsyncFunctionPointer for the forwarder with
  //   the async context size of the function which is partially applied.  In
  //   any case, a fixed function whose async context size can be used is
  //   required.
  if (instruction->getReferencedFunctionOrNull() != nullptr) {
    return nullptr;
  }
  // if (calleeType->getRepresentation() !=
  // SILFunctionTypeRepresentation::Thick) {
  //  return nullptr;
  //}

  // A partial apply of a thick, async function:
  //
  //     %closure1 = partial_apply %thick_input(%captures)
  //                 : @async @convention(thick) (
  //                     Rest...,
  //                     Captures...
  //                 )
  //                     -> Return
  //
  // Make two changes:
  // (1) Introduce a function
  //
  //     sil thunk(
  //         %rest... : Rest...,
  //         %captures... : Captures...,
  //         %thick_input : @async @convention(thick) (
  //             Rest...,
  //             Captures...
  //         )
  //             -> Return,
  //     )
  //         -> Return
  //     {
  //       %return = apply %thick_input(%rest..., %captures...)
  //                 : @async @convention(thick) (
  //                     Rest...,
  //                     Captures...
  //                 )
  //                     -> Return
  //       return %return : $Return
  //     }
  //
  // (2) Replace the original instruction
  //
  //     %callee = function_ref @thunk : $@async @convention(thin)
  //     %closure2 = partial_apply %callee(%captures..., %thick_input)
  //                : $@async @convention(thin) (
  //                      Captures...,
  //                      Rest...,
  //                      @async @convention(thick) (Rest..., Captures...) -> Return
  //                  )
  //
  // Note that the type of the original's return
  //
  //     %closure1 : $@async @convention(thick) (Rest...) -> Return
  //
  // matches that of the replacement instruction's return
  //
  //     %closure2 : $@async @convention(thick) (Rest...) -> Return

  // (1) Introduce the thunk.
  auto *thunk = createThunk(instruction);
  thunks.push_back(thunk);

  // (2) Replace the original partial_apply with a partial_apply of the
  //     thunk, adding the original callee as the final argument.
  llvm::SmallVector<SILValue, 8> arguments;
  {
    for (auto argument : instruction->getArguments()) {
      arguments.push_back(argument);
    }
    arguments.push_back(instruction->getCallee());
  }

  SILBuilderWithScope originalBuilder(instruction);
  // %callee = function_ref @thunk : $@async @convention(thin) (
  //               Rest...,
  //               Captures...,
  //               @async @convention(thick) (
  //                   Rest...,
  //                   Captures...
  //               ) -> Return
  //           ) -> Return
  SILValue thunkRef =
      originalBuilder.createFunctionRef(instruction->getLoc(), thunk);
  // %closure2 = partial_apply %callee(%captures..., %thick_input)
  //            : $@async @convention(thin) (
  //                  Captures...,
  //                  Rest...,
  //                  @async @convention(thick) (Rest..., Captures...) -> Return
  //              )
  auto *replacement = originalBuilder.createPartialApply(
      instruction->getLoc(), thunkRef, thunk->getForwardingSubstitutionMap(),
      arguments,
      instruction->getType().getAs<SILFunctionType>()->getCalleeConvention(),
      instruction->isOnStack(),
      GenericSpecializationInformation::create(instruction, originalBuilder));

  {
    // FIXME_NOW: Remove this last just to be safe.
    auto args = replacement->getArguments();
    auto calleeTy = replacement->getSubstCalleeType();
    auto params = calleeTy->getParameters();
    params = params.slice(params.size() - args.size(), args.size());
    for (auto index : indices(args)) {
      auto paramTy = IGM.silConv.getSILType(
          params[index], calleeTy, IGM.getMaximalTypeExpansionContext());
      assert(args[index]->getType() == paramTy);
    }
  }

  return replacement;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {
class PartialApplyLowering : public SILFunctionTransform {
  SmallVector<SILFunction *, 2> thunks;

  void run() override {
    auto *function = getFunction();
    SILOptFunctionBuilder thunkBuilder(*this);
    PartialApplyLowerer lowerer(*function, *getIRGenModule(), thunkBuilder,
                                thunks);
    if (lowerer.run()) {
      // FIXME_NOW: Remove this last just to be safe.
#ifndef NDEBUG
      function->verify(true);
#endif
      invalidateAnalysis(SILAnalysis::InvalidationKind::Everything);

      for (auto thunk : thunks) {
        addFunctionToPassManagerWorklist(thunk, function);
      }
    }
  }
};
} // end anonymous namespace

SILTransform *irgen::createPartialApplyLowering() {
  return new PartialApplyLowering();
}
