//===--- FunctionSignatureOptUtils.cpp ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Utils/FunctionSignatureOptUtils.h"
#include "swift/SILOptimizer/Utils/Local.h"

using namespace swift;

static bool isSpecializableRepresentation(SILFunctionTypeRepresentation Rep) {
  switch (Rep) {
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::CFunctionPointer:
    return true;
  case SILFunctionTypeRepresentation::WitnessMethod:
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::Block:
    return false;
  }
}

/// Returns true if F is a function which the pass know show to specialize
/// function signatures for.
bool swift::canSpecializeFunction(SILFunction *F) {
  // Do not specialize the signature of SILFunctions that are external
  // declarations since there is no body to optimize.
  if (F->isExternalDeclaration())
    return false;

  // Do not specialize functions that are available externally. If an external
  // function was able to be specialized, it would have been specialized in its
  // own module. We will inline the original function as a thunk. The thunk will
  // call the specialized function.
  if (F->isAvailableExternally())
    return false;

  // Do not specialize the signature of always inline functions. We
  // will just inline them and specialize each one of the individual
  // functions that these sorts of functions are inlined into.
  if (F->getInlineStrategy() == Inline_t::AlwaysInline)
    return false;

  // For now ignore generic functions to keep things simple...
  if (F->getLoweredFunctionType()->isPolymorphic())
    return false;

  // Make sure F has a linkage that we can optimize.
  if (!isSpecializableRepresentation(F->getRepresentation()))
    return false;

  return true;
}

void swift::
addReleasesForConvertedOwnedParameter(SILBuilder &Builder,
                                      SILLocation Loc,
                                      ArrayRef<SILArgument*> Parameters,
                                      ArrayRef<ArgumentDescriptor> &ArgDescs) {
  // If we have any arguments that were consumed but are now guaranteed,
  // insert a release_value.
  for (auto &ArgDesc : ArgDescs) {
    if (ArgDesc.CalleeRelease.empty())
      continue;
    Builder.createReleaseValue(Loc, Parameters[ArgDesc.Index]);
  }
}

void swift::
addReleasesForConvertedOwnedParameter(SILBuilder &Builder,
                                      SILLocation Loc,
                                      OperandValueArrayRef Parameters,
                                      ArrayRef<ArgumentDescriptor> &ArgDescs) {
  // If we have any arguments that were consumed but are now guaranteed,
  // insert a release_value.
  for (auto &ArgDesc : ArgDescs) {
    // The argument is dead. Make sure we have a release to balance out
    // the retain for creating the @owned parameter.
    if (ArgDesc.IsEntirelyDead && 
        ArgDesc.Arg->getKnownParameterInfo().getConvention() ==
        ParameterConvention::Direct_Owned) {
       Builder.createReleaseValue(Loc, Parameters[ArgDesc.Index]);
       continue;
    }
    if (ArgDesc.CalleeRelease.empty())
      continue;
    Builder.createReleaseValue(Loc, Parameters[ArgDesc.Index]);
  }
}

void swift::
addRetainsForConvertedDirectResults(SILBuilder &Builder,
                                    SILLocation Loc,
                                    SILValue ReturnValue,
                                    SILInstruction *AI,
                                    ArrayRef<ResultDescriptor> DirectResults) {
  for (auto I : indices(DirectResults)) {
    auto &RV = DirectResults[I];
    if (RV.CalleeRetain.empty()) continue;

    bool IsSelfRecursionEpilogueRetain = false;
    for (auto &X : RV.CalleeRetain) {
      IsSelfRecursionEpilogueRetain |= (AI == X);
    }

    // We do not create a retain if this ApplyInst is a self-recursion.
    if (IsSelfRecursionEpilogueRetain)
      continue;

    // Extract the return value if necessary.
    SILValue SpecificResultValue = ReturnValue;
    if (DirectResults.size() != 1)
      SpecificResultValue = Builder.createTupleExtract(Loc, ReturnValue, I);

    Builder.createRetainValue(Loc, SpecificResultValue);
  }
}
