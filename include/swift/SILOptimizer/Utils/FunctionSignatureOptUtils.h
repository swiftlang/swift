//===--- FunctionSignatureOptUtils.h ----------------------------*- C++ -*-===//
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

#ifndef SWIFT_SIL_FUNCTIONSIGOPTUTILS_H
#define SWIFT_SIL_FUNCTIONSIGOPTUTILS_H

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SILOptimizer/Analysis/FunctionSignatureAnalysis.h"

namespace swift {

bool canSpecializeFunction(SILFunction *F);

void 
addReleasesForConvertedOwnedParameter(SILBuilder &Builder,
                                      SILLocation Loc,
                                      OperandValueArrayRef Parameters,
                                      ArrayRef<ArgumentDescriptor> &ArgDescs);

void
addReleasesForConvertedOwnedParameter(SILBuilder &Builder,
                                      SILLocation Loc,
                                      ArrayRef<SILArgument*> Parameters,
                                      ArrayRef<ArgumentDescriptor> &ArgDescs);
void
addRetainsForConvertedDirectResults(SILBuilder &Builder,
                                    SILLocation Loc,
                                    SILValue ReturnValue,
                                    SILInstruction *AI,
                                    ArrayRef<ResultDescriptor> DirectResults);

} // end namespace swift

#endif
