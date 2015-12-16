//===-- Generics.h - Utilities for transforming generics --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This contains utilities for transforming generics.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_GENERICS_H
#define SWIFT_SIL_GENERICS_H

#include "swift/AST/Mangle.h"
#include "swift/SIL/Mangle.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

namespace swift {

ApplySite trySpecializeApplyOfGeneric(ApplySite Apply,
                                      SILFunction *&NewFunction,
                                      CloneCollector &Collector);

/// Checks if a given mangled name could be a name of a whitelisted specialization.
bool isWhitelistedSpecialization(StringRef SpecName);

/// Create a new apply based on an old one, but with a different
/// function being applied.
ApplySite replaceWithSpecializedFunction(ApplySite AI, SILFunction *NewF);

SILFunction *getExistingSpecialization(SILModule &M, StringRef FunctionName);

} // end namespace swift

#endif
