//===--- MandatoryOptUtils.h ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Contains utility operations used by various mandatory passes.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_MANDATORY_MANDATORYOOPTUTILS_H
#define SWIFT_SILOPTIMIZER_MANDATORY_MANDATORYOOPTUTILS_H

#include "llvm/Support/Compiler.h"

namespace swift {

class SILBuilderWithScope;
class AssignInst;

enum class PartialInitializationKind {
  /// The box contains a fully-initialized value.
  IsNotInitialization,

  /// The box contains a class instance that we own, but the instance has
  /// not been initialized and should be freed with a special SIL
  /// instruction made for this purpose.
  IsReinitialization,

  /// The box contains an undefined value that should be ignored.
  IsInitialization,
};

void lowerAssignInstruction(SILBuilderWithScope &B, AssignInst *Inst,
                            PartialInitializationKind isInitialization)
    LLVM_LIBRARY_VISIBILITY;

} // namespace swift

#endif
