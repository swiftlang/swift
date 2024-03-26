//===--- GenKeyPath.h - IR generation for KeyPath ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file provides the private interface to the emission of KeyPath
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENKEYPATH_H
#define SWIFT_IRGEN_GENKEYPATH_H

#include "GenericRequirement.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILValue.h"
#include "llvm/IR/Value.h"

namespace swift {
namespace irgen {
class Explosion;
class IRGenFunction;
class StackAddress;

std::pair<llvm::Value *, llvm::Value *>
emitKeyPathArgument(IRGenFunction &IGF, SubstitutionMap subs,
                    const CanGenericSignature &sig,
                    ArrayRef<SILType> indiceTypes, Explosion &indiceValues,
                    std::optional<StackAddress> &dynamicArgsBuf);
} // end namespace irgen
} // end namespace swift

#endif
