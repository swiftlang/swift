//===--- IRGenSILPasses.cpp - The IRGen Prepare SIL Passes ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the registration of SILOptimizer passes necessary for
//  IRGen.
//
//===----------------------------------------------------------------------===//

#include "swift/IRGen/IRGenSILPasses.h"
#include "swift/AST/ASTContext.h"
#include "swift/Subsystems.h"

namespace swift {
using SILTransformCtor = SILTransform *(void);
static SILTransformCtor *irgenSILPassFns[] = {
#define PASS(Name, Tag, Desc)
#define IRGEN_PASS(Name, Tag, Desc) &irgen::create##Name,
#include "swift/SILOptimizer/PassManager/Passes.def"
};
} // end namespace swift

void swift::registerIRGenSILTransforms(ASTContext &ctx) {
  ctx.registerIRGenSILTransforms(irgenSILPassFns);
}
