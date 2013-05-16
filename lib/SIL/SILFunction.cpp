//===--- SILFunction.cpp - Defines the SILFunction data structure ---------===//
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

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
using namespace swift;

SILFunction::SILFunction(SILModule &Module,
                         SILLinkage Linkage,
                         StringRef Name,
                         SILType LoweredType)
: ModuleAndLinkage(&Module, Linkage), MangledName(Name),
LoweredType(LoweredType) {
  Module.functions.push_back(this);
}

SILFunction::~SILFunction() {
}



ASTContext &SILFunction::getContext() const {
  return getModule().Context;
}
