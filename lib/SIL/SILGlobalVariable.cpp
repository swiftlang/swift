//===--- SILGlobalVariable.cpp - Defines SILGlobalVariable structure ------===//
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

#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

SILGlobalVariable::SILGlobalVariable(SILModule &Module, SILLinkage Linkage,
                                     StringRef Name, SILType LoweredType,
                                     bool IsDefinition,
                                     Optional<SILLocation> Loc)
  : ModuleAndLinkage(&Module, Linkage),
    Name(Name),
    LoweredType(LoweredType),
    Location(Loc),
    IsDefinition(IsDefinition) {
  Module.silGlobals.push_back(this);
}

SILGlobalVariable::~SILGlobalVariable() {}
