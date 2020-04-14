//===--- Parse.cpp - SIL File Parsing logic -------------------------------===//
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

#include "swift/SIL/SILParser.h"

using namespace swift;
using namespace swift::syntax;

//===----------------------------------------------------------------------===//
// EmitSIL implementation
//===----------------------------------------------------------------------===//

namespace swift {

SILInstruction *EmitSIL::emit(SILBuilder &builder, SILParserResult instData) {
  // If we don't know what the kind is that is because this parser doesn't yet
  // suppor that instruction. Return nullptr so that the old parser knows it
  // needs to parse this instruction. This is the only time this method should
  // ever return a null instruction.
  if (unsigned(instData.kind) == 0)
    return nullptr;
  
  switch (instData.kind) {
#define INST(CLASS, PARENT)                                                    \
  case SILInstructionKind::CLASS:                                              \
    return emit##CLASS(builder, instData);
#include "swift/SIL/SILNodes.def"
  }
}

//===----------------------------------------------------------------------===//
// EmitSIL visitors
//===----------------------------------------------------------------------===//

} // namespace swift
