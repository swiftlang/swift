//===--- CFGValue.h - Value reference in the CFG ----------------*- C++ -*-===//
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
// This file defines the CFGType class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CFG_CFGVALUE_H
#define SWIFT_CFG_CFGVALUE_H

#include "swift/CFG/CFGType.h"

namespace swift {

  class Instruction;
  class BasicBlockArg;

  // TODO: Expand out to a class.
  typedef llvm::PointerUnion<Instruction*, BasicBlockArg*> CFGValue;
} // end namespace swift

#endif
