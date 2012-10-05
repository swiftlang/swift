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
// This file defines the CFGValue class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CFG_CFGVALUE_H
#define SWIFT_CFG_CFGVALUE_H

#include "swift/AST/Type.h"

namespace swift {

  class Instruction;
  class BasicBlockArg;

  /// CFGValue - This class is a value that can be used as an "operand" to an
  /// instruction.  It is either a reference to another instruction, or an
  /// incoming basic block argument.
  class CFGValue {
    llvm::PointerUnion<Instruction*, BasicBlockArg*> V;
  public:
    explicit CFGValue() : V() {}
    CFGValue(Instruction *I) : V(I) { assert(I); }
    CFGValue(BasicBlockArg *A) : V(A) { assert(A); }

    bool isNull() const { return V.isNull(); }
    bool isInstruction() const { return V.is<Instruction*>(); }
    bool isBasicBlockArg() const { return V.is<BasicBlockArg*>(); }

    Instruction *getInst() const { return V.get<Instruction*>(); }
    BasicBlockArg *getBBArg() const { return V.get<BasicBlockArg*>(); }

    Instruction *getInstOrNull() const { return V.dyn_cast<Instruction*>(); }
    BasicBlockArg *getBBArgOrNull() const {return V.dyn_cast<BasicBlockArg*>();}

    Type getType() const;
  };
} // end namespace swift

#endif
