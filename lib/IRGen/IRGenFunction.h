//===--- IRGenFunction.h - IR Generation for Swift Functions ---*- C++ -*-===//
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
// This file defines the structure used to generate the IR body of a
// function.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRGENFUNCTION_H
#define SWIFT_IRGEN_IRGENFUNCTION_H

#include "llvm/Support/IRBuilder.h"

namespace llvm {
  class Constant;
  class Function;
}

namespace swift {
  class BraceStmt;
  class FuncDecl;

namespace irgen {
  class IRGenModule;

  typedef llvm::IRBuilder<> IRBuilder;

/// IRGenModule - Primary class for emitting IR for global declarations.
/// 
class IRGenFunction {
public:
  IRGenModule &IGM;
  IRBuilder Builder;

  IRGenFunction(IRGenModule &IGM);
};

} // end namespace irgen
} // end namespace swift

#endif
