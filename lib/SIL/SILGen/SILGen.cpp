//===--- SILGen.cpp - Implements Lowering of ASTs -> SIL ------------------===//
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

#include "SILGen.h"
#include "swift/AST/AST.h"
using namespace swift;
using namespace Lowering;

//===--------------------------------------------------------------------===//
// SILGen Class implementation
//===--------------------------------------------------------------------===//

SILGen::SILGen(CFG &C, FuncExpr *FE)
  : C(C), B(new (C) BasicBlock(&C), C), Cleanups(*this) {

  emitProlog(FE);
}

/// SILGen destructor - called when the entire AST has been visited.  This
/// handles "falling off the end of the function" logic.
SILGen::~SILGen() {
  // If the end of the function isn't reachable (e.g. it ended in an explicit
  // return), then we're done.
  if (!B.hasValidInsertionPoint())
    return;

  // If we have an unterminated block, it is either an implicit return of an
  // empty tuple, or a dynamically unreachable location.
  // FIXME: When the function returns a "voidable" result, we should produce it.
  // hoist some logic from IRGen into a common place.
  B.createUnreachable();

  //auto EmptyTuple = B.createTuple(nullptr, ArrayRef<Value*>());
  //B.createReturn(nullptr, EmptyTuple);
}

CFG *CFG::constructSIL(FuncExpr *FE) {
  CFG *C = new CFG(FE->getASTContext());

  SILGen(*C, FE).visit(FE->getBody());

  C->verify();
  return C;
}


