//===--- SemaExpr.cpp - Swift Expression Semantic Analysis ----------------===//
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
//  This file implements semantic analysis for Swift expressions.
//
//===----------------------------------------------------------------------===//

#include "swift/Sema/SemaExpr.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/StringRef.h"
using namespace swift;

SemaExpr::SemaExpr(Sema &S) : SemaBase(S) {
}

void SemaExpr::NumericConstant(llvm::StringRef Text, llvm::SMLoc Loc) {
}

void SemaExpr::ParenExpr(llvm::SMLoc LPLoc, llvm::SMLoc RPLoc) {
  
}
