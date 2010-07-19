//===--- ASTContext.cpp - ASTContext Implementation -----------------------===//
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
//  This file implements the ASTContext class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Type.h"
#include "llvm/Support/Allocator.h"
using namespace swift;

ASTContext::ASTContext()
  : Allocator(new llvm::BumpPtrAllocator()),
    VoidType(new (*this) BuiltinType(BuiltinVoidKind)),
    IntType(new (*this) BuiltinType(BuiltinIntKind)) {
}

ASTContext::~ASTContext() {
  delete Allocator;
}

void *ASTContext::Allocate(unsigned long Bytes, unsigned Alignment) {
  return Allocator->Allocate(Bytes, Alignment);
}
