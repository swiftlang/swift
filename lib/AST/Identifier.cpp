//===--- Identifier.cpp - Uniqued Identifier --------------------*- C++ -*-===//
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
// This file implements the Identifier interface.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Identifier.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;


raw_ostream &llvm::operator<<(raw_ostream &OS, Identifier I) {
  if (I.get() == 0) return OS << "(null identifier)";
  return OS << I.get();
}
