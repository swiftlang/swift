//===--- Instruction.cpp - Instructions for high-level CFGs ------*- C++ -*-==//
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
// This file defines the high-level Instruction classes used for Swift CFGs.
//
//===----------------------------------------------------------------------===//

#include "swift/CFG/Instruction.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

Instruction::~Instruction() {}
void Instruction::print(llvm::raw_ostream &OS) const {}
void Instruction::dump() const { print(llvm::errs()); }
bool Instruction::validate() const { return true; }
