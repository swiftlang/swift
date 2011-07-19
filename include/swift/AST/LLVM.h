//===--- LLVM.h - Import various common LLVM datatypes ----------*- C++ -*-===//
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
// This file forward declares and imports various common LLVM datatypes that
// swift wants to use unqualified.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LLVM_H
#define SWIFT_AST_LLVM_H

// Forward declarations.
namespace llvm {
  class SMLoc;
  
  // also cast/dyn_cast etc.
  // also raw_ostream
  
  // also SmallVector, ArrayRef, StringRef, Twine ...
  
} // end namespace llvm;


namespace swift {
  using llvm::SMLoc;
  
} // end namespace swift

#endif
