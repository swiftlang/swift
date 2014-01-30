//===--- PassesFwd.h - Creation functions for LLVM  passes ------*- C++ -*-===//
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

#ifndef SWIFT_OPTIMIZEARC_PASSESFWD_H
#define SWIFT_OPTIMIZEARC_PASSESFWD_H

namespace llvm {
  class FunctionPass;
}

namespace swift {
  llvm::FunctionPass *createSwiftARCOptPass();
  llvm::FunctionPass *createSwiftARCExpandPass();
} // end namespace swift

#endif
