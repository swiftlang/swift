//===--- PassesFwd.h - Creation functions for LLVM passes -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LLVMPASSES_PASSESFWD_H
#define SWIFT_LLVMPASSES_PASSESFWD_H

namespace llvm {
  class ModulePass;
  class FunctionPass;
  class ImmutablePass;
  class PassRegistry;

  void initializeSwiftAAWrapperPassPass(PassRegistry &);
  void initializeSwiftRCIdentityPass(PassRegistry &);
  void initializeSwiftARCOptPass(PassRegistry &);
  void initializeSwiftARCContractPass(PassRegistry &);
  void initializeInlineTreePrinterPass(PassRegistry &);
  void initializeSwiftMergeFunctionsPass(PassRegistry &);
}

namespace swift {
  llvm::FunctionPass *createSwiftARCOptPass();
  llvm::FunctionPass *createSwiftARCContractPass();
  llvm::ModulePass *createInlineTreePrinterPass();
  llvm::ModulePass *createSwiftMergeFunctionsPass();
  llvm::ImmutablePass *createSwiftAAWrapperPass();
  llvm::ImmutablePass *createSwiftRCIdentityPass();
} // end namespace swift

#endif
