//===--- LLVMContext.cpp --------------------------------------------------===//
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

#include "swift/Basic/LLVMContext.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/IR/LLVMContext.h"

/// Hack: because of \c IntrinsicInfo::hasAttribute in Builtins.cpp
/// SIL construction goes into the LLVM, so initialize it here, but really should not be inited
static std::unique_ptr<llvm::LLVMContext> GlobalContext(new llvm::LLVMContext);

llvm::LLVMContext& swift::getGlobalLLVMContext() {
  return *GlobalContext.get();
}

void swift::resetGlobalLLVMContext() {
  GlobalContext.reset(new llvm::LLVMContext);
}
