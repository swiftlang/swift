//===--- RuntimeFnWrappersGen.h - LLVM IR Generation for runtime functions ===//
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
//
//  Helper functions providing the LLVM IR generation for runtime entry points.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_RUNTIME_RUNTIMEFNWRAPPERSGEN_H
#define SWIFT_RUNTIME_RUNTIMEFNWRAPPERSGEN_H

#include "llvm/IR/Module.h"
#include "llvm/ADT/ArrayRef.h"

namespace swift {
  
class AvailabilityContext;
class ASTContext;

enum class RuntimeAvailability {
  AlwaysAvailable,
  AvailableByCompatibilityLibrary,
  ConditionallyAvailable
};

/// Generate an llvm declaration for a runtime entry with a
/// given name, return types, argument types, attributes and
/// a calling convention.
llvm::Constant *getRuntimeFn(llvm::Module &Module,
                      llvm::Constant *&cache,
                      char const *name,
                      llvm::CallingConv::ID cc,
                      RuntimeAvailability availability,
                      llvm::ArrayRef<llvm::Type*> retTypes,
                      llvm::ArrayRef<llvm::Type*> argTypes,
                      llvm::ArrayRef<llvm::Attribute::AttrKind> attrs);

} /* Namespace swift */
#endif
