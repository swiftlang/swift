//===--- Ownership.cpp - Mangling/demangling for ownership ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Ownership.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Compiler.h"

using namespace swift;

llvm::StringRef swift::keywordOf(ReferenceOwnership ownership) {
  switch (ownership) {
  case ReferenceOwnership::Strong:
    break;
  case ReferenceOwnership::Weak: return "weak";
  case ReferenceOwnership::Unowned: return "unowned";
  case ReferenceOwnership::Unmanaged: return "unowned(unsafe)";
  }
  // We cannot use llvm_unreachable() because this is used by the stdlib.
  assert(false && "impossible");
  LLVM_BUILTIN_UNREACHABLE;
}

llvm::StringRef swift::manglingOf(ReferenceOwnership ownership) {
  switch (ownership) {
  case ReferenceOwnership::Strong:
    break;
  case ReferenceOwnership::Weak: return "Xw";
  case ReferenceOwnership::Unowned: return "Xo";
  case ReferenceOwnership::Unmanaged: return "Xu";
  }
  // We cannot use llvm_unreachable() because this is used by the stdlib.
  assert(false && "impossible");
  LLVM_BUILTIN_UNREACHABLE;
}
