//===--- Ownership.cpp - Swift ASTs for Ownership ------------------------===//
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
#include "swift/AST/Decl.h"
#include "swift/AST/StorageImpl.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Compiler.h"

using namespace swift;

llvm::StringRef swift::getOwnershipSpelling(ValueOwnership ownership) {
  switch (ownership) {
  case ValueOwnership::Default:
    return "default";
  case ValueOwnership::InOut:
    return "inout";
  case ValueOwnership::Shared:
    return "borrowing";
  case ValueOwnership::Owned:
    return "consuming";
  }
  llvm_unreachable("Invalid ValueOwnership");
}

ValueOwnership swift::getLoweredOwnership(ValueOwnership written,
                                          const AbstractFunctionDecl *fn,
                                          bool isSelf) {
  if (written != ValueOwnership::Default)
    return written;
  if (isa_and_nonnull<ConstructorDecl>(fn))
    return ValueOwnership::Owned;
  if (auto *ad = dyn_cast_or_null<AccessorDecl>(fn)) {
    if (ad->getAccessorKind() == AccessorKind::Set)
      return isSelf ? ValueOwnership::InOut : ValueOwnership::Owned;
    if (isYieldingMutableAccessor(ad->getAccessorKind())) {
      assert(isSelf);
      return ValueOwnership::InOut;
    }
  }
  return ValueOwnership::Shared;
}
