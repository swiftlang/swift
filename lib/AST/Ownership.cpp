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

ValueOwnership swift::asValueOwnership(LoweredOwnership ownership) {
  switch (ownership) {
  case LoweredOwnership::Shared:
    return ValueOwnership::Shared;
  case LoweredOwnership::InOut:
    return ValueOwnership::InOut;
  case LoweredOwnership::Owned:
    return ValueOwnership::Owned;
  }
  llvm_unreachable("Invalid LoweredOwnership");
}

llvm::StringRef swift::getOwnershipSpelling(LoweredOwnership ownership) {
  return getOwnershipSpelling(asValueOwnership(ownership));
}

LoweredOwnership swift::getLoweredOwnership(ValueOwnership written,
                                            const AbstractFunctionDecl *fn,
                                            bool isSelf) {
  switch (written) {
  case ValueOwnership::Shared:
    return LoweredOwnership::Shared;
  case ValueOwnership::InOut:
    return LoweredOwnership::InOut;
  case ValueOwnership::Owned:
    return LoweredOwnership::Owned;
  case ValueOwnership::Default:
    break;
  }
  // A mutating function writes `self` as `inout`, so the defaults below only
  // apply to the `self` of a function that isn't mutating.
  if (isa_and_nonnull<ConstructorDecl>(fn))
    return LoweredOwnership::Owned;
  if (auto *ad = dyn_cast_or_null<AccessorDecl>(fn)) {
    // A setter or an init accessor owns its parameters other than `self`.
    if (ad->getAccessorKind() == AccessorKind::Set ||
        ad->getAccessorKind() == AccessorKind::Init)
      return isSelf ? LoweredOwnership::Shared : LoweredOwnership::Owned;
  }
  // The deinit of a noncopyable struct or enum owns `self`, unlike the deinit
  // of a class, whose body only borrows it.
  if (auto *dd = dyn_cast_or_null<DestructorDecl>(fn)) {
    if (isSelf && !dd->getDeclContext()->getSelfClassDecl())
      return LoweredOwnership::Owned;
  }
  return LoweredOwnership::Shared;
}
