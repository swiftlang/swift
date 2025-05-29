//===--- StorageImpl.cpp - Storage declaration access impl ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines types for describing the implementation of an
// AbstractStorageDecl.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/StorageImpl.h"
#include "swift/AST/ASTContext.h"

using namespace swift;

StorageImplInfo StorageImplInfo::getMutableOpaque(OpaqueReadOwnership ownership,
                                                  const ASTContext &ctx) {
  ReadWriteImplKind rwKind;
  if (ctx.LangOpts.hasFeature(Feature::CoroutineAccessors))
    rwKind = ReadWriteImplKind::Modify2;
  else
    rwKind = ReadWriteImplKind::Modify;
  return {getOpaqueReadImpl(ownership, ctx), WriteImplKind::Set, rwKind};
}

ReadImplKind StorageImplInfo::getOpaqueReadImpl(OpaqueReadOwnership ownership,
                                                const ASTContext &ctx) {
  switch (ownership) {
  case OpaqueReadOwnership::Owned:
    return ReadImplKind::Get;
  case OpaqueReadOwnership::OwnedOrBorrowed:
  case OpaqueReadOwnership::Borrowed:
    if (ctx.LangOpts.hasFeature(Feature::CoroutineAccessors))
      return ReadImplKind::Read2;
    return ReadImplKind::Read;
  }
  llvm_unreachable("bad read-ownership kind");
}
