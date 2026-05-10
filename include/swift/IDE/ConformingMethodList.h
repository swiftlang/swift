//===--- ConformingMethodList.h --- -----------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_CONFORMINGMETHODLIST_H
#define SWIFT_IDE_CONFORMINGMETHODLIST_H

#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"

namespace swift {
class IDEInspectionCallbacksFactory;

namespace ide {

/// A result item for context info query.
class ConformingMethodListResult {
public:
  /// The decl context of the parsed expression.
  DeclContext *DC;

  /// The resolved type of the expression.
  Type ExprType;

  /// Methods which satisfy the criteria.
  SmallVector<ValueDecl *, 0> Members;

  ConformingMethodListResult(DeclContext *DC, Type ExprType)
      : DC(DC), ExprType(ExprType) {}
};

/// An abstract base class for consumers of context info results.
class ConformingMethodListConsumer {
public:
  virtual ~ConformingMethodListConsumer() {}
  virtual void handleResult(const ConformingMethodListResult &result) = 0;
};

/// Create a factory for code completion callbacks.
IDEInspectionCallbacksFactory *makeConformingMethodListCallbacksFactory(
    ArrayRef<const char *> expectedTypeNames,
    ConformingMethodListConsumer &Consumer);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_CONFORMINGMETHODLIST_H
