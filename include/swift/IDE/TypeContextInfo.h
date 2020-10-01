//===--- TypeContextInfo.h --------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_IDE_TYPECONTEXTINFO_H
#define SWIFT_IDE_TYPECONTEXTINFO_H

#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"

namespace swift {
class CodeCompletionCallbacksFactory;

namespace ide {

/// A result item for context info query.
class TypeContextInfoItem {
public:
  /// Possible expected type.
  Type ExpectedTy;
  /// Members of \c ExpectedTy which can be referenced by "Implicit Member
  /// Expression".
  SmallVector<ValueDecl *, 0> ImplicitMembers;

  TypeContextInfoItem(Type ExpectedTy) : ExpectedTy(ExpectedTy) {}
};

/// An abstract base class for consumers of context info results.
class TypeContextInfoConsumer {
public:
  virtual ~TypeContextInfoConsumer() {}
  virtual void handleResults(ArrayRef<TypeContextInfoItem>) = 0;
  virtual void setReusingASTContext(bool flag) = 0;
};

/// Printing consumer
class PrintingTypeContextInfoConsumer : public TypeContextInfoConsumer {
  llvm::raw_ostream &OS;

public:
  PrintingTypeContextInfoConsumer(llvm::raw_ostream &OS) : OS(OS) {}

  void handleResults(ArrayRef<TypeContextInfoItem>) override;
  void setReusingASTContext(bool flag) override {}
};

/// Create a factory for code completion callbacks.
CodeCompletionCallbacksFactory *
makeTypeContextInfoCallbacksFactory(TypeContextInfoConsumer &Consumer);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_TYPECONTEXTINFO_H
