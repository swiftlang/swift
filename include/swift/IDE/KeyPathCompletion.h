//===--- KeyPathCompletion.h ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_KEYPATHCOMPLETION_H
#define SWIFT_IDE_KEYPATHCOMPLETION_H

#include "swift/IDE/CodeCompletionConsumer.h"
#include "swift/IDE/CodeCompletionContext.h"
#include "swift/IDE/TypeCheckCompletionCallback.h"

namespace swift {
namespace ide {

class KeyPathTypeCheckCompletionCallback : public TypeCheckCompletionCallback {
  struct Result {
    /// The type on which completion should occur, i.e. a result type of the
    /// previous component.
    Type BaseType;
    /// Whether code completion happens on the key path's root.
    bool OnRoot;
  };

  KeyPathExpr *KeyPath;
  SmallVector<Result, 4> Results;

  void sawSolutionImpl(const constraints::Solution &solution) override;

public:
  KeyPathTypeCheckCompletionCallback(KeyPathExpr *KeyPath) : KeyPath(KeyPath) {}

  void collectResults(DeclContext *DC, SourceLoc DotLoc,
                      ide::CodeCompletionContext &CompletionCtx);
};

} // end namespace ide
} // end namespace swift

#endif // SWIFT_IDE_KEYPATHCOMPLETION_H
