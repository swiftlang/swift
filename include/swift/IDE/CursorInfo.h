//===--- CursorInfo.h --- ---------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_IDE_CURSORINFO_H
#define SWIFT_IDE_CURSORINFO_H

#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"
#include "swift/IDE/Utils.h"

namespace swift {
class IDEInspectionCallbacksFactory;

namespace ide {

/// An abstract base class for consumers of context info results.
class CursorInfoConsumer {
public:
  virtual ~CursorInfoConsumer() {}
  virtual void handleResults(std::vector<ResolvedCursorInfoPtr>) = 0;
};

/// Create a factory for code completion callbacks.
IDEInspectionCallbacksFactory *
makeCursorInfoCallbacksFactory(CursorInfoConsumer &Consumer,
                               SourceLoc RequestedLoc);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_CURSORINFO_H
