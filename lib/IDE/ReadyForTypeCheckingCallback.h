//===--- ReadyForTypeCheckingCallback.h -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_READYFORTYPECHECKINGCALLBACK_H
#define SWIFT_IDE_READYFORTYPECHECKINGCALLBACK_H

#include "swift/Parse/IDEInspectionCallbacks.h"

namespace swift {
namespace ide {

/// IDE inspection callback that is invoked once the resulting AST is ready
/// for type-checking work.
class ReadyForTypeCheckingCallback : public DoneParsingCallback {
public:
  virtual ~ReadyForTypeCheckingCallback() {}

  virtual void doneParsing(SourceFile *SrcFile) override;

  /// Called when the AST has been parsed and had its imports resolved and
  /// extensions bound.
  virtual void readyForTypeChecking(SourceFile *SrcFile) = 0;
};

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_READYFORTYPECHECKINGCALLBACK_H
