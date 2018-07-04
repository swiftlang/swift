//===--- SILDebuggerClient.h - Interfaces from SILGen to LLDB ---*- C++ -*-===//
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
// This file defines the abstract SILDebuggerClient class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILDEBUGGERCLIENT_H
#define SWIFT_SILDEBUGGERCLIENT_H

#include "swift/AST/DebuggerClient.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILValue.h"

namespace swift {

class SILBuilder;

class SILDebuggerClient : public DebuggerClient {
public:
  using ResultVector = SmallVectorImpl<LookupResultEntry>;

  SILDebuggerClient(ASTContext &C) : DebuggerClient(C) { }
  virtual ~SILDebuggerClient() = default;

  /// DebuggerClient is asked to emit SIL references to locals,
  /// permitting SILGen to access them like any other variables.
  /// This avoids generation of properties.
  virtual SILValue emitLValueForVariable(VarDecl *var,
                                         SILBuilder &builder) = 0;

  inline SILDebuggerClient *getAsSILDebuggerClient() {
    return this;
  }
private:
  virtual void anchor();
};

} // namespace swift

#endif
