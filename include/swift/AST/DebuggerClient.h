//===--- DebuggerClient.h - Interfaces LLDB uses for parsing ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the abstract DebuggerClient class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEBUGGERCLIENT_H
#define SWIFT_DEBUGGERCLIENT_H

#include "swift/AST/NameLookup.h"

namespace swift {

class SILDebuggerClient;

class DebuggerClient {
protected:
  ASTContext &Ctx;
public:
  typedef SmallVectorImpl<UnqualifiedLookupResult> ResultVector;

  DebuggerClient(ASTContext &C) : Ctx(C) { }
  virtual ~DebuggerClient() = default;

  /// DebuggerClient is consulted at two times during name
  /// lookup.  This is the first time: after all names in a
  /// source file have been checked but before external
  /// Modules are checked.  The results in the ResultVector will
  /// be consulted first.  Return true if results have been added
  /// to RV.
  virtual bool lookupOverrides(Identifier Name, DeclContext *DC,
                               SourceLoc Loc, bool IsTypeLookup,
                               ResultVector &RV) = 0;

  /// This is the second time DebuggerClient is consulted:
  /// after all names in external Modules are checked, if nothing
  /// suitable was found.  The idea is that lookupFallbacks can
  /// perform more cost-intensive checks.  Return true if results
  /// have been added to RV.
  virtual bool lookupFallbacks(Identifier Name, DeclContext *DC,
                               SourceLoc Loc, bool IsTypeLookup,
                               ResultVector &RV) = 0;

  virtual SILDebuggerClient *getAsSILDebuggerClient() = 0;
private:
  virtual void anchor();
};

} // namespace swift

#endif
