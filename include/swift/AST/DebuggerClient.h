//===--- DebuggerClient.h - Interfaces LLDB uses for parsing ----*- C++ -*-===//
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
  
  // DebuggerClient is consulted at the beginning of the parsing
  // of various DeclKinds to see whether the decl should be parsed
  // in the global context rather than the current context.
  // This question will only be asked if the decl's current context
  // is a function marked with the LLDBDebuggerFunction attribute.
  virtual bool shouldGlobalize(Identifier Name, DeclKind kind) = 0;
  
  virtual void didGlobalize (Decl *Decl) = 0;

  /// DebuggerClient is consulted at two times during name
  /// lookup.  This is the first time: after all names in a
  /// source file have been checked but before external
  /// Modules are checked.  The results in the ResultVector will
  /// be consulted first.  Return true if results have been added
  /// to RV.
  /// FIXME: I don't think this ever does anything useful.
  virtual bool lookupOverrides(Identifier Name, DeclContext *DC,
                               SourceLoc Loc, bool IsTypeLookup,
                               ResultVector &RV) = 0;
 
  /// This is the second time DebuggerClient is consulted:
  /// after all names in external Modules are checked, the client
  /// gets a chance to add names to the list of candidates that
  /// have been found in the external module lookup.  

  virtual bool lookupAdditions(Identifier Name, DeclContext *DC,
                               SourceLoc Loc, bool IsTypeLookup,
                               ResultVector &RV) = 0;

  /// When evaluating an expression in the context of an existing source file,
  /// we may want to prefer declarations from that source file.
  /// The DebuggerClient can return a private-discriminator to tell lookup to
  /// prefer these certain decls.
  virtual Identifier getPreferredPrivateDiscriminator() = 0;

  virtual SILDebuggerClient *getAsSILDebuggerClient() = 0;
private:
  virtual void anchor();
};

} // namespace swift

#endif
