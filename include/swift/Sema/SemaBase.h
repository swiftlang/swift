//===--- SemaBase.h - Swift Semantic Analysis Utilities ---------*- C++ -*-===//
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
// This file defines the SemaBase interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMABASE_H
#define SWIFT_SEMABASE_H

namespace llvm {
  class SMLoc;
  class StringRef;
}

namespace swift {
  class Sema;
  class Expr;
  
/// SemaBase - Semantic analysis support for Swift expressions.  This provides
/// the common stuff used by all sema modules.  It should not have any
/// non-trivial state because it is replicated into every SemaXXX subclass.
class SemaBase {
  SemaBase(const SemaBase&);           // DO NOT IMPLEMENT
  void operator=(const SemaBase&);     // DO NOT IMPLEMENT
protected:
  Sema &S;
public:
  SemaBase(Sema &s);
  
  // TODO: Diagnostics stuff.
};
    
} // end namespace swift

#endif
