//===--- SILDebugScope.h - DebugScopes for SIL code -----------*- C++ -*-===//
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
// This file defines a container for scope information used to
// generate debug info.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_DEBUGSCOPE_H
#define SWIFT_SIL_DEBUGSCOPE_H

#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILLocation.h"

namespace swift {

/// SILDebugScope - This class stores a lexical scope as it is
/// presented to the debugger.
class SILDebugScope : public SILAllocated<SILDebugScope> {
public:
  SILLocation Loc;
  SILDebugScope* Parent;

  SILDebugScope(SILLocation Loc = (Expr*)nullptr,
                SILDebugScope* Parent = nullptr)
    :Loc(Loc), Parent(Parent)
  {}

  void setParent(SILDebugScope* P) { Parent = P; }
};

}

#endif
