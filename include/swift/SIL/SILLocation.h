//===--- SILLocation.h - Location information for SIL nodes -----*- C++ -*-===//
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

#ifndef SWIFT_SIL_LOCATION_H
#define SWIFT_SIL_LOCATION_H

#include "llvm/ADT/PointerUnion.h"

namespace swift {
class Decl;
class Expr;
class Stmt;

/// SILLocation - This is a pointer to the AST node that a SIL instruction was
/// derived from. This may be null if AST information is unavailable or
/// stripped.
///
/// FIXME: This should eventually include inlining history, generics
/// instantiation info, etc (when we get to it).
///
typedef llvm::PointerUnion3<Stmt*,Expr*, Decl*> SILLocation;
  
} // end swift namespace


#endif
