//===--- WalkOrder.h - WalkOrder enum ---------------------------*- C++ -*-===//
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

#ifndef SWIFT_AST_WALKORDER_H
#define SWIFT_AST_WALKORDER_H

namespace swift {

/// This enum is used in AST traversals.
enum class WalkOrder {
  PreOrder,
  PostOrder
};

} // end namespace swift

#endif
