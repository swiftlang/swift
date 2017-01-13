//===--- References.h - Swift Syntax Reference-Counting Misc. ---*- C++ -*-===//
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

#ifndef SWIFT_SYNTAX_REFERENCES_H
#define SWIFT_SYNTAX_REFERENCES_H

#include "llvm/ADT/IntrusiveRefCntPtr.h"

namespace swift {
namespace syntax {

/// A shorthand to clearly indicate that a value is a reference counted and
/// heap-allocated.
template <typename Inner>
using RC = llvm::IntrusiveRefCntPtr<Inner>;

} // end namespace syntax
} // end namespae swift

#endif // SWIFT_SYNTAX_REFERENCES_H
