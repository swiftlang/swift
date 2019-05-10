//===--- Utils.h - Index utilities that are generally useful ----*- C++ -*-===//
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

#ifndef SWIFT_INDEX_UTILS_H
#define SWIFT_INDEX_UTILS_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
class ValueDecl;

/// Collect all the protocol requirements that a given declaration can
///   provide default implementations for. VD is a declaration in extension
///   declaration. Scratch is the buffer to collect those protocol
///   requirements.
///
/// \returns the slice of Scratch
ArrayRef<ValueDecl*>
canDeclProvideDefaultImplementationFor(ValueDecl* VD,
                                       llvm::SmallVectorImpl<ValueDecl*> &Scratch);

/// Get decls that the given decl overrides, protocol requirements that
///   it serves as a default implementation of, and optionally protocol
///   requirements it satisfies in a conforming class
std::vector<ValueDecl*>
getOverriddenDecls(ValueDecl *VD, bool IncludeProtocolRequirements = true,
                   bool Transitive = false);

} // end namespace swift
#endif // SWIFT_INDEX_UTILS_H
