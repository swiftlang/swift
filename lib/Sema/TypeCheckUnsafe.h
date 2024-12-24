//===--- TypeCheckUnasfe.h - Strict Safety Diagnostics ----------*- C++ -*-===//
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

#ifndef SWIFT_SEMA_TYPE_CHECK_UNSAFE_H
#define SWIFT_SEMA_TYPE_CHECK_UNSAFE_H

#include "swift/AST/UnsafeUse.h"

namespace swift {

/// Diagnose the given unsafe use right now.
void diagnoseUnsafeUse(const UnsafeUse &use, bool asNote = false);

/// Diagnose any unsafe uses within the signature or definition of the given
/// declaration, if there are any.
void diagnoseUnsafeUsesIn(const Decl *decl);

/// Determine whether a reference to this declaration is considered unsafe,
/// either explicitly (@unsafe) or because it references an unsafe type.
bool isUnsafe(ConcreteDeclRef declRef);

}

#endif // SWIFT_SEMA_TYPE_CHECK_UNSAFE_H
