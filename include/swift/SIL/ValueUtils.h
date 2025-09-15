//===--- ValueUtils.h -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_VALUEUTILS_H
#define SWIFT_SIL_VALUEUTILS_H

#include "swift/SIL/SILValue.h"

namespace swift {

/// Attempt to merge the ValueOwnershipKind of the passed in range's
/// SILValues. Returns OwnershipKind::Any if we found an incompatibility.
/// If \p is a move-only type, we return OwnershipKind::Owned since such
/// values can have deinit side-effects.
///
/// NOTE: This assumes that the passed in SILValues are not values used as type
/// dependent operands.
ValueOwnershipKind getSILValueOwnership(ArrayRef<SILValue> values,
                                        SILType ty = SILType());

} // namespace swift

#endif
