//===--- ConcurrencyUtils.h -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_CONCURRENCYUTILS_H
#define SWIFT_SIL_CONCURRENCYUTILS_H

#include "swift/SIL/SILType.h"

namespace swift {

class SILValue;
class SILBuilder;
class SILLocation;

/// Clear the implicit isolated bits of value.
///
/// \p value must be Builtin.ImplicitActor
///
/// \p finalType if empty, we always return
/// Builtin.ImplicitActor. Otherwise we bitcast to finalType after
/// tieing the lifetime of the result to \p value.
SILValue clearImplicitActorBits(SILBuilder &b, SILLocation loc, SILValue value,
                                SILType finalType = {});

SILValue setImplicitActorBits(SILBuilder &b, SILLocation loc, SILValue value);

} // namespace swift

#endif
