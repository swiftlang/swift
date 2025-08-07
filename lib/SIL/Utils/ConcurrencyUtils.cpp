//===--- ConcurrencyUtils.cpp ---------------------------------------------===//
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

#include "swift/SIL/ConcurrencyUtils.h"

#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILLocation.h"

using namespace swift;

SILValue swift::clearImplicitActorBits(SILBuilder &b, SILLocation loc,
                                       SILValue value, SILType finalType) {
  if (!finalType)
    finalType = SILType::getBuiltinImplicitActorType(b.getASTContext());
  if (value->getType() == finalType)
    return value;
  return b.emitUncheckedValueCast(loc, value, finalType);
}

SILValue swift::setImplicitActorBits(SILBuilder &b, SILLocation loc,
                                     SILValue value) {
  return value;
}
