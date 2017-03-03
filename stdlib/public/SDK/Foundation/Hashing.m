//===----------------------------------------------------------------------===//
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

// THIS IS JUST TEMPORARY TO AVOID LOCKSTEP ISSUES WITH COREFOUNDATION

#import <CoreFoundation/CoreFoundation.h>
#include "swift/Runtime/Config.h"

#define HASHFACTOR 2654435761U

SWIFT_CC(swift)
CFHashCode __CFHashInt(long i) {
    return ((i > 0) ? (CFHashCode)(i) : (CFHashCode)(-i)) * HASHFACTOR;
}

SWIFT_CC(swift)
CFHashCode __CFHashDouble(double d) {
    double dInt;
    if (d < 0) d = -d;
    dInt = floor(d+0.5);
    CFHashCode integralHash = HASHFACTOR * (CFHashCode)fmod(dInt, (double)ULONG_MAX);
    return (CFHashCode)(integralHash + (CFHashCode)((d - dInt) * ULONG_MAX));
}

extern CFHashCode CFHashBytes(uint8_t *bytes, long len);

SWIFT_CC(swift)
CFHashCode __CFHashBytes(uint8_t *bytes, long len) {
  return CFHashBytes(bytes, len);
}
