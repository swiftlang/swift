//
// Created by Kavon Farvardin on 11/19/25.
//

#ifndef SWIFT_SILGENUTILS_H
#define SWIFT_SILGENUTILS_H

#include "swift/SIL/SILValue.h"

namespace swift {

// Unsafe access may have invalid storage (e.g. a RawPointer).
bool isPossibleUnsafeAccessInvalidStorage(SILValue access, SILFunction *F);

} // namespace swift

#endif // SWIFT_SILGENUTILS_H
