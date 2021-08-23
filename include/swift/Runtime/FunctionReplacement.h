//===--- FunctionReplacement.h --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_FUNCTIONREPLACEMENT_H
#define SWIFT_RUNTIME_FUNCTIONREPLACEMENT_H

#include "swift/Runtime/Config.h"

namespace swift {

/// Loads the replacement function pointer from \p ReplFnPtr and returns the
/// replacement function if it should be called.
/// Returns null if the original function (which is passed in \p CurrFn) should
/// be called.
#ifdef __APPLE__
__attribute__((weak_import))
#endif
SWIFT_RUNTIME_EXPORT char *
swift_getFunctionReplacement(char **ReplFnPtr, char *CurrFn);

/// Returns the original function of a replaced function, which is loaded from
/// \p OrigFnPtr.
/// This function is called from a replacement function to call the original
/// function.
#ifdef __APPLE__
__attribute__((weak_import))
#endif
SWIFT_RUNTIME_EXPORT char *
swift_getOrigOfReplaceable(char **OrigFnPtr);

} // namespace swift

#endif
