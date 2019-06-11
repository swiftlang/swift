//===--- DynamicReplaceable.h - dynamic replaceable support -----*- C++ -*-===//
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
//
// Swift runtime support for dynamic replaceable functions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_DYNAMIC_REPLACEABLE_H
#define SWIFT_RUNTIME_DYNAMIC_REPLACEABLE_H

#include "swift/Runtime/Config.h"

namespace swift {

/// Loads the replacement function pointer from \p ReplFnPtr and returns the
/// replacement function if it should be called.
/// Returns null if the original function (which is passed in \p CurrFn) should
/// be called.
SWIFT_RUNTIME_EXPORT
char *swift_getFunctionReplacement(char **ReplFnPtr, char *CurrFn);

/// Returns the original function of a replaced function, which is loaded from
/// \p OrigFnPtr.
/// This function is called from a replacement function to call the original
/// function.
SWIFT_RUNTIME_EXPORT
char *swift_getOrigOfReplaceable(char **OrigFnPtr);

} // end namespace swift

#endif
