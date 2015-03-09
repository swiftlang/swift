//===--- RuntimeStubs.h ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Misc stubs for functions which should be defined in the core standard
// library, but are difficult or impossible to write in Swift at the
// moment.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_RUNTIMESTUBS_H_
#define SWIFT_STDLIB_SHIMS_RUNTIMESTUBS_H_

#include "DarwinShims.h"

__swift_ssize_t swift_stdlib_readLine_stdin(char **LinePtr);

#endif // SWIFT_STDLIB_SHIMS_RUNTIMESTUBS_H_

