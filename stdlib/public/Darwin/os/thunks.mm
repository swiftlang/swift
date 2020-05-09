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

#include <swift/Runtime/Debug.h>
#include "thunks.h"

extern "C" void
_swift_os_log_reportError(uint32_t flags, const char *message)
{
	swift::swift_reportError(flags, message);
}
