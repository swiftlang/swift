//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <sys/uio.h>

// process_vm_readv/writeva are available in glibc but are not in the headers
// provided with the Swift toolchain
ssize_t process_vm_readv(pid_t, const struct iovec*, unsigned long,
    const struct iovec*, unsigned long, unsigned long);
ssize_t process_vm_writev(pid_t, const struct iovec*, unsigned long,
    const struct iovec*, unsigned long, unsigned long);
