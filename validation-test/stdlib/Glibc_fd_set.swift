//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=linux-gnu

// '__fds_bits' is the spelling of 'fd_set''s member from before we defined
// '_GNU_SOURCE'. The old spelling is preserved as a deprecated accessor
// by the Glibc overlay.
//
// Note: This should only import only 'Glibc' here, nothing else.
// The alias is declared on 'SwiftGlibc.fd_set'; if the module name
// were removed, the extension might bind to some other version of 'fd_set',
// and might not be re-exported by the 'Glibc' module. '<sys/select.h>'
// is not declared in 'glibc.modulemap', so each module that includes it
// gets its own copy of 'fd_set'.
import Glibc

var set = fd_set()
set.fds_bits.3 = 42

// CHECK: same type: true
print("same type: \(type(of: set.__fds_bits) == type(of: set.fds_bits))")

// CHECK-NEXT: same value: 42
print("same value: \(set.__fds_bits.3)")
