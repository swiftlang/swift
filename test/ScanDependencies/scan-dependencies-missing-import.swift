// RUN: not %target-swiftc_driver -scan-dependencies %s -o %t/deps.json 2>&1 | %FileCheck %s

import invalid_module_that_never_exists

// CHECK: error: unable to resolve module dependency: 'invalid_module_that_never_exists'
