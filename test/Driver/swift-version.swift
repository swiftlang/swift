// RUN: %target-swiftc_driver -swift-version 3 %s
// RUN: %target-swiftc_driver -swift-version 3.0 %s
// RUN: not %target-swiftc_driver -swift-version 1 %s 2>&1 | %FileCheck --check-prefix BAD %s
// RUN: not %target-swiftc_driver -swift-version 2 %s 2>&1 | %FileCheck --check-prefix BAD %s
// RUN: not %target-swiftc_driver -swift-version 2.3 %s 2>&1 | %FileCheck --check-prefix BAD %s
// RUN: not %target-swiftc_driver -swift-version 7 %s 2>&1 | %FileCheck --check-prefix BAD %s
// RUN: not %target-swiftc_driver -swift-version 7.2 %s 2>&1 | %FileCheck --check-prefix BAD %s

// BAD: invalid value

let x = 1
