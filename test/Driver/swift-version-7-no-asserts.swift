// RUN: not %target-swiftc_driver -swift-version 7 -typecheck %s 2>&1 | %FileCheck --check-prefix ERROR_7 %s
// REQUIRES: no_asserts

// ERROR_7: <unknown>:0: error: invalid value '7' in '-swift-version 7'
// ERROR_7: <unknown>:0: note: valid arguments to '-swift-version' are '4', '4.2', '5', '6'
