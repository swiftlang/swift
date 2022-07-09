// Tests temporary -swift-version 6 behavior in compilers with asserts disabled,
// where we don't allow -swift-version 6 to keep it out of release compilers.

// UNSUPPORTED: asserts

// RUN: not %target-swiftc_driver -swift-version 6 -typecheck %s 2>&1 | %FileCheck --check-prefix ERROR_6 %s

// ERROR_6: <unknown>:0: error: invalid value '6' in '-swift-version 6'
// ERROR_7: <unknown>:0: note: valid arguments to '-swift-version'
// ERROR_7-NOT: '6'
