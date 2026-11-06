// RUN: not %target-swiftc_driver -swift-version 7 -typecheck %s 2>&1 | %FileCheck --check-prefix ERROR_7 %s
// RUN: not %target-swiftc_driver -swift-version future -typecheck %s 2>&1 | %FileCheck --check-prefix ERROR_FUTURE %s
// REQUIRES: no_asserts

// ERROR_7: <unknown>:0: error: invalid value '7' in '-language-mode 7'
// ERROR_7: <unknown>:0: note: valid arguments to '-language-mode' are '4', '4.2', '5', '6'

// ERROR_FUTURE: <unknown>:0: error: invalid value 'future' in '-language-mode future'
// ERROR_FUTURE: <unknown>:0: note: valid arguments to '-language-mode' are '4', '4.2', '5', '6'
