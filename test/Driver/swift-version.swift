// RUN: not %target-swiftc_driver -swift-version foo %s 2>&1 | %FileCheck --check-prefix BAD %s
// RUN: not %target-swiftc_driver -swift-version 1 %s 2>&1 | %FileCheck --check-prefix BAD %s
// RUN: not %target-swiftc_driver -swift-version 2 %s 2>&1 | %FileCheck --check-prefix BAD %s
// RUN: not %target-swiftc_driver -swift-version 2.3 %s 2>&1 | %FileCheck --check-prefix BAD %s
// RUN: not %target-swiftc_driver -swift-version 7 %s 2>&1 | %FileCheck --check-prefix BAD %s
// RUN: not %target-swiftc_driver -swift-version 7.2 %s 2>&1 | %FileCheck --check-prefix BAD %s
// RUN: not %target-swiftc_driver -swift-version 3.0 %s 2>&1 | %FileCheck --check-prefix FIXIT_3 %s
// RUN: not %target-swiftc_driver -swift-version 3.3 %s 2>&1 | %FileCheck --check-prefix FIXIT_3 %s
// RUN: not %target-swiftc_driver -swift-version 4.3 %s 2>&1 | %FileCheck --check-prefix FIXIT_4 %s

// RUN: not %target-swiftc_driver -swift-version 3 -typecheck %s 2>&1 | %FileCheck --check-prefix ERROR_3 %s
// RUN: not %target-swiftc_driver -swift-version 4 -typecheck %s 2>&1 | %FileCheck --check-prefix ERROR_4 %s

// BAD: invalid value
// BAD: note: valid arguments to '-swift-version' are '3', '4'

// FIXIT_3: use major version, as in '-swift-version 3'
// FIXIT_4: use major version, as in '-swift-version 4'


#if swift(>=3)
asdf
// ERROR_3: [[@LINE-1]]:1: error: {{use of unresolved identifier}}
// ERROR_4: [[@LINE-2]]:1: error: {{use of unresolved identifier}}
#else
jkl
#endif

#if swift(>=3.1)
asdf
// ERROR_3: [[@LINE-1]]:1: error: {{use of unresolved identifier}}
// ERROR_4: [[@LINE-2]]:1: error: {{use of unresolved identifier}}
#else
jkl
#endif

#if swift(>=4)
asdf // ERROR_4: [[@LINE]]:1: error: {{use of unresolved identifier}}
#else
jkl // ERROR_3: [[@LINE]]:1: error: {{use of unresolved identifier}}
#endif

#if swift(>=4.1)
asdf
#else
jkl
// ERROR_3: [[@LINE-1]]:1: error: {{use of unresolved identifier}}
// ERROR_4: [[@LINE-2]]:1: error: {{use of unresolved identifier}}
#endif

#if swift(>=5)
asdf
#else
jkl
// ERROR_3: [[@LINE-1]]:1: error: {{use of unresolved identifier}}
// ERROR_4: [[@LINE-2]]:1: error: {{use of unresolved identifier}}
#endif
