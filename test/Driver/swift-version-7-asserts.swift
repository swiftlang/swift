// RUN: not %target-swiftc_driver -swift-version 7 -typecheck %s 2>&1 | %FileCheck --check-prefix ERROR_7 %s
// REQUIRES: swift7

#if swift(>=3)
asdf
// ERROR_7: [[@LINE-1]]:1: error: {{cannot find 'asdf' in scope}}
#else
jkl
#endif

#if swift(>=3.1)
asdf
// ERROR_7: [[@LINE-1]]:1: error: {{cannot find 'asdf' in scope}}
#else
jkl
#endif

#if swift(>=4)
asdf
// ERROR_7: [[@LINE-1]]:1: error: {{cannot find 'asdf' in scope}}
#else
jkl
#endif

#if swift(>=4.1)
asdf
// ERROR_7: [[@LINE-1]]:1: error: {{cannot find 'asdf' in scope}}
#else
jkl
#endif

#if swift(>=6)
asdf
// ERROR_7: [[@LINE-1]]:1: error: {{cannot find 'asdf' in scope}}
#else
jkl
#endif

#if swift(>=7)
asdf
// ERROR_7: [[@LINE-1]]:1: error: {{cannot find 'asdf' in scope}}
#else
jkl
#endif
