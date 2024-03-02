// RUN: not %target-swiftc_driver -swift-version 6 -typecheck %s 2>&1 | %FileCheck --check-prefix ERROR_6 %s
// RUN: not %target-swiftc_driver -swift-version 7 -typecheck %s 2>&1 | %FileCheck --check-prefix ERROR_7 %s

#if swift(>=3)
asdf
// ERROR_6: [[@LINE-1]]:1: error: {{cannot find 'asdf' in scope}}
#else
jkl
#endif

#if swift(>=3.1)
asdf
// ERROR_6: [[@LINE-1]]:1: error: {{cannot find 'asdf' in scope}}
#else
jkl
#endif

#if swift(>=4)
asdf 
// ERROR_6: [[@LINE-1]]:1: error: {{cannot find 'asdf' in scope}}
#else
jkl
#endif

#if swift(>=4.1)
asdf
// ERROR_6: [[@LINE-1]]:1: error: {{cannot find 'asdf' in scope}}
#else
jkl
#endif

#if swift(>=6)
asdf
// ERROR_6: [[@LINE-1]]:1: error: {{cannot find 'asdf' in scope}}
#else
jkl
#endif

#if swift(>=7)
asdf
#else
jkl
// ERROR_6: [[@LINE-1]]:1: error: {{cannot find 'jkl' in scope}}
#endif

// ERROR_7: <unknown>:0: error: invalid value '7' in '-swift-version 7'
// ERROR_7: <unknown>:0: note: valid arguments to '-swift-version' are '4', '4.2', '5', '6'
