// RUN: not %target-swift-frontend -F %S/Inputs/defer-supermodule-import -import-objc-header %S/Inputs/defer-supermodule-import/Some-Bridging-Header.h -typecheck %s 2>&1 | %FileCheck -check-prefix=HEADER-ERROR %s
// HEADER-ERROR: Some-Bridging-Header.h:4:13: error: expected a type
// HEADER-ERROR: Some-Bridging-Header.h:7:10: error: declaration of 'TYPE' must be imported from module 'Some' before it is required
// REQUIRES: objc_interop

// The bug we're testing here is that:
//
//  - Given a supermodule defining some types
//  - Given a submodule of that supermodule
//  - Given an _erroneous_ bridging header that imports the _submodule_ and tries
//    to use the _supermodule's_ types
//
// That we emit an error. Previously we did not: Swift's "implicit supermodule
// import" rule would fire _eagerly_, so by the time the submodule import was
// complete the supermodule was also imported and the bridging header would pass
// through without flagging the error. This made it possible to write textual
// bridging headers that would not, themselves, be considered valid objc code as
// far as clang (or more importantly: clang's PCH-generating pass) was
// concerned.

print(bar(foo()))
