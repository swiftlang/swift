// RUN: %target-swift-frontend -typecheck -verify %s -import-objc-header %S/Inputs/sdk-bridging-header.h
// RUN: not %target-swift-frontend -typecheck %s -import-objc-header %S/Inputs/bad-bridging-header.h 2>&1 | %FileCheck -check-prefix=CHECK-FATAL %s

// RUN: %target-swift-frontend -typecheck -verify %s -Xcc -include -Xcc %S/Inputs/sdk-bridging-header.h -import-objc-header %S/../Inputs/empty.swift

// RUN: not %target-swift-frontend -typecheck %s -Xcc -include -Xcc %S/Inputs/bad-bridging-header.h 2>&1 | %FileCheck -check-prefix=CHECK-INCLUDE %s
// RUN: not %target-swift-frontend -typecheck %s -Xcc -include -Xcc %S/Inputs/bad-bridging-header.h -import-objc-header %S/../Inputs/empty.swift 2>&1 | %FileCheck -check-prefix=CHECK-INCLUDE %s
// RUN: not %target-swift-frontend -typecheck %s -Xcc -include -Xcc %S/Inputs/bad-bridging-header.h -import-objc-header %S/Inputs/sdk-bridging-header.h 2>&1 | %FileCheck -check-prefix=CHECK-INCLUDE %s

// CHECK-FATAL: failed to import bridging header

// CHECK-INCLUDE: error: 'this-header-does-not-exist.h' file not found
// CHECK-INCLUDE: error: use of unresolved identifier 'MyPredicate'

// REQUIRES: objc_interop

import Foundation

let `true` = MyPredicate.`true`()
let not = MyPredicate.not()
let and = MyPredicate.and([])
let or = MyPredicate.or([not, and])

_ = MyPredicate.foobar() // expected-error{{type 'MyPredicate' has no member 'foobar'}}
