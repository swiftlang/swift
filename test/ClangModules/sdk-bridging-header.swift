// RUN: %target-swift-frontend -parse -verify %s -import-objc-header %S/Inputs/sdk-bridging-header.h
// RUN: not %target-swift-frontend -parse %s -import-objc-header %S/Inputs/bad-bridging-header.h 2>&1 | FileCheck -check-prefix=CHECK-FATAL %s

// RUN: %target-swift-frontend -parse -verify %s -Xcc -include -Xcc %S/Inputs/sdk-bridging-header.h -import-objc-header %S/../Inputs/empty.swift

// RUN: not %target-swift-frontend -parse %s -Xcc -include -Xcc %S/Inputs/bad-bridging-header.h 2>&1 | FileCheck -check-prefix=CHECK-INCLUDE %s
// RUN: not %target-swift-frontend -parse %s -Xcc -include -Xcc %S/Inputs/bad-bridging-header.h -import-objc-header %S/../Inputs/empty.swift 2>&1 | FileCheck -check-prefix=CHECK-INCLUDE %s
// RUN: not %target-swift-frontend -parse %s -Xcc -include -Xcc %S/Inputs/bad-bridging-header.h -import-objc-header %S/Inputs/sdk-bridging-header.h 2>&1 | FileCheck -check-prefix=CHECK-INCLUDE %s

// CHECK-FATAL: failed to import bridging header

// CHECK-INCLUDE: error: 'this-header-does-not-exist.h' file not found
// CHECK-INCLUDE: error: use of unresolved identifier 'Predicate'

// REQUIRES: objc_interop

import Foundation

let `true` = Predicate.`true`()
let not = Predicate.not()
let and = Predicate.and([])
let or = Predicate.or([not, and])

_ = Predicate.foobar() // expected-error{{type 'Predicate' has no member 'foobar'}}
