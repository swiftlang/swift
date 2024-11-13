// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)
// RUN: %target-swift-frontend -emit-module -o %t-scratch/def_objc_async~partial.swiftmodule -primary-file %S/Inputs/def_objc_async.swift -module-name def_objc_async  -target %target-swift-5.1-abi-triple
// RUN: %target-swift-frontend -merge-modules -emit-module -parse-as-library -enable-testing %t-scratch/def_objc_async~partial.swiftmodule -module-name def_objc_async -o %t/def_objc_async.swiftmodule  -target %target-swift-5.1-abi-triple
// RUN: %target-swift-frontend -typecheck -I%t -verify %s  -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency
// REQUIRES: objc_interop

// Temporarily disable for asan (rdar://89808212)
// UNSUPPORTED: asan

import def_objc_async
import Foundation

class Derived: Base {
}

extension Derived {
	override func foo(id: Int) async { }
}
