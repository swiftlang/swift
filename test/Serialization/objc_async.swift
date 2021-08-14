// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)
// RUN: %target-swift-frontend -emit-module -o %t-scratch/def_objc_async~partial.swiftmodule -primary-file %S/Inputs/def_objc_async.swift -module-name def_objc_async  -disable-availability-checking
// RUN: %target-swift-frontend -merge-modules -emit-module -parse-as-library -enable-testing %t-scratch/def_objc_async~partial.swiftmodule -module-name def_objc_async -o %t/def_objc_async.swiftmodule  -disable-availability-checking
// RUN: %target-swift-frontend -typecheck -I%t -verify %s  -disable-availability-checking

// REQUIRES: concurrency
// REQUIRES: objc_interop

import def_objc_async
import Foundation

class Derived: Base {
}

extension Derived {
	override func foo(id: Int) async { }
}
