// This test is by no means exhaustive, but attempts to catch any places in the
// implementation of JSONASTDumper where it might do an unprotected call to
// some method on AST node that would cause an assertion due to some
// unsatisfied precondition. This is a good place to put regression tests if
// issues are discovered in the future.

// REQUIRES: objc_interop
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -swift-version 6 -parse-as-library -dump-ast -dump-ast-format json %s -module-name main -o -

import Foundation

@objc public class NotRenamed: NSObject {}

@objc(PFXRenamed) @objcMembers public class Renamed: NSObject {
    @objc(f1) public func f1() {}
    @objc(f2WithInteger:) public func f2(_ x: Int) {}
    @objc(f3withInteger:error:) public func f3(_ x: Int) throws {}

    @objc(strobjc) public var str: NSString = ""
}

func f() {
    _ = #selector(Renamed.f1)
    _ = #keyPath(Renamed.str.length)
}
