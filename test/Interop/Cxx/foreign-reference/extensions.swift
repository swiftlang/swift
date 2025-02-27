// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xfrontend -disable-availability-checking -Onone)
// REQUIRES: executable_test

import ReferenceCounted

extension NS.LocalCount {
    static func g() {}

    public func f() {
        Self.g()
    }
}
