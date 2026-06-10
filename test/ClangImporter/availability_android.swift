// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs/custom-modules -verify-ignore-unknown -target aarch64-unknown-linux-android24 %s

// REQUIRES: OS=linux-android || OS=linux-androideabi

// Disable this test till we get a LTS NDK on the CI that supports it.
// REQUIRES: NDK28

import AndroidVersioning
import Android

FunctionIntroducedIn24()

FunctionIntroducedIn28()
// expected-error@-1 {{'FunctionIntroducedIn28()' is only available in Android 28 or newer}}
// expected-note@-2 {{add 'if #available' version check}}

func test_ifaddrs_introduced_in_24() {
    var ifaddr_ptr: UnsafeMutablePointer<ifaddrs>? = nil
    if getifaddrs(&ifaddr_ptr) == 0 {
        freeifaddrs(ifaddr_ptr)
    }
}

func test_getentropy_introduced_in_28() {
    // expected-note@-1 {{add '@available' attribute to enclosing global function}}

    var buffer: [UInt8] = .init(repeating: 0, count: 16)
    _ = getentropy(&buffer, buffer.count)
    // expected-error@-1 {{'getentropy' is only available in Android 28 or newer}}
    // expected-note@-2 {{add 'if #available' version check}}
}
