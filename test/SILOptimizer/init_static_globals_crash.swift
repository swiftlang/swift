// RUN: %target-swift-frontend -parse-as-library -emit-sil %s -o /dev/null -verify

// Don't crash in the initializeStaticGlobalsPass when the global is not valid.

let f: @convention(c) () -> Void = { // expected-error {{a C function pointer cannot be formed from a closure that captures generic parameters}}
    func g<T>(_ x: T) {}
}
