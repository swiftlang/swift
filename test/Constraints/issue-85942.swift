// RUN: %target-typecheck-verify-swift -disable-experimental-parser-round-trip

// https://github.com/swiftlang/swift/issues/85942
// Verify that we do not crash when a KeyPath is used in a map with missing arguments.

func f(buttonsStr: [Substring]) {
    // expected-error@+2 {{key path cannot refer to instance method 'compactMap'}}
    // expected-error@+1 {{cannot convert value of type '() -> Int' to expected argument type '(ArraySlice<Character>) throws -> Int?'}}
    let _: [[Int]] = buttonsStr.map(\.split(separator: ",").compactMap(Int.init))
}
