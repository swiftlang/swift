// RUN: %target-typecheck-verify-swift -disable-experimental-parser-round-trip

// https://github.com/swiftlang/swift/issues/85942
// Verify that we do not crash when a KeyPath is used in a map with missing arguments.

func f(buttonsStr: [Substring]) {
    // expected-cparser-error@+2 {{key path cannot refer to instance method 'compactMap'}} expected-astgen-error@+2 {{key path cannot refer to instance method 'split(separator:maxSplits:omittingEmptySubsequences:)'}}
    // expected-cparser-error@+1 {{cannot convert value of type '() -> Int' to expected argument type '(ArraySlice<Character>) throws -> Int?'}} expected-astgen-error@+1 {{unexpected code '(separator: ",").compactMap(Int.init)' in function call}}
    let _: [[Int]] = buttonsStr.map(\.split(separator: ",").compactMap(Int.init)) // expected-astgen-error {{key path value type '(Character, Int, Bool) -> [Substring.SubSequence]' (aka '(Character, Int, Bool) -> Array<Substring>') cannot be converted to contextual type '[Int]'}}
}
