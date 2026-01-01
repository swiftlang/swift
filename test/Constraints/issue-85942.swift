// RUN: %target-typecheck-verify-swift

// expected-error@1 {{new Swift parser generated errors for code that C++ parser accepted}}
// https://github.com/swiftlang/swift/issues/85942
// Verify that we do not crash when a KeyPath is used in a map with missing arguments.

func f(buttonsStr: [Substring]) {
    // expected-error@+3 {{unexpected code '(separator: ",").compactMap(Int.init)' in function call}}
    // expected-error@+2 {{missing arguments for parameters 'maxSplits', 'omittingEmptySubsequences' in call}}
    // expected-error@+1 {{key path cannot refer to instance method 'compactMap'}}
    let _: [[Int]] = buttonsStr.map(\.split(separator: ",").compactMap(Int.init))
}
