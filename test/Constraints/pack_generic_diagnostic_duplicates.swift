// RUN: %target-typecheck-verify-swift

// Test that pack generic parameter conflict diagnostics don't show
// duplicate types. This was issue #87349.

struct Data<each T> {
    init(left: repeat each T, right: repeat each T) {}
}

// Two conflicting pack arguments - should show 2 unique pack types, not 4
Data(left: 1, 2, right: 1, "hi")
// expected-error@-1 {{conflicting arguments to generic parameter 'each T' ('Pack{Int, String}' vs. 'Pack{Int, Int}')}}

// Three conflicting positions - should not show exact duplicates
Data(left: 1, 2, 3, right: 1, "hi", true)
// expected-error@-1 {{conflicting arguments to generic parameter 'each T'}}

struct Data2<each T> {
    init(a: repeat each T, b: repeat each T, c: repeat each T) {}
}

// Three conflicting arguments - should not show exact duplicates
Data2(a: 1, 2, 3, b: 1, 2, true, c: "a", "b", "c")
// expected-error@-1 {{conflicting arguments to generic parameter 'each T'}}

func foo<each T>(left: repeat each T, right: repeat each T) {}

// Function with two pack parameters - should not show duplicates
foo(left: 1, 2, 3, right: 1, "hi", true)
// expected-error@-1 {{conflicting arguments to generic parameter 'each T'}}

func bar<each T>(left: repeat each T, right: repeat each T) {}

// Only last element differs - should show exactly 2 types
bar(left: 1, 2, 3, right: 1, 2, true)
// expected-error@-1 {{conflicting arguments to generic parameter 'each T' ('Pack{Int, Int, Bool}' vs. 'Pack{Int, Int, Int}')}}
