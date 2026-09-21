// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -Xcc -std=c++20 %if OS_FAMILY=darwin %{ -verify-additional-prefix darwin- %}
// REQUIRES: std_span

import StdSpan

func takesSequence<T: Sequence>(_ _: T) {}
// expected-note@-1 {{where 'T' = 'SpanOfNonCopyable'}}

@available(SwiftStdlib 6.4, *)
func takesIterable<T: CxxIterable>(_ _: T) where T.Element: ~Copyable {}

func takesSpan<S: CxxMutableSpan>(_ _: S) where S.Element: ~Copyable {}

let arr: [Int32] = [1, 2, 3]
arr.withUnsafeBufferPointer { ubpointer in
    let _ = ConstSpanOfInt(ubpointer) // okay
    let _ = ConstSpanOfInt(ubpointer.baseAddress!, ubpointer.count) 
    // expected-warning@-1 {{'init(_:_:)' is deprecated: use 'init(_:)' instead.}}
}

arr.withUnsafeBufferPointer { ubpointer in 
    // FIXME: this crashes the compiler once we import span's templated ctors as Swift generics.
    let _ = ConstSpanOfInt(ubpointer.baseAddress, ubpointer.count)
    // expected-warning@-1 {{'init(_:_:)' is deprecated: use 'init(_:)' instead.}}
}

let s1 = initSpan()
for _ in s1 {}
takesSequence(s1)
if #available(SwiftStdlib 6.4, *) {
  takesIterable(s1)
}
takesSpan(s1)

let s2 = makeSpanOfNonCopyable()
for _ in s2 {}
// expected-darwin-error@-1 {{conform to 'Iterable', which is only available in}}
// expected-darwin-note@-2 {{add 'if #available' version check}}
takesSequence(s2)
// expected-error@-1 {{global function 'takesSequence' requires that 'SpanOfNonCopyable'}} conform to 'Sequence'
takesIterable(s2)
// expected-darwin-error@-1 {{'takesIterable' is only available}}
// expected-darwin-note@-2 {{add 'if #available' version check}}
takesSpan(s2)
