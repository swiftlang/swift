// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop -Xcc -std=c++20 2>&1 -disable-availability-checking
// REQUIRES: std_span

import StdSpan

func takesSequence<T: Sequence>(_ _: T) {}
// expected-note@-1 {{where 'T' = 'SpanOfNonCopyable'}}

func takesBorrowingSequence<T: CxxBorrowingSequence>(_ _: T) where T.Element: ~Copyable {}

func takesSpan<S: CxxMutableSpan>(_ _: S) where S.Element: ~Copyable {}

let arr: [Int32] = [1, 2, 3]
arr.withUnsafeBufferPointer { ubpointer in
    let _ = ConstSpanOfInt(ubpointer) // okay
    let _ = ConstSpanOfInt(ubpointer.baseAddress!, ubpointer.count) 
}

arr.withUnsafeBufferPointer { ubpointer in 
    // FIXME: this crashes the compiler once we import span's templated ctors as Swift generics.
    let _ = ConstSpanOfInt(ubpointer.baseAddress, ubpointer.count)
}

let s1 = initSpan()
for _ in s1 {}
takesSequence(s1)
takesBorrowingSequence(s1)
takesSpan(s1)

let s2 = makeSpanOfNonCopyable()
for _ in s2 {}
// expected-error@-1 {{for-in loop requires 'SpanOfNonCopyable'}} to conform to 'Sequence'
takesSequence(s2)
// expected-error@-1 {{global function 'takesSequence' requires that 'SpanOfNonCopyable'}} conform to 'Sequence'
takesBorrowingSequence(s2)
takesSpan(s2)
