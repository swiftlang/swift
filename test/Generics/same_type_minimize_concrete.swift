// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

struct G<T> { }

protocol P {
  associatedtype A
  associatedtype B
}

// FIXME: We minimize the signatures correctly, but the warnings are
// slightly bogus.

// expected-warning@+2 {{redundant same-type constraint 'T.B' == 'Int'}}
// expected-note@+1 {{same-type constraint 'T.B' == 'Int' written here}}
func test1<T: P>(_: T) where T.A == G<Int>, T.A == G<T.B>, T.B == Int { }
// CHECK: Generic signature: <T where T : P, T.A == G<Int>, T.B == Int>

// expected-warning@+2 {{redundant same-type constraint 'T.A' == 'G<Int>'}}
// expected-note@+1 {{same-type constraint 'T.A' == 'G<Int>' written here}}
func test2<T: P>(_: T) where T.A == G<Int>, T.B == Int, T.A == G<T.B> { }
// CHECK: Generic signature: <T where T : P, T.A == G<Int>, T.B == Int>

// expected-warning@+2 {{redundant same-type constraint 'T.B' == 'Int'}}
// expected-note@+1 {{same-type constraint 'T.B' == 'Int' written here}}
func test3<T: P>(_: T) where T.A == G<T.B>, T.A == G<Int>, T.B == Int { }
// CHECK: Generic signature: <T where T : P, T.A == G<Int>, T.B == Int>

func test4<T: P>(_: T) where T.A == G<T.B>, T.B == Int, T.A == G<Int> { }
// CHECK: Generic signature: <T where T : P, T.A == G<Int>, T.B == Int>

// expected-warning@+2 {{redundant same-type constraint 'T.A' == 'G<Int>'}}
// expected-note@+1 {{same-type constraint 'T.A' == 'G<Int>' written here}}
func test5<T: P>(_: T) where T.B == Int, T.A == G<Int>, T.A == G<T.B> { }
// CHECK: Generic signature: <T where T : P, T.A == G<Int>, T.B == Int>

// expected-warning@+2 {{redundant same-type constraint 'T.A' == 'G<Int>'}}
// expected-note@+1 {{same-type constraint 'T.A' == 'G<Int>' written here}}
func test6<T: P>(_: T) where T.B == Int, T.A == G<T.B>, T.A == G<Int> { }
// CHECK: Generic signature: <T where T : P, T.A == G<Int>, T.B == Int>

func test7<T: P>(_: T) where T.B == Int, T.A == G<T.B> { }
// CHECK: Generic signature: <T where T : P, T.A == G<Int>, T.B == Int>

func test8<T: P>(_: T) where T.A == G<T.B>, T.B == Int { }
// CHECK: Generic signature: <T where T : P, T.A == G<Int>, T.B == Int>

func test9<T: P>(_: T) where T.B == Int, T.A == G<Int> { }
// CHECK: Generic signature: <T where T : P, T.A == G<Int>, T.B == Int>

func test10<T: P>(_: T) where T.A == G<Int>, T.B == Int { }
// CHECK: Generic signature: <T where T : P, T.A == G<Int>, T.B == Int>

func test11<T: P>(_: T) where T.A == G<T.B>, T.A == G<Int> { }
// CHECK: Generic signature: <T where T : P, T.A == G<Int>, T.B == Int>

func test12<T: P>(_: T) where T.A == G<Int>, T.A == G<T.B> { }
// CHECK: Generic signature: <T where T : P, T.A == G<Int>, T.B == Int>

// CHECK-NOT: Generic signature
