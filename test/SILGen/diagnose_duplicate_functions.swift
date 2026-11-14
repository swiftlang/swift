// RUN: %target-swift-emit-silgen-ossa -enable-sil-opaque-values -module-name diagnose_duplicate_functions %s -o /dev/null -verify
// RUN: %target-swift-emit-silgen -module-name diagnose_duplicate_functions %s -o /dev/null -verify

// Also check with FileCheck to differentiate between the "please file a bug report".
// TODO: We ought to have a way to match an exact message for the verifier.
// RUN: not %target-swift-emit-silgen -module-name diagnose_duplicate_functions %s -o /dev/null -diagnostic-style=llvm 2>&1 | %FileCheck %s

@_silgen_name("foo")
func a(_ x: Int) -> Int { // expected-note {{other definition here}}
  return x
}

@_silgen_name("foo")
func b(_ x: Int) -> Int { // expected-error {{multiple definitions of symbol 'foo'}}
  return x
}
// CHECK: error: multiple definitions of symbol 'foo'{{$}}

@_cdecl("bar")
func c(_ x: Int) -> Int { // duplication only detected at the LLVM IR level
  return x
}

@_cdecl("bar")
func d(_ x: Int) -> Int { // duplication only detected at the LLVM IR level
  return x
}

@_silgen_name("$s28diagnose_duplicate_functions1fyyF")
func e() {} // expected-note {{other definition here}}

func f() {} // expected-error {{multiple definitions of symbol '$s28diagnose_duplicate_functions1fyyF'}}
// CHECK: error: multiple definitions of symbol '$s28diagnose_duplicate_functions1fyyF'{{$}}

func g() {} // expected-note {{other definition here}}

@_silgen_name("$s28diagnose_duplicate_functions1gyyF")
func h() {} // expected-error {{multiple definitions of symbol '$s28diagnose_duplicate_functions1gyyF'}}
// CHECK: error: multiple definitions of symbol '$s28diagnose_duplicate_functions1gyyF'{{$}}

struct S {
  @_silgen_name("baz")
  init(a: Int) {} // expected-note {{other definition here}}

  @_silgen_name("baz")
  init(b: Int) {} // expected-error {{multiple definitions of symbol 'baz'}}
  // CHECK: error: multiple definitions of symbol 'baz'{{$}}
}
