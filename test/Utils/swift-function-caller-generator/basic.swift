// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-function-caller-generator Test %t/test.swift > %t/out.swift
// RUN: %diff %t/out.swift %t/out.swift.expected

//--- test.swift
func foo(x: Int) -> Int

func bar(_ y: UnsafePointer<CInt>)

@_lifetime(borrow z)
func baz(_ z: Span<CInt>) -> Span<CInt>

@_lifetime(`func`: copy `func`)
func qux(_ func: inout MutableSpan<CInt>)

//--- out.swift.expected
import Test

func call_foo(x: Int) -> Int {
  return foo(x: x)
}


func call_bar(_ y: UnsafePointer<CInt>) {
  return unsafe bar(y)
}


@_lifetime(borrow z)
func call_baz(_ z: Span<CInt>) -> Span<CInt> {
  return baz(z)
}


@_lifetime(`func`: copy `func`)
func call_qux(_ func: inout MutableSpan<CInt>) {
  return qux(&`func`)
}
