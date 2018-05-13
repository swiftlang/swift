// RUN: %target-swift-emit-silgen -Xllvm -sil-full-demangle %s
// XFAIL: *

class C<T> {
  func f(t: T) { print(t) }
}

class B : C<(Int, Float)> {
  override func f(t: (Int, Float)) {
  	print(t)
  }
}

let fn2: (B) -> ((Int, Float)) -> () = B.f

fn2(B())((100, 200.0))
