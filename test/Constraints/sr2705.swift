// RUN: %target-typecheck-verify-swift

func f<T>(_: () -> T) {}
func f<T>(_: @autoclosure () -> T) {}

f { } // OK

func f1<T>(_: () -> T, _: () -> T) {}
func f1<T>(_: @autoclosure () -> T, _: @autoclosure () -> T) {}

f1({}, {}) // OK

func f2<T>(_: () -> T, _: () -> T) { }
func f2<T>(_: () -> T, _: @autoclosure () -> T) { }

f2({}, {}) // OK

func f3(_: () -> Int) {}
func f3(_: @autoclosure () -> Int) {}

f3 { 0 } // OK
 
func autoclosure(f: () -> Int) { }
func autoclosure(f: @autoclosure () -> Int) { }
func autoclosure(f: Int) { }

autoclosure(f: { 0 }) // OK
let _ = autoclosure as (() -> (Int)) -> () // OK

func test(_: (@autoclosure () -> Int) -> Void) {}
func test(_: (() -> Int) -> Void) {}

func fn(_: () -> Int) {}

test(fn) // OK
