// RUN: %target-swift-emit-silgen %s -verify | %FileCheck %s

// https://github.com/apple/swift/issues/45309

func f<T>(_: () -> T) {}
func f<T>(_: @autoclosure () -> T) {}

// CHECK: function_ref @$s6sr27051fyyxyXElF
f { } // OK

func f1<T>(_: () -> T, _: () -> T) {}
func f1<T>(_: @autoclosure () -> T, _: @autoclosure () -> T) {}

// CHECK: function_ref @$s6sr27052f1yyxyXE_xyXEtlF 
f1({}, {}) // OK

func f2<T>(_: () -> T, _: () -> T) { }
func f2<T>(_: () -> T, _: @autoclosure () -> T) { }

// CHECK: function_ref @$s6sr27052f2yyxyXE_xyXEtlF 
f2({}, {}) // OK

func f3(_: () -> Int) {}
func f3(_: @autoclosure () -> Int) {}

// CHECK: function_ref @$s6sr27052f3yySiyXEF 
f3 { 0 } // OK
 
func autoclosure(f: () -> Int) { }
func autoclosure(f: @autoclosure () -> Int) { }
func autoclosure(f: Int) { }

// CHECK: function_ref @$s6sr270511autoclosure1fySiyXE_tF
autoclosure(f: { 0 }) // OK

// CHECK: function_ref @$s6sr27052fnyySiyXEF 
let _ = autoclosure as (() -> (Int)) -> () // OK

func test(_: (@autoclosure () -> Int) -> Void) {}
func test(_: (() -> Int) -> Void) {}

func fn(_: () -> Int) {}

// CHECK: function_ref @$s6sr27054testyyySiyXEXEF
test(fn) // OK
