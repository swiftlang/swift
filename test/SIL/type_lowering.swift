// RUN: %swift -dump-sil %s | FileCheck %s

func a()
func b(x:Int)
func c(x:Int) -> Int
func d<T>(x:T) -> Int
func e<T>(x:T) -> T
func f<T>(x:(Int, T)) -> T
func g<T>(x:T) -> (Int, T)
func h<T>(x:T) -> () -> T

var x = 0
var xy = (0,0)

a()
// CHECK: constant_ref $() -> (), @a
b(x)
// CHECK:  constant_ref $(x : Int64) -> (), @b
c(x)
// CHECK:  constant_ref $(x : Int64) -> Int64, @c
d(x)
// CHECK:  constant_ref $<T> (x : [byref] T) -> Int64, @d
e(x)
// CHECK:  constant_ref $<T> (x : [byref] T, [byref] T) -> (), @e
f(xy)
// CHECK:  constant_ref $<T> (x : [byref] (Int64, T), [byref] T) -> (), @f
g(x)
// CHECK:  constant_ref $<T> (x : [byref] T, [byref] (Int64, T)) -> (), @g
h(x)()
// CHECK:  constant_ref $<T> (x : [byref] T) -> [byref] T -> (), @h
// FIXME 'specialize' changes the ABI of the function?!
