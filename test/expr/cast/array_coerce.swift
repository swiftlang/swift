// RUN: %swift -parse %s -verify

class C {
	var x = 0;
}
class D: C {}

var c1 = C()
c1.x = 1

var d1 = D()
d1.x = 2

// Array<T>
var ca: Array<C> = [c1]
var da: Array<D> = [d1]

ca = da
da = ca // expected-error{{expression does not type-check}}

// Array slice type
var cas: C[] = [c1]
var das: D[] = [d1]

cas = das
das = cas // expected-error{{expression does not type-check}}

// Slice<T>
var cs = ca[0..0]
var ds = da[0..0]

cs = ds
ds = cs // expected-error{{expression does not type-check}}

// NativeArray<T>
var cna: NativeArray<C> = [c1]
var dna: NativeArray<D> = [d1]

cna = dna
dna = cna // expected-error{{expression does not type-check}}}
