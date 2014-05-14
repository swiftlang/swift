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
da = ca // expected-error{{cannot convert the expression's type '()' to type 'Array<D>'}}

var caa = [ca]
var daa = [da]

caa = daa // expected-error{{cannot convert the expression's type '()' to type 'Array<Array<C>>'}}

// Array slice type
var cas: C[] = [c1]
var das: D[] = [d1]

cas = das
das = cas // expected-error{{cannot convert the expression's type '()' to type 'D[]'}}

// Slice<T>
var cs = ca[0...0]
var ds = da[0...0]

cs = ds // expected-error{{cannot convert the expression's type '()' to type 'Slice<C>'}}
ds = cs // expected-error{{cannot convert the expression's type '()' to type 'Slice<D>'}}

// NativeArray<T>
var cna: NativeArray<C> = [c1]
var dna: NativeArray<D> = [d1]

cna = dna // expected-error{{cannot convert the expression's type '()' to type 'NativeArray<C>'}}
dna = cna // expected-error{{cannot convert the expression's type '()' to type 'NativeArray<D>'}}
