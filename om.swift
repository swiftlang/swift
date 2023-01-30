// RUN: %target-typecheck-verify-swift

struct borrowing {}
struct consuming {}

struct Foo {}

func foo(x: borrowing Foo) {}
func bar(x: consuming Foo) {}
func baz(x: (borrowing Foo, consuming Foo) -> ()) {}

//func bad(x: borrowing borrowing Foo) {} // expected-error{{at most one}}
//func worse(x: borrowing consuming Foo) {} // expected-error{{at most one}}
//func worst(x: (borrowing consuming Foo) -> ()) {} // expected-error{{at most one}}

// `borrowing` and `consuming` are contextual keywords, so they should also
// continue working as type and/or parameter names

func zim(x: borrowing) {}
func zang(x: consuming) {}
func zung(x: borrowing consuming) {}
func zip(x: consuming borrowing) {}
func zap(x: (borrowing, consuming) -> ()) {}
func zoop(x: (borrowing consuming, consuming borrowing) -> ()) {}

//func worster(x: borrowing borrowing borrowing) {} // expected-error{{at most one}}
//func worstest(x: (borrowing borrowing borrowing) -> ()) {} // expected-error{{at most one}}
