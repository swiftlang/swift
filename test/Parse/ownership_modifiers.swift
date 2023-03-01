// RUN: %target-typecheck-verify-swift

struct borrowing {}
struct consuming {}

struct Foo {}

func foo(x: borrowing Foo) {}
func bar(x: consuming Foo) {}
func baz(x: (borrowing Foo, consuming Foo) -> ()) {}

func bad(x: borrowing borrowing Foo) {} // expected-error{{at most one}}
func worse(x: borrowing consuming Foo) {} // expected-error{{at most one}}
func worst(x: (borrowing consuming Foo) -> ()) {} // expected-error{{at most one}}

// `borrowing` and `consuming` are contextual keywords, so they should also
// continue working as type and/or parameter names

func zim(x: borrowing) {}
func zang(x: consuming) {}
func zung(x: borrowing consuming) {}
func zip(x: consuming borrowing) {}
func zap(x: (borrowing, consuming) -> ()) {}
func zoop(x: (borrowing consuming, consuming borrowing) -> ()) {}

func worster(x: borrowing borrowing borrowing) {} // expected-error{{at most one}}
func worstest(x: (borrowing borrowing borrowing) -> ()) {} // expected-error{{at most one}}

// Parameter specifier names are regular identifiers in other positions,
// including argument labels.

func argumentLabel(borrowing consuming: Int) {}
func argumentLabel(consuming borrowing: Int) {}
func argumentLabel(__shared __owned: Int) {}
func argumentLabel(__owned __shared: Int) {}

// We should parse them as argument labels in function types, even though that
// isn't currently supported.

func argumentLabel(borrowingInClosure: (borrowing consuming: Int) -> ()) {} // expected-error{{function types cannot have argument labels}}
func argumentLabel(consumingInClosure: (consuming borrowing: Int) -> ()) {} // expected-error{{function types cannot have argument labels}}
func argumentLabel(sharedInClosure: (__shared __owned: Int) -> ()) {} // expected-error{{function types cannot have argument labels}}
func argumentLabel(ownedInClosure: (__owned __shared: Int) -> ()) {} // expected-error{{function types cannot have argument labels}}

func argumentLabel(anonBorrowingInClosure: (_ borrowing: Int) -> ()) {}
func argumentLabel(anonConsumingInClosure: (_ consuming: Int) -> ()) {}
func argumentLabel(anonSharedInClosure: (_ __shared: Int) -> ()) {}
func argumentLabel(anonOwnedInClosure: (_ __owned: Int) -> ()) {}
