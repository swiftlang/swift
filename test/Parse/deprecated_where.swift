// RUN: %target-typecheck-verify-swift -swift-version 4

protocol Mashable { }
protocol Womparable { }

// FuncDecl: Choose 0
func f1<T>(x: T) {}

// FuncDecl: Choose 1
// 1: Inherited constraint
func f2<T: Mashable>(x: T) {} // no-warning
// 2: Non-trailing where
func f3<T where T: Womparable>(x: T) {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{10-30=}} {{37-37= where T: Womparable}}
// 3: Has return type
func f4<T>(x: T) -> Int { return 2 } // no-warning
// 4: Trailing where
func f5<T>(x: T) where T : Equatable {} // no-warning

// FuncDecl: Choose 2
// 1,2
func f12<T: Mashable where T: Womparable>(x: T) {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{21-41=}} {{48-48= where T: Womparable}}
// 1,3
func f13<T: Mashable>(x: T) -> Int { return 2 } // no-warning
// 1,4
func f14<T: Mashable>(x: T) where T: Equatable {} // no-warning
// 2,3
func f23<T where T: Womparable>(x: T) -> Int { return 2 } // expected-error {{'where' clause next to generic parameters is obsolete}} {{11-31=}} {{45-45= where T: Womparable}}
// 2,4
func f24<T where T: Womparable>(x: T) where T: Equatable {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{11-31=}} {{39-44=where T: Womparable,}}
// 3,4
func f34<T>(x: T) -> Int where T: Equatable { return 2 } // no-warning

// FuncDecl: Choose 3
// 1,2,3
func f123<T: Mashable where T: Womparable>(x: T) -> Int { return 2 } // expected-error {{'where' clause next to generic parameters is obsolete}} {{22-42=}} {{56-56= where T: Womparable}}
// 1,2,4
func f124<T: Mashable where T: Womparable>(x: T) where T: Equatable {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{22-42=}} {{50-55=where T: Womparable,}}
// 2,3,4
func f234<T where T: Womparable>(x: T) -> Int where T: Equatable { return 2 } // expected-error {{'where' clause next to generic parameters is obsolete}} {{12-32=}} {{47-52=where T: Womparable,}}

// FuncDecl: Choose 4
// 1,2,3,4
func f1234<T: Mashable where T: Womparable>(x: T) -> Int where T: Equatable { return 2 } // expected-error {{'where' clause next to generic parameters is obsolete}} {{23-43=}} {{58-63=where T: Womparable,}}



// NominalTypeDecl: Choose 0
struct S0<T> {}

// NominalTypeDecl: Choose 1
// 1: Inherited constraint
struct S1<T: Mashable> {} // no-warning
// 2: Non-trailing where
struct S2<T where T: Womparable> {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{12-32=}} {{33-33= where T: Womparable}}
// 3: Trailing where
struct S3<T> where T : Equatable {} // no-warning

// NominalTypeDecl: Choose 2
// 1,2
struct S12<T: Mashable where T: Womparable> {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{23-43=}} {{44-44= where T: Womparable}}
// 1,3
struct S13<T: Mashable> where T: Equatable {} // no-warning
// 2,3
struct S23<T where T: Womparable> where T: Equatable {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{13-33=}} {{35-40=where T: Womparable,}}

// NominalTypeDecl: Choose 3
// 1,2,3
struct S123<T: Mashable where T: Womparable> where T: Equatable {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{24-44=}} {{46-51=where T: Womparable,}}


protocol ProtoA {}
protocol ProtoB {}
protocol ProtoC {}
protocol ProtoD {}
func testCombinedConstraints<T: ProtoA & ProtoB where T: ProtoC>(x: T) {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{48-64=}} {{71-71= where T: ProtoC}}
func testCombinedConstraints<T: ProtoA & ProtoB where T: ProtoC>(x: T) where T: ProtoD {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{48-64=}} {{72-77=where T: ProtoC,}}

func testCombinedConstraintsOld<T: protocol<ProtoA, ProtoB> where T: ProtoC>(x: T) {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{60-76=}} {{83-83= where T: ProtoC}}
// expected-error@-1 {{'protocol<...>' composition syntax has been removed}}
func testCombinedConstraintsOld<T: protocol<ProtoA, ProtoB> where T: ProtoC>(x: T) where T: ProtoD {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{60-76=}} {{84-89=where T: ProtoC,}}
// expected-error@-1 {{'protocol<...>' composition syntax has been removed}}

