// RUN: %target-swift-frontend -typecheck -verify %s

// FuncDecl: Choose 0
func f1<T>(x: T) {}

// FuncDecl: Choose 1
// 1: Inherited constraint
func f2<T: Hashable>(x: T) {} // no-warning
// 2: Non-trailing where
func f3<T where T: Comparable>(x: T) {} // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{10-30=}} {{37-37= where T: Comparable}}
// 3: Has return type
func f4<T>(x: T) -> Int { return 2 } // no-warning
// 4: Trailing where
func f5<T>(x: T) where T : Equatable {} // no-warning

// FuncDecl: Choose 2
// 1,2
func f12<T: Hashable where T: Comparable>(x: T) {} // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{21-41=}} {{48-48= where T: Comparable}}
// 1,3
func f13<T: Hashable>(x: T) -> Int { return 2 } // no-warning
// 1,4
func f14<T: Hashable>(x: T) where T: Equatable {} // no-warning
// 2,3
func f23<T where T: Comparable>(x: T) -> Int { return 2 } // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{11-31=}} {{45-45= where T: Comparable}}
// 2,4
func f24<T where T: Comparable>(x: T) where T: Equatable {} // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{11-31=}} {{39-44=where T: Comparable,}}
// 3,4
func f34<T>(x: T) -> Int where T: Equatable { return 2 } // no-warning

// FuncDecl: Choose 3
// 1,2,3
func f123<T: Hashable where T: Comparable>(x: T) -> Int { return 2 } // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{22-42=}} {{56-56= where T: Comparable}}
// 1,2,4
func f124<T: Hashable where T: Comparable>(x: T) where T: Equatable {} // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{22-42=}} {{50-55=where T: Comparable,}}
// 2,3,4
func f234<T where T: Comparable>(x: T) -> Int where T: Equatable { return 2 } // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{12-32=}} {{47-52=where T: Comparable,}}

// FuncDecl: Choose 4
// 1,2,3,4
func f1234<T: Hashable where T: Comparable>(x: T) -> Int where T: Equatable { return 2 } // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{23-43=}} {{58-63=where T: Comparable,}}



// NominalTypeDecl: Choose 0
struct S0<T> {}

// NominalTypeDecl: Choose 1
// 1: Inherited constraint
struct S1<T: Hashable> {} // no-warning
// 2: Non-trailing where
struct S2<T where T: Comparable> {} // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{12-32=}} {{33-33= where T: Comparable}}
// 3: Trailing where
struct S3<T> where T : Equatable {} // no-warning

// NominalTypeDecl: Choose 2
// 1,2
struct S12<T: Hashable where T: Comparable> {} // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{23-43=}} {{44-44= where T: Comparable}}
// 1,3
struct S13<T: Hashable> where T: Equatable {} // no-warning
// 2,3
struct S23<T where T: Comparable> where T: Equatable {} // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{13-33=}} {{35-40=where T: Comparable,}}

// NominalTypeDecl: Choose 3
// 1,2,3
struct S123<T: Hashable where T: Comparable> where T: Equatable {} // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{24-44=}} {{46-51=where T: Comparable,}}


protocol ProtoA {}
protocol ProtoB {}
protocol ProtoC {}
protocol ProtoD {}
func testCombinedConstraints<T: ProtoA & ProtoB where T: ProtoC>(x: T) {} // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{48-64=}} {{71-71= where T: ProtoC}}
func testCombinedConstraints<T: ProtoA & ProtoB where T: ProtoC>(x: T) where T: ProtoD {} // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{48-64=}} {{72-77=where T: ProtoC,}}

func testCombinedConstraintsOld<T: protocol<ProtoA, ProtoB> where T: ProtoC>(x: T) {} // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{60-76=}} {{83-83= where T: ProtoC}}
// expected-warning@-1 {{'protocol<...>' composition syntax is deprecated}}
func testCombinedConstraintsOld<T: protocol<ProtoA, ProtoB> where T: ProtoC>(x: T) where T: ProtoD {} // expected-warning {{'where' clause next to generic parameters is deprecated and will be removed in the future version of Swift}} {{60-76=}} {{84-89=where T: ProtoC,}}
// expected-warning@-1 {{'protocol<...>' composition syntax is deprecated}}

