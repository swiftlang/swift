// RUN: %batch-code-completion

enum A {
    case one, two
}

struct B<T> {
    let value: T
    
    init(_ value: T) {
        self.value = value
    }
    
    static func void() -> B<Void> {
        return B<Void>(())
    }
    
    static func data(_ data: Data) -> B<Data> {
        return B<Data>(data)
    }
}

class C {
    func a(s: String = "", a: A) {}
    func b(s: String = "", i: Int = 0, a: A) {}
}

class D {
    func a<T>(b: B<T>, s: String = "") {}
    func b<T>(s: String = "", i: Int = 0, b: B<T>) {}
}

// type a point "." in placeholders to see the substitution list

// correct substitution
C().a(s: .#^STR_1?check=STRING^#, a: .#^A_1?check=A^#)
C().a(a: .#^A_2?check=A^#)

C().b(s: .#^STR_2?check=STRING^#, i: .#^INT_1?check=INT^#, a: .#^A_3?check=A^#)
C().b(i: .#^INT_2?check=INT^#, a: .#^A_4?check=A^#)
C().b(a: .#^A_5?check=A^#)

D().a(b: .#^B_1?check=B^#, s: .#^STR_3?check=STRING^#)
D().a(b: .#^B_2?check=B^#)

D().b(s: .#^STR_4?check=STRING^#, i: .#^INT_3?check=INT^#, b: .#^B_3?check=B^#)

// // incorrect substitution from previous parameter
D().b(i: .#^INT_4?check=INT^#, b: .#^B_4?check=B^#)
D().b(b: .#^B_5?check=B^#)

// INT-DAG: Decl[Constructor]/CurrNominal/IsSystem/TypeRelation[Convertible]: init({#bitPattern: UInt#})[#Int#]; name=init(bitPattern:)

// STRING-DAG: Decl[Constructor]/CurrNominal/IsSystem/TypeRelation[Convertible]: init()[#String#]; name=init()

// A: Begin completions, 3 items
// A-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: one[#A#]; name=one
// A-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: two[#A#]; name=two
// A-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): A#})[#(into: inout Hasher) -> Void#]; name=hash(:)

// B: Begin completions, 3 items
// B-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Convertible]: init({#(value): T#})[#B<T>#]; name=init(:)
// B-DAG: Decl[StaticMethod]/CurrNominal/TypeRelation[Convertible]: void()[#B<Void>#]; name=void()
// B-DAG: Decl[StaticMethod]/CurrNominal:     data({#(data): <<error type>>#})[#<<error type>>#]; name=data(:)
