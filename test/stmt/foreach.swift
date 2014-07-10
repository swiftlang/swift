// RUN: %swift %s -verify

// Bad containers and ranges
struct BadContainer1 {
}

func bad_containers_1(bc: BadContainer1) {
  for e in bc { } // expected-error{{type 'BadContainer1' does not conform to protocol 'Sequence'}}
}

struct BadContainer2 : Sequence { // expected-error{{type 'BadContainer2' does not conform to protocol '_Sequence_'}} expected-error{{type 'BadContainer2' does not conform to protocol 'Sequence'}}
  var generate : Int // expected-note{{candidate is not a function}} expected-note{{candidate is not a function}}
}

func bad_containers_2(bc: BadContainer2) {
  for e in bc { }
}

struct BadContainer3 : Sequence { // expected-error{{type 'BadContainer3' does not conform to protocol '_Sequence_'}} expected-error{{type 'BadContainer3' does not conform to protocol 'Sequence'}} 
  func generate() { } // expected-note{{candidate has non-matching type '() -> ()'}} expected-note{{candidate has non-matching type '() -> ()'}}
}

func bad_containers_3(bc: BadContainer3) {
  for e in bc { }
}

struct BadGeneratorType1 {
  
}

struct BadContainer4 : Sequence { // expected-error{{type 'BadContainer4' does not conform to protocol '_Sequence_'}} expected-error{{type 'BadContainer4' does not conform to protocol 'Sequence'}}
  typealias GeneratorType = BadGeneratorType1 // expected-note{{possibly intended match 'GeneratorType' does not conform to 'Generator'}} expected-note{{possibly intended match 'GeneratorType' does not conform to 'Generator'}}
  func generate() -> BadGeneratorType1 { }
}

func bad_containers_4(bc: BadContainer4) {
  for e in bc { }
}

// Pattern type-checking

struct GoodRange<Int> : Sequence, Generator {
  typealias Element = Int
  func next() -> Int? {}

  typealias GeneratorType = GoodRange<Int>
  func generate() -> GoodRange<Int> { return self }
}

struct GoodTupleGeneratorType : Sequence, Generator {
  typealias Element = (Int, Float)
  func next() -> (Int, Float)? {}

  typealias GeneratorType = GoodTupleGeneratorType
  func generate() -> GoodTupleGeneratorType {}
}

func patterns(gir: GoodRange<Int>, gtr: GoodTupleGeneratorType) {
  var sum : Int
  var sumf : Float
  for i : Int in gir { sum = sum + i }
  for i in gir { sum = sum + i }
  for f : Float in gir { sum = sum + f } // expected-error{{'Int' is not convertible to 'Float'}}

  for (i, f) : (Int, Float) in gtr { sum = sum + i }

  for (i, f) in gtr {
    sum = sum + i
    sumf = sumf + f
    sum = sum + f  // expected-error{{'Int' is not convertible to 'UInt8'}}
  }

  for (i, _) : (Int, Float) in gtr { sum = sum + i }

  for (i, _) : (Int, Int) in gtr { sum = sum + i } // expected-error{{'Float' is not convertible to 'Int'}}

  for (i = 7, f) in gtr {} // expected-error{{default argument is only permitted for a non-curried function parameter}}
}

func slices(i_s: [Int], ias: [[Int]]) {
  var sum = 0
  for i in i_s { sum = sum + i }

  for ia in ias {
    for i in ia {
      sum = sum + i
    }
  }
}

func discard_binding() {
  for _ in [0] {}
}

struct X<T> { 
  var value: T
}

struct Gen<T> : Generator {
  func next() -> T? { return nil }
}

struct Seq<T> : Sequence {
  func generate() -> Gen<T> { return Gen() }
}

func getIntSeq() -> Seq<Int> { return Seq() }

func getOvlSeq() -> Seq<Int> { return Seq() }
func getOvlSeq() -> Seq<Double> { return Seq() }
func getOvlSeq() -> Seq<X<Int>> { return Seq() }

func getGenericSeq<T>() -> Seq<T> { return Seq() }

func getXIntSeq() -> Seq<X<Int>> { return Seq() }

func getXIntSeqIUO() -> Seq<X<Int>>! { return nil }

func testForEachInference() {
  for i in getIntSeq() { }

  // Overloaded sequence resolved contextually
  for i: Int in getOvlSeq() { }
  for d: Double in getOvlSeq() { }

  // Overloaded sequence not resolved contextually
  for v in getOvlSeq() { } // expected-error{{could not find an overload for 'getOvlSeq' that accepts the supplied arguments}}

  // Generic sequence resolved contextually
  for i: Int in getGenericSeq() { }
  for d: Double in getGenericSeq() { }
  
  // Inference of generic arguments in the element type from the
  // sequence.
  for x: X in getXIntSeq() { 
    let z = x.value + 1
  }

  for x: X in getOvlSeq() { 
    let z = x.value + 1
  }

  // Inference with implicitly unwrapped optional
  for x: X in getXIntSeqIUO() {
    let z = x.value + 1
  }

  // Range overloading.
  for i: Int8 in 0..<10 { }
  for i: UInt in 0...10 { }
}
