// RUN: %target-parse-verify-swift

// Bad containers and ranges
struct BadContainer1 {
}

func bad_containers_1(bc: BadContainer1) {
  for e in bc { } // expected-error{{type 'BadContainer1' does not conform to protocol 'SequenceType'}}
}

struct BadContainer2 : SequenceType { // expected-error{{type 'BadContainer2' does not conform to protocol '_Sequence_Type'}} expected-error{{type 'BadContainer2' does not conform to protocol 'SequenceType'}}
  var generate : Int
}

func bad_containers_2(bc: BadContainer2) {
  for e in bc { } // expected-error{{'BadContainer2' does not have a member named 'Generator'}}
}

struct BadContainer3 : SequenceType { // expected-error{{type 'BadContainer3' does not conform to protocol '_Sequence_Type'}} expected-error{{type 'BadContainer3' does not conform to protocol 'SequenceType'}}
  func generate() { } // expected-note{{inferred type '()' (by matching requirement 'generate()') is invalid: does not conform to 'GeneratorType'}}
}

func bad_containers_3(bc: BadContainer3) {
  for e in bc { } // expected-error{{'BadContainer3' does not have a member named 'Generator'}}
}

struct BadGeneratorType1 {
  
}

struct BadContainer4 : SequenceType { // expected-error{{type 'BadContainer4' does not conform to protocol 'SequenceType'}} expected-error{{type 'BadContainer4' does not conform to protocol '_Sequence_Type'}}
  typealias Generator = BadGeneratorType1 // expected-note{{possibly intended match 'Generator' does not conform to 'GeneratorType'}} expected-note{{possibly intended match 'Generator' does not conform to 'GeneratorType'}}
  func generate() -> BadGeneratorType1 { }
}

func bad_containers_4(bc: BadContainer4) {
  for e in bc { } // expected-error{{'Generator' does not have a member named 'Element'}}
}

// Pattern type-checking

struct GoodRange<Int> : SequenceType, GeneratorType {
  typealias Element = Int
  func next() -> Int? {}

  typealias Generator = GoodRange<Int>
  func generate() -> GoodRange<Int> { return self }
}

struct GoodTupleGeneratorType : SequenceType, GeneratorType {
  typealias Element = (Int, Float)
  func next() -> (Int, Float)? {}

  typealias Generator = GoodTupleGeneratorType
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
    sum = sum + f  // expected-error{{binary operator '+' cannot be applied to operands of type 'Int' and 'Float'}} expected-note{{overloads for '+' exist with these partially matching parameter lists:}}
  }

  for (i, _) : (Int, Float) in gtr { sum = sum + i }

  for (i, _) : (Int, Int) in gtr { sum = sum + i } // expected-error{{'Float' is not convertible to 'Int'}}

  for (i, f) in gtr {}
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

struct Gen<T> : GeneratorType {
  func next() -> T? { return nil }
}

struct Seq<T> : SequenceType {
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
  for v in getOvlSeq() { } // expected-error{{cannot invoke 'getOvlSeq' with no arguments}}

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
