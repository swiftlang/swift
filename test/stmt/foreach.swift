// RUN: %swift %s -verify

// Bad containers and ranges
struct BadContainer1 {
}

func bad_containers_1(bc: BadContainer1) {
  for e in bc { } // expected-error{{type 'BadContainer1' does not conform to protocol 'Sequence'}}
}

struct BadContainer2 : Sequence { // expected-error{{type 'BadContainer2' does not conform to protocol '_Sequence_'}} expected-error{{type 'BadContainer2' does not conform to protocol 'Sequence'}}
  var generate : Int // expected-note{{candidate has is not a function}} expected-note{{candidate has is not a function}}
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
  for f : Float in gir { sum = sum + f } // expected-error{{type annotation does not match contextual type 'Int'}}

  for (i, f) : (Int, Float) in gtr { sum = sum + i }

  for (i, f) in gtr {
    sum = sum + i
    sumf = sumf + f
    sum = sum + f  // expected-error{{could not find an overload for '+' that accepts the supplied arguments}}
  }

  for (i, _) : (Int, Float) in gtr { sum = sum + i }

  for (i, _) : (Int, Int) in gtr { sum = sum + i } // expected-error{{type annotation does not match contextual type 'Element'}}

  for (i = 7, f) in gtr {} // expected-error{{default argument is only permitted for a non-curried function parameter}}
}

func slices(i_s: Int[], ias: Int[][]) {
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
