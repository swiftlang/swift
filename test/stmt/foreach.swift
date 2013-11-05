// RUN: %swift %s -verify

// Bad containers and ranges
struct BadContainer1 {
}

def bad_containers_1(bc: BadContainer1) {
  for e in bc { } // expected-error{{type 'BadContainer1' does not conform to protocol 'Enumerable'}}
}

struct BadContainer2 : Enumerable { // expected-error{{type 'BadContainer2' does not conform to protocol 'Enumerable'}}
  var enumerate : Int // expected-note{{candidate has is not a function}}
}

def bad_containers_2(bc: BadContainer2) {
  for e in bc { } // expected-error{{type 'BadContainer2' does not conform to protocol 'Enumerable'}}
}

struct BadContainer3 : Enumerable { // expected-error{{type 'BadContainer3' does not conform to protocol 'Enumerable'}}
  def enumerate() { } // expected-note{{candidate has non-matching type '() -> ()'}}
}

def bad_containers_3(bc: BadContainer3) {
  for e in bc { } // expected-error{{type 'BadContainer3' does not conform to protocol 'Enumerable'}}
}

struct BadGeneratorType1 {
  
}

struct BadContainer4 : Enumerable { // expected-error{{type 'BadContainer4' does not conform to protocol 'Enumerable'}}
  typealias GeneratorType = BadGeneratorType1 // expected-note{{possibly intended match 'GeneratorType' does not conform to 'Generator'}}
  def enumerate() -> BadGeneratorType1 { }
}

def bad_containers_4(bc: BadContainer4) {
  for e in bc { } // expected-error{{type 'BadContainer4' does not conform to protocol 'Enumerable'}}
}

// Pattern type-checking

struct GoodIntGeneratorType : Enumerable, Generator {
  typealias Element = Int
  def next() -> Int? {}

  typealias GeneratorType = GoodIntGeneratorType
  def enumerate() -> GoodIntGeneratorType { return self }
}

struct GoodTupleGeneratorType : Enumerable, Generator {
  typealias Element = (Int, Float)
  def next() -> (Int, Float)? {}

  typealias GeneratorType = GoodTupleGeneratorType
  def enumerate() -> GoodTupleGeneratorType {}
}

def patterns(gir: GoodIntGeneratorType, gtr: GoodTupleGeneratorType) {
  var sum : Int
  var sumf : Float
  for i : Int in gir { sum = sum + i }
  for i in gir { sum = sum + i }
  for f : Float in gir { sum = sum + f } // expected-error{{type annotation does not match contextual type 'Element'}}

  for (i, f) : (Int, Float) in gtr { sum = sum + i }

  for (i, f) in gtr {
    sum = sum + i
    sumf = sumf + f
    sum = sum + f  // expected-error{{expression does not type-check}}
  }

  for (i, _) : (Int, Float) in gtr { sum = sum + i }

  for (i, _) : (Int, Int) in gtr { sum = sum + i } // expected-error{{type annotation does not match contextual type 'Element'}}

  for (i = 7, f) in gtr {} // expected-error{{default argument is only permitted for a non-curried function parameter}}
}

def slices(i_s: Int[], ias: Int[][]) {
  var sum = 0
  for i in i_s { sum = sum + i }

  for ia in ias {
    for i in ia {
      sum = sum + i
    }
  }
}

def discard_binding() {
  for _ in [0] {}
}
