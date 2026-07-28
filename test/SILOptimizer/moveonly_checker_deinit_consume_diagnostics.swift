// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature MutateAndConsumeInDeinit -verify %s

// REQUIRES: swift_feature_MutateAndConsumeInDeinit

struct Inner1: ~Copyable {
  mutating func mutate() {}
  consuming func consume() {}
}

struct Inner2: ~Copyable {
  mutating func mutate() {}
  consuming func consume() {}
}

func mutate<T: ~Copyable>(_: inout T) {}
func consume<T: ~Copyable>(_: consuming T) {}

struct TriesToMutateWhole: ~Copyable {
  var i1: Inner1
  var i2: Inner2

  deinit {
    mutate(&self)  // expected-error{{cannot be mutated as an entire}}
  }
}

struct TriesToConsumeWhole: ~Copyable {
  var i1: Inner1
  var i2: Inner2

  deinit {
    consume(self)  // expected-error{{cannot be consumed as an entire}}
  }
}

struct MutatesParts: ~Copyable {
  var i1: Inner1
  var i2: Inner2

  // OK
  deinit {
    mutate(&i1)
    mutate(&i2)
  }
}

struct ConsumesPart1: ~Copyable {
  var i1: Inner1
  var i2: Inner2

  // OK
  deinit {
    consume(i1)
  }
}
struct ConsumesPart2: ~Copyable {
  var i1: Inner1
  var i2: Inner2

  // OK
  deinit {
    consume(i2)
  }
}

struct ConsumesAllParts: ~Copyable {
  var i1: Inner1
  var i2: Inner2

  // OK
  deinit {
    consume(i2)
    consume(i1)
  }
}

struct MutatesThenConsumesAllParts: ~Copyable {
  var i1: Inner1
  var i2: Inner2

  // OK
  deinit {
    mutate(&i1)
    mutate(&i2)
    consume(i2)
    consume(i1)
  }
}

struct ConsumesThenMutatesParts: ~Copyable {
  var i1: Inner1
  var i2: Inner2

  // OK
  deinit {  // expected-error{{used after consume}}
    consume(i2)  // expected-note{{consumed here}}
    mutate(&i1)
    mutate(&i2)  // expected-note{{used here}}
  }
}
