// RUN: %target-swift-frontend -emit-silgen-ossa -enable-lifetime-resolution -verify %s

// Diagnostics from LifetimeResolution for implicit copies of Copyable values.

class C {}
struct S { var c: C }

func takeC(_ c: consuming C) {}
func useC(_ c: borrowing C) {}
func takeS(_ s: consuming S) {}

func fieldThenWhole(_ s: consuming S) { takeC(s.c); takeS(s) }
func explicitThenUse(_ c: consuming C) { takeC(copy c); useC(c) }
func lastUse(_ c: consuming C) -> C { useC(c); return c }

func consumeThenUse(_ c: consuming C) {
  takeC(c) // expected-note {{consumed here}} // expected-note {{write 'copy c' to keep 'c' valid after this consume}}
  useC(c) // expected-error {{'c' used after consume}}
}

func bindThenUse(_ c: consuming C) {
  let x = c // expected-note {{consumed here}}
            // expected-note@-1 {{write 'copy c' to keep 'c' valid after this consume}}
  useC(x)
  useC(c) // expected-error {{'c' used after consume}}
}

func pair(_ c: consuming C) -> (C, C) {
  return (c, c) // expected-error {{'c' consumed and used at the same time}}
                // expected-note@-1 {{consumed here}}
                // expected-note@-2 {{write 'copy c' to keep 'c' valid after this consume}}
}

func bindThenUse_fixed(_ c: consuming C) {
  let x = copy c
  useC(x)
  useC(c)
}

func pair_fixed(_ c: consuming C) -> (C, C) {
  return (copy c, c)
}

func varThenUse(_ c: consuming C) {
  var y = c // expected-note {{consumed here}}
            // expected-note@-1 {{write 'copy c' to keep 'c' valid after this consume}}
  y = C()
  useC(y)
  useC(c) // expected-error {{'c' used after consume}}
}

extension S {
  consuming func twice() -> (S, S) {
    return (self, self) // expected-error {{'self' consumed and used at the same time}}
                        // expected-note@-1 {{consumed here}}
                        // expected-note@-2 {{write 'copy self' to keep 'self' valid after this consume}}
  }
}

extension S {
  consuming func twice_fixed() -> (S, S) {
    return (copy self, self)
  }
}

func genericConsuming<T>(_ t: consuming T) -> (T, T) {
return (t, // expected-note {{write 'copy t' to keep 't' valid after this consume}} // expected-note {{consumed here}}
        t) // expected-error {{'t' consumed and used at the same time}}
}

func unwrap(_ t: consuming (some Equatable)?) -> Bool {
  if let x = t {  // expected-note {{consumed here}} // expected-note {{write 'copy t' to keep 't' valid after this consume}}
    return t == x  // expected-error {{'t' used after consume}}
  }
  return false
}

func unwrap_fixed(_ t: consuming (some Equatable)?) -> Bool {
  if let x = copy t {
    return t == x
  }
  return false
}

func boo<Spider>(_ spider: consuming Spider) {
_ = genericConsuming(spider) // expected-note {{consumed here; 'genericConsuming' takes ownership of this argument}}
                             // expected-note@-1 {{write 'copy spider' to keep 'spider' valid after this consume}}

_ = genericConsuming(spider) // expected-error {{'spider' used after consume}}
}

func boo_fixed<Spider>(_ spider: consuming Spider) {
_ = genericConsuming(copy spider)
_ = genericConsuming(spider)
}

func ghost(_ floppy: consuming Any) {
  if .random() {
    _ = genericConsuming (floppy) // expected-note {{consumed here; 'genericConsuming' takes ownership of this argument}}
    // expected-note@-1 {{write 'copy floppy' to keep 'floppy' valid after this consume}}
  }

  _ = genericConsuming(floppy) // expected-error {{'floppy' used after consume}}
}


func ghost_fixed(_ floppy: consuming Any) {
  if .random() {
    _ = genericConsuming(copy floppy)
  }

  _ = genericConsuming(floppy)
}

func varThenUse_fixed(_ c: consuming C) {
  var y = copy c
  y = C()
  useC(y)
  useC(c)
}
