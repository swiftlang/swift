// RUN: %target-swift-frontend -emit-sil -verify %s -o /dev/null -enable-experimental-feature NondeinitableTypes

// REQUIRES: swift_feature_NondeinitableTypes

struct ND: ~Copyable, ~Deinitable {
  var x: Int
  consuming func finish() { discard self }
  mutating func bump() { x += 1 }
}

enum Choice: ~Copyable, ~Deinitable {
  case a(Int), b
  consuming func finish() { discard self }
}

// FIXME: The synthesized setter of a stored property drops its old value.
struct Holder: ~Copyable, ~Deinitable {
  var nd: ND
  // expected-error@-1 {{'self.nd' is not consumed on all paths}}
  // expected-note@-2 {{path exits here without a consume}}
}

struct Box<T: ~Copyable & ~Deinitable>: ~Copyable, ~Deinitable {
  let t: T
  consuming func take() -> T { t }
}

func make() -> ND { ND(x: 0) }
func mayThrow() throws {}
func transfer(_ nd: consuming ND) -> ND { nd }

// MARK: - Values that aren't consumed on every path

func neverConsumed() {
  let nd = make() // expected-error {{'nd' is not consumed on all paths}}
  _ = nd.x
} // expected-note {{path exits here without a consume}}

func consumedOnOneBranch(_ b: Bool) {
  let nd = make() // expected-error {{'nd' is not consumed on all paths}}
  if b { nd.finish() } // expected-note {{path exits here without a consume}}
}

func dropped() {
  let nd = make() // expected-error {{'nd' is not consumed on all paths}}
  _ = consume nd // expected-note {{path exits here without a consume}}
}

func varOverwrite() {
  var nd = make() // expected-error {{'nd' is not consumed on all paths}}
  nd = make() // expected-note {{path exits here without a consume}}
  nd.finish()
}

func inoutOverwrite(_ nd: inout ND) { // expected-error {{'nd' is not consumed on all paths}}
  nd = make() // expected-note {{path exits here without a consume}}
}

func storedPropertyOverwrite(_ h: inout Holder) { // expected-error {{'h.nd' is not consumed on all paths}}
  h.nd = make() // expected-note {{path exits here without a consume}}
}

func throwingPath() throws {
  let nd = make() // expected-error {{'nd' is not consumed on all paths}}
  try mayThrow() // expected-note {{path exits here without a consume}}
  nd.finish()
}

func genericParameter<T: ~Copyable & ~Deinitable>(_ t: consuming T) {} // expected-error {{'t' is not consumed on all paths}}
// expected-note@-1 {{path exits here without a consume}}

func genericContainer(_ b: consuming Box<ND>) {} // expected-error {{'b' is not consumed on all paths}}
// expected-note@-1 {{path exits here without a consume}}

func switchDropsPayload(_ c: consuming Choice) { // expected-error {{'c' is not consumed on all paths}}
  switch consume c { // expected-note {{path exits here without a consume}}
  case .a: break
  case .b: break
  }
}

func escapingCaptureNotConsumed() {
  var nd = make() // expected-error {{'nd' is not consumed on all paths}}
  let f = { nd.x += 1 }
  f()
} // expected-note {{path exits here without a consume}}

// MARK: - Accepted

func transfers(_ nd: consuming ND) -> ND { nd }

func finishes() { make().finish() }

func consumedOnEveryBranch(_ b: Bool) {
  let nd = make()
  if b { nd.finish() } else { transfer(nd).finish() }
}

func mutatesInPlace() {
  var nd = make()
  nd.bump()
  nd.x = 3
  nd.finish()
}

func reinitialized() {
  var nd = make()
  nd.finish()
  nd = make()
  nd.finish()
}

func traps() {
  let nd = make()
  _ = nd.x
  fatalError()
}

func trapsOnOneBranch(_ b: Bool) {
  let nd = make()
  if b { fatalError() }
  nd.finish()
}

func unwrapsGenericContainer(_ b: consuming Box<ND>) { b.take().finish() }

func finishesEnum(_ c: consuming Choice) { c.finish() }

func escapingCapture() {
  var nd = make()
  let f = { nd.x += 1 }
  f()
  nd.finish()
}

func loop(_ n: Int) {
  for _ in 0..<n {
    let nd = make()
    nd.finish()
  }
}

// Copyable and ordinary noncopyable values can still be destroyed implicitly.
struct NC: ~Copyable {}

func ordinaryValues() {
  let _ = NC()
  let _ = [1, 2, 3]
}
