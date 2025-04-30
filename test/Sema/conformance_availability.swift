// RUN: %target-typecheck-verify-swift -swift-version 6

// REQUIRES: OS=macosx

public protocol Horse {}
func takesHorse<T : Horse>(_: T) {}
func takesHorseExistential(_: Horse) {}

extension Horse {
  func giddyUp() {}
  var isGalloping: Bool { true }
}

struct UsesHorse<T : Horse> {}

// Unconditional unavailability
public struct HasUnavailableConformance1 {}

@available(*, unavailable)
extension HasUnavailableConformance1 : Horse {}
// expected-note@-1 7{{conformance of 'HasUnavailableConformance1' to 'Horse' has been explicitly marked unavailable here}}

func passUnavailableConformance1(x: HasUnavailableConformance1) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
  takesHorseExistential(x) // expected-error {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
  _ = x.isGalloping // expected-error {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
  _ = x[keyPath: \.isGalloping] // expected-error {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance1>.self // expected-error {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
}

@available(*, unavailable)
func passUnavailableConformance1a(x: HasUnavailableConformance1) {
  takesHorse(x)
  takesHorseExistential(x)
  x.giddyUp()
  _ = x.isGalloping
  _ = x[keyPath: \.isGalloping]
  _ = UsesHorse<HasUnavailableConformance1>.self
}

// Platform unavailability
public struct HasUnavailableConformance2 {}

@available(macOS, unavailable)
extension HasUnavailableConformance2 : Horse {}
// expected-note@-1 6{{conformance of 'HasUnavailableConformance2' to 'Horse' has been explicitly marked unavailable here}}

func passUnavailableConformance2(x: HasUnavailableConformance2) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance2' to 'Horse' is unavailable in macOS}}
  takesHorseExistential(x) // expected-error {{conformance of 'HasUnavailableConformance2' to 'Horse' is unavailable in macOS}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance2' to 'Horse' is unavailable in macOS}}
  _ = x.isGalloping // expected-error {{conformance of 'HasUnavailableConformance2' to 'Horse' is unavailable in macOS}}
  _ = x[keyPath: \.isGalloping] // expected-error {{conformance of 'HasUnavailableConformance2' to 'Horse' is unavailable in macOS}}
  _ = UsesHorse<HasUnavailableConformance2>.self // expected-error {{conformance of 'HasUnavailableConformance2' to 'Horse' is unavailable in macOS}}
}

@available(macOS, unavailable)
func passUnavailableConformance2a(x: HasUnavailableConformance2) {
  // This is allowed
  takesHorse(x)
  takesHorseExistential(x)
  x.giddyUp()
  _ = x.isGalloping
  _ = x[keyPath: \.isGalloping]
}

// Swift version unavailability
public struct HasUnavailableConformance3 {}

@available(swift 12)
extension HasUnavailableConformance3 : Horse {}
// expected-note@-1 12{{conformance of 'HasUnavailableConformance3' to 'Horse' was introduced in Swift 12}}

func passUnavailableConformance3(x: HasUnavailableConformance3) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  takesHorseExistential(x) // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  _ = x.isGalloping // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  _ = x[keyPath: \.isGalloping] // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance3>.self // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
}

@available(swift 12)
func passUnavailableConformance3a(x: HasUnavailableConformance3) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  takesHorseExistential(x) // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  _ = x.isGalloping // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  _ = x[keyPath: \.isGalloping] // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance3>.self // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
}

// Platform obsoleted
public struct HasUnavailableConformance4 {}

@available(macOS, obsoleted: 10.1)
extension HasUnavailableConformance4 : Horse {}
// expected-note@-1 12{{conformance of 'HasUnavailableConformance4' to 'Horse' was obsoleted in macOS 10.1}}

func passUnavailableConformance4(x: HasUnavailableConformance4) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
  takesHorseExistential(x) // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
  _ = x.isGalloping // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable}}
  _ = x[keyPath: \.isGalloping] // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance4>.self // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
}

@available(macOS, obsoleted: 10.1)
func passUnavailableConformance4a(x: HasUnavailableConformance4) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
  takesHorseExistential(x) // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
  _ = x.isGalloping // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable}}
  _ = x[keyPath: \.isGalloping] // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance4>.self // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
}

// Swift obsoleted
public struct HasUnavailableConformance5 {}

@available(swift, obsoleted: 4)
extension HasUnavailableConformance5 : Horse {}
// expected-note@-1 12{{conformance of 'HasUnavailableConformance5' to 'Horse' was obsoleted in Swift 4}}

func passUnavailableConformance5(x: HasUnavailableConformance5) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  takesHorseExistential(x) // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  _ = x.isGalloping // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  _ = x[keyPath: \.isGalloping] // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance5>.self // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
}

@available(swift, obsoleted: 4)
func passUnavailableConformance5a(x: HasUnavailableConformance5) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  takesHorseExistential(x) // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  _ = x.isGalloping // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  _ = x[keyPath: \.isGalloping] // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance5>.self // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
}

// Unavailable with message
public struct HasUnavailableConformance6 {}

@available(*, unavailable, message: "This conformance is bad")
extension HasUnavailableConformance6 : Horse {}
// expected-note@-1 6{{conformance of 'HasUnavailableConformance6' to 'Horse' has been explicitly marked unavailable here}}

func passUnavailableConformance6(x: HasUnavailableConformance6) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance6' to 'Horse' is unavailable: This conformance is bad}}
  takesHorseExistential(x) // expected-error {{conformance of 'HasUnavailableConformance6' to 'Horse' is unavailable: This conformance is bad}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance6' to 'Horse' is unavailable: This conformance is bad}}
  _ = x.isGalloping // expected-error {{conformance of 'HasUnavailableConformance6' to 'Horse' is unavailable: This conformance is bad}}
  _ = x[keyPath: \.isGalloping] // expected-error {{conformance of 'HasUnavailableConformance6' to 'Horse' is unavailable: This conformance is bad}}
  _ = UsesHorse<HasUnavailableConformance6>.self // expected-error {{conformance of 'HasUnavailableConformance6' to 'Horse' is unavailable: This conformance is bad}}
}

// Deprecated
public struct HasDeprecatedConformance1 {}

@available(*, deprecated)
extension HasDeprecatedConformance1 : Horse {}

func passDeprecatedConformance1(x: HasDeprecatedConformance1) {
  takesHorse(x) // expected-warning {{conformance of 'HasDeprecatedConformance1' to 'Horse' is deprecated}}{{documentation-file=deprecated-declaration}}
  takesHorseExistential(x) // expected-warning {{conformance of 'HasDeprecatedConformance1' to 'Horse' is deprecated}}{{documentation-file=deprecated-declaration}}
  x.giddyUp() // expected-warning {{conformance of 'HasDeprecatedConformance1' to 'Horse' is deprecated}}{{documentation-file=deprecated-declaration}}
  _ = x.isGalloping // expected-warning {{conformance of 'HasDeprecatedConformance1' to 'Horse' is deprecated}}{{documentation-file=deprecated-declaration}}
  _ = x[keyPath: \.isGalloping] // expected-warning {{conformance of 'HasDeprecatedConformance1' to 'Horse' is deprecated}}{{documentation-file=deprecated-declaration}}
  _ = UsesHorse<HasDeprecatedConformance1>.self // expected-warning {{conformance of 'HasDeprecatedConformance1' to 'Horse' is deprecated}}{{documentation-file=deprecated-declaration}}
}

@available(*, deprecated)
func passDeprecatedConformance1a(x: HasDeprecatedConformance1) {
  takesHorse(x)
  takesHorseExistential(x)
  x.giddyUp()
  _ = x.isGalloping
  _ = x[keyPath: \.isGalloping]
  _ = UsesHorse<HasDeprecatedConformance1>.self
}

// Deprecated with message
public struct HasDeprecatedConformance2 {}

@available(*, deprecated, message: "This conformance is deprecated")
extension HasDeprecatedConformance2 : Horse {}

func passDeprecatedConformance2(x: HasDeprecatedConformance2) {
  takesHorse(x) // expected-warning {{conformance of 'HasDeprecatedConformance2' to 'Horse' is deprecated: This conformance is deprecated}}{{documentation-file=deprecated-declaration}}
  takesHorseExistential(x) // expected-warning {{conformance of 'HasDeprecatedConformance2' to 'Horse' is deprecated: This conformance is deprecated}}{{documentation-file=deprecated-declaration}}
  x.giddyUp() // expected-warning {{conformance of 'HasDeprecatedConformance2' to 'Horse' is deprecated: This conformance is deprecated}}{{documentation-file=deprecated-declaration}}
  _ = x.isGalloping // expected-warning {{conformance of 'HasDeprecatedConformance2' to 'Horse' is deprecated: This conformance is deprecated}}{{documentation-file=deprecated-declaration}}
  _ = x[keyPath: \.isGalloping] // expected-warning {{conformance of 'HasDeprecatedConformance2' to 'Horse' is deprecated: This conformance is deprecated}}{{documentation-file=deprecated-declaration}}
  _ = UsesHorse<HasDeprecatedConformance2>.self // expected-warning {{conformance of 'HasDeprecatedConformance2' to 'Horse' is deprecated: This conformance is deprecated}}{{documentation-file=deprecated-declaration}}
}

@available(*, deprecated)
func passDeprecatedConformance2a(x: HasDeprecatedConformance2) {
  takesHorse(x)
  takesHorseExistential(x)
  x.giddyUp()
  _ = x.isGalloping
  _ = x[keyPath: \.isGalloping]
  _ = UsesHorse<HasDeprecatedConformance2>.self
}

// Deprecated with version
public struct HasDeprecatedConformance3 {}

@available(macOS, introduced: 10.7, deprecated: 10.8)
extension HasDeprecatedConformance3 : Horse {}

func passDeprecatedConformance3(x: HasDeprecatedConformance3) {
  takesHorse(x) // expected-warning {{conformance of 'HasDeprecatedConformance3' to 'Horse' was deprecated in macOS 10.8}}{{documentation-file=deprecated-declaration}}
  takesHorseExistential(x) // expected-warning {{conformance of 'HasDeprecatedConformance3' to 'Horse' was deprecated in macOS 10.8}}{{documentation-file=deprecated-declaration}}
  x.giddyUp() // expected-warning {{conformance of 'HasDeprecatedConformance3' to 'Horse' was deprecated in macOS 10.8}}{{documentation-file=deprecated-declaration}}
  _ = x.isGalloping // expected-warning {{conformance of 'HasDeprecatedConformance3' to 'Horse' was deprecated in macOS 10.8}}{{documentation-file=deprecated-declaration}}
  _ = x[keyPath: \.isGalloping] // expected-warning {{conformance of 'HasDeprecatedConformance3' to 'Horse' was deprecated in macOS 10.8}}{{documentation-file=deprecated-declaration}}
  _ = UsesHorse<HasDeprecatedConformance3>.self // expected-warning {{conformance of 'HasDeprecatedConformance3' to 'Horse' was deprecated in macOS 10.8}}{{documentation-file=deprecated-declaration}}
}

func passDeprecatedConformance3a(x: HasDeprecatedConformance3) {
  if #available(macOS 10.8, *) {
  } else {
    // This branch is dead with our minimum deployment target, so don't emit
    // deprecation diagnostics in it.
    takesHorse(x)
    takesHorseExistential(x)
    x.giddyUp()
    _ = x.isGalloping
    _ = x[keyPath: \.isGalloping]
    _ = UsesHorse<HasDeprecatedConformance3>.self
  }
}

// Availability with version
public struct HasAvailableConformance1 {}

@available(macOS 100, *)
extension HasAvailableConformance1 : Horse {}

// These availability violations are errors because this test passes the
// -swift-version 6.
// See the other test case in test/Sema/conformance_availability_warn.swift for
// the same example for -swift-version 5.

func passAvailableConformance1(x: HasAvailableConformance1) { // expected-note 6{{add '@available' attribute to enclosing global function}}
  takesHorse(x) // expected-error {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  takesHorseExistential(x) // expected-error {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  x.giddyUp() // expected-error {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  _ = x.isGalloping // expected-error {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  _ = x[keyPath: \.isGalloping] // expected-error {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  _ = UsesHorse<HasAvailableConformance1>.self // expected-error {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
}

@available(macOS 100, *)
func passAvailableConformance1a(x: HasAvailableConformance1) {
  takesHorse(x)
  takesHorseExistential(x)
  x.giddyUp()
  _ = x.isGalloping
  _ = x[keyPath: \.isGalloping]
  _ = UsesHorse<HasAvailableConformance1>.self
}

// Always available conformance
struct HasAvailableConformance2: Horse {}

// Conformance available in macOS 200
struct HasAvailableConformance3 {}

@available(macOS 200, *)
extension HasAvailableConformance3: Horse {}

// Associated conformance with unavailability
protocol Rider {
  associatedtype H : Horse
}

struct AssocConformanceUnavailable : Rider {
// expected-error@-1 {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
// expected-note@-2 {{in associated type 'Self.H' (inferred as 'HasUnavailableConformance1')}}
  typealias H = HasUnavailableConformance1
}

// Associated conformance with deprecation
struct AssocConformanceDeprecated : Rider {
// expected-warning@-1 {{conformance of 'HasDeprecatedConformance1' to 'Horse' is deprecated}}{{documentation-file=deprecated-declaration}}
// expected-note@-2 {{in associated type 'Self.H' (inferred as 'HasDeprecatedConformance1')}}
  typealias H = HasDeprecatedConformance1
}

// Associated conformance with availability
struct AssocConformanceAvailable1 : Rider {
// expected-error@-1 {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
// expected-note@-2 {{in associated type 'Self.H' (inferred as 'HasAvailableConformance1')}}
// expected-note@-3 {{add '@available' attribute to enclosing struct}}
  typealias H = HasAvailableConformance1
}

@available(macOS 100, *)
struct AssocConformanceAvailable2 : Rider {
  typealias H = HasAvailableConformance1
}

struct AssocConformanceAvailable3 {}

extension AssocConformanceAvailable3 : Rider {
// expected-error@-1 {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
// expected-note@-2 {{in associated type 'Self.H' (inferred as 'HasAvailableConformance1')}}
// expected-note@-3 {{add '@available' attribute to enclosing extension}}
  typealias H = HasAvailableConformance1
}

struct AssocConformanceAvailable4 {}

@available(macOS 100, *)
extension AssocConformanceAvailable4 : Rider {
  typealias H = HasAvailableConformance1
}

@available(macOS 100, *)
protocol Saddle {
  associatedtype H : Horse = HasAvailableConformance1
}

struct ConformsToSaddle1 : Saddle {}

struct ConformsToSaddle2 : Saddle {
  typealias H = HasAvailableConformance2
}

struct ConformsToSaddle3 : Saddle {
  // expected-error@-1 {{conformance of 'HasAvailableConformance3' to 'Horse' is only available in macOS 200 or newer}}
  // expected-note@-2 {{add '@available' attribute to enclosing struct}}
  // expected-note@-3 {{in associated type 'Self.H' (inferred as 'HasAvailableConformance3'}}
  typealias H = HasAvailableConformance3
}

struct ConformsToSaddle4 : Saddle {
  // expected-error@-1 {{conformance of 'HasAvailableConformance3' to 'Horse' is only available in macOS 200 or newer}}
  // expected-note@-2 {{add '@available' attribute to enclosing struct}}
  // expected-note@-3 {{in associated type 'Self.H' (inferred as 'HasAvailableConformance3'}}
  @available(macOS 200, *)
  typealias H = HasAvailableConformance3
}

protocol Barn {
  @available(macOS 100, *)
  associatedtype H : Horse = HasAvailableConformance1
}

struct ConformsToBarn1 : Barn {}

struct ConformsToBarn2 : Barn {
  typealias H = HasAvailableConformance2
}

struct ConformsToBarn3 : Barn {
  // expected-error@-1 {{conformance of 'HasAvailableConformance3' to 'Horse' is only available in macOS 200 or newer}}
  // expected-note@-2 {{add '@available' attribute to enclosing struct}}
  // expected-note@-3 {{in associated type 'Self.H' (inferred as 'HasAvailableConformance3'}}
  typealias H = HasAvailableConformance3
}

struct ConformsToBarn4 : Barn {
  // expected-error@-1 {{conformance of 'HasAvailableConformance3' to 'Horse' is only available in macOS 200 or newer}}
  // expected-note@-2 {{add '@available' attribute to enclosing struct}}
  // expected-note@-3 {{in associated type 'Self.H' (inferred as 'HasAvailableConformance3'}}
  @available(macOS 200, *)
  typealias H = HasAvailableConformance3
}

// Solution ranking should down-rank solutions involving unavailable conformances
protocol First {}
extension First {
  func doStuff<T>(_: T) -> Bool {}
}

protocol Second {}
extension Second {
  func doStuff(_: Int) -> Int {}
}

struct ConformingType1 {}

extension ConformingType1 : First {}

@available(macOS 100, *)
extension ConformingType1 : Second {}

func usesConformingType1(_ c: ConformingType1) {
  // We should pick First.doStuff() here, since Second.doStuff() is unavailable
  let result = c.doStuff(123)
  let _: Bool = result
}

@available(macOS 100, *)
func usesConformingType1a(_ c: ConformingType1) {
  // We should pick Second.doStuff() here, since it is more specialized than
  // First.doStuff()
  let result = c.doStuff(123)
  let _: Int = result
}

// Same as above but unconditionally unavailable
struct ConformingType2 {}

extension ConformingType2 : First {}

@available(*, unavailable)
extension ConformingType2 : Second {}

func usesConformingType2(_ c: ConformingType2) {
  // We should pick First.doStuff() here, since Second.doStuff() is unavailable
  let result = c.doStuff(123)
  let _: Bool = result
}

// Make sure this also works for synthesized conformances
struct UnavailableHashable {
  let x: Int
  let y: Int
}

@available(macOS 100, *)
extension UnavailableHashable : Hashable {}

func usesUnavailableHashable(_ c: UnavailableHashable) {
  // expected-note@-1 2 {{add '@available' attribute to enclosing global function}}
  _ = Set([c])
  // expected-error@-1 2 {{conformance of 'UnavailableHashable' to 'Hashable' is only available in macOS 100 or newer}}
  // expected-note@-2 2 {{add 'if #available' version check}}
}

// Actually make sure we check witness availability correctly.
protocol Vehicle {
  func move() // expected-note {{protocol requirement here}}
}

@available(macOS 100, *)
struct Pony : Vehicle {
  func move() {}
}

struct Bike {}

@available(macOS 100, *)
extension Bike : Vehicle {
  func move() {}
}

@available(macOS, introduced: 100)
struct Wagon {}

@available(macOS, introduced: 100)
extension Wagon : Vehicle {
  func move() {}
}

class Car {}
class ClownCar : Car {}

@available(macOS 200, *)
extension Car {
  func move() {} // expected-note {{'move()' declared here}}
}

@available(macOS 100, *)
extension ClownCar : Vehicle {}
// expected-error@-1 {{protocol 'Vehicle' requires 'move()' to be available in macOS 100 and newer}}

@available(macOS, unavailable)
struct Truck : Vehicle {
  func move() {}
}

struct Scooter {}

@available(macOS, unavailable)
extension Scooter : Vehicle {
  func move() {}
}

struct Motorcycle {}

@available(macOS, unavailable)
extension Motorcycle : Vehicle {
  @available(macOS, introduced: 100)
  func move() {}
}

@available(macOS, unavailable)
struct AircraftCarrier {
  struct Jet : Vehicle {
    @available(macOS, introduced: 100)
    func move() {}
  }
}

struct Unicycle {
  @available(macOS, introduced: 100)
  func move() {}
}

@available(macOS, unavailable)
extension Unicycle : Vehicle {}

@available(macOS, unavailable, introduced: 100)
struct Train : Vehicle {
  func move() {}
}

struct Blimp {}

@available(macOS, unavailable, introduced: 100)
extension Blimp : Vehicle {
  func move() {}
}

@available(macOS, unavailable, introduced: 100)
struct Spaceship {
}

@available(macOS, unavailable, introduced: 100)
extension Spaceship : Vehicle {
  func move() {}
}

// rdar://problem/75430966 - Allow using unavailable conformances from unavailable contexts.
@available(*, unavailable)
public enum UnavailableEnum {
  case horse
}

@available(*, unavailable)
extension UnavailableEnum : Swift.Equatable {}

@available(*, unavailable)
extension UnavailableEnum : Swift.Hashable {}
