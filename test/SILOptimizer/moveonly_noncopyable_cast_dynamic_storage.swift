// RUN: %target-swift-emit-sil -verify -target %target-future-triple -enable-experimental-feature NoncopyableCasting -parse-as-library %s -o /dev/null

// REQUIRES: swift_feature_NoncopyableCasting

// A cast that binds its payload out of non-`Copyable` storage consumes that
// storage on success. Storage that may not be consumed at all -- a class
// property, a global or static, a capture of an escaping closure -- must be
// diagnosed exactly as any other consume of it would be. These used to reach
// IRGen with a `begin_access [deinit] [dynamic]` and crash there.

protocol P: ~Copyable {}
struct Pay: P, ~Copyable { var tag: Int }
func mk() -> any P & ~Copyable { Pay(tag: 1) }

final class Hold { var f: any P & ~Copyable = mk() }

func classField(_ h: Hold) -> Int {
  if let b = h.f as? Pay { return b.tag } // expected-error {{cannot consume non-'Copyable' stored property 'h.f' of a class}}
  return 0
}

func classFieldIfCase(_ h: Hold) -> Int {
  if case let b as Pay = h.f { return b.tag } // expected-error {{cannot consume non-'Copyable' stored property 'h.f' of a class}}
  return 0
}

func classFieldGuard(_ h: Hold) -> Int {
  guard let b = h.f as? Pay else { return 0 } // expected-error {{cannot consume non-'Copyable' stored property 'h.f' of a class}}
  return b.tag
}

extension Hold {
  func selfField() -> Int {
    if let b = f as? Pay { return b.tag } // expected-error {{cannot consume non-'Copyable' stored property 'self.f' of a class}}
    return 0
  }
}

var g: any P & ~Copyable = mk()

func globalVar() -> Int {
  if let b = g as? Pay { return b.tag } // expected-error {{cannot consume non-'Copyable' stored property 'g' that is global}}
  return 0
}

struct S { static var s: any P & ~Copyable = mk() }

func staticVar() -> Int {
  if let b = S.s as? Pay { return b.tag } // expected-error {{cannot consume non-'Copyable' stored property 's' that is global}}
  return 0
}

func escapingCapture() -> () -> Int {
  var x: any P & ~Copyable = mk()
  let c = { () -> Int in
    if let b = x as? Pay { return b.tag } // expected-error {{non-'Copyable' 'x' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
    return 0
  }
  x = mk()
  return c
}

// Consumable storage is unaffected: a failed attempt leaves it intact.
struct Box: ~Copyable { var f: any P & ~Copyable }

func structField(_ b: consuming Box) -> Int {
  if let p = b.f as? Pay { return p.tag }
  return 0
}
