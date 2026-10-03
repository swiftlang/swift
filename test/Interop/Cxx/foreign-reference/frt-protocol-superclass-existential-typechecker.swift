// An existential whose class bound is a foreign reference type has no
// representation: a class existential stores no metadata and recovers Self
// from the instance's isa, which an FRT does not have.

// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking %s

import FRTProtocolSuperclass

protocol Unrelated {}

struct Tag {}

// MARK: shared FRT bound

protocol SharedTagged: SharedBase {}

func explicitAny(_: any SharedTagged) {}
// expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}

// Spelled without `any`, so the implicit existential path has to diagnose too.
func implicitAny(_: SharedTagged) {}
// expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}

func inResultPosition() -> any SharedTagged { fatalError() }
// expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}

func inComposition(_: any SharedTagged & Unrelated) {}
// expected-error@-1 {{cannot form existential type 'any SharedTagged & Unrelated' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'SharedTagged & Unrelated' instead}}

// The class bound can also be written explicitly, without a protocol carrying it.
func explicitClassBound(_: any SharedBase & Unrelated) {}
// expected-error@-1 {{cannot form existential type 'any SharedBase & Unrelated' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'SharedBase & Unrelated' instead}}

// `some P` is just sugar for a generic parameter, so it should still work
func opaqueParameter(_: some SharedTagged) {}

extension SharedBase: Unrelated {}
func opaqueExistential(_: any Unrelated) {}

// MARK: immortal FRT bound

protocol ImmortalTagged: ImmortalBase {}

func immortalBound(_: any ImmortalTagged) {}
// expected-error@-1 {{cannot form existential type 'any ImmortalTagged' because 'ImmortalBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'ImmortalTagged' instead}}

// MARK: parameterized protocol

protocol Keyed<Key>: SharedBase {
  associatedtype Key
}

func parameterized(_: any Keyed<Tag>) {}
// expected-error@-1 {{cannot form existential type 'any Keyed<Tag>' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'Keyed<Tag>' instead}}

// MARK: nested in another type

func nestedInGeneric(_: [any SharedTagged]) {}
// expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}

func nestedInFunction(_: (any SharedTagged) -> Void) {}
// expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}

// MARK: the bound may be inherited transitively

protocol TransitiveMid: SharedBase {}
protocol TransitiveLeaf: TransitiveMid {}

func transitiveBound(_: any TransitiveLeaf) {}
// expected-error@-1 {{cannot form existential type 'any TransitiveLeaf' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'TransitiveLeaf' instead}}

// MARK: behind a typealias, diagnosed at use instead of definition

typealias AliasedExistential = any SharedTagged
typealias AliasedContainer = [any SharedTagged]
typealias AliasedFunction = (any SharedTagged) -> Void

func viaAliasedExistential(_: AliasedExistential) {}
// expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}

func viaAliasedContainer(_: AliasedContainer) {}
// expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}

func viaAliasedFunction(_: AliasedFunction) {}
// expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}

// MARK: declaration kinds other than a function

// Each is diagnosed once, on the declaration as written. An accessor repeats the
// type of its storage, so it must not diagnose again: were accessors not skipped,
// the property with an explicit getter and setter would report three times.

struct Holder {
  var stored: any SharedTagged
  // expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
  // expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}

  var computed: any SharedTagged { fatalError() }
  // expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
  // expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}

  var explicitAccessors: any SharedTagged {
  // expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
  // expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}
    get { fatalError() }
    set {}
  }

  subscript(_: Int) -> any SharedTagged { fatalError() }
  // expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
  // expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}
}

enum Payload {
  case one(any SharedTagged)
  // expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
  // expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}
}

// MARK: the error suppresses the `any`-spelling diagnostic

// A protocol with an associated type spelled without `any` is normally
// diagnosed with "use of protocol 'P' as a type must be written 'any P'". This
// is suppressed for `any FRT`.

protocol Tagging: SharedBase {
  associatedtype Key
}

func implicitAnyWithAssocType(_: Tagging) {}
// expected-error@-1 {{cannot form existential type 'any Tagging' because 'SharedBase' is a foreign reference type}}
// expected-note@-2 {{use a generic parameter constrained to 'Tagging' instead}}

// MARK: a native class bound stays legal (CONTROL)

// The check keys on the bound being a foreign reference type, not on the
// existential merely having a class bound.

class NativeBase {}
protocol NativeTagged: NativeBase {}

func nativeBoundIsFine(_: any NativeTagged) {}
func nativeExplicitBoundIsFine(_: any NativeBase & Unrelated) {}

// MARK: an existential metatype stays legal

// `any P.Type` carries its metadata explicitly rather than recovering it from
// an instance, so it is representable.

protocol MetaBound: SharedBase {}

func metatypeInParameter(_: any MetaBound.Type) {}
func metatypeInResult() -> any MetaBound.Type { fatalError() }

// MARK: FIXME: a context `where` clause is reported once per member

// `Box<any SharedTagged>` is unrepresentable (like `any SharedTagged` itself)
// so we should probably diagnose `extension Box where T == any SharedTagged`
// at the where clause. However, the where clause lacks an interface type,
// which is what we currently check. Instead we diagnose every (invalid) member.
struct Box<T> {}
extension Box where T == any SharedTagged {
  func first() {}
  // expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
  // expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}

  func second() {}
  // expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
  // expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}

  // This still triggers the diagnostics because its interface type is
  //     (Box<any SharedTagged>.Type) -> () -> ()
  static func third() {}
  // expected-error@-1 {{cannot form existential type 'any SharedTagged' because 'SharedBase' is a foreign reference type}}
  // expected-note@-2 {{use a generic parameter constrained to 'SharedTagged' instead}}
}

// No diagnostics if the extension contains no lowerable members.
struct Nox<T> {}
extension Nox where T == any SharedTagged { typealias T_T = () }
