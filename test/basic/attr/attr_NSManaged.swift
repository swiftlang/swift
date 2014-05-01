// RUN: %swift -parse -disable-objc-attr-requires-objc-module %s -verify

protocol SwiftProto { }

@objc class C {
  // okay
  @NSManaged var x: Int

  // okay
  @NSManaged var c: C

  // expected-error@+1{{property cannot be marked @NSManaged because its type cannot be represented in Objective-C}}
  @NSManaged var nonobjc_var: SwiftProto?

  // expected-error@+1{{'NSManaged' not allowed on computed properties}}
  @NSManaged var computed_var: Int {
    return 5
  }

  // expected-error@+1{{'NSManaged' not allowed on observing properties}}
  @NSManaged var observing_var: Int {
    willSet { }
  }

  // FIXME: Junk code used to suppress incorrect diagnostics in the
  // short term.
  init() {
    x = 0
    c = C()
  }
}

