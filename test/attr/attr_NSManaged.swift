// RUN: %target-parse-verify-swift

protocol SwiftProto { }

@objc class C {
  // okay
  @NSManaged var x: Int

  // expected-error@+1{{'NSManaged' property cannot have an initial value}}
  @NSManaged var c: C = C()

  // expected-error@+1{{property cannot be marked @NSManaged because its type cannot be represented in Objective-C}}
  @NSManaged var nonobjc_var: SwiftProto?

  // expected-error@+1{{'NSManaged' attribute not allowed on a 'let' property}}
  @NSManaged let let_property: Int = 10

  // expected-error@+1{{'NSManaged' not allowed on computed properties}}
  @NSManaged var computed_var: Int {
    return 5
  }

  // expected-error@+1{{'NSManaged' not allowed on observing properties}}
  @NSManaged var observing_var: Int {
    willSet { }
  }

  init() {
    observing_var = 10
  }
}

