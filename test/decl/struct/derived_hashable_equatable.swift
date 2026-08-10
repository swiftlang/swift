// RUN: %target-swift-frontend -print-ast %s | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-FRAGILE %s
// RUN: %target-swift-frontend -print-ast -enable-library-evolution %s | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-RESILIENT %s

// CHECK-LABEL: internal struct Empty : Hashable
struct Empty: Hashable {

  // CHECK-FRAGILE:   @_implements(Equatable, ==(_:_:)) internal static func __derived_struct_equals(_ a: Empty, _ b: Empty) -> Bool {
  // CHECK-RESILIENT: internal static func == (a: Empty, b: Empty) -> Bool {
  // CHECK-NEXT:        return true
  // CHECK-MEXT:      }

  // CHECK:           internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:      }

  // CHECK:           internal var hashValue: Int {
  // CHECK-NEXT:        get {
  // CHECK-NEXT:          return _hashValue(for: self)
  // CHECK-NEXT:        }
  // CHECK-NEXT:      }
}

// CHECK-LABEL: internal struct HasMembers : Hashable
struct HasMembers: Hashable {
  // CHECK:           internal var a: Int
  var a: Int

  // CHECK:           internal var b: String
  var b: String

  // CHECK-FRAGILE:   @_implements(Equatable, ==(_:_:)) internal static func __derived_struct_equals(_ a: HasMembers, _ b: HasMembers) -> Bool {
  // CHECK-RESILIENT: internal static func == (a: HasMembers, b: HasMembers) -> Bool {
  // CHECK-NEXT:        guard a.a == b.a else {
  // CHECK-NEXT:          return false
  // CHECK-NEXT:        }
  // CHECK-NEXT:        guard a.b == b.b else {
  // CHECK-NEXT:          return false
  // CHECK-NEXT:        }
  // CHECK-NEXT:        return true
  // CHECK-NEXT:      }

  // CHECK:           internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:        hasher.combine(self.a)
  // CHECK-NEXT:        hasher.combine(self.b)
  // CHECK-NEXT:      }

  // CHECK:           internal var hashValue: Int {
  // CHECK-NEXT:        get {
  // CHECK-NEXT:          return _hashValue(for: self)
  // CHECK-NEXT:        }
  // CHECK-NEXT:      }
}

// CHECK-LABEL: internal struct UnavailableStruct : Hashable
@available(*, unavailable)
struct UnavailableStruct: Hashable {
  // CHECK:           internal var a: Int
  var a: Int

  // CHECK:           internal var b: String
  var b: String

  // CHECK-FRAGILE:   @_implements(Equatable, ==(_:_:)) internal static func __derived_struct_equals(_ a: UnavailableStruct, _ b: UnavailableStruct) -> Bool {
  // CHECK-RESILIENT: internal static func == (a: UnavailableStruct, b: UnavailableStruct) -> Bool {
  // CHECK-NEXT:        guard a.a == b.a else {
  // CHECK-NEXT:          return false
  // CHECK-NEXT:        }
  // CHECK-NEXT:        guard a.b == b.b else {
  // CHECK-NEXT:          return false
  // CHECK-NEXT:        }
  // CHECK-NEXT:        return true
  // CHECK-NEXT:      }

  // CHECK:           internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:        hasher.combine(self.a)
  // CHECK-NEXT:        hasher.combine(self.b)
  // CHECK-NEXT:      }

  // CHECK:           internal var hashValue: Int {
  // CHECK-NEXT:        get {
  // CHECK-NEXT:          return _hashValue(for: self)
  // CHECK-NEXT:        }
  // CHECK-NEXT:      }
}

// CHECK-LABEL: internal struct HasStaticMembers : Hashable
struct HasStaticMembers: Hashable {
  // CHECK:           @_hasInitialValue internal static let foo: String = "foo"
  static let foo: String = "foo"

  // CHECK:           @_hasInitialValue internal static var bar: String = "bar"
  static var bar: String = "bar"

  // CHECK:           internal var a: Int
  var a: Int

  // The static members should not be guarded for equality
  // CHECK-FRAGILE:   @_implements(Equatable, ==(_:_:)) internal static func __derived_struct_equals(_ a: HasStaticMembers, _ b: HasStaticMembers) -> Bool {
  // CHECK-RESILIENT: internal static func == (a: HasStaticMembers, b: HasStaticMembers) -> Bool {
  // CHECK-NEXT:        guard a.a == b.a else {
  // CHECK-NEXT:          return false
  // CHECK-NEXT:        }
  // CHECK-NEXT:        return true
  // CHECK-NEXT:      }

  // The hasher should not combine the static members' value
  // CHECK:             internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:        hasher.combine(self.a)
  // CHECK-NEXT:      }

  // CHECK:           internal var hashValue: Int {
  // CHECK-NEXT:        get {
  // CHECK-NEXT:          return _hashValue(for: self)
  // CHECK-NEXT:        }
  // CHECK-NEXT:      }
}

// CHECK-LABEL: internal struct HasComputedProperties : Hashable
struct HasComputedProperties: Hashable {

  // CHECK:           internal var length: Int
  var length: Int

  // CHECK:           internal var area: Int 
  var area: Int {
    return length * length
  }

  // CHECK:           internal var double_length: Int
  var double_length: Int {
    return 2 * length
  }

  // The computed properties should not be guarded for equality
  // CHECK-FRAGILE:   @_implements(Equatable, ==(_:_:)) internal static func __derived_struct_equals(_ a: HasComputedProperties, _ b: HasComputedProperties) -> Bool {
  // CHECK-RESILIENT: internal static func == (a: HasComputedProperties, b: HasComputedProperties) -> Bool {
  // CHECK-NEXT:        guard a.length == b.length else {
  // CHECK-NEXT:          return false
  // CHECK-NEXT:        }
  // CHECK-NEXT:        return true
  // CHECK-NEXT:      }

  // The hasher should not combine the computed properties
  // CHECK:           internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:        hasher.combine(self.length)
  // CHECK-NEXT:      }

  // CHECK:           internal var hashValue: Int {
  // CHECK-NEXT:        get {
  // CHECK-NEXT:          return _hashValue(for: self)
  // CHECK-NEXT:        }
  // CHECK-NEXT:      }
}
