// RUN: %target-swift-frontend -typecheck %s -I %t -swift-version 5 -package-name mypkg -verify

package struct PkgStruct {
  package var one: Int
  package var two: String
  package func f() {}
}

@frozen
package struct FrozenPkgStruct  {
  package var one: Int
  package var two: String
  package func f() {}
}

@frozen
@usableFromInline
package struct FrozenUfiPkgStruct {
  package var one: Int
  package var two: String
  package func f() {}
}

@frozen // expected-error {{'@frozen' attribute can only be applied to '@usableFromInline', package, or public declarations, but 'FrozenInternalStruct' is internal}} {{1-9=}}
struct FrozenInternalStruct  {
  var one: Int
  var two: String
  func f() {}
}

@_fixed_layout
package class FixedPkgKlass  {
  package var one: Int = 1
  package var two: String = ""
  package func f() {}
}

@_fixed_layout // expected-error {{'@_fixed_layout' attribute can only be applied to '@usableFromInline', package, or public declarations, but 'FixedInternalKlass' is internal}} {{1-16=}}
class FixedInternalKlass  {
  var one: Int = 1
  var two: String = ""
  func f() {}
}

