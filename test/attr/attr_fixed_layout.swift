// RUN: %target-swift-frontend -typecheck -verify -dump-ast -enable-resilience %s 2>&1 | %FileCheck --check-prefix=RESILIENCE-ON %s
// RUN: %target-swift-frontend -typecheck -verify -dump-ast -enable-resilience -enable-testing %s 2>&1 | %FileCheck --check-prefix=RESILIENCE-ON %s
// RUN: not %target-swift-frontend -typecheck -dump-ast %s 2>&1 | %FileCheck --check-prefix=RESILIENCE-OFF %s
// RUN: not %target-swift-frontend -typecheck -dump-ast %s -enable-testing 2>&1 | %FileCheck --check-prefix=RESILIENCE-OFF %s

//
// Public types with @_fixed_layout are always fixed layout
//

// RESILIENCE-ON: struct_decl "Point" interface type='Point.Type' access=public non-resilient
// RESILIENCE-OFF: struct_decl "Point" interface type='Point.Type' access=public non-resilient
@_fixed_layout public struct Point {
  let x, y: Int
}

// RESILIENCE-ON: enum_decl "ChooseYourOwnAdventure" interface type='ChooseYourOwnAdventure.Type' access=public non-resilient
// RESILIENCE-OFF: enum_decl "ChooseYourOwnAdventure" interface type='ChooseYourOwnAdventure.Type' access=public non-resilient
@_frozen public enum ChooseYourOwnAdventure {
  case JumpIntoRabbitHole
  case EatMushroom
}

//
// Public types are resilient when -enable-resilience is on
//

// RESILIENCE-ON: struct_decl "Size" interface type='Size.Type' access=public resilient
// RESILIENCE-OFF: struct_decl "Size" interface type='Size.Type' access=public non-resilient
public struct Size {
  let w, h: Int
}

// RESILIENCE-ON: enum_decl "TaxCredit" interface type='TaxCredit.Type' access=public resilient
// RESILIENCE-OFF: enum_decl "TaxCredit" interface type='TaxCredit.Type' access=public non-resilient
public enum TaxCredit {
  case EarnedIncome
  case MortgageDeduction
}

//
// Internal types are always fixed layout
//

// RESILIENCE-ON: struct_decl "Rectangle" interface type='Rectangle.Type' access=internal non-resilient
// RESILIENCE-OFF: struct_decl "Rectangle" interface type='Rectangle.Type' access=internal non-resilient
struct Rectangle {
  let topLeft: Point
  let bottomRight: Size
}

//
// Diagnostics
//

@_fixed_layout struct InternalStruct {
// expected-error@-1 {{'@_fixed_layout' attribute can only be applied to '@usableFromInline' or public declarations, but 'InternalStruct' is internal}}

  @_fixed_layout public struct NestedStruct {}
}

@_fixed_layout fileprivate struct FileprivateStruct {}
// expected-error@-1 {{'@_fixed_layout' attribute can only be applied to '@usableFromInline' or public declarations, but 'FileprivateStruct' is fileprivate}}

@_fixed_layout private struct PrivateStruct {}
// expected-error@-1 {{'@_fixed_layout' attribute can only be applied to '@usableFromInline' or public declarations, but 'PrivateStruct' is private}}
