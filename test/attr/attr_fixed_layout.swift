// RUN: %target-swift-frontend -parse -dump-ast -enable-resilience %s 2>&1 | %FileCheck --check-prefix=RESILIENCE-ON %s
// RUN: %target-swift-frontend -parse -dump-ast %s 2>&1 | %FileCheck --check-prefix=RESILIENCE-OFF %s

//
// Public types with @_fixed_layout are always fixed layout
//

// RESILIENCE-ON: struct_decl "Point" type='Point.Type' access=public @_fixed_layout
// RESILIENCE-OFF: struct_decl "Point" type='Point.Type' access=public @_fixed_layout
@_fixed_layout public struct Point {
  let x, y: Int
}

// RESILIENCE-ON: enum_decl "ChooseYourOwnAdventure" type='ChooseYourOwnAdventure.Type' access=public @_fixed_layout
// RESILIENCE-OFF: enum_decl "ChooseYourOwnAdventure" type='ChooseYourOwnAdventure.Type' access=public @_fixed_layout
@_fixed_layout public enum ChooseYourOwnAdventure {
  case JumpIntoRabbitHole
  case EatMushroom
}

//
// Public types are resilient when -enable-resilience is on
//

// RESILIENCE-ON: struct_decl "Size" type='Size.Type' access=public @_resilient_layout
// RESILIENCE-OFF: struct_decl "Size" type='Size.Type' access=public @_fixed_layout
public struct Size {
  let w, h: Int
}

// RESILIENCE-ON: enum_decl "TaxCredit" type='TaxCredit.Type' access=public @_resilient_layout
// RESILIENCE-OFF: enum_decl "TaxCredit" type='TaxCredit.Type' access=public @_fixed_layout
public enum TaxCredit {
  case EarnedIncome
  case MortgageDeduction
}

//
// Internal types are always fixed layout
//

// RESILIENCE-ON: struct_decl "Rectangle" type='Rectangle.Type' access=internal @_fixed_layout
// RESILIENCE-OFF: struct_decl "Rectangle" type='Rectangle.Type' access=internal @_fixed_layout
struct Rectangle {
  let topLeft: Point
  let bottomRight: Size
}
