// RUN: %target-swift-frontend -typecheck -swift-version 4.2 -verify -dump-ast -enable-library-evolution %s | %FileCheck --check-prefixes=BOTH,RESILIENCE-ON %s
// RUN: %target-swift-frontend -typecheck -swift-version 4.2 -verify -dump-ast -enable-library-evolution -enable-testing %s | %FileCheck --check-prefixes=BOTH,RESILIENCE-ON %s
// RUN: not %target-swift-frontend -typecheck -swift-version 4.2 -dump-ast %s | %FileCheck --check-prefixes=BOTH,RESILIENCE-OFF %s
// RUN: not %target-swift-frontend -typecheck -swift-version 4.2 -dump-ast %s -enable-testing | %FileCheck --check-prefixes=BOTH,RESILIENCE-OFF %s

//
// Public types with @frozen are always fixed layout
//

// BOTH-LABEL: struct_decl{{.*}}"Point"
// RESILIENCE-ON-NOT: resilient
// RESILIENCE-OFF-NOT: resilient
@frozen public struct Point {
  let x, y: Int
}

// BOTH-LABEL: struct_decl{{.*}}"FixedPoint"
// RESILIENCE-ON-NOT: resilient
// RESILIENCE-OFF-NOT: resilient
@_fixed_layout public struct FixedPoint {
  // expected-warning@-1 {{'@frozen' attribute is now used for fixed-layout structs}}
  let x, y: Int
}

// BOTH-LABEL: enum_decl{{.*}}"ChooseYourOwnAdventure"
// RESILIENCE-ON-NOT: resilient
// RESILIENCE-OFF-NOT: resilient
@frozen public enum ChooseYourOwnAdventure {
  case JumpIntoRabbitHole
  case EatMushroom
}

//
// Public types are resilient when -enable-library-evolution is on
//

// BOTH-LABEL: struct_decl{{.*}}"Size"
// RESILIENCE-ON-SAME: resilient
// RESILIENCE-OFF-NOT: resilient
public struct Size {
  let w, h: Int
}

// BOTH-LABEL: struct_decl{{.*}}"UsableFromInlineStruct"
// RESILIENCE-ON-NOT: resilient
// RESILIENCE-OFF-NOT: resilient
@frozen @usableFromInline struct UsableFromInlineStruct {}

// BOTH-LABEL: struct_decl{{.*}}"UsableFromInlineFixedStruct"
// RESILIENCE-ON-NOT: resilient
// RESILIENCE-OFF-NOT: resilient
@_fixed_layout @usableFromInline struct UsableFromInlineFixedStruct {}
// expected-warning@-1 {{'@frozen' attribute is now used for fixed-layout structs}}

// BOTH-LABEL: enum_decl{{.*}}"TaxCredit"
// RESILIENCE-ON-SAME: resilient
// RESILIENCE-OFF-NOT: resilient
public enum TaxCredit {
  case EarnedIncome
  case MortgageDeduction
}

//
// Internal types are always fixed layout
//

// BOTH-LABEL: struct_decl{{.*}}"Rectangle"
// RESILIENCE-ON-NOT: resilient
// RESILIENCE-OFF-NOT: resilient
struct Rectangle {
  let topLeft: Point
  let bottomRight: Size
}

//
// Diagnostics
//

// BOTH-LABEL: struct_decl{{.*}}"InternalStruct"

@frozen struct InternalStruct { // expected-note * {{declared here}}
// expected-error@-1 {{'@frozen' attribute can only be applied to '@usableFromInline' or public declarations, but 'InternalStruct' is internal}}

  @frozen public struct NestedStruct {}
}

@_fixed_layout struct FixedInternalStruct { // expected-note * {{declared here}}
// expected-error@-1 {{'@_fixed_layout' attribute can only be applied to '@usableFromInline' or public declarations, but 'FixedInternalStruct' is internal}}
// expected-warning@-2 {{'@frozen' attribute is now used for fixed-layout structs}}

  @_fixed_layout public struct NestedStruct {}
  // expected-warning@-1 {{'@frozen' attribute is now used for fixed-layout structs}}
}

@frozen fileprivate struct FileprivateStruct {}
// expected-error@-1 {{'@frozen' attribute can only be applied to '@usableFromInline' or public declarations, but 'FileprivateStruct' is fileprivate}}

@_fixed_layout fileprivate struct FixedFileprivateStruct {}
// expected-error@-1 {{'@_fixed_layout' attribute can only be applied to '@usableFromInline' or public declarations, but 'FixedFileprivateStruct' is fileprivate}}
// expected-warning@-2 {{'@frozen' attribute is now used for fixed-layout structs}}

@frozen private struct PrivateStruct {} // expected-note * {{declared here}}
// expected-error@-1 {{'@frozen' attribute can only be applied to '@usableFromInline' or public declarations, but 'PrivateStruct' is private}}

@_fixed_layout private struct FixedPrivateStruct {} // expected-note * {{declared here}}
// expected-error@-1 {{'@_fixed_layout' attribute can only be applied to '@usableFromInline' or public declarations, but 'FixedPrivateStruct' is private}}
// expected-warning@-2 {{'@frozen' attribute is now used for fixed-layout structs}}


@frozen public struct BadFields1 {
  private var field: PrivateStruct // expected-error {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
}

@_fixed_layout public struct FixedBadFields1 {
// expected-warning@-1 {{'@frozen' attribute is now used for fixed-layout structs}}
  private var field: PrivateStruct // expected-error {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
}

@frozen public struct BadFields2 {
  private var field: PrivateStruct? // expected-error {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
}

@_fixed_layout public struct FixedBadFields2 {
// expected-warning@-1 {{'@frozen' attribute is now used for fixed-layout structs}}
  private var field: PrivateStruct? // expected-error {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
}

@frozen public struct BadFields3 {
  internal var field: InternalStruct? // expected-error {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
}

@_fixed_layout public struct FixedBadFields3 {
// expected-warning@-1 {{'@frozen' attribute is now used for fixed-layout structs}}
  internal var field: InternalStruct? // expected-error {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
}

@frozen @usableFromInline struct BadFields4 {
  internal var field: InternalStruct? // expected-error {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
}

@_fixed_layout @usableFromInline struct FixedBadFields4 {
// expected-warning@-1 {{'@frozen' attribute is now used for fixed-layout structs}}
  internal var field: InternalStruct? // expected-error {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
}

@frozen public struct BadFields5 {
  private var field: PrivateStruct? { // expected-error {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
    didSet {}
    
    
  }
}

@_fixed_layout public struct FixedBadFields5 {
// expected-warning@-1 {{'@frozen' attribute is now used for fixed-layout structs}}
  private var field: PrivateStruct? { // expected-error {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
    didSet {}
    
    
  }
}

// expected-warning@+1 {{the result of a '@usableFromInline' function should be '@usableFromInline' or public}}
@usableFromInline func notReallyUsableFromInline() -> InternalStruct? { return nil }
@frozen public struct BadFields6 {
  private var field = notReallyUsableFromInline() // expected-error {{type referenced from a stored property with inferred type 'InternalStruct?' in a '@frozen' struct must be '@usableFromInline' or public}}
}

// expected-warning@+1 {{the result of a '@usableFromInline' function should be '@usableFromInline' or public}}
@usableFromInline func notReallyUsableFromInlineFixed() -> FixedInternalStruct? { return nil }
@_fixed_layout public struct FrozenBadFields6 {
// expected-warning@-1 {{'@frozen' attribute is now used for fixed-layout structs}}
  private var field = notReallyUsableFromInlineFixed() // expected-error {{type referenced from a stored property with inferred type 'FixedInternalStruct?' in a '@frozen' struct must be '@usableFromInline' or public}}
}

@frozen public struct OKFields {
  private var publicTy: Size
  internal var ufiTy: UsableFromInlineStruct?

  internal static var staticProp: InternalStruct?

  private var computed: PrivateStruct? { return nil }
}

@_fixed_layout public struct FixedOKFields {
// expected-warning@-1 {{'@frozen' attribute is now used for fixed-layout structs}}
  private var publicTy: Size
  internal var ufiTy: UsableFromInlineStruct?

  internal static var staticProp: InternalStruct?

  private var computed: PrivateStruct? { return nil }
}
