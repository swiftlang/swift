// DEFINE: %{args} = -module-name main -parse-as-library -swift-version 5

// RUN: %target-swift-frontend -print-ast %s -verify %{args} | %FileCheck %s

// Also make sure that lowering the synthesized code doesn't crash.
// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -verify %{args}
// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -verify %{args} -enable-library-evolution

// CHECK-LABEL:  public enum RawValueEnum : Int {

public enum RawValueEnum: Int {
  case alwaysAvailable = 0

  @available(*, unavailable)
  case universallyUnavailable = 1

  @available(*, deprecated)
  case universallyDeprecated = 2

  @available(*, deprecated, message: "use something else")
  case universallyDeprecatedWithMessage = 3

  @available(swift 4)
  case introducedInSwift4 = 4

  @available(swift 99)
  case introducedInSwift99 = 5

  @available(swift, obsoleted: 4)
  case obsoletedInSwift4 = 6

  @available(swift, obsoleted: 99)
  case obsoletedInSwift99 = 7

  @available(swift, introduced: 4, obsoleted: 99)
  case introducedInSwift4AndObsoletedInSwift99 = 8

  @available(swift, deprecated: 4)
  case deprecatedInSwift4 = 9

  @available(swift, deprecated: 99)
  case deprecatedInSwift99 = 10

  // CHECK-LABEL:   public init?(rawValue: Int) {
  // CHECK-NEXT:     switch rawValue {
  // CHECK-NEXT:     case 0:
  // CHECK-NEXT:       self = RawValueEnum.alwaysAvailable
  // CHECK-NEXT:     case 2:
  // CHECK-NEXT:       self = RawValueEnum.universallyDeprecated
  // CHECK-NEXT:     case 3:
  // CHECK-NEXT:       self = RawValueEnum.universallyDeprecatedWithMessage
  // CHECK-NEXT:     case 4:
  // CHECK-NEXT:       self = RawValueEnum.introducedInSwift4
  // CHECK-NEXT:     case 7:
  // CHECK-NEXT:       self = RawValueEnum.obsoletedInSwift99
  // CHECK-NEXT:     case 8:
  // CHECK-NEXT:       self = RawValueEnum.introducedInSwift4AndObsoletedInSwift99
  // CHECK-NEXT:     case 9:
  // CHECK-NEXT:       self = RawValueEnum.deprecatedInSwift4
  // CHECK-NEXT:     case 10:
  // CHECK-NEXT:       self = RawValueEnum.deprecatedInSwift99
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-LABEL:   public var rawValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .alwaysAvailable:
  // CHECK-NEXT:         return 0
  // CHECK-NEXT:       case .universallyUnavailable:
  // CHECK-NEXT:         return 1
  // CHECK-NEXT:       case .universallyDeprecated:
  // CHECK-NEXT:         return 2
  // CHECK-NEXT:       case .universallyDeprecatedWithMessage:
  // CHECK-NEXT:         return 3
  // CHECK-NEXT:       case .introducedInSwift4:
  // CHECK-NEXT:         return 4
  // CHECK-NEXT:       case .introducedInSwift99:
  // CHECK-NEXT:         return 5
  // CHECK-NEXT:       case .obsoletedInSwift4:
  // CHECK-NEXT:         return 6
  // CHECK-NEXT:       case .obsoletedInSwift99:
  // CHECK-NEXT:         return 7
  // CHECK-NEXT:       case .introducedInSwift4AndObsoletedInSwift99:
  // CHECK-NEXT:         return 8
  // CHECK-NEXT:       case .deprecatedInSwift4:
  // CHECK-NEXT:         return 9
  // CHECK-NEXT:       case .deprecatedInSwift99:
  // CHECK-NEXT:         return 10
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL:  public enum NoPayloadEnum : Hashable {

public enum NoPayloadEnum: Hashable {
  case alwaysAvailable

  @available(*, unavailable)
  case universallyUnavailable

  @available(*, deprecated)
  case universallyDeprecated

  @available(swift 99)
  case introducedInSwift99

  @available(swift, obsoleted: 4)
  case obsoletedInSwift4

  @available(swift, deprecated: 4)
  case deprecatedInSwift4

  // CHECK-LABEL:   @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) public static func __derived_enum_equals(_ a: NoPayloadEnum, _ b: NoPayloadEnum) -> Bool {
  // CHECK-NEXT:     var index_a: Int
  // CHECK-NEXT:     switch a {
  // CHECK-NEXT:     case .alwaysAvailable:
  // CHECK-NEXT:       index_a = 0
  // CHECK-NEXT:     case .universallyUnavailable:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .universallyDeprecated:
  // CHECK-NEXT:       index_a = 1
  // CHECK-NEXT:     case .introducedInSwift99:
  // CHECK-NEXT:       index_a = 2
  // CHECK-NEXT:     case .obsoletedInSwift4:
  // CHECK-NEXT:       index_a = 3
  // CHECK-NEXT:     case .deprecatedInSwift4:
  // CHECK-NEXT:       index_a = 4
  // CHECK-NEXT:     }
  // CHECK-NEXT:     var index_b: Int
  // CHECK-NEXT:     switch b {
  // CHECK-NEXT:     case .alwaysAvailable:
  // CHECK-NEXT:       index_b = 0
  // CHECK-NEXT:     case .universallyUnavailable:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .universallyDeprecated:
  // CHECK-NEXT:       index_b = 1
  // CHECK-NEXT:     case .introducedInSwift99:
  // CHECK-NEXT:       index_b = 2
  // CHECK-NEXT:     case .obsoletedInSwift4:
  // CHECK-NEXT:       index_b = 3
  // CHECK-NEXT:     case .deprecatedInSwift4:
  // CHECK-NEXT:       index_b = 4
  // CHECK-NEXT:     }
  // CHECK-NEXT:     return index_a == index_b
  // CHECK-NEXT:   }

  // CHECK-LABEL:   public func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:     var discriminator: Int
  // CHECK-NEXT:     switch self {
  // CHECK-NEXT:     case .alwaysAvailable:
  // CHECK-NEXT:       discriminator = 0
  // CHECK-NEXT:     case .universallyUnavailable:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .universallyDeprecated:
  // CHECK-NEXT:       discriminator = 1
  // CHECK-NEXT:     case .introducedInSwift99:
  // CHECK-NEXT:       discriminator = 2
  // CHECK-NEXT:     case .obsoletedInSwift4:
  // CHECK-NEXT:       discriminator = 3
  // CHECK-NEXT:     case .deprecatedInSwift4:
  // CHECK-NEXT:       discriminator = 4
  // CHECK-NEXT:     }
  // CHECK-NEXT:     hasher.combine(discriminator)
  // CHECK-NEXT:   }
}

// CHECK-LABEL:  public enum PayloadEnum : Hashable {

public enum PayloadEnum: Hashable {
  case alwaysAvailable(Int)

  @available(*, unavailable)
  case universallyUnavailable(Int)

  @available(*, deprecated)
  case universallyDeprecated(Int)

  @available(swift, obsoleted: 4)
  case obsoletedInSwift4(Int)

  // CHECK-LABEL:   @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) public static func __derived_enum_equals(_ a: PayloadEnum, _ b: PayloadEnum) -> Bool {
  // CHECK-NEXT:     switch (a, b) {
  // CHECK-NEXT:     case (.alwaysAvailable(let l0), .alwaysAvailable(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     case (.universallyUnavailable, .universallyUnavailable):
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case (.universallyDeprecated(let l0), .universallyDeprecated(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     case (.obsoletedInSwift4(let l0), .obsoletedInSwift4(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return false
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-LABEL:   public func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:     switch self {
  // CHECK-NEXT:     case .alwaysAvailable(let a0):
  // CHECK-NEXT:       hasher.combine(0)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     case .universallyUnavailable:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .universallyDeprecated(let a0):
  // CHECK-NEXT:       hasher.combine(1)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     case .obsoletedInSwift4(let a0):
  // CHECK-NEXT:       hasher.combine(2)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL:  public enum UnavailableEnum : Int {

@available(*, unavailable)
public enum UnavailableEnum: Int {
  case a = 0

  @available(*, unavailable)
  case b = 1

  @available(swift, obsoleted: 4)
  case c = 2

  // CHECK-LABEL:   public init?(rawValue: Int) {
  // CHECK-NEXT:     switch rawValue {
  // CHECK-NEXT:     case 0:
  // CHECK-NEXT:       self = UnavailableEnum.a
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-LABEL:   public var rawValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .a:
  // CHECK-NEXT:         return 0
  // CHECK-NEXT:       case .b:
  // CHECK-NEXT:         return 1
  // CHECK-NEXT:       case .c:
  // CHECK-NEXT:         return 2
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}
