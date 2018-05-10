// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -o - -emit-module-path %t/Lib.swiftmodule -module-name Lib -I %S/Inputs/custom-modules -disable-objc-attr-requires-foundation-module %s > /dev/null

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules | %FileCheck %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -Xcc -DBAD | %FileCheck -check-prefix CHECK-RECOVERY %s

// RUN: %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules -Xcc -DBAD -DTEST %s -verify

#if TEST

import Typedefs
import Lib

func use(_: OkayEnum) {}
// FIXME: Better to import the enum and make it unavailable.
func use(_: BadEnum) {} // expected-error {{use of undeclared type 'BadEnum'}}

func test() {
  _ = producesOkayEnum()
  _ = producesBadEnum() // expected-error {{use of unresolved identifier 'producesBadEnum'}}

  // Force a lookup of the ==
  _ = Optional(OkayEnum.noPayload).map { $0 == .noPayload }
}

#else // TEST

import Typedefs

public enum BadEnum {
  case noPayload
  case perfectlyOkayPayload(Int)
  case problematic(Any, WrappedInt)
  case alsoOkay(Any, Any, Any)

  public static func ==(a: BadEnum, b: BadEnum) -> Bool {
    return false
  }
}
// CHECK-LABEL: enum BadEnum {
// CHECK-RECOVERY-NOT: enum BadEnum

public enum GenericBadEnum<T: HasAssoc> where T.Assoc == WrappedInt {
  case noPayload
  case perfectlyOkayPayload(Int)

  public static func ==(a: GenericBadEnum<T>, b: GenericBadEnum<T>) -> Bool {
    return false
  }
}
// CHECK-LABEL: enum GenericBadEnum<T> where T : HasAssoc, T.Assoc == WrappedInt {
// CHECK-RECOVERY-NOT: enum GenericBadEnum

public enum OkayEnum {
  case noPayload
  case plainOldAlias(Any, UnwrappedInt)
  case other(Int)

  public static func ==(a: OkayEnum, b: OkayEnum) -> Bool {
    return false
  }
}
// CHECK-LABEL: enum OkayEnum {
// CHECK-NEXT:   case noPayload
// CHECK-NEXT:   case plainOldAlias(Any, UnwrappedInt)
// CHECK-NEXT:   case other(Int)
// CHECK-NEXT:   static func == (a: OkayEnum, b: OkayEnum) -> Bool
// CHECK-NEXT: }
// CHECK-RECOVERY-LABEL: enum OkayEnum {
// CHECK-RECOVERY-NEXT:   case noPayload
// CHECK-RECOVERY-NEXT:   case plainOldAlias(Any, Int32)
// CHECK-RECOVERY-NEXT:   case other(Int)
// CHECK-RECOVERY-NEXT:   static func == (a: OkayEnum, b: OkayEnum) -> Bool
// CHECK-RECOVERY-NEXT: }

public enum OkayEnumWithSelfRefs {
  public struct Nested {}
  indirect case selfRef(OkayEnumWithSelfRefs)
  case nested(Nested)
}
// CHECK-LABEL: enum OkayEnumWithSelfRefs {
// CHECK-NEXT:   struct Nested {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT:   indirect case selfRef(OkayEnumWithSelfRefs)
// CHECK-NEXT:   case nested(OkayEnumWithSelfRefs.Nested)
// CHECK-NEXT: }
// CHECK-RECOVERY-LABEL: enum OkayEnumWithSelfRefs {
// CHECK-RECOVERY-NEXT:   struct Nested {
// CHECK-RECOVERY-NEXT:     init()
// CHECK-RECOVERY-NEXT:   }
// CHECK-RECOVERY-NEXT:   indirect case selfRef(OkayEnumWithSelfRefs)
// CHECK-RECOVERY-NEXT:   case nested(OkayEnumWithSelfRefs.Nested)
// CHECK-RECOVERY-NEXT: }

public protocol HasAssoc {
  associatedtype Assoc
}

public func producesBadEnum() -> BadEnum { return .noPayload }
// CHECK-LABEL: func producesBadEnum() -> BadEnum
// CHECK-RECOVERY-NOT: func producesBadEnum() -> BadEnum

public func producesGenericBadEnum<T>() -> GenericBadEnum<T> { return .noPayload }
// CHECK-LABEL: func producesGenericBadEnum<T>() -> GenericBadEnum<T>
// CHECK-RECOVERY-NOT: func producesGenericBadEnum

public func producesOkayEnum() -> OkayEnum { return .noPayload }
// CHECK-LABEL: func producesOkayEnum() -> OkayEnum
// CHECK-RECOVERY-LABEL: func producesOkayEnum() -> OkayEnum

#endif // TEST
