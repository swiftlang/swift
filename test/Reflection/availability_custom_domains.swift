// RUN: %empty-directory(%t)

// RUN: %target-build-swift \
// RUN:   -target %target-swift-5.2-abi-triple \
// RUN:   -parse-as-library -emit-module -emit-library \
// RUN:   -module-name Test \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -Xfrontend -define-enabled-availability-domain -Xfrontend EnabledDomain \
// RUN:   -Xfrontend -define-disabled-availability-domain -Xfrontend DisabledDomain \
// RUN:   -Xfrontend -define-dynamic-availability-domain -Xfrontend DynamicDomain \
// RUN:   %no-fixup-chains %s \
// RUN:   -o %t/%target-library-name(Test)

// RUN: %target-swift-reflection-dump %t/%target-library-name(Test) \
// RUN:   > %t/reflection.txt

// RUN: %FileCheck %s --check-prefix=CHECK --input-file=%t/reflection.txt
// RUN: %FileCheck %s --check-prefix=CHECK-NEGATIVE --input-file=%t/reflection.txt

// REQUIRES: swift_feature_CustomAvailability

// Every type/member that should be absent from the reflection dump has
// "Disabled" or "disabled" in its name.
// CHECK-NEGATIVE-NOT: Disabled
// CHECK-NEGATIVE-NOT: disabled

// Types and cases appear in declaration order; disabled types and their cases
// are absent so their declaration positions are simply skipped.

// CHECK: FIELDS:
// CHECK-NEXT: =======
// CHECK-NEXT: Test.EnabledEnum
// CHECK-NEXT: ----------------
// CHECK-NEXT: enabledEnumElement

// CHECK: Test.DynamicEnum
// CHECK-NEXT: ----------------
// CHECK-NEXT: dynamicEnumElement

// CHECK: Test.AlwaysAvailableEnum
// CHECK-NEXT: ------------------------
// CHECK-NEXT: enabledElement
// FIXME: Blank lines here represent the absent element
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-NEXT: dynamicElement

// CHECK: Test.EnabledStruct
// CHECK-NEXT: ------------------
// CHECK-NEXT: enabledStructProp: Swift.Bool
// CHECK-NEXT: (struct Swift.Bool)

// CHECK: Test.DynamicStruct
// CHECK-NEXT: ------------------
// CHECK-NEXT: dynamicStructProp: Swift.Bool
// CHECK-NEXT: (struct Swift.Bool)

// CHECK: Test.EnabledClass
// CHECK-NEXT: -----------------
// CHECK-NEXT: enabledClassProp: Swift.Bool
// CHECK-NEXT: (struct Swift.Bool)

// CHECK: Test.DynamicClass
// CHECK-NEXT: -----------------
// CHECK-NEXT: dynamicClassProp: Swift.Bool
// CHECK-NEXT: (struct Swift.Bool)

public enum EnabledEnum {
  case enabledEnumElement
}

@available(DisabledDomain)
public enum DisabledEnum {
  case disabledEnumElement
}

@available(DynamicDomain)
public enum DynamicEnum {
  case dynamicEnumElement
}

@available(EnabledDomain)
@available(DisabledDomain)
public enum DisabledAndEnabledEnum {
  case disabledAndEnabledEnumElement
}

public enum AlwaysAvailableEnum {
  @available(EnabledDomain)
  case enabledElement

  @available(DisabledDomain)
  case disabledElement

  @available(DynamicDomain)
  case dynamicElement

  @available(EnabledDomain)
  @available(DisabledDomain)
  case disabledAndEnabledElement
}

@available(EnabledDomain)
public struct EnabledStruct {
  public var enabledStructProp: Bool = false
}

@available(DisabledDomain)
public struct DisabledStruct {
  public var disabledStructProp: Bool = false
}

@available(DynamicDomain)
public struct DynamicStruct {
  public var dynamicStructProp: Bool = false
}

@available(EnabledDomain)
@available(DisabledDomain)
public struct DisabledAndEnabledStruct {
  public var disabledAndEnabledStructProp: Bool = false
}

@available(EnabledDomain)
public class EnabledClass {
  public var enabledClassProp: Bool = false
  public init() {}
}

@available(DisabledDomain)
public class DisabledClass {
  public var disabledClassProp: Bool = false
  public init() {}
}

@available(DynamicDomain)
public class DynamicClass {
  public var dynamicClassProp: Bool = false
  public init() {}
}

@available(EnabledDomain)
@available(DisabledDomain)
public class DisabledAndEnabledClass {
  public var disabledAndEnabledClassProp: Bool = false
  public init() {}
}
