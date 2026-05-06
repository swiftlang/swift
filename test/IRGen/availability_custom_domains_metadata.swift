// DEFINE: %{frontend-flags} = \
// DEFINE:   -module-name Test \
// DEFINE:   -enable-experimental-feature CustomAvailability \
// DEFINE:   -define-enabled-availability-domain EnabledDomain \
// DEFINE:   -define-disabled-availability-domain DisabledDomain \
// DEFINE:   -define-dynamic-availability-domain DynamicDomain

// RUN: %target-swift-emit-irgen %{frontend-flags} %s -verify \
// RUN:   -Onone > %t.Onone.ir

// RUN: %target-swift-emit-irgen %{frontend-flags} %s -verify \
// RUN:   -Onone -enable-library-evolution > %t.Onone-evo.ir

// RUN: %target-swift-emit-irgen %{frontend-flags} %s -verify \
// RUN:   -O > %t.O.ir

// RUN: %FileCheck %s --check-prefixes=CHECK-REFLECTION --input-file=%t.Onone.ir
// RUN: %FileCheck %s --check-prefixes=CHECK-REFLECTION --input-file=%t.Onone-evo.ir
// RUN: %FileCheck %s --check-prefixes=CHECK-REFLECTION --input-file=%t.O.ir

// RUN: %FileCheck %s --check-prefixes=CHECK-NEGATIVE --input-file=%t.Onone.ir
// RUN: %FileCheck %s --check-prefixes=CHECK-NEGATIVE --input-file=%t.Onone-evo.ir
// RUN: %FileCheck %s --check-prefixes=CHECK-NEGATIVE --input-file=%t.O.ir

// RUN: %FileCheck %s --check-prefixes=CHECK-METADATA --input-file=%t.Onone.ir
// RUN: %FileCheck %s --check-prefixes=CHECK-METADATA --input-file=%t.Onone-evo.ir
// RUN: %FileCheck %s --check-prefixes=CHECK-METADATA --input-file=%t.O.ir

// RUN: %FileCheck %s --check-prefixes=CHECK-WITNESS --input-file=%t.Onone.ir
// RUN: %FileCheck %s --check-prefixes=CHECK-WITNESS --input-file=%t.Onone-evo.ir
// RUN: %FileCheck %s --check-prefixes=CHECK-WITNESS --input-file=%t.O.ir

// RUN: %FileCheck %s --check-prefixes=CHECK-DESCRIPTOR --input-file=%t.Onone.ir
// RUN: %FileCheck %s --check-prefixes=CHECK-DESCRIPTOR --input-file=%t.Onone-evo.ir
// RUN: %FileCheck %s --check-prefixes=CHECK-DESCRIPTOR --input-file=%t.O.ir

// RUN: %FileCheck %s --check-prefixes=CHECK-DESCRIPTOR-STORED --input-file=%t.Onone.ir
// RUN: %FileCheck %s --check-prefixes=CHECK-DESCRIPTOR-STORED --input-file=%t.Onone-evo.ir
// RUN: %FileCheck %s --check-prefixes=CHECK-DESCRIPTOR-STORED --input-file=%t.O.ir

// REQUIRES: swift_feature_CustomAvailability

// Every type/member in this test that should be skipped during codegen has
// either "disabled" or "Disabled" in the name.
// CHECK-NEGATIVE-NOT: disabled
// CHECK-NEGATIVE-NOT: Disabled

// CHECK-METADATA: @"$s4Test19AlwaysAvailableEnumOMf" = internal constant
// CHECK-WITNESS: @"$s4Test19AlwaysAvailableEnumOWV" = internal constant
public enum AlwaysAvailableEnum {
  // CHECK-REFLECTION: constant [15 x i8] c"enabledElement\00"
  @available(EnabledDomain)
  case enabledElement

  // CHECK-REFLECTION-NOT: constant [16 x i8] c"disabledElement\00"
  @available(DisabledDomain)
  case disabledElement

  // CHECK-REFLECTION: constant [15 x i8] c"dynamicElement\00"
  @available(DynamicDomain)
  case dynamicElement

  // CHECK-REFLECTION-NOT: constant [26 x i8] c"disabledAndEnabledElement\00"
  @available(EnabledDomain)
  @available(DisabledDomain)
  case disabledAndEnabledElement
}

// CHECK-METADATA: @"$s4Test11EnabledEnumOMf" = internal constant
// CHECK-WITNESS: @"$s4Test11EnabledEnumOWV" = internal constant
@available(EnabledDomain)
public enum EnabledEnum {
  // CHECK-REFLECTION: constant [19 x i8] c"enabledEnumElement\00"
  case enabledEnumElement
}

// CHECK-METADATA-NOT: @"$s4Test12DisabledEnumO
// CHECK-WITNESS-NOT: @"$s4Test12DisabledEnumO
@available(DisabledDomain)
public enum DisabledEnum {
  // CHECK-REFLECTION-NOT: constant [20 x i8] c"disabledEnumElement\00"
  case disabledEnumElement
}

// CHECK-METADATA: @"$s4Test11DynamicEnumOMf" = internal constant
// CHECK-WITNESS: @"$s4Test11DynamicEnumOWV" = internal constant
@available(DynamicDomain)
public enum DynamicEnum {
  // CHECK-REFLECTION: constant [19 x i8] c"dynamicEnumElement\00"
  case dynamicEnumElement
}

// CHECK-METADATA-NOT: @"$s4Test22DisabledAndEnabledEnumO
// CHECK-WITNESS-NOT: @"$s4Test22DisabledAndEnabledEnumO
@available(EnabledDomain)
@available(DisabledDomain)
public enum DisabledAndEnabledEnum {
  // CHECK-REFLECTION-NOT: constant [30 x i8] c"disabledAndEnabledEnumElement\00"
  case disabledAndEnabledEnumElement
}

// CHECK-METADATA: @"$s4Test21AlwaysAvailableStructVMf" = internal constant
// CHECK-WITNESS: @"$s4Test21AlwaysAvailableStructVWV" = internal constant
public struct AlwaysAvailableStruct {
  // CHECK-REFLECTION: constant [32 x i8] c"alwaysAvailableStructStoredProp\00"
  // CHECK-DESCRIPTOR-STORED: @"$s4Test21AlwaysAvailableStructV06alwayscD10StoredPropSiSgvpMV"
  public var alwaysAvailableStructStoredProp: Int? = nil

  // CHECK-DESCRIPTOR: @"$s4Test21AlwaysAvailableStructV07enabledD12ComputedPropSivpMV"
  @available(EnabledDomain)
  public var enabledStructComputedProp: Int { 0 }

  // CHECK-DESCRIPTOR-NOT: @"$s4Test21AlwaysAvailableStructV08disabledD12ComputedProp
  @available(DisabledDomain)
  public var disabledStructComputedProp: Int { 0 }

  // CHECK-DESCRIPTOR: @"$s4Test21AlwaysAvailableStructV07dynamicD12ComputedPropSivpMV"
  @available(DynamicDomain)
  public var dynamicStructComputedProp: Int { 0 }
}

// CHECK-METADATA: @"$s4Test13EnabledStructVMf" = internal constant
// CHECK-WITNESS: @"$s4Test13EnabledStructVWV" = internal constant
@available(EnabledDomain)
public struct EnabledStruct {
  // CHECK-REFLECTION: constant [24 x i8] c"enabledStructStoredProp\00"
  // CHECK-DESCRIPTOR-STORED: @"$s4Test13EnabledStructV07enabledC10StoredPropSiSgvpMV"
  public var enabledStructStoredProp: Int? = nil
}

// CHECK-METADATA-NOT: @"$s4Test14DisabledStructV
// CHECK-WITNESS-NOT: @"$s4Test14DisabledStructV
// CHECK-DESCRIPTOR-STORED-NOT: @"$s4Test14DisabledStructV
@available(DisabledDomain)
public struct DisabledStruct {
  // CHECK-REFLECTION-NOT: constant [25 x i8] c"disabledStructStoredProp\00"
  public var disabledStructStoredProp: Int? = nil
}

// CHECK-METADATA: @"$s4Test13DynamicStructVMf" = internal constant
// CHECK-WITNESS: @"$s4Test13DynamicStructVWV" = internal constant
@available(DynamicDomain)
public struct DynamicStruct {
  // CHECK-REFLECTION: constant [24 x i8] c"dynamicStructStoredProp\00"
  // CHECK-DESCRIPTOR-STORED: @"$s4Test13DynamicStructV07dynamicC10StoredPropSiSgvpMV"
  public var dynamicStructStoredProp: Int? = nil
}

// CHECK-METADATA-NOT: @"$s4Test24DisabledAndEnabledStructV
// CHECK-WITNESS-NOT: @"$s4Test24DisabledAndEnabledStructV
// CHECK-DESCRIPTOR-STORED-NOT: @"$s4Test24DisabledAndEnabledStructV
@available(EnabledDomain)
@available(DisabledDomain)
public struct DisabledAndEnabledStruct {
  // CHECK-REFLECTION-NOT: constant [35 x i8] c"disabledAndEnabledStructStoredProp\00"
  public var disabledAndEnabledStructStoredProp: Int? = nil
}

// CHECK-METADATA: @"$s4Test20AlwaysAvailableClassCMf" = internal global
// CHECK-WITNESS: define {{.*}}@"$s4Test20AlwaysAvailableClassCfD"
// CHECK-DESCRIPTOR-STORED: @"$s4Test20AlwaysAvailableClassC06alwayscD10StoredPropSivpMV"
public class AlwaysAvailableClass {
  // CHECK-REFLECTION: constant [31 x i8] c"alwaysAvailableClassStoredProp\00"
  public var alwaysAvailableClassStoredProp: Int = 0

  // CHECK-DESCRIPTOR: @"$s4Test20AlwaysAvailableClassC07enabledD12ComputedPropSivpMV"
  @available(EnabledDomain)
  public var enabledClassComputedProp: Int { 0 }

  // CHECK-DESCRIPTOR-NOT: @"$s4Test20AlwaysAvailableClassC08disabledD12ComputedProp
  @available(DisabledDomain)
  public var disabledClassComputedProp: Int { 0 }

  // CHECK-DESCRIPTOR: @"$s4Test20AlwaysAvailableClassC07dynamicD12ComputedPropSivpMV"
  @available(DynamicDomain)
  public var dynamicClassComputedProp: Int { 0 }

  public init() { }
}

// CHECK-METADATA: @"$s4Test12EnabledClassCMf" = internal global
// CHECK-WITNESS: define {{.*}}@"$s4Test12EnabledClassCfD"
// CHECK-DESCRIPTOR-STORED: @"$s4Test12EnabledClassC07enabledC10StoredPropSivpMV"
@available(EnabledDomain)
public class EnabledClass {
  // CHECK-REFLECTION: constant [23 x i8] c"enabledClassStoredProp\00"
  public var enabledClassStoredProp: Int = 0
  public init() { }
}

// CHECK-METADATA-NOT: @"$s4Test13DisabledClassC
// CHECK-WITNESS-NOT: @"$s4Test13DisabledClassCfD"
// CHECK-DESCRIPTOR-STORED-NOT: @"$s4Test13DisabledClassC
@available(DisabledDomain)
public class DisabledClass {
  // CHECK-REFLECTION-NOT: constant [24 x i8] c"disabledClassStoredProp\00"
  public var disabledClassStoredProp: Int = 0
  public init() { }
}

// CHECK-METADATA: @"$s4Test12DynamicClassCMf" = internal global
// CHECK-WITNESS: define {{.*}}@"$s4Test12DynamicClassCfD"
// CHECK-DESCRIPTOR-STORED: @"$s4Test12DynamicClassC07dynamicC10StoredPropSivpMV"
@available(DynamicDomain)
public class DynamicClass {
  // CHECK-REFLECTION: constant [23 x i8] c"dynamicClassStoredProp\00"
  public var dynamicClassStoredProp: Int = 0
  public init() { }
}

// CHECK-METADATA-NOT: @"$s4Test23DisabledAndEnabledClassC
// CHECK-WITNESS-NOT: @"$s4Test23DisabledAndEnabledClassCfD"
// CHECK-DESCRIPTOR-STORED-NOT: @"$s4Test23DisabledAndEnabledClassC
@available(EnabledDomain)
@available(DisabledDomain)
public class DisabledAndEnabledClass {
  // CHECK-REFLECTION-NOT: constant [34 x i8] c"disabledAndEnabledClassStoredProp\00"
  public var disabledAndEnabledClassStoredProp: Int = 0
  public init() { }
}
