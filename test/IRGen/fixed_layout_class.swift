// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/class_resilience.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t %S/../Inputs/resilient_enum.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_class.swiftmodule -module-name=resilient_class -I %t %S/../Inputs/resilient_class.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/fixed_layout_class.swiftmodule -module-name=fixed_layout_class -I %t %S/../Inputs/fixed_layout_class.swift
// RUN: %target-swift-frontend %use_no_opaque_pointers -I %t -emit-ir -enable-library-evolution %t/class_resilience.swift | %FileCheck %t/class_resilience.swift --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK-%target-runtime -DINT=i%target-ptrsize
// RUN: %target-swift-frontend -I %t -emit-ir -enable-library-evolution %t/class_resilience.swift
// RUN: %target-swift-frontend -I %t -emit-ir -enable-library-evolution -O %t/class_resilience.swift

// This tests @_fixed_layout classes in resilient modules.
import fixed_layout_class

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience20useRootClassPropertyyy013fixed_layout_A0026OutsideParentWithResilientF0CF"(%T18fixed_layout_class34OutsideParentWithResilientPropertyC* %0)
public func useRootClassProperty(_ o: OutsideParentWithResilientProperty) {
  // CHECK: getelementptr inbounds %T18fixed_layout_class34OutsideParentWithResilientPropertyC, %T18fixed_layout_class34OutsideParentWithResilientPropertyC* %0, i32 0, i32 1
  let a = o.p
  // CHECK: load [[INT]], [[INT]]* @"$s18fixed_layout_class34OutsideParentWithResilientPropertyC1s16resilient_struct4SizeVvpWvd"
  let b = o.s
  // CHECK: load [[INT]], [[INT]]* @"$s18fixed_layout_class34OutsideParentWithResilientPropertyC5colors5Int32VvpWvd"
  let c = o.color
  // CHECK: ret void
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience19useSubclassPropertyyy013fixed_layout_A012OutsideChildCF"(%T18fixed_layout_class12OutsideChildC* %0)
public func useSubclassProperty(_ o: OutsideChild) {
  // CHECK: getelementptr inbounds %T18fixed_layout_class13OutsideParentC, %T18fixed_layout_class13OutsideParentC* {{%[0-9]+}}, i32 0, i32 1
  let a = o.property
  // CHECK: getelementptr inbounds %T18fixed_layout_class12OutsideChildC, %T18fixed_layout_class12OutsideChildC* %0, i32 0, i32 2
  let b = o.childProperty
  // CHECK: ret void
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience27useGenericRootClassPropertyyy013fixed_layout_A00D13OutsideParentCyxGlF"(%T18fixed_layout_class20GenericOutsideParentC* %0)
public func useGenericRootClassProperty<A>(_ o: GenericOutsideParent<A>) {
  // -- we load the base offset twice, first to get the generic parameter out and
  // then for the property itself.

  // CHECK: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ({ [[INT]], i32, i32 }, { [[INT]], i32, i32 }* @"$s18fixed_layout_class20GenericOutsideParentCMo", i32 0, i32 0)

  // CHECK: [[METADATA_ADDR:%.*]] = bitcast %T18fixed_layout_class20GenericOutsideParentC* %0 to %swift.type**
  // CHECK: [[METADATA:%.*]] = load %swift.type*, %swift.type** [[METADATA_ADDR]]

  // CHECK: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ({ [[INT]], i32, i32 }, { [[INT]], i32, i32 }* @"$s18fixed_layout_class20GenericOutsideParentCMo", i32 0, i32 0)
  // CHECK: [[FIELD_OFFSET_OFFSET:%.*]] = add [[INT]] [[BASE]], {{4|8}}

  // CHECK: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8*
  // CHECK: [[FIELD_OFFSET_ADDR:%.*]] = getelementptr inbounds i8, i8* [[METADATA_ADDR]], [[INT]] [[FIELD_OFFSET_OFFSET]]
  // CHECK: [[FIELD_OFFSET_PTR:%.*]] = bitcast i8* [[FIELD_OFFSET_ADDR]] to [[INT]]*
  // CHECK: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_PTR]]
  let a = o.property

  // CHECK: ret void
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience27useGenericRootClassPropertyyy013fixed_layout_A00D13OutsideParentCySiGF"(%T18fixed_layout_class20GenericOutsideParentCySiG* %0)
public func useGenericRootClassProperty(_ o: GenericOutsideParent<Int>) {
  // CHECK: getelementptr inbounds %T18fixed_layout_class20GenericOutsideParentCySiG, %T18fixed_layout_class20GenericOutsideParentCySiG* %0, i32 0, i32 1
  let a = o.property

  // CHECK: ret void
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience26useGenericSubclassPropertyyy013fixed_layout_A00D12OutsideChildCyxGlF"(%T18fixed_layout_class19GenericOutsideChildC* %0)
public func useGenericSubclassProperty<A>(_ o: GenericOutsideChild<A>) {
  // -- we load the base offset twice, first to get the generic parameter out and
  // then for the property itself.

  // CHECK: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ({ [[INT]], i32, i32 }, { [[INT]], i32, i32 }* @"$s18fixed_layout_class19GenericOutsideChildCMo", i32 0, i32 0)

  // CHECK: [[UPCAST:%.*]] = bitcast %T18fixed_layout_class19GenericOutsideChildC* %0 to %T18fixed_layout_class20GenericOutsideParentC*
  // CHECK: [[METADATA_ADDR:%.*]] = bitcast %T18fixed_layout_class20GenericOutsideParentC* [[UPCAST]] to %swift.type**
  // CHECK: [[METADATA:%.*]] = load %swift.type*, %swift.type** [[METADATA_ADDR]]

  // CHECK: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ({ [[INT]], i32, i32 }, { [[INT]], i32, i32 }* @"$s18fixed_layout_class20GenericOutsideParentCMo", i32 0, i32 0)
  // CHECK: [[FIELD_OFFSET_OFFSET:%.*]] = add [[INT]] [[BASE]], {{4|8}}

  // CHECK: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8*
  // CHECK: [[FIELD_OFFSET_ADDR:%.*]] = getelementptr inbounds i8, i8* [[METADATA_ADDR]], [[INT]] [[FIELD_OFFSET_OFFSET]]
  // CHECK: [[FIELD_OFFSET_PTR:%.*]] = bitcast i8* [[FIELD_OFFSET_ADDR]] to [[INT]]*
  // CHECK: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_PTR]]
  let a = o.property

  // CHECK: [[METADATA_ADDR:%.*]] = bitcast %T18fixed_layout_class19GenericOutsideChildC* %0 to %swift.type**
  // CHECK: [[METADATA:%.*]] = load %swift.type*, %swift.type** [[METADATA_ADDR]]

  // CHECK: [[BASE:%.*]] = load [[INT]], [[INT]]* getelementptr inbounds ({ [[INT]], i32, i32 }, { [[INT]], i32, i32 }* @"$s18fixed_layout_class19GenericOutsideChildCMo", i32 0, i32 0)
  // CHECK: [[FIELD_OFFSET_OFFSET:%.*]] = add [[INT]] [[BASE]], {{4|8}}

  // CHECK: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8*
  // CHECK: [[FIELD_OFFSET_ADDR:%.*]] = getelementptr inbounds i8, i8* [[METADATA_ADDR]], [[INT]] [[FIELD_OFFSET_OFFSET]]
  // CHECK: [[FIELD_OFFSET_PTR:%.*]] = bitcast i8* [[FIELD_OFFSET_ADDR]] to [[INT]]*
  // CHECK: [[FIELD_OFFSET:%.*]] = load [[INT]], [[INT]]* [[FIELD_OFFSET_PTR]]
  let b = o.childProperty

  // CHECK: ret void
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience26useGenericSubclassPropertyyy013fixed_layout_A00D12OutsideChildCySiGF"(%T18fixed_layout_class19GenericOutsideChildCySiG* %0)
public func useGenericSubclassProperty(_ o: GenericOutsideChild<Int>) {
  // CHECK: [[UPCAST:%.*]] = bitcast %T18fixed_layout_class19GenericOutsideChildCySiG* %0 to %T18fixed_layout_class20GenericOutsideParentCySiG*
  // CHECK: getelementptr inbounds %T18fixed_layout_class20GenericOutsideParentCySiG, %T18fixed_layout_class20GenericOutsideParentCySiG* [[UPCAST]], i32 0, i32 1
  let a = o.property

  // CHECK: getelementptr inbounds %T18fixed_layout_class19GenericOutsideChildCySiG, %T18fixed_layout_class19GenericOutsideChildCySiG* %0, i32 0, i32 2
  let b = o.childProperty

  // CHECK: ret void
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience17callVirtualMethodyy013fixed_layout_A013OutsideParentCF"(%T18fixed_layout_class13OutsideParentC* %0)
public func callVirtualMethod(_ o: OutsideParent) {
  // Note: virtual method calls still use dispatch thunks

  // CHECK: call swiftcc void @"$s18fixed_layout_class13OutsideParentC6methodyyFTj"
  let a = o.method()

  // CHECK: ret void
}

@_fixed_layout open class MyChildOfOutsideParent : OutsideParent {
  public func newMethod() {}
}

// Make sure we emit the dispatch thunk:

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s16class_resilience22MyChildOfOutsideParentC9newMethodyyFTj"(%T16class_resilience22MyChildOfOutsideParentC* swiftself %0)
