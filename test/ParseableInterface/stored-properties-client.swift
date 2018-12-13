// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %S/stored-properties.swift -module-name StoredProperties -emit-parseable-module-interface-path %t/StoredProperties.swiftinterface
// RUN: %target-swift-frontend -emit-ir %s -I %t -enable-parseable-module-interface | %FileCheck %s -check-prefix CHECK -check-prefix COMMON

// RUN: %target-swift-frontend -typecheck %S/stored-properties.swift -enable-resilience -module-name StoredProperties -emit-parseable-module-interface-path %t/StoredProperties.swiftinterface
// RUN: %target-swift-frontend -emit-ir %s -I %t -enable-parseable-module-interface | %FileCheck %s -check-prefix RESILIENT -check-prefix COMMON

import StoredProperties

/// This test makes sure clients of a parseable interface see correct type
/// layout and use the right access patterns in the presence of a
/// .swiftinterface file, in both resilient and non-resilient cases.

// COMMON: %[[BAGOFVARIABLES:T16StoredProperties14BagOfVariablesV]] = type <{ %TSi, %TSb, [{{(3|7)}} x i8], %TSi }>

// This type is non-@_fixed_layout, so it becomes opaque in a resilient module
// CHECK: %[[HASSTOREDPROPERTIES:T16StoredProperties03HasaB0V]] = type <{ %TSi, %TSi, %TSb, [{{(3|7)}} x i8], %TSi, %TSb }>
// RESILIENT: %[[HASSTOREDPROPERTIES:swift.opaque]] = type opaque

// COMMON: %[[HASSTOREDPROPERTIESFIXEDLAYOUT:T16StoredProperties03HasaB11FixedLayoutV]] = type <{ %[[BAGOFVARIABLES]], %[[BAGOFVARIABLES]] }>

// These are here just to make sure the compiler doesn't optimize away accesses.
// They're overloaded instead of generic so we avoid generic dispatch.

@inline(never)
func _blackHole(_ value: Int) {}

@inline(never)
func _blackHole(_ value: Bool) {}

@inline(never)
func _blackHole(_ value: BagOfVariables) {}

/// In the following code, getting and setting is split because otherwise
/// the resulting IR is ordered differently.

/// Test that we call the correct accessors in a resilient module, and that
/// we use trivial storage accesses in a non-resilient module.

func testGetting() {
  // CHECK: %[[VALUE:.*]] = alloca %[[HASSTOREDPROPERTIES]]
  // CHECK: call swiftcc void @"$s16StoredProperties03HasaB0VACycfC"(%[[HASSTOREDPROPERTIES]]* {{.*}} %[[VALUE]])
  // RESILIENT: %[[VALUE:.*]] = alloca i8, [[WORD:i[0-9]+]] %size
  // RESILIENT: %[[VALUECAST:.*]] = bitcast i8* %value to %[[HASSTOREDPROPERTIES]]*
  // RESILIENT: call swiftcc void @"$s16StoredProperties03HasaB0VACycfC"(%[[HASSTOREDPROPERTIES]]* noalias nocapture sret %[[VALUECAST]])
  var value = HasStoredProperties()

  // CHECK: getelementptr inbounds %[[HASSTOREDPROPERTIES]], %[[HASSTOREDPROPERTIES]]* %[[VALUE]], i32 0, i32 1
  // CHECK: getelementptr inbounds %[[HASSTOREDPROPERTIES]], %[[HASSTOREDPROPERTIES]]* %[[VALUE]], i32 0, i32 2

  // These are accessed in declaration order so the IR is emitted in the same
  // order.

  // COMMON: call swiftcc [[WORD:i[0-9]+]] @"$s16StoredProperties03HasaB0V14computedGetterSivg"
  _blackHole(value.computedGetter)

  // COMMON: call swiftcc [[WORD]] @"$s16StoredProperties03HasaB0V14computedGetSetSivg"
  _blackHole(value.computedGetSet)

  // CHECK: getelementptr inbounds %[[HASSTOREDPROPERTIES]], %[[HASSTOREDPROPERTIES]]* %[[VALUE]], i32 0, i32 0
  // CHECK: getelementptr inbounds %[[HASSTOREDPROPERTIES]], %[[HASSTOREDPROPERTIES]]* %[[VALUE]], i32 0, i32 4

  // RESILIENT: call swiftcc [[WORD]] @"$s16StoredProperties03HasaB0V06simpleA9ImmutableSivg"
  _blackHole(value.simpleStoredImmutable)

  // RESILIENT: call swiftcc [[WORD]] @"$s16StoredProperties03HasaB0V06simpleA7MutableSivg"
  _blackHole(value.simpleStoredMutable)

  // RESILIENT: call swiftcc i1 @"$s16StoredProperties03HasaB0V19storedWithObserversSbvg"
  _blackHole(value.storedWithObservers)

  // RESILIENT: call swiftcc [[WORD]] @"$s16StoredProperties03HasaB0V16storedPrivateSetSivg"
  _blackHole(value.storedPrivateSet)
}

func testSetting() {
  // CHECK: call swiftcc void @"$s16StoredProperties03HasaB0VACycfC"(%[[HASSTOREDPROPERTIES]]* {{.*}})
  // RESILIENT: call swiftcc %swift.metadata_response @"$s16StoredProperties03HasaB0VMa"([[WORD]] 0)
  // RESILIENT: %[[VALUEALLOC:.*]] = alloca i8, [[WORD]] %size
  // RESILIENT: bitcast i8* %[[VALUEALLOC]] to %[[HASSTOREDPROPERTIES]]*
  var value = HasStoredProperties()

  // COMMON: call swiftcc void @"$s16StoredProperties03HasaB0V19storedWithObserversSbvs"(i1 false, %[[HASSTOREDPROPERTIES]]* {{.*}})
  value.storedWithObservers = false

  // COMMON-NEXT: call swiftcc void @"$s16StoredProperties03HasaB0V14computedGetSetSivs"([[WORD]] 4, %[[HASSTOREDPROPERTIES]]* {{.*}})
  value.computedGetSet = 4

  // CHECK: %[[MUTABLE_PTR:.*]] = getelementptr inbounds %[[HASSTOREDPROPERTIES]], %[[HASSTOREDPROPERTIES]]* {{.*}}, i32 0, i32 1
  // CHECK: %[[MUTABLE_INT_PTR:.*]] = getelementptr inbounds %TSi, %TSi* %[[MUTABLE_PTR]], i32 0, i32 0
  // CHECK: store [[WORD]] 4, [[WORD]]* %[[MUTABLE_INT_PTR]]
  // RESILIENT: call swiftcc void @"$s16StoredProperties03HasaB0V06simpleA7MutableSivs"([[WORD]] 4, %[[HASSTOREDPROPERTIES]]* {{.*}})
  value.simpleStoredMutable = 4
}

testGetting()
testSetting()

/// Test that we always use trivial access patterns for @_fixed_layout types
/// in resilient or non-resilient modules.

func testFixedLayoutGetting() {
  // COMMON: %[[VALUEALLOCA:.*]] = alloca %[[HASSTOREDPROPERTIESFIXEDLAYOUT]]
  // COMMON: call swiftcc void @"$s16StoredProperties03HasaB11FixedLayoutVACycfC"(%[[HASSTOREDPROPERTIESFIXEDLAYOUT]]* {{.*}} %[[VALUEALLOCA]])
  var value = HasStoredPropertiesFixedLayout()

  // These next two tests just make sure we don't use resilient access patterns
  // for these fixed_layout structs.

  // COMMON: getelementptr inbounds %[[HASSTOREDPROPERTIESFIXEDLAYOUT]], %[[HASSTOREDPROPERTIESFIXEDLAYOUT]]* %[[VALUEALLOCA]], i32 0, i32 0
  // COMMON: getelementptr inbounds %[[HASSTOREDPROPERTIESFIXEDLAYOUT]], %[[HASSTOREDPROPERTIESFIXEDLAYOUT]]* %[[VALUEALLOCA]], i32 0, i32 1

  // COMMON: call swiftcc void @"$s4main10_blackHoleyy16StoredProperties14BagOfVariablesVF"([[WORD]] %{{.*}}, i1 %{{.*}}, [[WORD]] %{{.*}})
  _blackHole(value.simpleStoredMutable)

  // COMMON: call swiftcc void @"$s4main10_blackHoleyy16StoredProperties14BagOfVariablesVF"([[WORD]] %{{.*}}, i1 %{{.*}}, [[WORD]] %{{.*}})
  _blackHole(value.storedWithObservers)
}

func testFixedLayoutSetting() {
  // COMMON: %[[VALUEALLOCA:.*]] = alloca %[[HASSTOREDPROPERTIESFIXEDLAYOUT]]
  // COMMON: call swiftcc void @"$s16StoredProperties03HasaB11FixedLayoutVACycfC"(%[[HASSTOREDPROPERTIESFIXEDLAYOUT]]* {{.*}} %[[VALUEALLOCA]])
  var value = HasStoredPropertiesFixedLayout()

  // COMMON: call swiftcc void @"$s16StoredProperties03HasaB11FixedLayoutV19storedWithObserversAA14BagOfVariablesVvs"
  value.storedWithObservers = BagOfVariables()

  // COMMON: %[[PROP:.*]] = getelementptr inbounds %[[HASSTOREDPROPERTIESFIXEDLAYOUT]], %[[HASSTOREDPROPERTIESFIXEDLAYOUT]]* %value, i32 0, i32 0
  // COMMON: %[[PROPA:.*]] = getelementptr inbounds %[[BAGOFVARIABLES]], %[[BAGOFVARIABLES]]* %[[PROP]], i32 0, i32 0
  // COMMON: %[[PROPAVAL:.*]] = getelementptr inbounds %TSi, %TSi* %[[PROPA]], i32 0, i32 0
  // COMMON: store [[WORD]] {{.*}} %[[PROPAVAL]]
  // COMMON: %[[PROPB:.*]] = getelementptr inbounds %[[BAGOFVARIABLES]], %[[BAGOFVARIABLES]]* %[[PROP]], i32 0, i32 1
  // COMMON: %[[PROPBVAL:.*]] = getelementptr inbounds %TSb, %TSb* %[[PROPB]], i32 0, i32 0
  // COMMON: store i1 {{.*}} %[[PROPBVAL]]
  // COMMON: %[[PROPC:.*]] = getelementptr inbounds %[[BAGOFVARIABLES]], %[[BAGOFVARIABLES]]* %[[PROP]], i32 0, i32 3
  // COMMON: %[[PROPCVAL:.*]] = getelementptr inbounds %TSi, %TSi* %[[PROPC]], i32 0, i32 0
  // COMMON: store [[WORD]] {{.*}} %[[PROPCVAL]]
  value.simpleStoredMutable = BagOfVariables()
}

testFixedLayoutGetting()
testFixedLayoutSetting()
