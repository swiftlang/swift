// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s --check-prefix=CHECK --check-prefix=ONONE

@_silgen_name("useMetadata")
func useMetadata<T>(_: T.Type)

// TODO: Although this is not explicitly final, class hierarchy analysis
// should figure out that it's effectively final because it has no
// subclasses.
private class PrivateEffectivelyFinal<T, U, V> {
  final func butts() {
    useMetadata(PrivateEffectivelyFinal<T, U, V>.self)
    useMetadata(PrivateEffectivelyFinal<Int, String, V>.self)
  }
}

// The class is not final and has subclasses, so we can only peephole
// metadata requests in limited circumstances.
private class PrivateNonfinal<T, U, V> {
  // TODO: The designated init allocating entry point is always overridden
  // by subclasses, so it can use the self metadata it was passed.

  // Methods in general on nonfinal classes cannot use the self metadata as
  // is.
  // CHECK-LABEL: define {{.*}}PrivateNonfinal{{.*}}butts
  @inline(never)
  final func butts() {
    // CHECK: [[INSTANTIATED_TYPE_RESPONSE:%.*]] = call {{.*}} @{{.*}}PrivateNonfinal{{.*}}Ma
    // CHECK: [[INSTANTIATED_TYPE:%.*]] = extractvalue {{.*}} [[INSTANTIATED_TYPE_RESPONSE]]
    // CHECK: call {{.*}} @useMetadata(%swift.type* [[INSTANTIATED_TYPE]], %swift.type* [[INSTANTIATED_TYPE]])
    useMetadata(PrivateNonfinal<T, U, V>.self)
    // CHECK: [[INSTANTIATED_TYPE_RESPONSE:%.*]] = call {{.*}} @{{.*}}PrivateNonfinal{{.*}}Ma
    // CHECK: [[INSTANTIATED_TYPE:%.*]] = extractvalue {{.*}} [[INSTANTIATED_TYPE_RESPONSE]]
    // CHECK: call {{.*}} @useMetadata(%swift.type* [[INSTANTIATED_TYPE]], %swift.type* [[INSTANTIATED_TYPE]])
    useMetadata(PrivateNonfinal<Int, String, V>.self)
  }
}

// TODO: Although this is not explicitly final, class hierarchy analysis
// should figure out that it's effectively final because it has no
// subclasses.
private class PrivateNonfinalSubclass: PrivateNonfinal<Int, String, Float> {
  @inline(never)
  final func borts() {
    useMetadata(PrivateNonfinalSubclass.self)
  }
}

final private class FinalPrivateNonfinalSubclass<U>: PrivateNonfinal<U, String, Float> {
  // The class is final, so we can always form metadata for
  // FinalPrivateNonfinalSubclass<U> from the self argument.

  // CHECK-LABEL: define {{.*}}FinalPrivateNonfinalSubclass{{.*}}burts
  @inline(never)
  final func burts() {
    // CHECK: [[TYPE:%.*]] = call {{.*}} @swift_getObjectType
    // CHECK: call {{.*}} @useMetadata(%swift.type* [[TYPE]], %swift.type* [[TYPE]])
    useMetadata(FinalPrivateNonfinalSubclass<U>.self)
    // CHECK: [[INSTANTIATED_TYPE:%.*]] = call {{.*}} @__swift_instantiateConcreteTypeFromMangledName({{.*}}FinalPrivateNonfinalSubclass
    // CHECK: call {{.*}} @useMetadata(%swift.type* [[INSTANTIATED_TYPE]], %swift.type* [[INSTANTIATED_TYPE]])
    useMetadata(FinalPrivateNonfinalSubclass<Int>.self)
  }

  // CHECK-LABEL: define {{.*}}FinalPrivateNonfinalSubclass{{.*}}cfC"
  // CHECK:         call {{.*}}@swift_allocObject(%swift.type* %0
}

final private class PrivateFinal<T, U, V> {
  // The class is final, so we can always form metadata for
  // PrivateFinal<T, U, V> from the self argument.

  // CHECK-LABEL: define {{.*}}PrivateFinal{{.*}}butts
  func butts() {
    // CHECK: [[TYPE:%.*]] = call {{.*}} @swift_getObjectType
    // CHECK: call {{.*}} @useMetadata(%swift.type* [[TYPE]], %swift.type* [[TYPE]])
    useMetadata(PrivateFinal<T, U, V>.self)
    // CHECK: [[INSTANTIATED_TYPE:%.*]] = call {{.*}} @__swift_instantiateConcreteTypeFromMangledName({{.*}}PrivateFinal
    // CHECK: call {{.*}} @useMetadata(%swift.type* [[INSTANTIATED_TYPE]], %swift.type* [[INSTANTIATED_TYPE]])
    useMetadata(PrivateFinal<Int, String, Float>.self)
  }

  // CHECK-LABEL: define {{.*}}PrivateFinal{{.*}}cfC"
  // CHECK:         call {{.*}}@swift_allocObject(%swift.type* %0
}

public func useStuff<T, U, V>(_: T, _: U, _: V) {
  PrivateEffectivelyFinal<T, U, V>().butts()
  PrivateNonfinal<T, U, V>().butts()
  PrivateNonfinalSubclass().borts()
  FinalPrivateNonfinalSubclass<U>().burts()
  PrivateFinal<T, U, V>().butts()
}

