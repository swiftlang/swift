// RUN: %target-swift-frontend -emit-ir -parse-stdlib %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-endian
// RUN: %target-swift-frontend -emit-ir -disable-concrete-type-metadata-mangled-name-accessors -parse-stdlib %s | %FileCheck %s --check-prefix=DISABLED

// DISABLED-NOT: __swift_instantiateConcreteTypeFromMangledNameV2
// DISABLED-NOT: MD" = {{.*}} global

// CHECK: @"$s36access_type_metadata_by_mangled_name3FooCyAA3BarCyAA3ZimCyAA4ZangCGGGMd" = linkonce_odr hidden global { ptr } zeroinitializer,{{( comdat,)?}} align 8

// CHECK: @"$s36access_type_metadata_by_mangled_name3FooCyAA3BarCyAA3ZimCyAA4ZangCGGGMR" = linkonce_odr hidden constant { i32, i32 }
// CHECK-little-SAME: @"symbolic{{.*}}36access_type_metadata_by_mangled_name3FooC{{.*}}AA3BarC{{.*}}AA3ZimC{{.*}}AA4ZangC{{.*}}"
// CHECK-little-SAME: i32 {{[0-9]+}}

// CHECK-big-SAME:    i32 {{[0-9]+}}
// CHECK-big-SAME:    @"symbolic{{.*}}36access_type_metadata_by_mangled_name3FooC{{.*}}AA3BarC{{.*}}AA3ZimCyAA4ZangC{{.*}}"

// CHECK-SAME: align 8

class Foo<T> {
  class NestedNonGeneric {}
  class NestedGeneric<U> {}
}

class Bar<T> { }

class Zim<T> {}

class Zang {
  class NestedNonGeneric {}
  class NestedGeneric<U> {}
}

extension Zim where T == Zang {
  class ExtensionNonGeneric {}
  class ExtensionGeneric<U> {}
}

precedencegroup AssignmentPrecedence {}

protocol Proto {}

// CHECK-LABEL: define {{.*}} @"$s36access_type_metadata_by_mangled_name4testyXlyF"()
public func test() -> Builtin.AnyObject {
  var x: Builtin.AnyObject

  // CHECK: call ptr @__swift_instantiateConcreteTypeFromMangledNameV2(ptr @"$s36access_type_metadata_by_mangled_name3FooCyAA3BarCyAA3ZimCyAA4ZangCGGGMd", ptr @"$s36access_type_metadata_by_mangled_name3FooCyAA3BarCyAA3ZimCyAA4ZangCGGGMR")
  x = Foo<Bar<Zim<Zang>>>()
  // CHECK: call ptr @__swift_instantiateConcreteTypeFromMangledNameV2(ptr @"$s36access_type_metadata_by_mangled_name3FooC16NestedNonGenericCyAA4ZangC_GMd", ptr @"$s36access_type_metadata_by_mangled_name3FooC16NestedNonGenericCyAA4ZangC_GMR")
  x = Foo<Zang>.NestedNonGeneric()
  // CHECK: call ptr @__swift_instantiateConcreteTypeFromMangledNameV2(ptr @"$s36access_type_metadata_by_mangled_name3FooC13NestedGenericCyAA4ZangC_AGGMd", ptr @"$s36access_type_metadata_by_mangled_name3FooC13NestedGenericCyAA4ZangC_AGGMR")
  x = Foo<Zang>.NestedGeneric<Zang>()
  // CHECK: call ptr @__swift_instantiateConcreteTypeFromMangledNameV2(ptr @"$s36access_type_metadata_by_mangled_name4ZangC13NestedGenericCy_ACGMd", ptr @"$s36access_type_metadata_by_mangled_name4ZangC13NestedGenericCy_ACGMR")
  x = Zang.NestedGeneric<Zang>()
  // CHECK: call ptr @__swift_instantiateConcreteTypeFromMangledNameV2(ptr @"$s36access_type_metadata_by_mangled_name3ZimCA2A4ZangCRszlE16ExtensionGenericCyAE_AEGMd", ptr @"$s36access_type_metadata_by_mangled_name3ZimCA2A4ZangCRszlE16ExtensionGenericCyAE_AEGMR")
  x = Zim<Zang>.ExtensionGeneric<Zang>()

  // Accessing nongeneric nominal type metadata should still go through the
  // accessor, which generally has to exist anyway. Using a mangled name would
  // only add code size.
  // CHECK: call swiftcc %swift.metadata_response @"$s36access_type_metadata_by_mangled_name4ZangCMa"
  x = Zang()
  // CHECK: call swiftcc %swift.metadata_response @"$s36access_type_metadata_by_mangled_name4ZangC16NestedNonGenericCMa"
  x = Zang.NestedNonGeneric()
  // CHECK: call swiftcc %swift.metadata_response @"$s36access_type_metadata_by_mangled_name3ZimCA2A4ZangCRszlE19ExtensionNonGenericCyAE_GMa"
  x = Zim<Zang>.ExtensionNonGeneric()

  // Protocols still have only existential type metadata, so it's better
  // to access them by mangled name.
  // CHECK: call ptr @__swift_instantiateConcreteTypeFromMangledNameV2(ptr @"$s36access_type_metadata_by_mangled_name5Proto_pMd", ptr @"$s36access_type_metadata_by_mangled_name5Proto_pMR")
  var y: Any.Type = Proto.self

  return x
}

