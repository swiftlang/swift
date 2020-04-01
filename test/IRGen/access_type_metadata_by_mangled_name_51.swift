// RUN: %target-swift-frontend -target x86_64-apple-macosx10.99 -parse-stdlib -emit-ir %s | %FileCheck %s --check-prefix=CHECK --check-prefix=SWIFT_52
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.15 -parse-stdlib -emit-ir %s | %FileCheck %s --check-prefix=CHECK --check-prefix=SWIFT_PRE_52

protocol P {
  associatedtype A
  var a: A { get }
}

@_silgen_name("useMetadata") func useMetadata<T>(_: T)

struct X: P { var a: X { return self } }

dynamic func foo() -> some P { return X() }

// CHECK-LABEL: define{{.*}}3bar
public func bar() {
  // SWIFT_52: @__swift_instantiateConcreteTypeFromMangledName({{.*}} @"$s39access_type_metadata_by_mangled_name_513fooQryFQOyQo_1AAA1PPQxMD")
  // SWIFT_PRE_52: @"$s39access_type_metadata_by_mangled_name_513fooQryFQOyQo_1AAA1PPQxMa"(
  useMetadata(foo().a)
}
