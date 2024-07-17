// RUN: %target-swift-frontend -emit-silgen -import-objc-header %S/Inputs/objc_implementation.h -swift-version 5 %s -target %target-stable-abi-triple | %FileCheck %s

// REQUIRES: objc_interop

@_objcImplementation extension ImplClass {
  public init(int: Int) {
    super.init()
  }

  // CHECK-LABEL: sil{{.*}} @$sSo9ImplClassC19objc_implementationEABycfc : $@convention(method) (@owned ImplClass) -> @owned ImplClass {
  // CHECK:         function_ref @$ss25_unimplementedInitializer9className04initD04file4line6columns5NeverOs12StaticStringV_A2JS2utF
  // CHECK:       } // end sil function '$sSo9ImplClassC19objc_implementationEABycfc'
}
