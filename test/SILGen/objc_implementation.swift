// RUN: %target-swift-frontend -emit-silgen -import-objc-header %S/Inputs/objc_implementation.h -swift-version 5 %s -target %target-stable-abi-triple > %t
// RUN: %FileCheck --input-file %t %s
// RUN: %FileCheck --input-file %t --check-prefix NEGATIVE %s

// REQUIRES: objc_interop

@_objcImplementation extension ImplClass {
  public init(int: Int) {
    super.init()
  }

  // CHECK-LABEL: sil{{.*}} @$sSo9ImplClassC19objc_implementationEABycfc : $@convention(method) (@owned ImplClass) -> @owned ImplClass {
  // CHECK:         function_ref @$ss25_unimplementedInitializer9className04initD04file4line6columns5NeverOs12StaticStringV_A2JS2utF
  // CHECK:       } // end sil function '$sSo9ImplClassC19objc_implementationEABycfc'
}

//
// objcImpl class with an initial value expression referencing a nonpublic
// function (rdar://114874429)
//

internal func internalFunc() -> Int { 42 }

@objc @implementation extension Rdar114874429 {
  let prop: Int = internalFunc()

  // CHECK-LABEL : sil{{.*}}@$sSo13Rdar114874429C19objc_implementationE4propSivpfi :
  // NEGATIVE-NOT: sil{{.*}} [serialized] {{.*}}@$sSo13Rdar114874429C19objc_implementationE4propSivpfi :
}
