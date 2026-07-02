// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: mkdir -p %t/InternalModule
// RUN: mkdir -p %t/IntermediateModule

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -parse-as-library -emit-object -o %t/InternalModule/Internal.o
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/IntermediateModule/Intermediate.swiftmodule %t/Intermediate.swift -I %t/InternalModule -parse-as-library -enable-experimental-feature SafeImplementationOnly -emit-object -o %t/IntermediateModule/Intermediate.o
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Intermediate.swift -I %t/InternalModule -o %t/Intermediate.ll
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-ir %t/Client.swift -I %t/IntermediateModule -o %t/Client.ll
// RUN: %FileCheck %s --check-prefix=INTERMEDIATE < %t/Intermediate.ll
// RUN: %FileCheck %s --check-prefix=CLIENT < %t/Client.ll
// RUN: %target-build-swift -Xfrontend -enable-experimental-feature -Xfrontend SafeImplementationOnly %t/Client.swift -I %t/IntermediateModule  %t/InternalModule/Internal.o %t/IntermediateModule/Intermediate.o -o %t/client-executable
// RUN: %target-run %t/client-executable

// UNSUPPORTED: CPU=wasm32
// REQUIRES: executable_test
// REQUIRES: swift_feature_SafeImplementationOnly

//--- Internal.swift

public class InternalClass {
  public var value: Int64

  public init(_ value: Int64) {
    self.value = value
  }
}

// A struct with 5 class reference fields, too large to pass in registers
// making it address-only. Non-trivial because copying requires retaining and destroying requires
// releasing each class reference.
public struct LargeNontrivialInternalStruct {
  public var a: InternalClass
  public var b: InternalClass
  public var c: InternalClass
  public var d: InternalClass
  public var e: InternalClass

  public init() {
    self.a = InternalClass(1)
    self.b = InternalClass(2)
    self.c = InternalClass(3)
    self.d = InternalClass(4)
    self.e = InternalClass(5)
  }
}

//--- Intermediate.swift

@_implementationOnly import Internal

public struct PublicWrapper {
  private var hidden: LargeNontrivialInternalStruct = LargeNontrivialInternalStruct()
  public var visible: Int64 = 42

  public func getHiddenA() -> Int64 { return hidden.a.value }
  public func getHiddenE() -> Int64 { return hidden.e.value }

  public init() {}
}

public func makePublicWrapper() -> PublicWrapper {
  return PublicWrapper()
}

//--- Client.swift

import Intermediate

func copyAndGetVisible(_ s: PublicWrapper) -> Int64 {
  var copy = s
  return copy.visible
}

var s = makePublicWrapper()
assert(s.visible == 42)
assert(s.getHiddenA() == 1)
assert(s.getHiddenE() == 5)
assert(copyAndGetVisible(s) == 42)

// The struct is too large for registers, so makePublicWrapper returns via sret.
// INTERMEDIATE: define {{.*}}swiftcc void @"$s12Intermediate17makePublicWrapperAA0cD0VyF"(ptr noalias sret
// CLIENT: declare {{.*}}swiftcc void @"$s12Intermediate17makePublicWrapperAA0cD0VyF"(ptr noalias sret

// copyAndGetVisible calls the outlined copy (WOc) to retain the hidden
// class references, then calls the outlined destroy (WOh) for the local copy:
// CLIENT: define {{.*}}@"$s6Client17copyAndGetVisible{{.*}}"(ptr noalias {{.*}} %0)
// CLIENT:   call ptr @"$s12Intermediate13PublicWrapperVWOc"(ptr %0, ptr
// CLIENT:   call ptr @"$s12Intermediate13PublicWrapperVWOh"(ptr
// CLIENT:   ret i64

// The outlined copy function retains all 5 hidden class reference fields:
// CLIENT-LABEL: define linkonce_odr hidden ptr @"$s12Intermediate13PublicWrapperVWOc"
// CLIENT:   call ptr @swift_retain
// CLIENT:   call ptr @swift_retain
// CLIENT:   call ptr @swift_retain
// CLIENT:   call ptr @swift_retain
// CLIENT:   call ptr @swift_retain
// CLIENT:   ret ptr

// The outlined destroy function releases all 5 hidden class reference fields:
// CLIENT-LABEL: define linkonce_odr hidden ptr @"$s12Intermediate13PublicWrapperVWOh"
// CLIENT:   call void @swift_release
// CLIENT:   call void @swift_release
// CLIENT:   call void @swift_release
// CLIENT:   call void @swift_release
// CLIENT:   call void @swift_release
// CLIENT:   ret ptr
