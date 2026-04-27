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

public struct InternalNontrivialStruct {
  public var ref1: InternalClass
  public var ref2: InternalClass

  public init(_ a: Int64, _ b: Int64) {
    self.ref1 = InternalClass(a)
    self.ref2 = InternalClass(b)
  }
}

//--- Intermediate.swift

@_implementationOnly import Internal

public struct PublicStructWrapper {
  private var hidden: InternalNontrivialStruct = InternalNontrivialStruct(42, 43)
  public var visible: Int64 = 2

  public func getHiddenValue1() -> Int64 {
    return hidden.ref1.value
  }

  public func getHiddenValue2() -> Int64 {
    return hidden.ref2.value
  }

  public init() {}
}

public func makePublicStructWrapper() -> PublicStructWrapper {
  return PublicStructWrapper()
}

//--- Client.swift

import Intermediate

func copyAndGetVisible(_ s: PublicStructWrapper) -> Int64 {
  var copy = s
  return copy.visible
}

var s = makePublicStructWrapper()
assert(s.visible == 2)
assert(s.getHiddenValue1() == 42)
assert(s.getHiddenValue2() == 43)
assert(copyAndGetVisible(s) == 2)

// InternalNontrivialStruct contains two class references. The hidden field
// contains two pointers:
// CLIENT-DAG: [[HIDDEN:%T31s8Internal0A16NontrivialStructVXHn]] = type <{ ptr, ptr }>
// CLIENT-DAG: %T12Intermediate19PublicStructWrapperV = type <{ [[HIDDEN]], %Ts5Int64V }>

// Verify calling conventions match:
// INTERMEDIATE: define {{.*}}swiftcc { ptr, ptr, i64 } @"$s12Intermediate23makePublicStructWrapperAA0cdE0VyF"()
// CLIENT: declare {{.*}}swiftcc { ptr, ptr, i64 } @"$s12Intermediate23makePublicStructWrapperAA0cdE0VyF"()

// copyAndGetVisible should retain both hidden fields when copying,
// then call the outlined destructor for the copy:
// CLIENT: define {{.*}}@"$s6Client17copyAndGetVisible{{.*}}"(ptr %0, ptr %1, i64 %2)
// CLIENT:   call ptr @swift_retain(ptr returned %0)
// CLIENT:   call ptr @swift_retain(ptr returned %1)
// CLIENT:   call ptr @"$s12Intermediate19PublicStructWrapperVWOh"(ptr
// CLIENT:   ret i64 %2

// The outlined destructor should GEP into the hidden struct fields and release each:
// CLIENT: define {{.*}}@"$s12Intermediate19PublicStructWrapperVWOh"(ptr %0)
// CLIENT:   %.hidden = getelementptr inbounds {{.*}}%T12Intermediate19PublicStructWrapperV, ptr %0, i32 0, i32 0
// CLIENT:   %.hidden. = getelementptr inbounds {{.*}}[[HIDDEN]], ptr %.hidden, i32 0, i32 0
// CLIENT:   %toDestroy = load ptr, ptr %.hidden.
// CLIENT:   call void @swift_release(ptr %toDestroy)
// CLIENT:   %.hidden.1 = getelementptr inbounds {{.*}}[[HIDDEN]], ptr %.hidden, i32 0, i32 1
// CLIENT:   %toDestroy2 = load ptr, ptr %.hidden.1
// CLIENT:   call void @swift_release(ptr %toDestroy2)
// CLIENT:   ret ptr %0
