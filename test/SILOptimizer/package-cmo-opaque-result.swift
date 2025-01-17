// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution -Xfrontend -disable-availability-checking

// RUN: %target-build-swift -module-name=Main -package-name Pkg -I%t -emit-sil -O %t/main.swift -o %t/Main-res.sil
// RUN: %FileCheck %s --check-prefixes=CHECK-OPAQUE < %t/Main-res.sil

// REQUIRES: swift_in_compiler

//--- main.swift

import Lib

// CHECK-OPAQUE: sil @$s4Main023testPackageInSerializedC4FuncyyF : $@convention(thin) () -> ()
// CHECK-OPAQUE: struct $Thing
// CHECK-OPAQUE: struct $Thing1
// CHECK-OPAQUE: function_ref @$s3Lib13getSomeProto2QryF
// CHECK-OPAQUE: function_ref @$s3Lib13getSomeProto3QryF
// CHECK-OPAQUE: } // end sil function '$s4Main023testPackageInSerializedC4FuncyyF'

public func testPackageInSerializedPackageFunc() {
    print(getSomeProto())
    print(getSomeProto1())
    print(getSomeProto2())
    print(getSomeProto3())
}

//--- Lib.swift

public protocol SomeProto {}

public struct Thing : SomeProto {}
package struct Thing1 : SomeProto {}
internal struct Thing2 : SomeProto {}
private struct Thing3 : SomeProto {}

// Don't crash on this example.
public func getSomeProto() -> some SomeProto {
    return Thing()
}
public func getSomeProto1() -> some SomeProto {
    return Thing1()
}
public func getSomeProto2() -> some SomeProto {
    return Thing2()
}
public func getSomeProto3() -> some SomeProto {
    return Thing3()
}
