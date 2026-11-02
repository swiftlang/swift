// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The module which defines an array global decides how the array is laid out and named, and its
// clients must follow, even if their deployment target is newer.
//
// The library deploys to Swift 5.10 and lazily initializes its arrays in a container object.
// The client deploys to Swift 6.0, where it would make a read-only static array, but must use the
// library's container.
// RUN: %target-build-swift -target %target-swift-5.10-abi-triple -O -wmo -parse-as-library -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule %t/Lib.swift -c -o %t/Lib.o -Xfrontend -validate-tbd-against-ir=missing
// RUN: %target-build-swift -target %target-swift-6.0-abi-triple -O -wmo -module-name Client -I %t %t/Client.swift %t/main.swift -emit-ir -o %t/Client.ll
// RUN: %FileCheck %s --input-file=%t/Client.ll -DINT=i%target-ptrsize
// RUN: %if executable_test %{ %target-build-swift -target %target-swift-6.0-abi-triple -O -wmo -module-name Client -I %t %t/Client.swift %t/main.swift %t/Lib.o -o %t/a.out %}
// RUN: %if executable_test %{ %target-run %t/a.out | %FileCheck %s --check-prefix=CHECK-OUTPUT %}

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib
// REQUIRES: OS_FAMILY=darwin

//--- Lib.swift
public let table: [UInt8] = [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66]

//--- main.swift
import Lib

// CHECK-OUTPUT:      0123456789abcdef
print(String(decoding: (0..<16).map(lookupTable), as: UTF8.self))
// CHECK-OUTPUT-NEXT: 0123456789abcdef
print(String(decoding: getTable(), as: UTF8.self))
// CHECK-OUTPUT-NEXT: 0123456789abcdef
print(String(decoding: alias, as: UTF8.self))

//--- Client.swift
import Lib

// The container can't be statically initialized as a part of the client's global.
// CHECK: @"$s6Client5aliasSays5UInt8VGvp" = {{.*}}global {{.*}}zeroinitializer
public let alias: [UInt8] = table

// The symbol has no `r` suffix.
// CHECK-LABEL: define {{.*}} @"$s6Client11lookupTableys5UInt8VSiF"(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[I:%.*]] = and [[INT]] %0, 15
// CHECK-NEXT:    [[P:%.*]] = getelementptr inbounds nuw i8, ptr getelementptr inbounds nuw (i8, ptr @"$s3Lib5table_WZTv_", [[INT]] {{[0-9]+}}), [[INT]] [[I]]
// CHECK-NEXT:    [[V:%.*]] = load i8, ptr [[P]], align 1
// CHECK-NEXT:    ret i8 [[V]]
// CHECK-NEXT:  }
public func lookupTable(_ i: Int) -> UInt8 {
  table[i & 15]
}

// The container initializes the object's header at runtime.
// CHECK-LABEL: define {{.*}} @"$s6Client8getTableSays5UInt8VGyF"(
// CHECK:         call ptr @swift_initStaticObject({{.*}}@"$s3Lib5table_WZTv_"
// CHECK-NEXT:    ret ptr
public func getTable() -> [UInt8] {
  table
}
