// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %empty-directory(%t/swift5)
// RUN: %empty-directory(%t/swift6)
// RUN: %empty-directory(%t/swift5-tbd)
// RUN: %empty-directory(%t/swift6-tbd)

// Each library validates its TBD explicitly, because a no-asserts compiler doesn't by default.
// That checks the symbols which cross-module optimization makes public against the TBD.

// Deploying to Swift 5.10, the arrays are lazily initialized. Deploying to Swift 6.0, the arrays are
// statically initialized read-only objects on Darwin and still lazily initialized elsewhere.
// RUN: %target-build-swift -target %target-swift-5.10-abi-triple -O -wmo -parse-as-library -module-name Lib -emit-module -emit-module-path %t/swift5/Lib.swiftmodule %t/Lib.swift -c -o %t/swift5/Lib.o -Xfrontend -validate-tbd-against-ir=missing
// RUN: %target-build-swift -target %target-swift-5.10-abi-triple -O -wmo -module-name Client -I %t/swift5 %t/Client.swift %t/main.swift -emit-ir -o %t/swift5/Client.ll
// RUN: %FileCheck %s --input-file=%t/swift5/Client.ll -DINT=i%target-ptrsize --check-prefixes=CHECK,CHECK-LAZY
// RUN: %target-build-swift -target %target-swift-6.0-abi-triple -O -wmo -parse-as-library -module-name Lib -emit-module -emit-module-path %t/swift6/Lib.swiftmodule %t/Lib.swift -c -o %t/swift6/Lib.o -Xfrontend -validate-tbd-against-ir=missing
// RUN: %target-build-swift -target %target-swift-6.0-abi-triple -O -wmo -module-name Client -I %t/swift6 %t/Client.swift %t/main.swift -emit-ir -o %t/swift6/Client.ll
// RUN: %FileCheck %s --input-file=%t/swift6/Client.ll -DINT=i%target-ptrsize %if OS_FAMILY=darwin %{ --check-prefixes=CHECK,CHECK-READONLY %} %else %{ --check-prefixes=CHECK,CHECK-LAZY %}

// With a TBD file, cross-module optimization can't introduce public symbols. So the library must not
// export the outlined object, the token and the initializer, and the client can't fold the lookup.
// RUN: %target-build-swift -target %target-swift-5.10-abi-triple -O -wmo -parse-as-library -module-name Lib -emit-module -emit-module-path %t/swift5-tbd/Lib.swiftmodule %t/Lib.swift -emit-tbd -emit-tbd-path %t/swift5-tbd/Lib.tbd -c -o %t/swift5-tbd/Lib.o -Xfrontend -tbd-install_name -Xfrontend Lib -Xfrontend -validate-tbd-against-ir=missing
// RUN: %llvm-nm --defined-only --extern-only --format=just-symbols %t/swift5-tbd/Lib.o | %FileCheck %s --check-prefix=CHECK-TBD-LIB --implicit-check-not=table_W
// RUN: %target-build-swift -target %target-swift-5.10-abi-triple -O -wmo -module-name Client -I %t/swift5-tbd %t/Client.swift %t/main.swift -emit-ir -o %t/swift5-tbd/Client.ll
// RUN: %FileCheck %s --input-file=%t/swift5-tbd/Client.ll -DINT=i%target-ptrsize --check-prefixes=CHECK-TBD,CHECK-TBD-LAZY --implicit-check-not='{{3Lib[A-Za-z0-9]+_W[Zz]}}'
// RUN: %target-build-swift -target %target-swift-6.0-abi-triple -O -wmo -parse-as-library -module-name Lib -emit-module -emit-module-path %t/swift6-tbd/Lib.swiftmodule %t/Lib.swift -emit-tbd -emit-tbd-path %t/swift6-tbd/Lib.tbd -c -o %t/swift6-tbd/Lib.o -Xfrontend -tbd-install_name -Xfrontend Lib -Xfrontend -validate-tbd-against-ir=missing
// RUN: %llvm-nm --defined-only --extern-only --format=just-symbols %t/swift6-tbd/Lib.o | %FileCheck %s --check-prefix=CHECK-TBD-LIB --implicit-check-not=table_W
// RUN: %target-build-swift -target %target-swift-6.0-abi-triple -O -wmo -module-name Client -I %t/swift6-tbd %t/Client.swift %t/main.swift -emit-ir -o %t/swift6-tbd/Client.ll
// RUN: %FileCheck %s --input-file=%t/swift6-tbd/Client.ll -DINT=i%target-ptrsize --implicit-check-not='{{3Lib[A-Za-z0-9]+_W[Zz]}}' %if OS_FAMILY=darwin %{ --check-prefixes=CHECK-TBD,CHECK-TBD-STATIC %} %else %{ --check-prefixes=CHECK-TBD,CHECK-TBD-LAZY %}

// Every client links with its library and prints the same values, whether it folds the lookups or not.
// RUN: %if executable_test %{ %target-build-swift -target %target-swift-5.10-abi-triple -O -wmo -module-name Client -I %t/swift5 %t/Client.swift %t/main.swift %t/swift5/Lib.o -o %t/swift5/a.out %}
// RUN: %if executable_test %{ %target-run %t/swift5/a.out | %FileCheck %s --check-prefix=CHECK-OUTPUT %}
// RUN: %if executable_test %{ %target-build-swift -target %target-swift-6.0-abi-triple -O -wmo -module-name Client -I %t/swift6 %t/Client.swift %t/main.swift %t/swift6/Lib.o -o %t/swift6/a.out %}
// RUN: %if executable_test %{ %target-run %t/swift6/a.out | %FileCheck %s --check-prefix=CHECK-OUTPUT %}
// RUN: %if executable_test %{ %target-build-swift -target %target-swift-5.10-abi-triple -O -wmo -module-name Client -I %t/swift5-tbd %t/Client.swift %t/main.swift %t/swift5-tbd/Lib.o -o %t/swift5-tbd/a.out %}
// RUN: %if executable_test %{ %target-run %t/swift5-tbd/a.out | %FileCheck %s --check-prefix=CHECK-OUTPUT %}
// RUN: %if executable_test %{ %target-build-swift -target %target-swift-6.0-abi-triple -O -wmo -module-name Client -I %t/swift6-tbd %t/Client.swift %t/main.swift %t/swift6-tbd/Lib.o -o %t/swift6-tbd/a.out %}
// RUN: %if executable_test %{ %target-run %t/swift6-tbd/a.out | %FileCheck %s --check-prefix=CHECK-OUTPUT %}

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

// Lock down how a client accesses array globals of another module:
//  - A public `let` global with an array literal is folded: the client reads the library's array
//    object directly, without calling the addressor.
//  - A `var` global, and a `let` global which only a function of the library accesses, are not folded.

// Each lookup masks its index via the AND operator, to the table's size.
// That folds away Swift's bounds checks.

//--- Lib.swift
public let table: [UInt8] = [0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66]
public var varTable: [UInt8] = [0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50]
let internalTable: [UInt8] = [0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36]

public func getInternal() -> [UInt8] {
  internalTable
}

public func varTableFirst() -> UInt8 {
  varTable[0]
}

//--- main.swift
import Lib

func text(_ lookup: (Int) -> UInt8) -> String {
  String(decoding: (0..<16).map(lookup), as: UTF8.self)
}

// CHECK-OUTPUT:      0123456789abcdef
print(text(lookupTable))
// CHECK-OUTPUT-NEXT: ABCDEFGHIJKLMNOP
print(text(lookupVarTable))
// CHECK-OUTPUT-NEXT: qrstuvwxyz123456
print(text(lookupInternal))
// CHECK-OUTPUT-NEXT: 0123456789abcdef
print(String(decoding: getTable(), as: UTF8.self))

// CHECK-OUTPUT-NEXT: 0123456789abcdef
print(String(decoding: alias, as: UTF8.self))

// The client and the library share the variable.
varTable[0] = 0x5a
// CHECK-OUTPUT-NEXT: 90
print(varTableFirst())
// CHECK-OUTPUT-NEXT: 90
print(lookupVarTable(0))

//--- Client.swift
import Lib

// A global which is initialized from the library's array is statically initialized with the library's
// read-only object. A lazily initialized array can't be referenced from a static initializer.
// CHECK-READONLY: @"$s6Client5aliasSays5UInt8VGvp" = {{.*}}constant {{.*}}ptr @"$s3Lib5table_WZTv_r"
public let alias: [UInt8] = table

// The folded lookup reads the elements behind the array's header, in the library's object. Its
// symbol has an `r` suffix if the library deploys to a Swift runtime with read-only static arrays.
// CHECK-LABEL: define {{.*}} @"$s6Client11lookupTableys5UInt8VSiF"(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[I:%.*]] = and [[INT]] %0, 15
// CHECK-NEXT:    [[P:%.*]] = getelementptr inbounds nuw i8, ptr getelementptr inbounds nuw (i8, ptr @"$s3Lib5table_WZTv_{{r?}}", [[INT]] {{[0-9]+}}), [[INT]] [[I]]
// CHECK-NEXT:    [[V:%.*]] = load i8, ptr [[P]], align 1
// CHECK-NEXT:    ret i8 [[V]]
// CHECK-NEXT:  }
// CHECK-TBD-LABEL: define {{.*}} @"$s6Client11lookupTableys5UInt8VSiF"(
// CHECK-TBD-LAZY:    call swiftcc ptr @"$s3Lib5tableSays5UInt8VGvau"()
// CHECK-TBD-STATIC:  load ptr, ptr @"$s3Lib5tableSays5UInt8VGvp"
// CHECK-TBD:         ret i8
public func lookupTable(_ i: Int) -> UInt8 {
  table[i & 15]
}

// The client can't know the array of a `var`.
// CHECK-LABEL: define {{.*}} @"$s6Client14lookupVarTableys5UInt8VSiF"(
// CHECK-NOT:     varTable_W
// CHECK-LAZY:    call swiftcc ptr @"$s3Lib8varTableSays5UInt8VGvau"()
// CHECK-READONLY: load ptr, ptr @"$s3Lib8varTableSays5UInt8VGvp"
// CHECK-NOT:     varTable_W
// CHECK:         ret i8
// CHECK-TBD-LABEL: define {{.*}} @"$s6Client14lookupVarTableys5UInt8VSiF"(
// CHECK-TBD-LAZY:    call swiftcc ptr @"$s3Lib8varTableSays5UInt8VGvau"()
// CHECK-TBD-STATIC:  load ptr, ptr @"$s3Lib8varTableSays5UInt8VGvp"
// CHECK-TBD:         ret i8
public func lookupVarTable(_ i: Int) -> UInt8 {
  varTable[i & 15]
}

// The global is not accessed by the client, but by a function of the library.
// CHECK-LABEL: define {{.*}} @"$s6Client14lookupInternalys5UInt8VSiF"(
// CHECK-NOT:     internalTable_W
// CHECK:         call swiftcc ptr @"$s3Lib11getInternalSays5UInt8VGyF"()
// CHECK-NOT:     internalTable_W
// CHECK:         ret i8
// CHECK-TBD-LABEL: define {{.*}} @"$s6Client14lookupInternalys5UInt8VSiF"(
// CHECK-TBD:         call swiftcc ptr @"$s3Lib11getInternalSays5UInt8VGyF"()
// CHECK-TBD:         ret i8
public func lookupInternal(_ i: Int) -> UInt8 {
  getInternal()[i & 15]
}

// A lazily initialized array lives in a container, which initializes the object's header at runtime.
// A read-only array needs no initialization.
// CHECK-LABEL: define {{.*}} @"$s6Client8getTableSays5UInt8VGyF"(
// CHECK-NEXT:  entry:
// CHECK-LAZY:           call ptr @swift_initStaticObject({{.*}}@"$s3Lib5table_WZTv_"
// CHECK-LAZY-NEXT:      ret ptr
// CHECK-READONLY-NEXT:  ret ptr @"$s3Lib5table_WZTv_r"
public func getTable() -> [UInt8] {
  table
}

// The library with a TBD file exports neither the outlined object, nor the token, nor the initializer.
// CHECK-TBD-LIB: {{_?}}$s3Lib5tableSays5UInt8VGvp
