// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Library module

// SIL checking
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -entry-point-function-name Library_main -enable-experimental-feature Embedded -emit-sil -emit-module-path %t/Modules/Library.swiftmodule -o - | %FileCheck -check-prefix LIBRARY-SIL %s

// IR checking to ensure we get the right weak symbols.
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -entry-point-function-name Library_main -enable-experimental-feature Embedded -emit-ir -o - | %FileCheck -check-prefix LIBRARY-IR --dump-input-filter all %s

// Application module

// RUN: %target-swift-frontend %t/Application.swift -I %t/Modules -parse-as-library -entry-point-function-name Application_main -enable-experimental-feature Embedded -emit-sil -o - | %FileCheck -check-prefix APPLICATION-SIL %s

// RUN: %target-swift-frontend %t/Application.swift -I %t/Modules -parse-as-library -entry-point-function-name Application_main -enable-experimental-feature Embedded -emit-ir -o - | %FileCheck -check-prefix APPLICATION-IR %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

//--- Library.swift

// LIBRARY-IR: define {{(protected |dllexport )?}}swiftcc ptr @"$e7Library5helloSaySiGyF"()
@_neverEmitIntoClient
public func hello() -> [Int] {
  getArray()
}

// LIBRARY-IR: define {{(protected |dllexport )?}}swiftcc ptr @"$e7Library8getArraySaySiGyF"()
public func getArray() -> [Int] {
  throughInternal()
}

// LIBRARY-IR: define {{(protected |dllexport )?}}swiftcc ptr @"$e7Library15throughInternalSaySiGyF"()
func throughInternal() -> [Int] {
  throughPrivate()
}

// LIBRARY-IR: define {{(protected |dllexport )?}}swiftcc ptr @"$e7Library14throughPrivate
private func throughPrivate() -> [Int] {
  [5, 6, 7]
}

// LIBRARY-IR-NOT: unnecessary
public func unnecessary() -> Int { 5 }

// LIBRARY-IR: define linkonce_odr hidden swiftcc { ptr, ptr } @"$es27_allocateUninitializedArrayySayxG_BptBwlFSi_Tg5"

// LIBRARY-SIL: sil @$e7Library5helloSaySiGyF
// LIBRARY-SIL: sil @$e7Library8getArraySaySiGyF : $@convention(thin) () -> @owned Array<Int> {

//--- Application.swift
import Library

@_neverEmitIntoClient
public func testMe() {
  _ = hello()
  _ = getArray()
}

// Note: "hello" is emitted only into the object file, so there is no definition
// here.

// APPLICATION-SIL: sil @$e7Library5helloSaySiGyF : $@convention(thin) () -> @owned Array<Int>{{$}}
// APPLICATION-IR: declare swiftcc ptr @"$e7Library5helloSaySiGyF"()

// Note: "getArray" is not @_neverEmitIntoClient, so it's definition is
// available.

// APPLICATION-SIL: sil public_external @$e7Library8getArraySaySiGyF : $@convention(thin) () -> @owned Array<Int> {
// APPLICATION-IR: define linkonce_odr hidden swiftcc ptr @"$e7Library8getArraySaySiGyF"()

// APPLICATION-IR: define{{.*}}@Application_main
@main
struct Main {
  static func main() {
  }
}

// APPLICATION-IR: define linkonce_odr hidden swiftcc { ptr, ptr } @"$es27_allocateUninitializedArrayySayxG_BptBwlFSi_Tg5"

