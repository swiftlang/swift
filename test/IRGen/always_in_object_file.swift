// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Library module

// SIL checking
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -entry-point-function-name Library_main -enable-experimental-feature AlwaysEmitIntoObjectFile -emit-sil -emit-module-path %t/Modules/Library.swiftmodule -o - | %FileCheck -check-prefix LIBRARY-SIL %s

// IR checking to ensure we get the right symbols.
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -entry-point-function-name Library_main -enable-experimental-feature AlwaysEmitIntoObjectFile -emit-ir -o - | %FileCheck -check-prefix LIBRARY-IR %s

// Application module

// RUN: %target-swift-frontend %t/Application.swift -I %t/Modules -parse-as-library -entry-point-function-name Application_main -enable-experimental-feature AlwaysEmitIntoObjectFile -emit-sil -o - | %FileCheck -check-prefix APPLICATION-SIL %s

// RUN: %target-swift-frontend %t/Application.swift -I %t/Modules -parse-as-library -entry-point-function-name Application_main -enable-experimental-feature AlwaysEmitIntoObjectFile -emit-ir -o - | %FileCheck -check-prefix APPLICATION-IR %s

//--- Library.swift

// LIBRARY-IR: define swiftcc ptr @"$s7Library5helloSaySiGyF"()
@alwaysEmitIntoObjectFile
public func hello() -> [Int] {
  getArray()
}

// LIBRARY-IR: define swiftcc ptr @"$s7Library8getArraySaySiGyF"()
public func getArray() -> [Int] {
  throughInternal()
}

// LIBRARY-IR: define hidden swiftcc ptr @"$s7Library15throughInternalSaySiGyF"()
func throughInternal() -> [Int] {
  throughPrivate()
}

// LIBRARY-IR: define internal swiftcc ptr @"$s7Library14throughPrivate
private func throughPrivate() -> [Int] {
  [5, 6, 7]
}

// LIBRARY-IR: declare swiftcc { ptr, ptr } @"$ss27_allocateUninitializedArrayySayxG_BptBwlF"

// LIBRARY-SIL: sil @$s7Library5helloSaySiGyF
// LIBRARY-SIL: sil @$s7Library8getArraySaySiGyF : $@convention(thin) () -> @owned Array<Int> {

//--- Application.swift
import Library

@alwaysEmitIntoObjectFile
public func testMe() {
  _ = hello()
  _ = getArray()
}

// Note: "hello" is emitted only into the object file, so there is no definition
// here.

// APPLICATION-SIL: sil @$s7Library5helloSaySiGyF : $@convention(thin) () -> @owned Array<Int>{{$}}
// APPLICATION-IR: declare swiftcc ptr @"$s7Library5helloSaySiGyF"()

// Note: "getArray" is not @alwaysEmitIntoObjectFile, so it's definition is
// available.

// APPLICATION-SIL: sil @$s7Library8getArraySaySiGyF : $@convention(thin) () -> @owned Array<Int>{{$}}
// APPLICATION-IR: declare swiftcc ptr @"$s7Library8getArraySaySiGyF"() #0{{$}}

// APPLICATION-IR: define i32 @Application_main
@main
struct Main {
  static func main() {
  }
}

