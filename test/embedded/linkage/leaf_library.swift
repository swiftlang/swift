// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Library module

// SIL checking
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -entry-point-function-name Library_main -enable-experimental-feature EmbeddedLinkageModel -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLeafLibrary -emit-sil -emit-module-path %t/Modules/Library.swiftmodule -o - | %FileCheck -check-prefix LIBRARY-SIL %s

// IR checking to ensure we get the right weak symbols.
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -entry-point-function-name Library_main -enable-experimental-feature EmbeddedLinkageModel -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLeafLibrary -emit-ir -o - | %FileCheck -check-prefix LIBRARY-IR %s

// Application module

// RUN: %target-swift-frontend %t/Application.swift -I %t/Modules -parse-as-library -entry-point-function-name Application_main -enable-experimental-feature EmbeddedLinkageModel -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLeafLibrary -emit-sil -o - | %FileCheck -check-prefix APPLICATION-SIL %s

// RUN: %target-swift-frontend %t/Application.swift -I %t/Modules -parse-as-library -entry-point-function-name Application_main -enable-experimental-feature EmbeddedLinkageModel -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLeafLibrary -emit-ir -o - | %FileCheck -check-prefix APPLICATION-IR %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedLinkageModel
// REQUIRESx: swift_feature_EmbeddedLeafLibrary

//--- Library.swift

// LIBRARY-IR: define swiftcc ptr @"$e7Library5helloSaySiGyF"()
public func hello() -> [Int] {
  getArray()
}

// LIBRARY-IR: define swiftcc ptr @"$e7Library8getArraySaySiGyF"()
public func getArray() -> [Int] {
  throughInternal()
}

// LIBRARY-IR: define linkonce_odr swiftcc ptr @"$e7Library15throughInternalSaySiGyF"()
func throughInternal() -> [Int] {
  throughPrivate()
}

// LIBRARY-IR: define linkonce_odr swiftcc ptr @"$e7Library14throughPrivate33_
private func throughPrivate() -> [Int] {
  [5, 6, 7]
}

// LIBRARY-IR: define linkonce_odr hidden swiftcc { ptr, ptr } @"$es27_allocateUninitializedArrayySayxG_BptBwlFSi_Tg5"

// LIBRARY-IR: define swiftcc i64 @"$e7Library11unnecessarySiyF"()
public func unnecessary() -> Int { 5 }

// LIBRARY-SIL: sil @$e7Library5helloSaySiGyF
// LIBRARY-SIL: sil @$e7Library8getArraySaySiGyF : $@convention(thin) () -> @owned Array<Int> {

//--- Application.swift
import Library

public func testMe() {
  _ = hello()
  _ = getArray()
}

// APPLICATION-IR: define swiftcc void @"$e11Application6testMeyyF"()

// APPLICATION-SIL: sil @$e7Library5helloSaySiGyF : $@convention(thin) () -> @owned Array<Int>{{$}}
// APPLICATION-IR: declare swiftcc ptr @"$e7Library5helloSaySiGyF"()

// APPLICATION-SIL: sil @$e7Library8getArraySaySiGyF : $@convention(thin) () -> @owned Array<Int>{{$}}
// APPLICATION-IR: declare swiftcc ptr @"$e7Library8getArraySaySiGyF"() #0

// APPLICATION-IR: define i32 @Application_main
@main
struct Main {
  static func main() {
  }
}

