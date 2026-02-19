// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Library module

// SIL checking
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -entry-point-function-name Library_main -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -emit-sil -emit-module-path %t/Modules/Library.swiftmodule -o - | %FileCheck -check-prefix LIBRARY-SIL %s

// IR checking to ensure we get the right weak symbols.
// RUN: %target-swift-frontend %t/Library.swift -disable-experimental-feature EmbeddedExistentials -parse-as-library -entry-point-function-name Library_main -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -emit-ir -o - | %FileCheck -check-prefix LIBRARY-IR --dump-input-filter all %s
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -entry-point-function-name Library_main -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -emit-ir -o - | %FileCheck -check-prefix LIBRARY-EXIST-IR --dump-input-filter all %s

// Application module

// RUN: %target-swift-frontend %t/Application.swift -I %t/Modules -parse-as-library -entry-point-function-name Application_main -enable-experimental-feature Embedded -emit-sil -o - | %FileCheck -check-prefix APPLICATION-SIL %s

// RUN: %target-swift-frontend %t/Application.swift -disable-experimental-feature EmbeddedExistentials -I %t/Modules -parse-as-library -entry-point-function-name Application_main -enable-experimental-feature Embedded -emit-ir -o - | %FileCheck -check-prefix APPLICATION-IR --dump-input-filter all %s
// RUN: %target-swift-frontend %t/Application.swift -I %t/Modules -parse-as-library -entry-point-function-name Application_main -enable-experimental-feature Embedded -emit-ir -o - | %FileCheck -check-prefix APPLICATION-IR --dump-input-filter all %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_DeferredCodeGen

//--- Library.swift

// LIBRARY-IR: @"$e7Library10PointClassCN" = linkonce_odr {{.*}}constant
// LIBRARY-EXIST-IR: @"$e7Library10PointClassCMf" = {{.*}}linkonce_odr {{.*}}constant

// Never referenced.
// LIBRARY-IR-NOT: @"$es23_swiftEmptyArrayStorageSi_S3itvp" = linkonce_odr {{(protected |dllexport )?}}constant

// LIBRARY-IR-NOT: define {{.*}}@"$e7Library5helloSaySiGyF"()
public func hello() -> [Int] {
  getArray()
}

// LIBRARY-IR-NOT: define {{.*}} @"$e7Library8getArraySaySiGyF"()
public func getArray() -> [Int] {
  throughInternal()
}

// LIBRARY-IR-NOT: define {{.*}} @"$e7Library15throughInternalSaySiGyF"()
func throughInternal() -> [Int] {
  throughPrivate()
}

// LIBRARY-IR-NOT: define {{.*}} @"$e7Library14throughPrivate
private func throughPrivate() -> [Int] {
  [5, 6, 7]
}

// LIBRARY-IR-NOT: unnecessary
public func unnecessary() -> Int64 { 5 }

// LIBRARY-IR: define {{.*}} @"$e7Library14unusedYetThere
@export(interface)
public func unusedYetThere() -> Int64 { 5 }

open class PointClass {
  public var x, y: Int

  public init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }

  private func notUsed() { }
}

public protocol Reflectable: AnyObject {
  func reflect()
}

// LIBRARY-IR: define linkonce_odr hidden swiftcc void @"$es4swapyyxz_xztlFSi_Tg5"
// LIBRARY-IR: define linkonce_odr hidden swiftcc void @"$e7Library10PointClassCAA11ReflectableA2aDP7reflectyyFTW"

extension PointClass: Reflectable {
  public func reflect() {
    swap(&x, &y)
  }
}

// LIBRARY-IR: define {{.*}} @"$e7Library18createsExistentialAA11Reflectable_pyF"()
@export(interface)
public func createsExistential() -> any Reflectable {
  return PointClass(x: 5, y: 5)
}

// LIBRARY-IR-NOT: define swiftcc
// LIBRARY-IR-NOT: define hidden swiftcc

// LIBRARY-IR-NOT: define {{.*}} @"$es27_allocateUninitializedArrayySayxG_BptBwlFSi_Tg5"


// LIBRARY-IR: define linkonce_odr hidden void @_swift_dead_method_stub

// LIBRARY-SIL: sil @$e7Library5helloSaySiGyF
// LIBRARY-SIL: sil @$e7Library8getArraySaySiGyF : $@convention(thin) () -> @owned Array<Int> {

//--- Application.swift
import Library

public func testMe() {
  _ = hello()
  _ = getArray()
}

// APPLICATION-IR: define {{(protected |dllexport )?}}swiftcc void @"$e11Application6testMeyyF"()

// APPLICATION-SIL: sil public_external @$e7Library5helloSaySiGyF : $@convention(thin) () -> @owned Array<Int> {
// APPLICATION-IR: define linkonce_odr hidden swiftcc ptr @"$e7Library5helloSaySiGyF"()

// APPLICATION-SIL: sil public_external @$e7Library8getArraySaySiGyF : $@convention(thin) () -> @owned Array<Int> {
// APPLICATION-IR: define linkonce_odr hidden swiftcc ptr @"$e7Library8getArraySaySiGyF"()

// APPLICATION-IR: define {{(protected |dllexport )?}}i32 @Application_main

// APPLICATION-IR: define linkonce_odr hidden swiftcc { ptr, ptr } @"$es27_allocateUninitializedArrayySayxG_BptBwlFSi_Tg5"

@main
struct Main {
  static func main() {
  }
}
