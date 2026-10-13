// An accessor inherits an explicit '@export(...)' from the variable or
// subscript it implements, whatever their access level.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -o %t/Lib.ll %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib -validate-tbd-against-ir=all
// RUN: %FileCheck %s < %t/Lib.ll
// RUN: %FileCheck -check-prefix TBD %s < %t/Lib.tbd
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib-O.tbd -tbd-install_name Lib -validate-tbd-against-ir=all -O

// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

@usableFromInline
struct Table {
  @usableFromInline var count: Int

  @usableFromInline init(count: Int) { self.count = count }

  // The getter is emitted into each client that uses it.
  // CHECK-DAG: define linkonce_odr {{.*}}@"$e3Lib5TableV7doubledSivg"(
  // TBD-NOT: doubled
  @export(implementation)
  internal var doubled: Int { count &* 2 }

  // CHECK-DAG: define linkonce_odr {{.*}}@"$e3Lib5TableVyS2icig"(
  // TBD-NOT: TableVyS2icig
  @export(implementation)
  internal subscript(i: Int) -> Int { count &+ i }
}

public func generic<T>(_ t: T) -> Int {
  let table = Table(count: MemoryLayout<T>.size)
  return table.doubled &+ table[1]
}

public func nonGeneric() -> Int {
  let table = Table(count: 3)
  return table.doubled &+ table[1]
}
