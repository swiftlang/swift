// When generic code only refers to declarations that are public or
// '@usableFromInline', as required when emitting a TBD file with the
// "interface" code generation model, the TBD file lists everything that
// clients need.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib -validate-tbd-against-ir=all
// RUN: %FileCheck %s < %t/Lib.tbd
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib-O.tbd -tbd-install_name Lib -validate-tbd-against-ir=all -O

// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

// CHECK-DAG: "_$e3Lib6helperyS2iF"
@usableFromInline
func helper(_ x: Int) -> Int { x &+ 1 }

// CHECK-DAG: "_$e3Lib10HelperTypeVMf"
// CHECK-DAG: "_$e3Lib10HelperTypeVACycfC"
@usableFromInline
struct HelperType {
  @usableFromInline var value: Int

  @usableFromInline init() { value = 3 }
}

// Clients emit their own copies of generic code, which refers to the
// declarations above by symbol.
// CHECK-NOT: genericFunction
public func genericFunction<T>(_ t: T) -> Int {
  helper(MemoryLayout<T>.size) + HelperType().value
}

@usableFromInline
func usableFromInlineGeneric<T>(_ t: T) -> Int { helper(1) }

public func callsUsableFromInlineGeneric<T>(_ t: T) -> Int {
  usableFromInlineGeneric(t)
}

// Clients access a struct's stored properties and form enum cases directly,
// so those don't need to be '@usableFromInline'.
@usableFromInline
struct HasStoredProperty {
  var stored: Int

  @usableFromInline init(stored: Int) { self.stored = stored }
}

@usableFromInline
enum HasCases {
  case first
  case second(Int)
}

public func genericUsingStoredAndCases<T>(_ t: T) -> Int {
  var s = HasStoredProperty(stored: MemoryLayout<T>.size)
  s.stored += 1
  switch HasCases.second(s.stored) {
  case .first: return 0
  case .second(let value): return value
  }
}
