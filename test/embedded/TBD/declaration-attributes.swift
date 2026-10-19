// Symbols named by attributes, symbols declared elsewhere, and storage that
// is never emitted.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -enable-experimental-feature Extern -validate-tbd-against-ir=all
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -enable-experimental-feature Extern -validate-tbd-against-ir=all -O

// RUN: %target-swift-frontend -typecheck %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -enable-experimental-feature Extern -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib
// RUN: %FileCheck %s < %t/Lib.tbd
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t/Lib.tbd

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern
// REQUIRES: VENDOR=apple

// The storage of a @_silgen_name variable uses that name.
// CHECK-DAG: "_renamed_storage"
// CHECK-DAG: "_$e3Lib14renamedStorageSivau"
@_silgen_name("renamed_storage")
public var renamedStorage: Int = 5

// Declarations of symbols that are defined elsewhere.
// NEGATIVE-NOT: defined_elsewhere
@_silgen_name("defined_elsewhere_var")
var definedElsewhereVar: Int

@_silgen_name("defined_elsewhere_func")
public func definedElsewhereFunc()

@_extern(c, "defined_elsewhere_c")
public func definedElsewhereC(_ x: Int) -> Int

public func useThem() -> Int {
  definedElsewhereFunc()
  return definedElsewhereC(definedElsewhereVar)
}

// IRGen does not emit storage for a global of empty type, but it does emit
// the addressor.
// CHECK-DAG: "_$e3Lib5EmptyV6sharedACvau"
// CHECK-DAG: "_$e3Lib10emptyTuple{{[^"]*}}vau"
// CHECK-DAG: "_$e3Lib11singleEmpty{{[^"]*}}vau"
// NEGATIVE-NOT: 6shared{{[^"]*}}vpZ"
// NEGATIVE-NOT: 10emptyTuple{{[^"]*}}vp"
// NEGATIVE-NOT: 11singleEmpty{{[^"]*}}vp"
public struct Empty {
  public static let shared = Empty()
  public init() {}
  public static var counter = 0
}

public var emptyTuple: ((), ()) = ((), ())

public enum EmptyEnum {
  case only(Empty)
}

public var singleEmpty: EmptyEnum = .only(Empty())

// A global of non-empty type does have storage.
// CHECK-DAG: "_$e3Lib5EmptyV7counterSivpZ"
