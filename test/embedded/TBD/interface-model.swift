// Under CodeGenerationModel=interface, every strong definition that IRGen
// produces for a public declaration must be in the TBD, and the TBD must not
// contain anything else.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all -O
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all -Osize

// RUN: %target-swift-frontend -typecheck %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib
// RUN: %FileCheck %s < %t/Lib.tbd
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t/Lib.tbd

// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

// Embedded Swift has none of the runtime metadata entities of the full Swift
// ABI: no nominal type descriptors (Mn), metadata accessors (Ma), method
// descriptors (Tq), field offsets (Wvd), conformance descriptors (Mc),
// protocol descriptors (Mp), dispatch thunks (Tj), or witness thunks (TW).
// NEGATIVE-NOT: {{Mn"|Ma"|Tq"|Wvd"|Mc"|Mp"|Tj"|TW"}}

// Generic declarations are emitted into each client that uses them.
// NEGATIVE-NOT: genericFunc

public protocol Greeter {
  func greet()
}

// CHECK-DAG: "_$e3Lib5PointVMf"
// CHECK-DAG: "_$e3Lib5PointVN"
// CHECK-DAG: "_$e3Lib5PointV1x1yACSi_SitcfC"
// CHECK-DAG: "_$e3Lib5PointVAA7GreeterAAWP"
// CHECK-DAG: "_$e3Lib5PointV5greetyyF"
public struct Point: Greeter {
  public var x, y: Int
  public init(x: Int, y: Int) { self.x = x; self.y = y }
  public func greet() {}
}

// Derived conformances to Equatable and Hashable.
// CHECK-DAG: "_$e3Lib5ColorOMf"
// CHECK-DAG: "_$e3Lib5ColorOSQAAWP"
// CHECK-DAG: "_$e3Lib5ColorOSHAAWP"
// CHECK-DAG: "_$e3Lib5ColorO9hashValueSivg"
public enum Color {
  case red, green
}

// CHECK-DAG: "_$e3Lib4BaseCMf"
// CHECK-DAG: "_$e3Lib4BaseCN"
// CHECK-DAG: "_$e3Lib4BaseCACycfC"
// CHECK-DAG: "_$e3Lib4BaseCACycfc"
// CHECK-DAG: "_$e3Lib4BaseCfD"
// CHECK-DAG: "_$e3Lib4BaseCfd"
// CHECK-DAG: "_$e3Lib4BaseC6methodyyF"
// CHECK-DAG: "_$e3Lib4BaseC11classMethodyyFZ"
open class Base {
  public init() {}
  open func method() {}
  open class func classMethod() {}
  deinit {}
}

// CHECK-DAG: "_$e3Lib7DerivedC6methodyyF"
public final class Derived: Base {
  public override func method() {}
}

// CHECK-DAG: "_$e3Lib11NonCopyableVMf"
// CHECK-DAG: "_$e3Lib11NonCopyableVfD"
public struct NonCopyable: ~Copyable {
  public var value: Int
  public init() { value = 0 }
  deinit {}
}

// CHECK-DAG: "_$e3Lib11ComputationV5valueSivg"
// CHECK-DAG: "_$e3Lib11ComputationV5valueSivs"
// CHECK-DAG: "_$e3Lib11ComputationVyS2icig"
public struct Computation {
  public var value: Int { get { 1 } set {} }
  public subscript(i: Int) -> Int { i }
}

// Globals have both storage and an addressor.
// CHECK-DAG: "_$e3Lib6globalSaySiGvp"
// CHECK-DAG: "_$e3Lib6globalSaySiGvau"
public var global = [1, 2, 3]

// CHECK-DAG: "_$e3Lib11ConcreteBoxV6sharedSivpZ"
// CHECK-DAG: "_$e3Lib11ConcreteBoxV6sharedSivau"
public struct ConcreteBox {
  public static let shared = 17
}

// Members of a constrained extension of a generic type that are concrete
// enough to emit.
// CHECK-DAG: "_$e3Lib3BoxVAASiRszlE8concreteyyF"
public struct Box<T> {
  public var value: T
}

extension Box where T == Int {
  public func concrete() {}
}

public func genericFunc<T>(_ t: T) {}

// Default argument generators are emitted into each client that uses them.
// CHECK-DAG: "_$e3Lib11withDefaultyySiF"
// NEGATIVE-NOT: withDefaultyySiFfA_
public func withDefault(_ x: Int = 0) {}

// @export(implementation) declarations have no symbols in this module.
// NEGATIVE-NOT: implementationOnly
@export(implementation)
public func implementationOnly() {}

// CHECK-DAG: "_c_entry_point"
@c(c_entry_point)
public func cEntryPoint() {}

// Typed throws.
// CHECK-DAG: "_$e3Lib11throwsTypedyyAA7MyErrorOYKF"
public enum MyError: Error {
  case failed
}
public func throwsTyped() throws(MyError) {}
