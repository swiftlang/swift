// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -enable-experimental-feature CoroutineAccessors -validate-tbd-against-ir=all
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -enable-experimental-feature CoroutineAccessors -validate-tbd-against-ir=all -O
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -enable-experimental-feature CoroutineAccessors -validate-tbd-against-ir=all -disable-callee-allocated-coro-abi

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_CoroutineAccessors

public struct S {
  var _x: Int
  public init() { _x = 0 }
  public var x: Int {
    yielding borrow { yield _x }
    yielding mutate { yield &_x }
  }
}

// The synthesized accessors of a class's stored property are emitted into
// each client that uses them, and so are their coroutine function pointers.
public class C {
  var _y = 0
  public init() {}
  public var y: Int {
    yielding borrow { yield _y }
    yielding mutate { yield &_y }
  }
}
