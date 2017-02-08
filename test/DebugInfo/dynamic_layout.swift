// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-ir -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

class Class <T> {
  var x: T

  init(_x : T) {x = _x}

  // Verify that the mangling of the decl context of the type U is correct.
  // CHECK: !DICompositeType({{.*}}name: "{{[^"]*}}_T014dynamic_layout5ClassC3foox_qd__tqd__lFQq_{{[^"]*}}"
  func foo <U> (_ y : U) -> (T,U) {
    var tuple = (x,y)
    return tuple
  }
}

func main() {
  var v = Class<Int64>(_x: 1)
  var tuple = v.foo("hi")
  markUsed(tuple.0)
  markUsed(tuple.1)
}

main()
