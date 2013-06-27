// RUN: %swift < %s -repl | FileCheck %s
import Builtin

var x = new Int[10]
println(swift_isUniquelyReferenced(x.owner) as Bool)
// CHECK: true

var y = x
println(swift_isUniquelyReferenced(x.owner) as Bool)
// CHECK: false

swift_keepAlive(Builtin.castToObjectPointer(x.owner))
swift_keepAlive(Builtin.castToObjectPointer(y.owner))
