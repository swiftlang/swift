// RUN: %swift -i %s | FileCheck %s

// CHECK: swift.Int64[32]
println(demangleType("A32Si"))
// CHECK: Builtin.Float80
println(demangleType("Bf80_"))
// CHECK: Builtin.Int32
println(demangleType("Bi32_"))
// CHECK: Builtin.ObjCPointer
println(demangleType("BO"))
// CHECK: Builtin.ObjectPointer
println(demangleType("Bo"))
// CHECK: Builtin.RawPointer
println(demangleType("Bp"))
// CHECK: Builtin.OpaquePointer
println(demangleType("Bu"))
// CHECK: swift.Bool
println(demangleType("Sb"))
// CHECK: swift.Char
println(demangleType("Sc"))
// CHECK: swift.Float64
println(demangleType("Sd"))
// CHECK: swift.Float32
println(demangleType("Sf"))
// CHECK: swift.Int64
println(demangleType("Si"))
// CHECK: swift.String
println(demangleType("SS"))
// CHECK: swift.UInt64
println(demangleType("Su"))
// CHECK: swift.CString
println(demangleType("VSs7CString"))
// CHECK: ObjectiveC.NSObject
println(demangleType("CSo8NSObject"))
// CHECK: Monads.Either
println(demangleType("O6Monads6Either"))

