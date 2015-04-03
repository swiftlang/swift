// RUN: %target-run-simple-swift | FileCheck %s

func double(x: Int) -> Int {
  return x+x
}

func curriedSubtract(x: Int)(_ y: Int) -> Int {
  return x-y
}

func twice(f: (Int) -> Int, x: Int) -> Int {
  return f(f(x))
}

// CHECK: 4
println(double(2))
// CHECK: 8
println(double(4))

// CHECK: 12
println(curriedSubtract(16)(4))

// CHECK: 20
println(twice(double, 5))
// CHECK: 7
println(twice({ $0 + 1 }, 5))
// CHECK: 3
println(twice({ x in x - 1 }, 5))

func curry1() {

}

func curry1Throws() throws {

}

func curry2() -> () -> () {
	return curry1
}

func curry2Throws() throws -> () -> () {
	return curry1
}

func curry3() -> () throws -> () {
	return curry1Throws
}

func curry3Throws() throws -> () throws -> () {
	return curry1Throws
}

println(curry1.dynamicType)
// CHECK: () -> ()

println(curry2.dynamicType)
// CHECK: () -> () -> ()

println(curry2Throws.dynamicType)
// CHECK: () throws -> () -> ()

println(curry3.dynamicType)
// CHECK: () -> () throws -> ()

println(curry3Throws.dynamicType)
// CHECK: () throws -> () throws -> ()
