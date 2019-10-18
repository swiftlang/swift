
@inline(never)
func incrementit(_ x: Int) -> Int {
	return x + 1
}

class Derived : Base {
	override func memberfunc(_ x: Int) -> Int {
		return x + 2
	}
}

public struct MyStruct : MyProto {

	var x: Int

	func protofunc() -> Int {
		return x
	}
}

@_transparent public func transparentfunc(_ x: Int) -> Int {
	return x + 3
}

public var g1 = 234

let i = testit(27)
print(i)

let i2 = callmember(Derived())
print(i2)

callproto(MyStruct(x: 42))

print(callPrivInc(g1))

