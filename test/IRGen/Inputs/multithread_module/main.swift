
@inline(never)
func incrementit(x: Int) -> Int {
	return x + 1
}

class Derived : Base {
	override func memberfunc(x: Int) -> Int {
		return x + 2
	}
}

private struct MyStruct : MyProto {

	var x: Int

	func protofunc() -> Int {
		return x
	}
}

public var g1 = 234

let i = testit(27)
print(i)

let i2 = callmember(Derived())
print(i2)

callproto(MyStruct(x: 42))

print(callPrivInc(g1))

