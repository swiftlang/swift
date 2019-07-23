
func foo() -> Int  { return 1 }

func bar(f: Float) -> Float { return bar(f: 1) }

protocol P {}

func fooP(_ p: P) { fooP(p) }

class C {}

func ArrayC(_ a: [C]) {
	_ = a.count
	_ = a.description.count.advanced(by: 1).description
}

struct S {
  let val = 4
}
func DictS(_ a: [Int: S]) {
  _ = a[2]?.val.advanced(by: 1).byteSwapped
}

typealias MyInt = Int

func calculateAdd(a: MyInt, b: MyInt) -> MyInt {
	return a + b
}