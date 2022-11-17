protocol ProtEmpty {}

protocol Prot {}

protocol Prot1 {}

class Clazz: Prot {
  var value: Clazz { return self }
  func getValue() -> Clazz { return self }
}

struct Stru: Prot, Prot1 {
  var value: Stru { return self }
  func getValue() -> Stru { return self }
}

class C {}

func ArrayC(_ a: [C]) {
	_ = a.count
	_ = a.description.count.advanced(by: 1).description
	_ = a[0]
}

func ArrayClass(_ a: [Clazz]) {
	_ = a[0].value.getValue().value
}

func ArrayClass(_ a: [Stru]) {
	_ = a[0].value.getValue().value
}
