protocol ProtEmpty {}

protocol Prot {}

protocol Prot1 {}

class Clas: Prot {
  var value: Clas { return self }
  func getValue() -> Clas { return self }
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

func ArrayClas(_ a: [Clas]) {
	_ = a[0].value.getValue().value
}

func ArrayClas(_ a: [Stru]) {
	_ = a[0].value.getValue().value
}
