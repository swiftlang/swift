// RUN: %swift -parse -verify %s

protocol Runcible {
  typealias Runcee 
}

class Mince {}

class Spoon : Runcible {
  typealias Runcee = Mince
}

class Owl<T:Runcible> {
  func eat(what:T.Runcee, with:T) { }
}

func owl1() -> Owl<Spoon> {
  return Owl<Spoon>()
}

func owl2() -> Owl<Spoon> {
  return Owl()
}

/*
func owl3() {
  Owl<Spoon>().eat(Mince(), with:Spoon())
}
 */
