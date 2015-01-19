// RUN: %target-parse-verify-swift

protocol Runcible {
  typealias Runcee 
}

class Mince { 
  init() {} 
}

class Spoon : Runcible {
  init() {} 

  typealias Runcee = Mince
}

class Owl<T:Runcible> {
  init() {} 

  func eat(what: T.Runcee, with: T) { }
}

func owl1() -> Owl<Spoon> {
  return Owl<Spoon>()
}

func owl2() -> Owl<Spoon> {
  return Owl()
}

func owl3() {
  Owl<Spoon>().eat(Mince(), with:Spoon())
}
