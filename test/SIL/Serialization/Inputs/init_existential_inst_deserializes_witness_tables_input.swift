
protocol P {
  func doSomething()
}

@asmname("unknown")
func unknown() -> ()

struct X : P {
  func doSomething() {
    unknown()
  }
}

func whatShouldIDo(p : P) {
  p.doSomething()
}
