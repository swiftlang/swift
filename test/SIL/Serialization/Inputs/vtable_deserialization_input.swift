
protocol P {
  func doSomething()
}

@asmname("unknown")
func unknown() -> ()

class Y : P {
  func doAnotherThing() {
    unknown()
  }

  func doSomething() {
    doAnotherThing()
  }
}
