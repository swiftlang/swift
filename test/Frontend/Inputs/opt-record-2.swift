open class C {
    var x = 1
    var y = 2

    public init() {
    }

    public func method() {
        print("x: \(x)")
    }

    public func method2() {
        print("x2: \(y)")
    }
}

@_assemblyVision
@inline(never)
public func runSomeTest(_ c: C) {
  for i in 0..<100 {
    c.method()
    c.method2()
  }
}
