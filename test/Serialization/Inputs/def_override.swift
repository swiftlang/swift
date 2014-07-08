@exported import def_class

public class OverrideComputedProperty : ComputedProperty {
  public override var value : Int {
    get {
      return super.value + 1
    }
    set {
      println(newValue)
      super.value = newValue
    }
  }

  public override var readOnly : Int {
    return super.readOnly + 1
  }

  public init () { super.init() }
}

public class OverrideAddsSetter : ComputedProperty {
  public override var readOnly : Int {
    get { return 1 }
    set { /* do nothing */ }
  }
  public init () { super.init() }
}

public class OverrideSimpleSubscript : ReadonlySimpleSubscript {
  public override subscript(x: Int) -> Bool {
    return false
  }
  public init () {}
}

public class OverrideAddsSubscriptSetter : ReadonlySimpleSubscript {
  public override subscript(x: Int) -> Bool {
    set(newValue) {
      // do nothing!
    }
    get {
      return super[x]
    }
  }
  public init () {}
}

public class OverrideComplexSubscript : ComplexSubscript {
  public override subscript(x : Int, y : Bool) -> Int {
    set(newValue) {
      // do nothing!
    }
    get {
      return super[x, y]
    }
  }
  public init () {}
}

public class OverrideFunc : StillEmpty {
  public override func reset() {
    println("ha!")
  }
  public init () {}
}
