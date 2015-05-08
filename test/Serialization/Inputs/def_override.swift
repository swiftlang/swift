@exported import def_class

public class OverrideComputedProperty : ComputedProperty {
  public override var value : Int {
    get {
      return super.value + 1
    }
    set {
      super.value = newValue
    }
  }

  public override var readOnly : Int {
    return super.readOnly + 1
  }

  public override init () { super.init() }
}

public class OverrideAddsSetter : ComputedProperty {
  public override var readOnly : Int {
    get { return 1 }
    set { /* do nothing */ }
  }
  public override init () { super.init() }
}

public class OverrideSimpleSubscript : ReadonlySimpleSubscript {
  public override subscript(x: Int) -> Bool {
    return false
  }
  public override init () {}
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
  public override init () {}
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
  public override init () {}
}

public class OverrideFunc : StillEmpty {
  public override func reset() {
  }
  public override init () {}
}
