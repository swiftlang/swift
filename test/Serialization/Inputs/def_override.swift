@exported import def_class

class OverrideComputedProperty : ComputedProperty {
  @override var value : Int {
    get {
      return super.value + 1
    }
    set {
      println(newValue)
      super.value = newValue
    }
  }

  @override var readOnly : Int {
    return super.readOnly + 1
  }
}

class OverrideAddsSetter : ComputedProperty {
  @override var readOnly : Int {
    get { return 1 }
    set { /* do nothing */ }
  }
}

class OverrideSimpleSubscript : ReadonlySimpleSubscript {
  @override subscript(x: Int) -> Bool {
    return false
  }
}

class OverrideAddsSubscriptSetter : ReadonlySimpleSubscript {
  @override subscript(x: Int) -> Bool {
    set(newValue) {
      // do nothing!
    }
    get {
      return super[x]
    }
  }
}

class OverrideComplexSubscript : ComplexSubscript {
  @override subscript(x : Int, y : Bool) -> Int {
    set(newValue) {
      // do nothing!
    }
    get {
      return super[x, y]
    }
  }
}

class OverrideFunc : StillEmpty {
  @override func reset() {
    println("ha!")
  }
}
