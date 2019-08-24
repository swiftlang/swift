// RUN: %target-swift-emit-silgen -Xllvm -sil-full-demangle %s

class BaseClass<T> {
  func inAndOut(_ t: T) -> T { return t }
  var property: T

  init(_ t: T) {
    self.property = t
  }
}

class DerivedClass : BaseClass<(Int, Int)> {
  override func inAndOut(_ t: (Int, Int)) -> (Int, Int) {
    let fn = super.inAndOut
    return fn(t)
  }

  override var property: (Int, Int) {
    get {
      return super.property
    }
    set {
      super.property = newValue
    }
  }

  override init(_ t: (Int, Int)) {
    super.init(t)
  }
}

let d = DerivedClass((1, 2))
_ = d.inAndOut((1, 2))

let value = d.property
d.property = value

d.property.0 += 1
