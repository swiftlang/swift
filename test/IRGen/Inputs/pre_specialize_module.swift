@usableFromInline
struct ResilientInternalBoxedThing<T> {
  @usableFromInline
  var t: T

  @usableFromInline
  init(_ t: T) {
    self.t = t
  }
}

@usableFromInline
@frozen
struct InternalThing<T> {
  @usableFromInline
  var t: T

  @usableFromInline
  init(_ t: T) {
    self.t = t
  }

  @_specialize(exported: true, where T == Int)
  @_specialize(exported: true, where T == Bool)
  @_specialize(exported: true, where T == ResilientInternalBoxedThing<Int>)
  @inlinable
  func compute() -> T {
    return t
  }

  @inlinable
  var computedX : T {
    @_specialize(exported: true, where T == Int)
    get {
      return t
    }
    @_specialize(exported: true, where T == Int)
    set {
      t = newValue
    }
  }

  @inlinable
  subscript(_ i: Int) -> T {
    @_specialize(exported: true, where T == Int)
    get {
      return t
    }
    @_specialize(exported: true, where T == Int)
    set {
      t = newValue
    }
  }
}

@usableFromInline
class InternalRef<T> {
  @usableFromInline
  var t: T

  @usableFromInline
  init(_ t: T) {
    self.t = t
  }

  @_specialize(exported: true, where T == Int)
  @_specialize(exported: true, where T == Bool)
  @_specialize(exported: true, where T == ResilientInternalBoxedThing<Int>)
  @inlinable
  final func compute() -> T {
    return t
  }

  @inlinable
  final var computedX : T {
    @_specialize(exported: true, where T == Int)
    get {
      return t
    }
    @_specialize(exported: true, where T == Int)
    set {
      t = newValue
    }
  }

  @inlinable
  final subscript(_ i: Int) -> T {
    @_specialize(exported: true, where T == Int)
    get {
      return t
    }
    @_specialize(exported: true, where T == Int)
    set {
      t = newValue
    }
  }
}

@inline(never)
@inlinable
public func testSpecialization<T>(_ t: T) {
  print(InternalThing(ResilientInternalBoxedThing(t)).compute())

  print(InternalThing(t).compute())

  var i = InternalThing(t)
  i.computedX = t
  print(i.computedX)

  i[1] = t
  print(i[2])
}
