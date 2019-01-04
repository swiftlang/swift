// RUN: %target-swift-emit-silgen -o /dev/null %s

func inoutFunc(_ x: inout Int) {}

// <rdar://22000564> Crash on Subscript taking a tuple argument list
class TupleSubscript {
  subscript (position: (Int, Int)) -> Int {
    get { return 32 }
    set {}
  }
  subscript(native native: Int) -> Int {
    get { return native }
    set {}
  }
  subscript (position position: (Int, Int)) -> Int {
    get { return 32 }
    set {}
  }
}

do {
  let t = TupleSubscript()

  _ = t[(1, 2)]
  t[(1, 2)] = 3
  inoutFunc(&t[(1, 2)])

  _ = t[native: 0]
  t[native: 0] = 1
  inoutFunc(&t[native: 0])

  _ = t[position: (1, 2)]
  t[position: (1, 2)] = 1
  inoutFunc(&t[position: (1, 2)])
}

// <rdar://problem/16189360> [DF] Assert on subscript with variadic parameter
class VariadicSubscript1 {
  subscript(subs: Int...) -> Int {
    get {
      return 42
    }
    set {}
  }
}

do {
  let v = VariadicSubscript1()

  _ = v[]
  v[] = 0
  inoutFunc(&v[])

  _ = v[1]
  v[1] = 0
  inoutFunc(&v[1])

  _ = v[0, 1, 2]
  v[0, 1, 2] = 3
  inoutFunc(&v[0, 1, 2])
}

class VariadicSubscript2 {
  subscript(x: String, subs: Int...) -> Int {
    get {
      return 42
    }
    set {}
  }
}

do {
  let v = VariadicSubscript2()

  _ = v[""]
  v[""] = 0
  inoutFunc(&v[""])

  _ = v["", 0, 1, 2]
  v["", 0, 1, 2] = 3
  inoutFunc(&v["", 0, 1, 2])
}

struct VariadicSubscript3 {
  subscript(indices: (Int, Int)...) -> Int {
    get { return 0 }
    set {}
  }
}

do {
  var b = VariadicSubscript3()
  _ = b[]
  b[] = 1
  inoutFunc(&b[])

  _ = b[(1, 2)]
  b[(1,2)] = 1
  inoutFunc(&b[(1,2)])

  _ = b[(1, 2),(2,3)]
  b[(1,2),(2,3)] = 1
  inoutFunc(&b[(1,2),(2,3)])
}

// https://bugs.swift.org/browse/SR-1816
public struct Flags: OptionSet {
  public var rawValue: Int
  public init(rawValue: Int) { self.rawValue = rawValue }

  public static let flag = Flags(rawValue: 1 << 0)
}

class VariadicSubscript4 {
  subscript(_ values: Int..., flags flags: Flags) -> Int {
    get { return 0 }
    set { }
  }
}

do {
  let t = VariadicSubscript4()

  _ = t[flags: .flag]
  t[flags: .flag] = 0
  inoutFunc(&t[flags: .flag])

  _ = t[1, flags: .flag]
  t[1, flags: .flag] = 0
  inoutFunc(&t[1, flags: .flag])

  _ = t[1, 2, 3, flags: .flag]
  t[1, 2, 3, flags: .flag] = 0
  inoutFunc(&t[1, 2, 3, flags: .flag])
}
