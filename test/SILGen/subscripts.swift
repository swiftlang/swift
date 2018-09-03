// RUN: %target-swift-emit-silgen -enable-sil-ownership -o /dev/null %s

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

