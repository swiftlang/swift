// RUN: not --crash %target-swift-frontend %s -parse

class N {}

class C {
  var number: N!
  var int64: Int64 = 0
}

let c: C? = C()
_ = (c!.number ?? 0) == (c?.int64 ?? 0)
