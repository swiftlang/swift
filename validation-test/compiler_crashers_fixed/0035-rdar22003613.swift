// RUN: not %target-swift-frontend %s -typecheck

class CFArray {}
struct U<T> {}

func yyy<T, Result>(arg: inout T, _ body: U<T> -> Result) -> Result {
  return body(U<T>())
}

enum YYY: Int, OptionSet {
  case A = 1
  
  init(rawValue: Int) {
    self = .A
  }
}

func XXX(flags: YYY, _ outItems: U<CFArray?>) -> Int
{
return 0
}

func f() {
  var importArray: CFArray? = nil
  yyy(&importArray) { importArrayPtr in
    XXX(0, importArrayPtr)
  }
}
