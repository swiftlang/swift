// RUN: %target-typecheck-verify-swift -swift-version 6

// rdar://137825558 - crash due to attempted existential opening

protocol FS {
  func exists(_ str: String) -> Bool
}

class Obj {
  var property: String = ""
}

func checkFunctionCall<T, Arg0>(
  _ lhs: T, calling functionCall: (T, Arg0) throws -> Bool, _ argument0: Arg0
) {
}

func checkFunctionCall<T, Arg0, R>(
  _ lhs: T, calling functionCall: (T, Arg0) throws -> R?, _ argument0: Arg0
) {
}

func test(fs: any FS, obj: Obj!) {
  checkFunctionCall(fs, calling: { $0.exists($1) }, obj.property)
}
