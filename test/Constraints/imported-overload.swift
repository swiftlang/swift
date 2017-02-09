// RUN: %target-typecheck-verify-swift -import-objc-header %S/Inputs/overload.h
// REQUIRES: objc_interop
protocol rdar29374163Proto {}

class rdar29374163 {
  init(_ itf: Rdar29374163Itf) { self.itf = itf }
  var itf: Rdar29374163Itf
  func bar() -> Int {
    return itf.foo as! Int
  }
}
