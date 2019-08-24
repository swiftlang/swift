// RUN: %target-typecheck-verify-swift

class B { }
class D: B { }

let b: B = (D() as B as! D) as B

let x: UInt8 = 0 as UInt8 + 12
