// RUN: %target-swift-frontend %s -O -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend %s -O -wmo -emit-sil | %FileCheck %s --check-prefix=WMO-CHECK

public protocol Pingable {
  func ping(_ x : Int) -> Int
}

public class Foo : Pingable {
  public func ping(_ x : Int) -> Int { x + 1 }
}

public class Err : Error { }

public protocol Pongable {
  func pong(_ x : Int) throws -> Int
}

public class Throwing : Pongable {
  public func pong(_ x : Int) throws -> Int {
    if x < 0 {
      throw Err()
    }
    return x + 1
  }
}

// Everything gets devirtualized, inlined, and promoted to the stack.
// CHECK-LABEL: sil @$s24devirtualize_existential17interesting_stuffSiyF
// CHECK: bb0
// CHECK-NEXT: integer_literal
// CHECK-NEXT: struct
// CHECK-NEXT: return
// CHECK-LABEL: end sil function '$s24devirtualize_existential17interesting_stuffSiyF'
public func interesting_stuff() -> Int {
 let x : Pingable = Foo()
 return x.ping(1)
}


// WMO-CHECK-LABEL: sil @$s24devirtualize_existential10ping_entryySiAA8Pingable_pF
// WMO-CHECK: bb0
// WMO-CHECK-NEXT: integer_literal
// WMO-CHECK-NEXT: struct
// WMO-CHECK-NEXT: return
// WMO-CHECK-LABEL: end sil function '$s24devirtualize_existential10ping_entryySiAA8Pingable_pF'
public func ping_entry(_ p: Pingable) -> Int {
  return p.ping(1)
}

// WMO-CHECK-LABEL: sil @$s24devirtualize_existential10pong_entryySiAA8Pongable_pF
// WMO-CHECK: bb0
// WMO-CHECK-NEXT: integer_literal
// WMO-CHECK-NEXT: struct
// WMO-CHECK-NEXT: return
// WMO-CHECK-LABEL: end sil function '$s24devirtualize_existential10pong_entryySiAA8Pongable_pF'
public func pong_entry(_ p: Pongable) -> Int {
  do {
    return try p.pong(1)
  } catch _ {
    return 0
  }
}
