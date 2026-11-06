// RUN: %target-swift-frontend -O -module-name test -emit-sil -sil-verify-all %s | %FileCheck %s

// https://github.com/swiftlang/swift/issues/93865
// The opened existential of a protocol with a suppressed primary associated
// type has a default `Self.Op: Copyable` requirement, which is not part of the
// existential's conformances.

protocol Operation: ~Copyable {}

struct CopyableOperation: Operation {
  let id: Int
}

struct MoveOnlyOperation: ~Copyable, Operation {
  let id: Int
}

protocol Tunnel<Op>: AnyObject {
  associatedtype Op: Operation & ~Copyable
  func begin() -> Op
}

final class CopyableTunnel: Tunnel {
  func begin() -> CopyableOperation { CopyableOperation(id: 42) }
}

final class MoveOnlyTunnel: Tunnel {
  func begin() -> MoveOnlyOperation { MoveOnlyOperation(id: 42) }
}

final class Holder {
  let tunnel: any Tunnel
  init(_ tunnel: any Tunnel) { self.tunnel = tunnel }
}

@inline(never)
func hold(_ tunnel: any Tunnel) -> Holder {
  Holder(tunnel)
}

// CHECK-LABEL: sil @$s4test0A8CopyableypyF :
// CHECK:         [[F:%.*]] = function_ref @$s4test4holdyAA6HolderCAA6Tunnel_pFTf4e_nAA08CopyableD0C_Tg5 : $@convention(thin) (@guaranteed CopyableTunnel) -> @owned Holder
// CHECK:         apply [[F]]
// CHECK:       } // end sil function '$s4test0A8CopyableypyF'
public func testCopyable() -> Any {
  return hold(CopyableTunnel())
}

// The concrete associated type is not Copyable: don't specialize for the
// concrete type.
// CHECK-LABEL: sil @$s4test0A8MoveOnlyypyF :
// CHECK:         [[F:%.*]] = function_ref @$s4test4holdyAA6HolderCAA6Tunnel_pFTf4e_n : $@convention(thin) <τ_0_0 where τ_0_0 : Tunnel, τ_0_0.Op : Copyable> (@guaranteed τ_0_0) -> @owned Holder
// CHECK:         [[O:%.*]] = open_existential_ref
// CHECK:         apply [[F]]<@opened({{.*}}, any Tunnel) Self>([[O]])
// CHECK:       } // end sil function '$s4test0A8MoveOnlyypyF'
public func testMoveOnly() -> Any {
  return hold(MoveOnlyTunnel())
}
