// RUN: %target-swift-frontend %s -emit-sil -g -o - | %FileCheck  %s
public protocol DelegateA {}
public protocol DelegateB {}
public protocol WithDelegate
{
    var delegate: DelegateA? { get }
    func f() throws -> Int
}
public enum Err: Swift.Error {
    case s(Int)
}
public class C {}
public class M {
    let field: C
    var value : Int
  // Verify that definite initialization doesn't create a bogus description of
  // self pointing to the liveness bitvector.
  
  // CHECK: sil @_T04main1MCAcA12WithDelegate_p4from_tKcfc
  // CHECK: bb0
  // CHECK-NEXT: %2 = alloc_stack $Builtin.Int2
  // CHECK-NOT: let
  // CHECK-NOT: name
  // CHECK: scope
    public init(from d: WithDelegate) throws {
        guard let delegate = d.delegate as? DelegateB
        else { throw Err.s(0) }
        self.field = C()
        let i: Int = try d.f()
        value = i
    }
}
