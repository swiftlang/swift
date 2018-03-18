// RUN: %target-swift-frontend -primary-file %s -Onone -emit-sil -Xllvm \
// RUN:   -sil-print-after=definite-init -Xllvm \
// RUN:   -sil-print-only-functions=$S3del1MC4fromAcA12WithDelegate_p_tKcfc \
// RUN:   -Xllvm -sil-print-debuginfo -o /dev/null 2>&1 | %FileCheck %s

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
    public init(from d: WithDelegate) throws {
        guard let delegate = d.delegate as? DelegateB
        else { throw Err.s(0) }
        self.field = C()
        let i: Int = try d.f()
        value = i
    }
}

// Make sure the expanded sequence gets the right scope.

// CHECK:   [[I:%.*]] = integer_literal $Builtin.Int2, 1, loc {{.*}}:20:12, scope 2
// CHECK:   [[V:%.*]] = load [trivial] %2 : $*Builtin.Int2, loc {{.*}}:20:12, scope 2
// CHECK:   [[OR:%.*]] = builtin "or_Int2"([[V]] : $Builtin.Int2, [[I]] : $Builtin.Int2) : $Builtin.Int2, loc {{.*}}:20:12, scope 2
// CHECK:   store [[OR]] to [trivial] %2 : $*Builtin.Int2, loc {{.*}}:20:12, scope 2
// CHECK:   store %{{.*}} to [init] %{{.*}} : $*C, loc {{.*}}:23:20, scope 2

// Make sure the dealloc_stack gets the same scope of the instructions surrounding it.

// CHECK:   destroy_addr %0 : $*WithDelegate, loc {{.*}}:26:5, scope 2
// CHECK:   dealloc_stack %2 : $*Builtin.Int2, loc {{.*}}:20:12, scope 2
// CHECK:   throw %{{.*}} : $Error, loc {{.*}}:20:12, scope 2
