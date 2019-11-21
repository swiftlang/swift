// RUN: %target-swift-frontend -emit-sil %s -Onone -Xllvm \
// RUN:   -sil-print-after=mandatory-inlining \
// RUN:   -Xllvm -sil-print-debuginfo -o /dev/null 2>&1 | %FileCheck %s

// CHECK: destroy_value {{.*}} : $@callee_guaranteed () -> (), loc {{.*}}:21:5, scope 9
// CHECK: destroy_value {{.*}} : $@callee_guaranteed () -> @out (), loc {{.*}}:18:17, scope 9
// CHECK: destroy_value {{.*}} : $@callee_guaranteed () -> (), loc {{.*}}:21:5, scope 9

func patatino<d, i>(_ function: @escaping (d) -> i, g: d...) -> () -> i {
    return { typealias h = ([d]) -> i
        let f = unsafeBitCast(function, to: h.self)
        return f(g)
    }
}
func tinky() {
    for b in 1...50{
        let c = {}
        let e = patatino(c)
        e()
        let f = {}
    }
}
