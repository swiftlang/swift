// RUN: %target-swift-frontend -typecheck -dump-ast %s | %FileCheck %s

struct A {
    static func ~=(pattern: B, subject: A) -> Bool { return true }
    static func ~=(pattern: C, subject: borrowing A) -> Bool { return true }
    static func ~=(pattern: D, subject: consuming A) -> Bool { return true }
}
struct B { }
struct C { }
struct D { }
struct E { }
struct F { }
struct G { }

func ~=(pattern: E, subject: A) -> Bool { return true }
func ~=(pattern: F, subject: borrowing A) -> Bool { return true }
func ~=(pattern: G, subject: consuming A) -> Bool { return true }

// CHECK-LABEL: (func_decl{{.*}} "test(value:)"
func test(value: A) {
    // CHECK: (switch_stmt
    switch value {
    // CHECK: (case_stmt
    // CHECK:   (case_label_item ownership=borrowing
    // CHECK:     (pattern_expr type="A" {{.*}} ownership=borrowing
    case B():
        break
    // CHECK: (case_stmt
    // CHECK:   (case_label_item ownership=borrowing
    // CHECK:     (pattern_expr type="A" {{.*}} ownership=borrowing
    case C():
        break
    // CHECK: (case_stmt
    // CHECK:   (case_label_item ownership=borrowing
    // CHECK:     (pattern_expr type="A" {{.*}} ownership=consuming
    case D():
        break
    // CHECK: (case_stmt
    // CHECK:   (case_label_item ownership=borrowing
    // CHECK:     (pattern_expr type="A" {{.*}} ownership=borrowing
    case E():
        break
    // CHECK: (case_stmt
    // CHECK:   (case_label_item ownership=borrowing
    // CHECK:     (pattern_expr type="A" {{.*}} ownership=borrowing
    case F():
        break
    // CHECK: (case_stmt
    // CHECK:   (case_label_item ownership=borrowing
    // CHECK:     (pattern_expr type="A" {{.*}} ownership=consuming
    case G():
        break
    // CHECK: (case_stmt
    // CHECK:   (case_label_item default ownership=borrowing
    default:
        break
    }
}
