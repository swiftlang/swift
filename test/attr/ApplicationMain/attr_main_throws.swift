// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s
// RUN: %target-swift-frontend -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-AST

@main
struct MyBase {
  static func main() throws {
  }
}

// CHECK-AST: (func_decl {{.*}} implicit range=[{{.*}}:[[@LINE-6]]:1 - line:[[@LINE-6]]:1] "$main()" interface_type="(MyBase.Type) -> () throws -> ()" access=internal static
// CHECK-AST-NEXT:  (parameter "self" {{.*}})
// CHECK-AST-NEXT:  (parameter_list)
// CHECK-AST-NEXT:  (brace_stmt implicit
// CHECK-AST-NEXT:    (return_stmt implicit
// CHECK-AST-NEXT:      (try_expr implicit
// CHECK-AST-NEXT:        (call_expr implicit type="()"
