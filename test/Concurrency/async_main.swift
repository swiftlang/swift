// RUN: %target-swift-frontend -dump-ast -enable-experimental-concurrency -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-AST
// REQUIRES: concurrency

func asyncFunc() async { }

@main struct MyProgram {
  static func main() async {
    await asyncFunc()
  }
}


// CHECK-AST-LABEL: "main()" interface
// CHECK-AST:       (await_expr type='()'
// CHECK-AST-NEXT:    (call_expr type='()'
// CHECK-AST-NEXT:       (declref_expr type='() async -> ()'
// CHECK-AST-SAME:        decl=async_main.(file).asyncFunc()@

// CHECK-AST-LABEL: (func_decl implicit "$main()" interface
// CHECK-AST:       (brace_stmt
// CHECK-AST-NEXT:    (return_stmt implicit
// CHECK-AST-NEXT:      (call_expr implicit type='()'
// CHECK-AST-NEXT:        (declref_expr implicit
// CHECK-AST-SAME:             type='(() async throws -> ()) -> ()'
// CHECK-AST-SAME:             decl=_Concurrency.(file)._runAsyncMain
// CHECK-AST-SAME:             function_ref=single
// CHECK-AST-NEXT:        (paren_expr implicit type='(() async throws -> ())'
// CHECK-AST-NEXT:          (function_conversion_expr implicit type='() async throws -> ()'
// CHECK-AST-NEXT:          (dot_syntax_call_expr
// CHECK-AST-NEXT:          (autoclosure_expr implicit type='(MyProgram.Type) -> () async -> ()'
