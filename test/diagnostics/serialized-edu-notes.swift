// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -enable-educational-notes -diagnostic-documentation-path /educational/notes/path/prefix -serialize-diagnostics-path %t/diagnostics.dia -typecheck %s
// RUN: llvm-bcanalyzer -dump %t/diagnostics.dia | %FileCheck %s


// A diagnostic with an educational note
extension (Int, Int) {}
// CHECK: <Diag
// CHECK: <DiagInfo
// CHECK: 'non-nominal type '(Int, Int)' cannot be extended'
// CHECK: <EduNotePath
// CHECK: '{{[/\\]+}}educational{{[/\\]+}}notes{{[/\\]+}}path{{[/\\]+}}prefix{{[/\\]+}}nominal-types.md'
// CHECK: </Diag>

// A diagnostic without an educational note
let x: Int = "hello, world!"
// CHECK: <Diag
// CHECK: <DiagInfo
// CHECK: 'cannot convert value of type 'String' to specified type 'Int''
// CHECK-NOT: <EduNotePath
// CHECK: </Diag>
