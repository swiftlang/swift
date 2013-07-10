// RUN: swift-ide-test -code-completion -source-filename %s -code-completion-token=T1 2>&1 | FileCheck %s

// Check that the lexer stops at code completion token.
func foo() {
  1 + #^T1^# 42
}

// CHECK: error: expected expression after operator
// CHECK: {{^}}  1 +  42{{$}}
// CHECK: {{^}}       ^{{$}}

