// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck %s -serialize-diagnostics-path %t/output.sarif.json 2>&1
// RUN: %S/validate-sarif.py %S/Outputs/sarif-json-extension.sarif.json %t/output.sarif.json %s

// Test .sarif.json extension support

let x = 5
x = 10
