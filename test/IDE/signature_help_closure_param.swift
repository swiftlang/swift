// RUN: %target-swift-ide-test -signature-help -code-completion-token=CLOSURE_PARAM -source-filename=%s | %FileCheck %s --check-prefix=CLOSURE_PARAM

func apply<Value, Result>(value: Value, body: (Value) -> Result) -> Result {
  return body(#^CLOSURE_PARAM^#)
  // CLOSURE_PARAM:     Begin signatures, 1 items
  // CLOSURE_PARAM-DAG: Signature[Active]: body(<param active>Value</param>) -> Result
}
