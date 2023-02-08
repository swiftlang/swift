// RUN: %empty-directory(%t) 
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

func foo(_ x: ((_ x: Int, _ y: Int) -> Void)?) {
  x?(1, #^OPTIONAL_PARAMETER^#)
  // OPTIONAL_PARAMETER: Begin completions
  // OPTIONAL_PARAMETER-DAG: Literal[Integer]/None/TypeRelation[Convertible]: 0[#Int#]; name=0
  // OPTIONAL_PARAMETER: End completions
}
