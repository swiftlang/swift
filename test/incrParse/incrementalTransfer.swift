// RUN: %incr-transfer-tree %s --expected-incremental-syntax-tree %S/Outputs/incrementalTransfer.json

func foo() {
}

func bar() {
  let x = "<<<hi|||hello>>> world"
  let y = 1
}

func anotherMethod() {
}
