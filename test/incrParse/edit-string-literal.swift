// RUN: %validate-incrparse %s

func foo() {
}

func bar() {
  let x = "<<<hi|||hello>>> world"
  let y = 1
}

func anotherMethod() {
}
