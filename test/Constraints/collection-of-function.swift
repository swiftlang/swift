// RUN: %target-typecheck-verify-swift

func foo(_:[() -> ()]){}
func foo(_:[() throws -> ()]){}

func x() {}

var bs = [x]

foo(bs)
