// RUN: %target-swift-frontend -typecheck -verify %s

func foo(_:[() -> ()]){}
func foo(_:[() throws -> ()]){}

func x() {}

var bs = [x]

foo(bs)
