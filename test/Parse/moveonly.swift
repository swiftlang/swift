// RUN: %target-typecheck-verify-swift  -disable-availability-checking -verify-syntax-tree

class Klass {}

func argumentsAndReturns(_ x: @_moveOnly Klass) -> @_moveOnly Klass {
    return x
}

func letDecls(_ x: @_moveOnly Klass) -> () {
    let y: @_moveOnly Klass = x
    print(y)
}

func varDecls(_ x: @_moveOnly Klass, _ x2: @_moveOnly Klass) -> () {
    var y: @_moveOnly Klass = x
    y = x2
    print(y)
}

// TODO: Globals
