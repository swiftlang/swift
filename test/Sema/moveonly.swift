// RUN: %target-typecheck-verify-swift -parse-stdlib -disable-availability-checking -verify-syntax-tree

import Swift

class Klass {}

func argumentsAndReturns(@_moveOnly _ x: Klass) -> /*@_moveOnly*/ Klass {
    return x
}
func passArguments(@_moveOnly _ x: Klass, @_moveOnly _ y : Klass) -> () {
    @_moveOnly let y = argumentsAndReturns(x)
    print(y)
}

func letDecls(@_moveOnly _ x: Klass) -> () {
    @_moveOnly let y: Klass = x
    print(y)
}

func varDecls(@_moveOnly _ x: Klass, @_moveOnly _ x2: Klass) -> () {
    @_moveOnly var y: Klass = x
    y = x2
    print(y)
}

func getKlass() -> @_moveOnly Builtin.NativeObject {
    let k = Klass()
    let b = Builtin.unsafeCastToNativeObject(k)
    return Builtin.move(b)
}

@_moveOnly var g: Builtin.NativeObject = getKlass()
@_moveOnly let g2: Builtin.NativeObject = getKlass()
@_moveOnly var g3: Builtin.NativeObject { getKlass() }

struct MyStruct {
    // Error if @_moveOnly on struct fields. We do not have move only types and
    // these are part of MyStruct.
    //
    // TODO: Make error specific for move only on struct/enum.
    @_moveOnly var x: Builtin.NativeObject = getKlass() // expected-error {{'@_moveOnly' attribute cannot be applied to stored properties}}
    @_moveOnly let y: Builtin.NativeObject = getKlass() // expected-error {{'@_moveOnly' attribute cannot be applied to stored properties}}

    // We do support @_moveOnly on computed properties though since they are
    // functions and we support @_moveOnly return values.
    @_moveOnly var myMoveOnly: Builtin.NativeObject {
        return getKlass()
    }
}

// Do not error on class fields. The moveOnly field is separate from the
// underlying class itself so the fact the class is not move only does not
// suggest that the binding inside the class can be.
class MyClass {
    @_moveOnly var x: Builtin.NativeObject = getKlass()
    @_moveOnly let y: Builtin.NativeObject = getKlass()

    @_moveOnly var myMoveOnly: Builtin.NativeObject {
        return getKlass()
    }
}

// We need to error on Enums since the case is part of the value and we do not
// support move only types.
enum MyEnum {
    case none
    case moveOnlyCase(Klass)

    // We suport doing it on computed properties though.
    @_moveOnly var myMoveOnly: Builtin.NativeObject {
        return getKlass()
    }
}

