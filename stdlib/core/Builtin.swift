//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// \brief Definitions that make elements of Builtin usable in real
/// code without gobs of boilerplate.  These APIs will probably *not*
/// be exposed outside the stdlib.

@transparent
func sizeof<T>(_:T.Type) -> Int {
  return Int(Builtin.sizeof(T.self))
}

@transparent
func sizeofValue<T>(_:T) -> Int {
  return sizeof(T.self)
}

@transparent
func alignof<T>(_:T.Type) -> Int {
  return Int(Builtin.alignof(T.self))
}

@transparent
func alignofValue<T>(_:T) -> Int {
  return alignof(T.self)
}

@transparent
func strideof<T>(_:T.Type) -> Int {
  return Int(Builtin.strideof(T.self))
}

@transparent
func strideofValue<T>(_:T) -> Int {
  return strideof(T.self)
}

func roundUpToAlignment(offset: Int, alignment: Int) -> Int {
  return (offset + alignment - 1) / alignment * alignment
}

@transparent
func _canBeClass<T>(_: T.Type) -> Bool {
  return Bool(Builtin.canBeClass(T.self))
}

/// A brutal bit-cast of something to anything of the same size
@transparent
func reinterpretCast<T, U>(var x: T) -> U {
  _precondition(sizeof(T.self) == sizeof(U.self),
    "can't reinterpretCast values of different sizes")
  return UnsafePointer<U>(Builtin.addressof(&x)).pointee
}

/// `reinterpretCast` something to `AnyObject`
@transparent
func _reinterpretCastToAnyObject<T>(x: T) -> AnyObject {
  return reinterpretCast(x)
}

@transparent
func ==(lhs: Builtin.NativeObject, rhs: Builtin.NativeObject) -> Bool {
  return (reinterpretCast(lhs) as Int) == (reinterpretCast(rhs) as Int)
}

@transparent
func !=(lhs: Builtin.NativeObject, rhs: Builtin.NativeObject) -> Bool {
  return (reinterpretCast(lhs) as Int) != (reinterpretCast(rhs) as Int)
}

@transparent
func ==(lhs: Builtin.RawPointer, rhs: Builtin.RawPointer) -> Bool {
  return (reinterpretCast(lhs) as Int) == (reinterpretCast(rhs) as Int)
}

@transparent
func !=(lhs: Builtin.RawPointer, rhs: Builtin.RawPointer) -> Bool {
  return (reinterpretCast(lhs) as Int) != (reinterpretCast(rhs) as Int)
}

/// Tell the optimizer that this code is unreachable if condition is
/// known at compile-time to be true.  If condition is false, or true
/// but not a compile-time constant, this call has no effect.
@transparent
func _unreachable(condition: Bool = true) {
  if condition {
    // FIXME: use a parameterized version of Builtin.unreachable when
    // <rdar://problem/16806232> is closed.
    Builtin.unreachable()
  }
}

/// Tell the optimizer that this code is unreachable if this builtin is
/// reachable after constant folding build configuration builtins.
@transparent @noreturn
func _conditionallyUnreachable() {
  Builtin.conditionallyUnreachable()
}

@asmname("swift_isClassOrObjCExistential")
func _swift_isClassOrObjCExistential<T>(x: T.Type) -> Bool

/// Returns true iff T is a class type or an @objc existential such as
/// AnyObject
func _isClassOrObjCExistential<T>(x: T.Type) -> Bool {
  return _canBeClass(x)
    // FIXME: Dirty hack; see <rdar://problem/16823238>
    && sizeof(x) == sizeof(AnyObject) 
    && _swift_isClassOrObjCExistential(x)
}

//===----------------------------------------------------------------------===//
// Branch hints
//===----------------------------------------------------------------------===//

@transparent
func _branchHint<C: LogicValue>(actual: C, expected: Bool) -> Bool {
  return Bool(Builtin.int_expect_Int1(actual.getLogicValue().value, expected.value))
}

@transparent
func _fastPath<C: LogicValue>(x: C) -> Bool {
  return _branchHint(x.getLogicValue(), true)
}

@transparent
func _slowPath<C: LogicValue>(x: C) -> Bool {
  return _branchHint(x.getLogicValue(), false)
}

