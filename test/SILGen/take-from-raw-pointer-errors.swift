// RUN: %target-swift-emit-silgen -enable-builtin-module -verify %s

import Builtin

func value(_ pointer: Builtin.RawPointer) -> Int {
  Builtin.takeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: takeFromRawPointer result must have an object or interface pointer representation}}
}

func generic<T>(_ pointer: Builtin.RawPointer) -> T {
  Builtin.takeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: takeFromRawPointer result must have an object or interface pointer representation}}
}

protocol P {}
func existential(_ pointer: Builtin.RawPointer) -> any P {
  Builtin.takeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: takeFromRawPointer result must have an object or interface pointer representation}}
}

class Object {}
func optional(_ pointer: Builtin.RawPointer) -> Object? {
  Builtin.takeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: takeFromRawPointer result must have an object or interface pointer representation}}
}

protocol ClassProtocol: AnyObject {}
func classExistential(_ pointer: Builtin.RawPointer) -> any ClassProtocol {
  Builtin.takeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: takeFromRawPointer result must have an object or interface pointer representation}}
}
