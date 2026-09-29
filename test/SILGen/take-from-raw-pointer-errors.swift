// RUN: %target-swift-emit-silgen -enable-builtin-module -verify %s

import Builtin

func value(_ pointer: Builtin.RawPointer) -> Int {
  Builtin.takeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: takeFromRawPointer result must be a single reference}}
}

func generic<T>(_ pointer: Builtin.RawPointer) -> T {
  Builtin.takeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: takeFromRawPointer result must be a single reference}}
}

protocol P {}
func existential(_ pointer: Builtin.RawPointer) -> any P {
  Builtin.takeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: takeFromRawPointer result must be a single reference}}
}

class Object {}
func optional(_ pointer: Builtin.RawPointer) -> Object? {
  Builtin.takeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: takeFromRawPointer result must be a single reference}}
}

protocol ClassProtocol: AnyObject {}
func classExistential(_ pointer: Builtin.RawPointer) -> any ClassProtocol {
  Builtin.takeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: takeFromRawPointer result must be a single reference}}
}
