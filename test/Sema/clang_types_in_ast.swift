// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -DNOCRASH1
// RUN: %target-swift-frontend %s -typecheck -DNOCRASH1 -use-clang-function-types
// RUN: %target-swift-frontend %s -typecheck -DNOCRASH2 -sdk %clang-importer-sdk
// RUN: %target-swift-frontend %s -typecheck -DNOCRASH2 -sdk %clang-importer-sdk -use-clang-function-types
// RUN: %target-swift-frontend %s -DAUXMODULE -module-name Foo -emit-module -o %t

// rdar://problem/57644243 : We shouldn't crash if -use-clang-function-types is not enabled.
// RUN: %target-swift-frontend %s -typecheck -DNOCRASH3 -I %t

// FIXME: [clang-function-type-serialization] This should stop crashing once we
// start serializing clang function types.
// RUN: not --crash %target-swift-frontend %s -typecheck -DCRASH -I %t -use-clang-function-types

#if NOCRASH1
public func my_signal() -> Optional<@convention(c) (Int32) -> Void> {
  let s : Optional<@convention(c) (Int32) -> Void> = .none;
  var s2 : Optional<@convention(c) (Int32) -> Void> = s;
  return s2;
}
#endif

#if NOCRASH2
import ctypes
func f() {
  _ = getFunctionPointer() as (@convention(c) (CInt) -> CInt)?
}
#endif

#if AUXMODULE
public var DUMMY_SIGNAL1 : Optional<@convention(c) (Int32) -> ()> = .none
public var DUMMY_SIGNAL2 : Optional<@convention(c) (Int32) -> Void> = .none
#endif

#if NOCRASH3
import Foo
public func my_signal1() -> Optional<@convention(c) (Int32) -> ()> {
  return Foo.DUMMY_SIGNAL1
}
public func my_signal2() -> Optional<@convention(c) (Int32) -> Void> {
  return Foo.DUMMY_SIGNAL1
}
public func my_signal3() -> Optional<@convention(c) (Int32) -> ()> {
  return Foo.DUMMY_SIGNAL2
}
public func my_signal4() -> Optional<@convention(c) (Int32) -> Void> {
  return Foo.DUMMY_SIGNAL2
}
#endif

#if CRASH
import Foo
public func my_signal1() -> Optional<@convention(c) (Int32) -> ()> {
  return Foo.DUMMY_SIGNAL1
}
public func my_signal2() -> Optional<@convention(c) (Int32) -> Void> {
  return Foo.DUMMY_SIGNAL1
}
public func my_signal3() -> Optional<@convention(c) (Int32) -> ()> {
  return Foo.DUMMY_SIGNAL2
}
public func my_signal4() -> Optional<@convention(c) (Int32) -> Void> {
  return Foo.DUMMY_SIGNAL2
}
#endif
