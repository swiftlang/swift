// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -DNOCRASH1
// RUN: %target-swift-frontend %s -typecheck -DNOCRASH1 -use-clang-function-types
// RUN: %target-swift-frontend %s -typecheck -DNOCRASH2 -sdk %clang-importer-sdk
// RUN: %target-swift-frontend %s -typecheck -DNOCRASH2 -sdk %clang-importer-sdk -use-clang-function-types
// RUN: %target-swift-frontend %s -DAUXMODULE -module-name Foo -emit-module -o %t

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
public var DUMMY_SIGNAL : Optional<@convention(c) (Int32) -> Void> = .none
#endif

#if CRASH
import Foo
public func my_signal() -> Optional<@convention(c) (Int32) -> Void> {
  return Foo.DUMMY_SIGNAL
}
#endif
