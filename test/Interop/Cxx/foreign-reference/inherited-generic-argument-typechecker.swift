// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking

// Native classes conform to AnyObject, but foreign reference types do not, so a
// base-constrained generic called with a derived FRT must not be ambiguous.

import InheritedGenericArgument

func add<T: BaseObj>(_ obj: T) {}

func test() {
  add(DerivedObj1.make())
  add(DerivedObj2.make())
}
