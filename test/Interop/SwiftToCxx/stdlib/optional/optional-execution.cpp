// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/use-optional.swift -module-name UseOptional -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/UseOptional.h

// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++20 -c %t/optional-execution.cpp -I %t -o %t/swift-stdlib-execution.o
// RUN: %target-build-swift %t/use-optional.swift -o %t/swift-stdlib-execution -Xlinker %t/swift-stdlib-execution.o -module-name UseOptional -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/swift-stdlib-execution
// RUN: %target-run %t/swift-stdlib-execution | %FileCheck %s

// REQUIRES: executable_test

//--- use-optional.swift

@_expose(Cxx)
public struct SmallStruct {
    public let x: Int16
}

@_expose(Cxx)
public enum RawEnum: Int16 {
    case first = 1
    case second = 3
}

@_expose(Cxx)
public class Klass {
    public let x: Int16

    init(_ x: Int16) {
        self.x = x
        print("init-Klass")
    }

    deinit {
        print("deinit-Klass")
    }
}

@_expose(Cxx)
public func createCIntOpt(_ val: CInt) -> Optional<CInt> {
    return val
}

@_expose(Cxx)
public func takeCIntOpt(_ val: Optional<CInt>) {
    print(String(describing: val))
}

@_expose(Cxx)
public func createSmallStructOpt(_ val: Int16) -> Optional<SmallStruct> {
    return SmallStruct(x: val)
}

@_expose(Cxx)
public func takeSmallStructOpt(_ val: Optional<SmallStruct>) {
    print(String(describing: val))
}

@_expose(Cxx)
public func createKlassOpt(_ val: Int16) -> Klass? {
    return Klass(val)
}

@_expose(Cxx)
public func takeKlassOpt(_ val: Klass?) {
    print(String(describing: val))
}

@_expose(Cxx)
public func resetOpt<T>(_ val: inout Optional<T>) {
    val = .none
}

//--- optional-execution.cpp

#include <cassert>
#include "UseOptional.h"

int main() {
  using namespace swift;
  using namespace UseOptional;

  {
    auto val = createCIntOpt(2);
    takeCIntOpt(val);
    assert((bool)val);
    assert(val.isSome());
    assert(val.getUnsafelyUnwrapped() == 2);
    assert(val.get() == 2);
    resetOpt(val);
    assert(!(bool)val);
    assert(val.isNone());
    takeCIntOpt(val);
  }
// CHECK: Optional(2)
// CHECK-NEXT: nil
  {
    auto val = Optional<int>::some(-97);
    takeCIntOpt(val);
    assert((bool)val);
    assert(val.get() == -97);
    auto val2 = Optional<int>::none();
    assert(!(bool)val2);
    takeCIntOpt(val2);
  }
// CHECK-NEXT: Optional(-97)
// CHECK-NEXT: nil
  {
    auto val = createSmallStructOpt(0xFA);
    takeSmallStructOpt(val);
    assert((bool)val);
    assert(val.isSome());
    assert(val.getUnsafelyUnwrapped().getX() == 0xFA);
    assert(val.get().getX() == 0xFA);
    resetOpt(val);
    assert(!(bool)val);
    assert(val.isNone());
    takeSmallStructOpt(val);
  }
// CHECK-NEXT: Optional(UseOptional.SmallStruct(x: 250))
// CHECK-NEXT: nil
  {
    auto val = createKlassOpt(-256);
    takeKlassOpt(val);
    assert(val.isSome());
    auto ptr = val.getUnsafelyUnwrapped();
    assert(ptr.getX() == -256);
    resetOpt(val);
    assert(val.isNone());
    takeKlassOpt(val);
  }
// CHECK-NEXT: init-Klass
// CHECK-NEXT: Optional(UseOptional.Klass)
// CHECK-NEXT: nil
// CHECK-NEXT: deinit-Klass

  {
    auto val = RawEnum::init(1);
    assert(val.isSome());
    assert(val.getUnsafelyUnwrapped() == RawEnum::first);
    assert(val.getUnsafelyUnwrapped().getRawValue() == 1);
    auto val2 = RawEnum::init(2);
    assert(val2.isNone());
  }
  return 0;
}
