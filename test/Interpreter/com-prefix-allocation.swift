// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -module-link-name COM -emit-module-path %t/COM.swiftmodule -enable-library-evolution %S/../Inputs/COM.swift
// RUN: %target-build-swift-dylib(%t/%target-library-name(PrefixLibrary)) -Xfrontend -enable-experimental-com-interop -I %t -L %t -module-name PrefixLibrary -module-link-name PrefixLibrary -emit-module-path %t/PrefixLibrary.swiftmodule -enable-library-evolution %S/Inputs/com-prefix-resilient.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -I %t -L %t -module-name main -O %s -o %t/test.exe %target-rpath(%t)
// RUN: %target-codesign %t/test.exe %t/%target-library-name(COM) %t/%target-library-name(PrefixLibrary)
// RUN: %target-run %t/test.exe | %FileCheck %s

// REQUIRES: executable_test

import PrefixLibrary

@com(interface: "30000000-0000-0000-0000-000000000002")
protocol IBase {
}

@com(interface: "30000000-0000-0000-0000-000000000003")
protocol IIndependent {
}

@com
class Base: IBase {
  required init() {
  }
}

@com
final class Derived: Base, IIndependent {
}

@_alignment(16)
struct OverAligned {
  var value: Int
}

@com
final class Generic<T>: IBase, IIndependent {
  let value: T

  init(_ value: T) {
    self.value = value
  }
}

@inline(never)
func instantiate<T: Base>(_ type: T.Type) -> T {
  type.init()
}

func prefix(_ object: AnyObject, count: Int) -> Bool {
  let address = Unmanaged.passUnretained(object).toOpaque()
  let word = MemoryLayout<UnsafeRawPointer>.stride
  for index in 1 ... count {
    if address.advanced(by: -index * word)
              .load(as: UnsafeRawPointer?.self) == nil {
      return false
    }
  }
  return true
}

// A statically allocated class receives its complete prefix.

let fixed = Base()
print("fixed: \(prefix(fixed, count: 2))")

// Dynamic allocation uses the subclass's larger prefix.

let dynamic = instantiate(Derived.self)
print("dynamic: \(prefix(dynamic, count: 3))")

// Prefixing preserves the native object's requested alignment.

let generic = Generic(OverAligned(value: 42))
let address = Unmanaged.passUnretained(generic).toOpaque()
print("generic: \(prefix(generic, count: 3) && generic.value.value == 42)")
print("aligned: \(Int(bitPattern: address) & 15 == 0)")

// PrefixLibrary was built resiliently, so allocation obtains the prefix size
// and template from dynamic metadata.

let resilient = ResilientCOMObject()
print("resilient: \(prefix(resilient, count: 2) && resilient.value == 3)")

// CHECK: fixed: true
// CHECK-NEXT: dynamic: true
// CHECK-NEXT: generic: true
// CHECK-NEXT: aligned: true
// CHECK-NEXT: resilient: true
