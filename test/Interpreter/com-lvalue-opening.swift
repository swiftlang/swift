// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -module-link-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -I %t -module-name main %s -o %t/test.exe -L %t %target-rpath(%t)
// RUN: %target-codesign %t/test.exe %t/%target-library-name(COM)
// RUN: %target-run %t/test.exe | %FileCheck %s

// REQUIRES: executable_test

@com(interface: "48000000-0000-0000-0000-000000000001")
protocol IWidget {
  func value(_ result: UnsafeMutablePointer<Int32>?) -> Int32
}

@com
final class Widget: IWidget {
  deinit {
    print("deinit")
  }

  func value(_ result: UnsafeMutablePointer<Int32>?) -> Int32 {
    result?.pointee = 42
    return 0
  }
}

@inline(never)
func exchange<T: IWidget>(_ value: inout T) {
  var copy = value
  swap(&value, &copy)
}

struct Storage {
  var value: any IWidget
}

do {
  var storage = Storage(value: Widget())
  exchange(&storage.value)

  var result: Int32 = 0
  _ = storage.value.value(&result)
  print("value: \(result)")
}
print("done")

// CHECK:      value: 42
// CHECK-NEXT: deinit
// CHECK-NEXT: done
