// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

import COM

@com(interface: "AAAAAAAA-BBBB-CCCC-DDDD-EEEEEEEEEEEE")
protocol IWidget {
}

final class Holder {
  weak var weakInterface: (any IWidget)?
  // expected-error@-1 {{'any IWidget' is incompatible with 'weak' references because COM interface existentials do not support managed reference storage}}
  // expected-note@-2 {{use 'unowned(unsafe)' to store the interface without ownership tracking}}

  unowned var unownedInterface: any IWidget
  // expected-error@-1 {{'any IWidget' is incompatible with 'unowned' references because COM interface existentials do not support managed reference storage}}
  // expected-note@-2 {{use 'unowned(unsafe)' to store the interface without ownership tracking}}

  unowned(unsafe) var unmanagedInterface: any IWidget

  init(_ interface: any IWidget) {
    unownedInterface = interface
    unmanagedInterface = interface
  }
}

// A native Swift reference to a @com class still points at the Swift heap
// object and therefore continues to use Swift's reference-storage operations.
@com(implementation: "AABBCCDD-EEFF-0011-2233-445566778899")
final class Widget: IWidget {
}

final class NativeHolder {
  weak var weakObject: Widget?
  unowned var unownedObject: Widget
  unowned(unsafe) var unmanagedObject: Widget

  init(_ object: Widget) {
    unownedObject = object
    unmanagedObject = object
  }
}
