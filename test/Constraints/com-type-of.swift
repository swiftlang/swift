// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

@com(interface: "43000000-0000-0000-0000-000000000001")
protocol IWidget {
}

func dynamicType(of value: borrowing any IWidget) -> Any.Type {
  type(of: value)
}

func classify(_: Any.Type) -> Bool {
  true
}

func classify(_: any IWidget.Type) -> Int {
  0
}

func selectsAnyMetatype(_ value: borrowing any IWidget) {
  let _: Bool = classify(type(of: value))
}
