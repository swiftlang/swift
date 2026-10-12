// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol ISource: AnyObject {}
protocol NativeSource: AnyObject {}
final class NativeObject {}

func recover(_ source: any ISource, _ optional: (any ISource)?) {
  _ = source as? NativeObject
  _ = source as! NativeObject
  _ = source is NativeObject
  _ = optional as? NativeObject
  _ = optional as! NativeObject
  _ = optional is NativeObject
  _ = source as? Int // expected-warning {{cast from 'any ISource' to unrelated type 'Int' always fails}}
}

func native(_ source: any NativeSource) {
  _ = source as? NativeObject // expected-warning {{cast from 'any NativeSource' to unrelated type 'NativeObject' always fails}}
}
