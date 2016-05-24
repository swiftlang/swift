// RUN: %target-swift-frontend -import-objc-header %S/Inputs/extension-inheritance-conformance-objc.h -parse -verify -primary-file %s %S/Inputs/extension-inheritance-conformance-objc-multi-file-2.swift
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/extension-inheritance-conformance-objc.h -parse -verify %s -primary-file %S/Inputs/extension-inheritance-conformance-objc-multi-file-2.swift
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/extension-inheritance-conformance-objc.h -parse -verify -primary-file %S/Inputs/extension-inheritance-conformance-objc-multi-file-2.swift %s
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/extension-inheritance-conformance-objc.h -parse -verify %S/Inputs/extension-inheritance-conformance-objc-multi-file-2.swift -primary-file %s

// REQUIRES: objc_interop

protocol Foo {}

extension Foo {
  func bar() -> Self { return self }
}

extension Object: Foo {}

let x = Object().bar()
let y = Responder().bar()
let z = View().bar()

