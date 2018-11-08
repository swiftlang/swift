// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-private-imports -o %t %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules %s -verify

@_private(sourceFile: "Array.swift") import Swift // expected-error {{module 'Swift' was not compiled for private import}}
@_private(sourceFile: "empty.swift") import empty // no-error

@_private(sourceFile: "none") func foo() {} // expected-error {{@_private may only be used on 'import' declarations}} {{1-11=}}
