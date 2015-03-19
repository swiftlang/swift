// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -enable-testing -o %t %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -parse -I %t %s -verify

@testable import Swift // expected-error {{module 'Swift' was not compiled for testing}}
@testable import empty // no-error

@testable func foo() {} // expected-error {{@testable may only be used on 'import' declarations}}
