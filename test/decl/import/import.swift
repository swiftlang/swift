// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: echo "public struct X {}; public var x = X()" | %target-swift-frontend -module-name import_builtin -parse-stdlib -emit-module -o %t -
// RUN: echo "public func foo() -> Int { return false }" > %t/import_text.swift
// RUN: echo "public func pho$(printf '\xC3\xBB')x() -> Int { return false }" > %t/fran$(printf '\xC3\xA7')ais.swift
// RUN: %target-swift-frontend -parse %s -I %t -sdk "" -enable-source-import -module-name main -verify -show-diagnostics-after-fatal

import Builtin  // expected-error {{no such module 'Builtin'}}

import import_builtin

extension Int32 {
  init(_: import_builtin.X) { }
}

func indirectBuiltin() {
  Int(Int32(import_builtin.x)) // expected-warning{{unused}}
}

func f0() {
  import Swift // expected-error{{declaration is only valid at file scope}}
}

import func Swift.print
func f1(a: Swift.Int) -> Swift.Void { print(a) }

import func Swift.print

// rdar://14418336
#import something_nonexistant // expected-error {{expected expression}} expected-error {{no such module 'something_nonexistant'}}

// Import specific decls
import typealias Swift.Int
import struct Swift.Int
import typealias Swift.ManagedBuffer
import class Swift.ManagedBuffer
import typealias Swift.Bool
import struct Swift.Bool
import protocol Swift.GeneratorType
import var import_builtin.x
import func Swift.min

import var x // expected-error {{expected module name}}
import struct Swift.nonexistent // expected-error {{no such decl in module}}

import Swift.import.abc // expected-error 2 {{expected identifier}}
import where Swift.Int // expected-error {{expected identifier}}
import 2 // expected-error {{expected identifier}}

import really.nonexistent // expected-error {{no such module 'really.nonexistent'}}


import import_text // no-warning despite function body problems
var _ : Int = foo()

import français
import func français.phoûx

import main // expected-error {{cannot import module being compiled}}
