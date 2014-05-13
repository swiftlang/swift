// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: echo "struct X {}; var x = X()" | %swift -module-name import_builtin -parse-stdlib -emit-module -o %t -
// RUN: echo "func foo() -> Int { return false }" > %t/import_text.swift
// RUN: echo "func pho$(printf '\xC3\xBB')x() -> Int { return false }" > %t/fran$(printf '\xC3\xA7')ais.swift
// RUN: %swift %s -I=%t -sdk "" -enable-source-import -verify -show-diagnostics-after-fatal
// RUN: not %swift %s -I=%t -sdk "" -enable-source-import 2>&1 | FileCheck %s

import Builtin  // expected-error {{no such module 'Builtin'}}

import import_builtin

extension Int32 {
  init(_: import_builtin.X) { }
}

func indirectBuiltin() {
  println(Int(Int32(import_builtin.x)))
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
import typealias Swift.HeapBufferStorage
import class Swift.HeapBufferStorage
import typealias Swift.Bool
import struct Swift.Bool
import protocol Swift.Generator
import var Swift.true
import func Swift.min

import var x // expected-error {{expected module name}}
import struct Swift.nonexistent // expected-error {{no such decl in module}}

import Swift.import.abc // expected-error 2 {{expected identifier}}
import where Swift.Int // expected-error {{expected identifier}}
import 2 // expected-error {{expected identifier}}

// CHECK-NOT: no such module 'really'
import really.nonexistent // expected-error {{no such module}}


import import_text // no-warning despite function body problems
var _ : Int = foo()

import français
import func français.phoûx
