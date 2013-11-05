// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: echo "var x : Builtin.Int32" | %swift -module-name import_builtin -parse-stdlib -emit-module -o %t -
// RUN: echo "def foo() -> Int { return false }" > %t/import_text.swift
// RUN: echo "def pho$(printf '\xC3\xBB')x() -> Int { return false }" > %t/fran$(printf '\xC3\xA7')ais.swift
// RUN: %swift %s -I=%t -sdk= -verify
// RUN: not %swift %s -I=%t -sdk= 2>&1 | FileCheck %s

import Builtin  // expected-error {{no such module 'Builtin'}}

import import_builtin
def indirectBuiltin() {
  println(Int(Int32(import_builtin.x)))
}

def f0() {
  import swift // expected-error{{declaration is only valid at file scope}}
}

import def swift.print
def f1(a: swift.Int) -> swift.Void { print(a) }

import def swift.print

// rdar://14418336
#import something_nonexistant // expected-error {{invalid character in source file}} expected-error {{no such module 'something_nonexistant'}}

// Import specific decls
import typealias swift.Int
import struct swift.Int
import typealias swift.Object
import class swift.Object
import typealias swift.Bool
import enum swift.Bool
import protocol swift.Generator
import var swift.true
import def swift.min

import var x // expected-error {{expected module name}}
import struct swift.nonexistent // expected-error {{no such decl in module}}

import swift.import.abc // expected-error 2 {{expected identifier}}
import where swift.Int // expected-error {{expected identifier}}
import 2 // expected-error {{expected identifier}}

// CHECK-NOT: no such module 'really'
import really.nonexistent // expected-error {{no such module}}


import import_text // no-warning despite function body problems
var _ : Int = foo()

import français
import def français.phoûx
