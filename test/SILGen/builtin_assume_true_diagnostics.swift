// RUN: %target-swift-emit-silgen -enable-builtin-module -verify %s

import Builtin

func isLess(_ x: Int, _ y: Int) -> Bool { x < y }

infix operator <<< : ComparisonPrecedence
func <<< (_ x: Int, _ y: Int) -> Bool { x < y }

struct Box { var value: Int }
func <= (_ x: Box, _ y: Box) -> Bool { x.value <= y.value }

func closure(_ x: Int, _ y: Int) {
  Builtin.assumeTrue(x, { $0 < $1 }, y) // expected-error {{'Builtin.assumeTrue' only recognizes the following operators: '==', '!=', '<', '<=', '>', '>='}}
}

func function(_ x: Int, _ y: Int) {
  Builtin.assumeTrue(x, isLess, y) // expected-error {{'Builtin.assumeTrue' only recognizes the following operators: '==', '!=', '<', '<=', '>', '>='}}
}

func customOperator(_ x: Int, _ y: Int) {
  Builtin.assumeTrue(x, <<<, y) // expected-error {{'Builtin.assumeTrue' only recognizes the following operators: '==', '!=', '<', '<=', '>', '>='}}
}

func userDefinedOperator(_ x: Box, _ y: Box) {
  Builtin.assumeTrue(x, <=, y) // expected-error {{'Builtin.assumeTrue' only recognizes the following operators: '==', '!=', '<', '<=', '>', '>='}}
}

func otherStandardLibraryOperator(_ x: Int, _ y: Int) {
  Builtin.assumeTrue(x, ~=, y) // expected-error {{'Builtin.assumeTrue' only recognizes the following operators: '==', '!=', '<', '<=', '>', '>='}}
}

func operatorInVariable(_ x: Int, _ y: Int) {
  let isLessOrEqual: (Int, Int) -> Bool = (<=)
  Builtin.assumeTrue(x, isLessOrEqual, y) // expected-error {{'Builtin.assumeTrue' only recognizes the following operators: '==', '!=', '<', '<=', '>', '>='}}
}

struct Version: Comparable {
  var number: Int
  static func < (_ x: Version, _ y: Version) -> Bool { x.number < y.number }
}

func floatingPoint(_ x: Double, _ y: Double) {
  Builtin.assumeTrue(x, <=, y) // expected-error {{'Builtin.assumeTrue' only recognizes the standard library's integer types, not 'Double'}}
}

func optionalInteger(_ x: Int?, _ y: Int?) {
  Builtin.assumeTrue(x, ==, y) // expected-error {{'Builtin.assumeTrue' only recognizes the standard library's integer types, not 'Int?'}}
}

func comparableStruct(_ x: Version, _ y: Version) {
  Builtin.assumeTrue(x, <=, y) // expected-error {{'Builtin.assumeTrue' only recognizes the standard library's integer types, not 'Version'}}
}
