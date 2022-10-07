// RUN: %target-typecheck-verify-swift

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test

import Swift

func check(_ things: Any...) {
  for thing in things {
    print(">\(String(describing: thing))<", type(of: thing))
  }
}

let a = '👩🏼‍🚀'

check('a'+1,
// CHECK: >98< Int
       "1"+"1",
// CHECK: >11< String
       "1"+'€',
// CHECK: >1€< String
       '1'+'1' as String,
// CHECK: >11< String
       '1'+'1' as Int,
// CHECK: >98< Int
       Int("0123") as Any,
// CHECK: >Optional(123)< Optional<Int>
       Int('3'),
// CHECK: >51< Int
       Int('€') as Any,
// CHECK: >nil< Optional<Int>
       '1' as Int+'1',
// CHECK: >98< Int
       "123".firstIndex(of: '2') as Any,
// CHECK: >Optional(Swift.String.Index(_rawBits: 65799))< Optional<Index>
       UInt8(ascii: 'a') == 'a',
// CHECK: >true< Bool
       '👩🏼‍🚀'.asciiValue as Any,
// CHECK: >nil< Optional<UInt8>
       UInt8(ascii: 'd') - 'a',
// CHECK: >3< UInt8
       ('😎' as UnicodeScalar).value,
// CHECK: >128526< UInt32
       a)
// CHECK: >👩🏼‍🚀< Character
