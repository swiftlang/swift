// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/dynamic_self -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/dynamic_self -type-from-mangled=%t/input | %FileCheck %s

class Me {
  func mine() -> Self {
    let metatype = type(of: self)
    return metatype.init()
  }

  required init() {}
}

// DEMANGLE: $s12dynamic_self2MeCXDXMTD
// CHECK: @thick Self.Type
