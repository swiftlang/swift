// RUN: %swift -cfg-dump %s | FileCheck %s

// CHECK: typealias_decl
func  typealias_decl() {
  typealias a = Int
}


