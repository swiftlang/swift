// RUN: %swift-ide-test -annotate -source-filename %s -sdk %sdk | FileCheck %s

// REQUIRES: sdk

import Foundation

func foo(c1: NSObject, c2: NSObject) {
  // CHECK: <Var@[[@LINE-1]]:10>c1</Var> <iFunc@>==</Func> <Var@[[@LINE-1]]:24>c2</Var>
  c1 == c2
}
