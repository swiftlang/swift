// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

// CHECK: [[@LINE+1]]:8 | struct/Swift | Int | {{.*}} | Ref | rel: 0
var _: Int { get { return 1 } }
