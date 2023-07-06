// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// I don't know why, but this must be invoked through the driver otherwise it doesn't reproduce
// RUN: %target-swiftc_driver -typecheck -strict-concurrency=targeted -I %t %t/main.swift %t/ProtocolBazWithPreConcurrency.swift 2>&1 | %FileCheck %s --check-prefix=CHECK-WITH-PRECONCURRENCY
// RUN: %target-swiftc_driver -typecheck -strict-concurrency=targeted -I %t %t/main.swift %t/ProtocolBazWithoutPreConcurrency.swift 2>&1 | %FileCheck %s --check-prefix=CHECK-WITHOUT-PRECONCURRENCY

//--- module.modulemap

module Bar {
    header "bar.h"
}

//--- bar.h

@protocol NonSendableObjCProtocolBar
@end

//--- ProtocolBazWithoutPreConcurrency.swift

import Bar
// CHECK-WITHOUT-PRECONCURRENCY: ProtocolBazWithoutPreConcurrency.swift:2:1: remark: add '@preconcurrency' to suppress 'Sendable'-related warnings from module 'Bar'

protocol SwiftProtocolBaz {
  func qux(_ param: NonSendableObjCProtocolBar) async
}

//--- ProtocolBazWithPreConcurrency.swift

@preconcurrency import Bar
// CHECK-WITH-PRECONCURRENCY-NOT: ProtocolBazWithPreConcurrency.swift:2:17: remark: '@preconcurrency' attribute on module 'Bar' is unused
// Which is it? Do I need preconcurrency or not?

protocol SwiftProtocolBaz {
  func qux(_ param: NonSendableObjCProtocolBar) async
}

//--- main.swift

import Bar

actor ActorFoo: SwiftProtocolBaz {
// imho if we are going to require @preconcurrency at the location of the protocol definition, we should also emit these diagnostics there
  func qux(_ param: NonSendableObjCProtocolBar) async {
// CHECK-WITHOUT-PRECONCURRENCY:  main.swift:5:8: warning: non-sendable type 'any NonSendableObjCProtocolBar' in parameter of the protocol requirement satisfied by actor-isolated instance method 'qux' cannot cross actor boundary
// CHECK-WITH-PRECONCURRENCY-NOT: main.swift:5:8: warning: non-sendable type 'any NonSendableObjCProtocolBar' in parameter of the protocol requirement satisfied by actor-isolated instance method 'qux' cannot cross actor boundary
  }
}
