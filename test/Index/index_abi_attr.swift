// RUN: %target-swift-ide-test -print-indexed-symbols -include-locals -source-filename %s | %FileCheck %s

// Make sure we use the API decl for mangling the USR.
@abi(func bar())
public func foo() {}
// CHECK: [[@LINE-1]]:13 | function(public)/Swift | foo() | s:14swift_ide_test3fooyyF | Def | rel: 0
