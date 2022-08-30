// RUN: %target-swift-ide-test -print-indexed-symbols -swift-version 5 -source-filename %s | %FileCheck %s

extension DefaultStringInterpolation {
    mutating func appendInterpolation(test value: Int) {
        appendInterpolation(value)
    }
}

"\(test: 1)"
// CHECK: [[@LINE-1]]:3 | instance-method/Swift | appendInterpolation(test:) | {{.*}} | Ref,Call | rel: 0