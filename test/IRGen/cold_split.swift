// RUN: %target-swift-frontend %s -module-name=test -emit-assembly \
// RUN:   -enable-throws-prediction -O -enable-split-cold-code \
// RUN:       | %FileCheck --check-prefix CHECK-ENABLED %s

//// Test disabling just the pass doesn't yield a split.
// RUN: %target-swift-frontend %s -module-name=test -emit-assembly \
// RUN:   -enable-throws-prediction -O -disable-split-cold-code \
// RUN:       | %FileCheck --check-prefix CHECK-DISABLED %s

//// Test using -Osize doesn't yield a split.
// RUN: %target-swift-frontend %s -module-name=test -emit-assembly \
// RUN:   -enable-throws-prediction -Osize -enable-split-cold-code \
// RUN:       | %FileCheck --check-prefix CHECK-DISABLED %s

//// Test disabling optimization entirely doesn't yield a split.
// RUN: %target-swift-frontend %s -module-name=test -emit-assembly \
// RUN:   -enable-throws-prediction -enable-split-cold-code \
// RUN:       | %FileCheck --check-prefix CHECK-DISABLED %s


// CHECK-ENABLED: cold

// CHECK-DISABLED-NOT: cold

enum MyError: Error { case err }

func getRandom(_ b: Bool) throws -> Int {
    if b {
        return Int.random(in: 0..<1024)
    } else {
        throw MyError.err
    }
}

public func numberWithLogging(_ b: Bool) -> Int {
    do {
        return try getRandom(b)
    } catch {
        print("Log: random number generator failed with b=\(b)")
        return 1337
    }
}
