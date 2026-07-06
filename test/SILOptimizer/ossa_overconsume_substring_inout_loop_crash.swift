// RUN: %target-swift-frontend -O -c -parse-as-library -module-name repro %s -o %t.o

// REQUIRES: swift_in_compiler

// https://github.com/swiftlang/swift/issues/89255
// SIL ownership verifier rejects the optimized SIL for a simple inout-Substring
// loop with "Found over consume?!" / "Found ownership error?!" during the
// OwnershipModelEliminator pass. Regression after swift-6.3.1-RELEASE.

public func f(_ s: inout Substring, _ b: Bool) -> Substring? {
    while !s.isEmpty {
        let c = s; var i = c.startIndex; var x: Substring.Index?
        while i < c.endIndex { if b { x = i }; c.formIndex(after: &i) }
        s = ""
        guard let x else { continue }
        return c[..<x]
    }
    return nil
}
