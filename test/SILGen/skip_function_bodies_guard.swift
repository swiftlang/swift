// RUN: %target-swift-frontend -experimental-skip-non-inlinable-function-bodies-without-types -emit-module %s

let s: Int? = nil
guard let m = s else { fatalError() }

let x = m

public func f(_: Int = m, _: String = "") {}

f()
