// RUN: not %target-swift-frontend %s -typecheck

let a = [1]
_ = a.firstIndex(of: a.min()) // a.min() returns an optional
