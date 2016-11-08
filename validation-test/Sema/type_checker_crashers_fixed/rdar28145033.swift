// RUN: not %target-swift-frontend %s -parse
 
let a = [1]
_ = a.index(of: a.min()) // a.min() returns an optional
