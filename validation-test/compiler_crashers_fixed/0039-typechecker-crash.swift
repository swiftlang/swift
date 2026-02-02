// RUN: not %target-swift-frontend %s -emit-silgen

struct BInt {}
typealias IncompleteRange = (BInt?, BInt?)
extension IncompleteRange {}
