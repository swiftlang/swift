// RUN: not %target-swift-frontend %s -typecheck

protocol ProtocolWithCount: Collection {
    var count : UInt64 { get }
}

class ClassWithoutCount : ProtocolWithCount {
//    var count: UInt64 = 0
    var startIndex: UInt64 { get { return 0 }}
    var endIndex: UInt64 { get { return 0 }}
    subscript(i:UInt64) -> Int64 { get {return 0}}
}

