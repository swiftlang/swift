// RUN: %target-swift-frontend %s -emit-sil -o - | %FileCheck %s

public final class GenClass<Element: Cl> {
    public subscript(index: Int) -> Element {
        get { return unsafeBitCast(0, to: Element.self) }
    }
}

public protocol Proto { }

public struct Iter<Element: Proto>: IteratorProtocol {
    public mutating func next() -> Element? { return nil }
}

extension GenClass: RandomAccessCollection {
    public func makeIterator() -> Iter<Element> { return Iter() }
    public var startIndex: Int { return 0 }
    public var endIndex: Int { return 0 }
}

open class Cl: Proto { }

class Bar: Cl {
    var x: Int?
}

// CHECK-LABEL: sil hidden @$s4main5crash4barsSbAA8GenClassCyAA3BarCG_tF
func crash(bars: GenClass<Bar>) -> Bool {
    // CHECK: apply [[FN:%.*]]<Bar, [Bar]>
    return Array(bars.filter { $0.x == nil }).isEmpty
}
