// RUN: not %target-swift-frontend -typecheck %s

let test1 = try Test1<Test2>(
    test2: Test2(
        test3_1: Test3(
            value1: [],
            value2: ["1"],
            value3: ["2", "2"],
            value4: ["3"],
            value5: ["\"9\", \"9\", \"9\""],
            value6: [],
            value7: ["UInt8(7)"]
        ),
        test3_2: Test3(
            value1: ["3, 3, 3"],
            value2: ["4 4, 4 4"],
            value3: [StaticString("5, 5, 5, 5, 5")],
            value4: [],
            value5: ["6"],
            value6: ["\"8\", 8"],
            value7: []
        )
    )
)

public protocol Test1Protocol: Sendable {
}
public final class Test1<ConcreteTest2: Test2Protocol>: Test1Protocol {
    public var test2:ConcreteTest2
    
    public init(test2: ConcreteTest2) throws {
        self.test2 = test2
    }
}

public protocol Test2Protocol: Sendable {
}
public struct Test2<
        ConcreteTest3_1: Test3Protocol,
        ConcreteTest3_2: Test3Protocol
    >: Test2Protocol {
    public private(set) var test3_1:ConcreteTest3_1
    public private(set) var test3_2:ConcreteTest3_2

    public init(
        test3_1: ConcreteTest3_1,
        test3_2: ConcreteTest3_2
    ) {
        self.test3_1 = test3_1
        self.test3_2 = test3_2
    }
}

public protocol Test3Protocol: CustomDebugStringConvertible, Sendable {
}
public struct Test3<
        let count1: Int,
        let count2: Int,
        let count3: Int,
        let count4: Int,
        let count5: Int,
        let count6: Int,
        let count7: Int
    >: Test3Protocol {
    public let value1:InlineArray<count1, Value<StaticString>>
    public let value2:InlineArray<count2, Value<StaticString>>
    public let value3:InlineArray<count3, Value<StaticString>>
    public let value4:InlineArray<count4, Value<StaticString>>
    public let value5:InlineArray<count5, Value<StaticString>>
    public let value6:InlineArray<count6, Value<StaticString>>
    public let value7:InlineArray<count7, Value<StaticString>>

    public init(
        value1: InlineArray<count1, Value<StaticString>>,
        value2: InlineArray<count2, Value<StaticString>>,
        value3: InlineArray<count3, Value<StaticString>>,
        value4: InlineArray<count4, Value<StaticString>>,
        value5: InlineArray<count5, Value<StaticString>>,
        value6: InlineArray<count6, Value<StaticString>>,
        value7: InlineArray<count7, Value<StaticString>>
    ) {
        self.value1 = value1
        self.value2 = value2
        self.value3 = value3
        self.value4 = value4
        self.value5 = value5
        self.value6 = value6
        self.value7 = value7
    }

    public var debugDescription: String { "" }

    public struct Value<T: Sendable>: Sendable {
        public let value:T

        public init(_ value: T) {
            self.value = value
        }
    }
}
