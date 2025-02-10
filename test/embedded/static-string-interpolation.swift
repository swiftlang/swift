// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -Xfrontend -throws-as-traps -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public struct MyInterpolation : StringInterpolationProtocol {
    public typealias StringLiteralType = StaticString
    public init(literalCapacity: Int, interpolationCount: Int) {}
    
    var literalCount = 0
    var interpolationCount = 0
    
    public mutating func appendLiteral(_ literal: StaticString) {
        print("appendLiteral")
        literalCount += 1
    }
    
    public mutating func appendInterpolation<T>(_ value: @autoclosure @escaping () -> T) {
        print("appendInterpolation<T>")
        interpolationCount += 1
    }
}

public struct MyMessage : ExpressibleByStringInterpolation {
    public typealias StringInterpolation = MyInterpolation
    var interpolation: MyInterpolation
    
    public init(stringInterpolation: MyInterpolation) {
        self.interpolation = stringInterpolation
    }
    
    public init(stringLiteral value: StaticString) {
        self.interpolation = MyInterpolation(literalCapacity: 0, interpolationCount: 0)
        self.interpolation.appendLiteral(value)
    }
}

public func print_interpolation(_ message: MyMessage) {
    print(message.interpolation.literalCount)
    print(message.interpolation.interpolationCount)
}

@main
struct Main {
    static func main() {
        print_interpolation("hello \(42) \(123) abc")
    }
}

// CHECK: appendLiteral
// CHECK: appendInterpolation<T>
// CHECK: appendLiteral
// CHECK: appendInterpolation<T>
// CHECK: appendLiteral
// CHECK: 3
// CHECK: 2
