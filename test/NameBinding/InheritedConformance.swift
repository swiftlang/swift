// XFAIL: linux
// RUN: rm -rf %t && mkdir %t
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -parse -primary-file %t/main.swift -emit-reference-dependencies-path - > %t.swiftdeps

// SR-1267, SR-1270
class TypeType<T> { }
protocol ProtocolType {}

struct StructType<T> { }
extension StructType where T : ProtocolType {
    func testCase<T>() -> TypeType<T> {
        return TypeType<T>()
    }
}

class TestView : ProtocolType {}
extension StructType where T : TestView {
    var typeType : TypeType<T> { return testCase() }
}