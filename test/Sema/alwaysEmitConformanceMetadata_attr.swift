// RUN: %target-typecheck-verify-swift -parse-stdlib

import Swift

@_alwaysEmitConformanceMetadata
protocol TestP {
    static var foo: Int { get }
}

@_alwaysEmitConformanceMetadata // expected-error {{@_alwaysEmitConformanceMetadata may only be used on 'protocol' declarations}}
class TestC {}

@_alwaysEmitConformanceMetadata // expected-error {{@_alwaysEmitConformanceMetadata may only be used on 'protocol' declarations}}
struct TestS {}

@_alwaysEmitConformanceMetadata // expected-error {{@_alwaysEmitConformanceMetadata may only be used on 'protocol' declarations}}
enum TestE {}

@_alwaysEmitConformanceMetadata // expected-error {{@_alwaysEmitConformanceMetadata may only be used on 'protocol' declarations}}
extension TestS {}

class TestC2 {
    @_alwaysEmitConformanceMetadata  // expected-error {{@_alwaysEmitConformanceMetadata may only be used on 'protocol' declarations}}
    var foo: Int = 11
}
