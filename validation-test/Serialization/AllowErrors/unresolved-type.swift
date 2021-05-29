// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/errors.swiftmodule -experimental-allow-module-with-compiler-errors %s

protocol SomeProto {}

extension SomeProto {
    func someFunc(arg:

enum SomeEnum {
    case a
}
