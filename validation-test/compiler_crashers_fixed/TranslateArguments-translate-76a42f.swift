// {"kind":"emit-silgen","signature":"(anonymous namespace)::TranslateArguments::translate(swift::Lowering::AbstractionPattern, swift::ArrayRefView<swift::AnyFunctionType::Param, swift::AnyFunctionType::CanParam, swift::AnyFunctionType::CanParam::getFromParam(swift::AnyFunctionType::Param const&), true>, swift::Lowering::AbstractionPattern, swift::ArrayRefView<swift::AnyFunctionType::Param, swift::AnyFunctionType::CanParam, swift::AnyFunctionType::CanParam::getFromParam(swift::AnyFunctionType::Param const&), true>, bool)"}
// RUN: %target-swift-frontend -emit-silgen -verify -language-mode 5 %s
// https://github.com/swiftlang/swift/issues/67091
func test<each T>(_: inout (repeat each T) -> Void) {
}
var a: () -> Void = {
}
var b: (Bool) -> Void = { _ in
}
var c: (Bool, Int) -> Void = { _, _ in
}
test(&a)
test(&b)
test(&c)
