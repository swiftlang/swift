// {"kind":"complete","original":"3a9a209d","signature":"(anonymous namespace)::InputMatcher::match(int, std::__1::function<bool (unsigned int, unsigned int)>)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a: ExpressibleByStringInterpolation { stringInterpolation: b
struct b: StringInterpolationProtocol {
appendInterpolation(c: @autoclosure d)
appendInterpolation( computed e a = """
Lazy \()Conditional 0
#^COMPLETE7^#0\(Stringrepeating = #^COMPLETE9^# count spaces var )\0\0\0\n\0"
"""
