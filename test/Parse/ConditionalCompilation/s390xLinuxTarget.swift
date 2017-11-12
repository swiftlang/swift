// RUN: %swift -typecheck %s -verify -D FOO -D BAR -target s390x-unknown-linux-gnu -disable-objc-interop -D FOO -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target s390x-unknown-linux-gnu

#if arch(s390x) && os(Linux) && _runtime(_Native) && _endian(big)
class C {}
var x = C()
#endif
var y = x
