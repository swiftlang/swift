// RUN: %target-typecheck-verify-swift -enable-objc-interop

class Base {}

@objc protocol Protocol1 : Base {}
// expected-error@-1 {{protocol 'Protocol1' is '@objc' and cannot have a superclass constraint}}

@objc protocol OtherProtocol {}

typealias Composition = OtherProtocol & Base

@objc protocol Protocol2 : Composition {}
// expected-error@-1 {{protocol 'Protocol2' is '@objc' and cannot have a superclass constraint}}
