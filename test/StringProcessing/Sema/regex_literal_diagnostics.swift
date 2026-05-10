// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking

// REQUIRES: swift_swift_parser

postfix operator ^^
postfix func ^^ <T> (_ x: T) -> T { x }

prefix operator !!
prefix func !! <T> (_ x: T) -> T { x }

// rdar://92469692 - Make sure we get a correct fix-it location here.
func foo<T>(_ x: T, y: Int) {} // expected-note 3{{'foo(_:y:)' declared here}}
foo(/a/) // expected-error {{missing argument for parameter 'y' in call}} {{8-8=, y: <#Int#>}}
foo(/,/) // expected-error {{missing argument for parameter 'y' in call}} {{8-8=, y: <#Int#>}}
foo(/a/^^) // expected-error {{missing argument for parameter 'y' in call}} {{10-10=, y: <#Int#>}}

func bar<T>(x: Int, _ y: T) {} // expected-note 3{{'bar(x:_:)' declared here}}
bar(/a/) // expected-error {{missing argument for parameter 'x' in call}} {{5-5=x: <#Int#>, }}
bar(/,/) // expected-error {{missing argument for parameter 'x' in call}} {{5-5=x: <#Int#>, }}
bar(!!/a/) // expected-error {{missing argument for parameter 'x' in call}} {{5-5=x: <#Int#>, }}
