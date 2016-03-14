// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: %target-swift-frontend %s -parse -verify
protocol c : b { // expected-error {{use of undeclared type 'b'}} expected-note {{in declaration of 'c'}}
	func b // expected-error {{expected '(' in argument list of function declaration}}
// expected-error@+1 {{expected declaration}}
