// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -parse -verify
protocol c : b { // expected-error {{use of undeclared type 'b'}} expected-note {{in declaration of 'c'}}
	func b // expected-error {{expected '(' in argument list of function declaration}}
// expected-error@+1 {{expected declaration}}
