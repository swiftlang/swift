// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a:d{func a}class B<T where B:P{class B:a{var _=a<c}struct c
