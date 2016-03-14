// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
struct c:protocol A{associatedtype f{}func g:B
class B<T where f.h:b,f=c
