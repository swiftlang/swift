// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct Q<d where g:P{class b{class a<T where g:c
class func g:c:struct c
