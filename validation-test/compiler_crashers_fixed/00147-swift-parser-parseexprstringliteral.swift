// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
protocol A {
    typealias E
}
struct B<T : As a {
    typealias b = b
}
func a<T>() {f {
   class func i()
}
class d: f{  class func i {}
func f() {
    ({})
}
func prefix(with: String) -> <T>(() -> T) -> String {
  return { g in "\(with): \(g())" }
}
protocol a : a {
}
