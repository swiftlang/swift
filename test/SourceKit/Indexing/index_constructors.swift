// RUN: %sourcekitd-test -req=index %s -- %s %S/Inputs/index_constructors_other.swift | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response

import Foundation

class HorseObject : DogObject {
  var name: NSString

  @objc public func flip() {}
}
