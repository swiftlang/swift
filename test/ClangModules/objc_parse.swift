// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -constraint-checker -parse -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | grep objc.pcm

import objc

// Subtyping
func treatBAsA(b : B) -> A { 
  return b
}

// Instance method invocation
func instanceMethods(b : B) {
  var i = b.method(1, withFloat=2.5)
  i = i + b.method(1, withDouble=2.5)
}

// Class method invocation
func classMethods(b : B) {
  var i = B.classMethod()
  i += B.classMethod(1)
  i += B.classMethod(1, withInt=2)

  i += b.classMethod()
}

// Properties
func properties(b : B) {
  var i = b.counter
  b.counter = i + 1
  i = i + b.readCounter
  b.readCounter = i + 1 // expected-error{{cannot assign to a property or subscript without a 'set' method}}

  b.setCounter(5) // expected-error{{expression does not type-check}}

  // Informal properties in Objective-C map to methods, not variables.
  b.informalProp()

  // An informal property cannot be made formal in a subclass. The
  // formal property is simply ignored.
  b.informalMadeFormal()
  b.informalMadeFormal = i // expected-error{{cannot assign to the result of this expression}}
  b.setInformalMadeFormal(5)

  b.overriddenProp = 17
}
