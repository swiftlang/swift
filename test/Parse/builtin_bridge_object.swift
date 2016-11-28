// RUN: %target-typecheck-verify-swift -parse-stdlib

precedencegroup AssignmentPrecedence { assignment: true }

var word: Builtin.Word

class C {}

var c: C
let bo = Builtin.castToBridgeObject(c, word)
c = Builtin.castReferenceFromBridgeObject(bo)
word = Builtin.castBitPatternFromBridgeObject(bo)
