// RUN: %target-parse-verify-swift -parse-stdlib

var word: Builtin.Word

class C {}

var c: C
let bo = Builtin.castToBridgeObject(c, word)
c = Builtin.castReferenceFromBridgeObject(bo)
word = Builtin.castBitPatternFromBridgeObject(bo)
