// {"kind":"typecheck","signature":"swift::TupleTypeElt::TupleTypeElt(swift::Type, swift::Identifier)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
[Int : Int](Int) { a, b in a[b b= b * b
