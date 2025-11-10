// {"kind":"typecheck","original":"ada65d78","signature":"swift::rewriting::PropertyBag::getAssociatedType(swift::Identifier)","signatureAssert":"Assertion failed: (assocType != nullptr && \"Need to look harder\"), function getAssociatedType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a
protocol b: RangeReplaceableCollection & a  :class a: b
