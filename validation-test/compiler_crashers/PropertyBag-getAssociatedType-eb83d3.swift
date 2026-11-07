// {"kind":"typecheck","original":"ada65d78","signature":"swift::rewriting::PropertyBag::getAssociatedType(swift::Identifier)","signatureAssert":"Assertion failed: (assocType != nullptr && \"Need to look harder\"), function getAssociatedType","signatureNext":"getTypeForSymbolRange"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
a
protocol b: RangeReplaceableCollection & a  :class a: b
