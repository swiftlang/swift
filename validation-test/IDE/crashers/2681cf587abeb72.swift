// {"kind":"complete","original":"2ad6764c","signature":"swift::constraints::Solution::getType(swift::KeyPathExpr const*, unsigned int) const","signatureAssert":"Assertion failed: (hasType(KP, I) && \"Expected type to have been set!\"), function getType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
0 -> \ a#^^#
!
