// {"kind":"typecheck","original":"46f686fe","signature":"swift::ValueDecl::copyFormalAccessFrom(swift::ValueDecl const*, bool)","signatureAssert":"Assertion failed: (!hasAccess() && \"access already set\"), function setAccess"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed
distributed open actor a {
}
