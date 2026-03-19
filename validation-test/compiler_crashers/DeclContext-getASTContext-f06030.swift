// {"kind":"typecheck","original":"cd62e233","signature":"swift::DeclContext::getASTContext() const","signatureNext":"ClosureExpr::getExpandedBody"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Foundation
@Observable({
}) var a
