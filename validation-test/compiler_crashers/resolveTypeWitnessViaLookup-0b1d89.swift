// {"kind":"typecheck","original":"a6c3f30f","signature":"resolveTypeWitnessViaLookup(swift::NormalProtocolConformance*, swift::AssociatedTypeDecl*)","signatureNext":"AssociatedTypeInference::solve"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Foundation
protocol a {
  associatedtype b
}
extension timeval {
  var c = timeval()
  let <#pattern#>: a = c
}
