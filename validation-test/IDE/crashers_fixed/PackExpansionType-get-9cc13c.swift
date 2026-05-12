// {"kind":"complete","original":"900d1390","signature":"swift::PackExpansionType::get(swift::Type, swift::Type)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"PackConformance::getAssociatedConformance"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a { associatedtype b }
protocol c { associatedtype d: a associatedtype e associatedtype f: c where f.d == e func g -> e.b struct h<i , j>: c struct k<each l : c>: c { typealias f = k<repeat each l.f> func m<n , each o > -> k<repeat h<n, each o>>#^^#
