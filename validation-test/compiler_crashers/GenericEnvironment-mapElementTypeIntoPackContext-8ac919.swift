// {"kind":"typecheck","original":"4d3e4f1a","signature":"swift::GenericEnvironment::mapElementTypeIntoPackContext(swift::Type) const","signatureNext":"FailureDiagnostic::resolveType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a<
  b,
  c
{
  associatedtype b
  associatedtype c
  associatedtype d
  func e(d )
}
func
  f<i, each g>(h: repeat a<i, each g>)
{
  repeat (h.e(
