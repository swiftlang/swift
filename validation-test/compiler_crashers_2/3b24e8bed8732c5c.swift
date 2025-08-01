// {"kind":"emit-ir","signature":"swift::irgen::FulfillmentMap::searchNominalTypeMetadata(swift::irgen::IRGenModule&, swift::CanType, swift::MetadataState, unsigned int, swift::irgen::MetadataPath&&, swift::irgen::FulfillmentMap::InterestingKeysCallback const&)"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
class a<b> {
}
protocol c {
  associatedtype f: a<d>
  associatedtype d
}
extension c where Self == f, f == d {
  func e() {
  }
}
