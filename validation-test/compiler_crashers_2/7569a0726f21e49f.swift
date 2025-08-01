// {"kind":"emit-silgen","original":"f261a4a8","signature":"(anonymous namespace)::ResultPlanner::planIntoDirect(swift::Lowering::AbstractionPattern, swift::CanType, swift::Lowering::AbstractionPattern, swift::CanType, swift::SILResultInfo)"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
infix operator <*> : AdditionPrecedence
func <*> <a, b>(c: ((a) -> b?)?, d: a) -> b? {
}
func cons<e, h>(c: e) -> (h) -> (e, h) {
}
var g = ""
if let f = cons <*> g <*> () {
}
