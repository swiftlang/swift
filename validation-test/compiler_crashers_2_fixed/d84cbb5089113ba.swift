// {"kind":"typecheck","signature":"swift::constraints::Solution::simplifyType(swift::Type, bool) const"}
// RUN: not %target-swift-frontend -typecheck %s
for (a(b, d)) in [(repeat .c)].enumerated(<#expression#>) {
}
