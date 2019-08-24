// RUN: cat %s | tr '\132' '\0' > %t.tr
// RUN: cp -f %t.tr %t
// RUN: %round-trip-syntax-test --swift-syntax-test %swift-syntax-test --file %t
let a = Z3Z // nul(Z)
func b() {}
