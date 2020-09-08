// RUN: %target-typecheck-verify-swift

// Comparison with '.nan' static property instead of using '.isNan' instance property.

let double: Double = 0.0
_ = double == .nan // expected-warning {{comparison with '.nan' may lead to unexpected result, use 'double.isNan' to check for presence of NaN}}
_ = double != .nan // expected-warning {{comparison with '.nan' may lead to unexpected result, use '!double.isNan' to check for absence of NaN}}
_ = 0.0 == .nan // // expected-warning {{comparison with '.nan' may lead to unexpected result, use '.isNan' to check for presence or absence of NaN}}
_ = Double.nan == Double.nan // expected-warning {{'.nan' == '.nan' is always false}}
_ = Double.nan != Double.nan // expected-warning {{'.nan' != '.nan' is always true}}
