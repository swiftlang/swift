// RUN: %target-typecheck-verify-swift

//////////////////////////////////////////////////////////////////////////////////////////////////
/////// Comparison with '.nan' static property instead of using '.isNan' instance property ///////
//////////////////////////////////////////////////////////////////////////////////////////////////

// MARK: One side is '.nan' and the other isn't. Using '==' or '!=' for comparison should suggest

let double: Double = 0.0
_ = double == .nan // expected-warning {{comparison with '.nan' using '==' is always false, use 'double.isNan' to check if 'double' is not a number}}
_ = double != .nan // expected-warning {{comparison with '.nan' using '!=' is always true, use '!double.isNan' to check if 'double' is a number}}
_ = 0.0 == .nan // // expected-warning {{comparison with '.nan' using '==' is always false, use '0.0.isNan' to check if '0.0' is not a number}}

// MARK: One side is '.nan' and the other isn't. Using '>=', '>', '<', '<=' for comparison:
// We can't suggest using '.isNan' here.

_ = 0.0 >= .nan // expected-warning {{comparison with '.nan' using '>=' is always false}}
_ = .nan > 1.1 // expected-warning {{comparison with '.nan' using '>' is always false}}
_ = .nan < 2.2 // expected-warning {{comparison with '.nan' using '<' is always false}}
_ = 3.3 <= .nan // expected-warning {{comparison with '.nan' using '<=' is always false}}

// MARK: Both sides are '.nan':
// We can't suggest using '.isNan' here.

_ = Double.nan == Double.nan // expected-warning {{'.nan' == '.nan' is always false}}
_ = Double.nan != Double.nan // expected-warning {{'.nan' != '.nan' is always true}}
_ = Double.nan < Double.nan // expected-warning {{'.nan' < '.nan' is always false}}
_ = Double.nan <= Double.nan // expected-warning {{'.nan' <= '.nan' is always false}}
_ = Double.nan > Double.nan // expected-warning {{'.nan' > '.nan' is always false}}
_ = Double.nan >= Double.nan // expected-warning {{'.nan' >= '.nan' is always false}}
