// {"kind":"typecheck","signature":"swift::LValueType::get(swift::Type)","signatureAssert":"Assertion failed: (!objectTy->is<LValueType>() && !objectTy->is<InOutType>() && \"cannot have 'inout' or @lvalue wrapped inside an @lvalue\"), function get"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a!!= 1
