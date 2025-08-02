// {"kind":"typecheck","signature":"swift::LValueType::get(swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a!!= 1
