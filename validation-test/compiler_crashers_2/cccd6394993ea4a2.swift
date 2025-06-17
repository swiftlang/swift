// {"signature":"swift::ErrorType::get(swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@convention(c) _->Int
