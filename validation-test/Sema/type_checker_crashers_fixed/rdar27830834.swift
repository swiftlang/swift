// RUN: %target-swift-frontend %s -typecheck -verify

var d = [String:String]()
_ = "\(d.map{ [$0 : $0] })" // expected-error {{type of expression is ambiguous without more context}}
