// RUN: %target-swift-frontend %s -typecheck -verify

var d = [String:String]()
_ = "\(d.map{ [$0 : $0] })" // expected-error {{'[(key: String, value: String) : (key: String, value: String)]' requires that '(key: String, value: String)' conform to 'Hashable'}}
