// RUN: %target-swift-frontend %s -typecheck -verify

var d = [String:String]()
_ = "\(d.map{ [$0 : $0] })" // expected-error {{generic struct 'Dictionary' requires that '(key: String, value: String)' conform to 'Hashable'}}
