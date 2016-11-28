// RUN: %target-swift-frontend %s -typecheck -verify

var d = [String:String]()
_ = "\(d.map{ [$0 : $0] })" // expected-error {{contextual closure type specifies '(key: String, value: String)', but 1 was used in closure body, try adding extra parentheses around the single tuple argument}}
