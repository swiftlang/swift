// RUN: %target-swift-frontend %s -typecheck -verify

var d = [String:String]()
_ = "\(d.map{ [$0 : $0] })" // expected-error {{type '(key: String, value: String)' cannot conform to 'Hashable'; only struct/enum/class types can conform to protocols}}
