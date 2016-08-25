// RUN: not --crash %target-swift-frontend %s -parse

var d = [String:String]()
_ = "\(d.map{ [$0 : $0] })"
