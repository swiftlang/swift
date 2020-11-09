// RUN: not %target-swift-frontend %s -typecheck

var d = [String:String]()
_ = "\(d.map{ [$0 : $0] })"
