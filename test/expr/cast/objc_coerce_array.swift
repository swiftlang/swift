// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -verify %s
// REQUIRES: objc_interop
import Foundation

var x = 1

_ = [x] as [NSNumber]

_ = ["x":["y":"z","a":1]] as [String : [String : AnyObject]]
_ = ["x":["z",1]] as [String : [AnyObject]]
_ = [["y":"z","a":1]] as [[String : AnyObject]]
_ = [["z",1]] as [[AnyObject]]

var y: Any = 1

_ = ["x":["y":"z","a":y]] as [String : [String : AnyObject]]
_ = ["x":["z",y]] as [String : [AnyObject]]
_ = [["y":"z","a":y]] as [[String : AnyObject]]
_ = [["z",y]] as [[AnyObject]]

