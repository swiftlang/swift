// RUN: %swift -dump-parse %s

operator postfix + {}

func [postfix] +(x:String) -> String { return x }

var a:Dictionary = ["foo":+1]
var b:Dictionary = ["foo"+:1]
var c:Dictionary = ["foo"+:+1]
