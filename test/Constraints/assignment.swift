// RUN: %swift -parse -verify -constraint-checker %s

var i : Int
var f : Float
func getIntFloat() -> (Int, Float)
var ift : (Int, Float)
var ovl : Int
var ovl : Float

var slice : Int[]

i = i
i = 1
(i, f) = getIntFloat()
(i, f) = ift
(i, f) = (i, f)
(ovl, ovl) = ift
(ovl, ovl) = (i, f)
slice[7] = i

slice[7] = f // expected-error{{assignment does not type-check}}
