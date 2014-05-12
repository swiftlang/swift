// RUN: %swift %s -verify

class V {}
class U : V {}
class T : U {}

var v = V()
var u = U()
var t = T()

var va = [v]
var ua = [u]
var ta = [t]

va = ta

var va2: (V[])? = va as V[]
var v2: V = va2![0]

var ua2: (U[])? = va as U[]
var u2: U = ua2![0]

var ta2: (T[])? = va as T[]
var t2: T = ta2![0]
