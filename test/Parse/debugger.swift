// RUN: %swift %s -verify -debugger-support

var ($x0, $x1) = (4, 3)
var z = $x0 + $x1

z // no error.
