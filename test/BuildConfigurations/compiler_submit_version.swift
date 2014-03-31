// RUN: %swift -parse %s -verify -D FOO

// FIXME: Remove this test when rdar://problem/16380797 is addressed.
#if compiler_submit_version == "0.0.0.0"
  var a = "s"	
#else
  var a = 0
#endif

var a1 = a + 1

#if compiler_submit_version != "0.0.0.0"
  var b = 0
#endif

var b1 = b

#if compiler_submit_version > "0.0.0.0"
  var c = 0
#endif

var c1 = c

#if compiler_submit_version < "0.0.0.0"
#else
  var d = 0
#endif

var d1 = d

#if compiler_submit_version >= "0.0.0.0"
  var e = 0
#endif

var e1 = e

#if compiler_submit_version <= "0.0.0.0"
#else
  var f = 0
#endif

var f1 = f

#if FOO || compiler_submit_version == "0.0.0.0"
  var g = 0
#endif

var g1 = g

#if FOO && compiler_submit_version == "0.0.0.0"
#else
  var h = 0	
#endif
var h1 = h

#if FOO && !(compiler_submit_version == "0.0.0.0")
  var i = 0
#endif

var i1 = i


