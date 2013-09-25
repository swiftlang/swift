// RUN: %swift %s -std=agp10 -verify

func [kernel] fa0() {}
func [kernel, kernel] fa1() {}      // expected-error {{duplicate 'kernel' attribute}}
func [kernel, vertex] fa2() {}      // expected-error {{attribute 'kernel' cannot be combined with this attribute}}
func [kernel, fragment] fa3() {}    // expected-error {{attribute 'kernel' cannot be combined with this attribute}}
func [vertex] fa4() {}
func [vertex, vertex] fa5() {}      // expected-error {{duplicate 'vertex' attribute}}
func [vertex, kernel] fa6() {}      // expected-error {{attribute 'vertex' cannot be combined with this attribute}}
func [vertex, fragment] fa7() {}    // expected-error {{attribute 'vertex' cannot be combined with this attribute}}
func [fragment] fa8() {}
func [fragment, fragment] fa9() {}  // expected-error {{duplicate 'fragment' attribute}}
func [fragment, kernel] fa10() {}   // expected-error {{attribute 'fragment' cannot be combined with this attribute}}
func [fragment, vertex] fa11() {}   // expected-error {{attribute 'fragment' cannot be combined with this attribute}}
var [kernel] va0 : Int              // expected-error {{kernel attribute only applies to function declarations}}
var [vertex] va1 : Int              // expected-error {{vertex attribute only applies to function declarations}}
var [fragment] va2 : Int            // expected-error {{fragment attribute only applies to function declarations}}
// RUN: %swift %s -std=agp10 -verify

func [kernel] fa0() {}
func [kernel, kernel] fa1() {}      // expected-error {{duplicate 'kernel' attribute}}
func [kernel, vertex] fa2() {}      // expected-error {{attribute 'kernel' cannot be combined with this attribute}}
func [kernel, fragment] fa3() {}    // expected-error {{attribute 'kernel' cannot be combined with this attribute}}
func [vertex] fa4() {}
func [vertex, vertex] fa5() {}      // expected-error {{duplicate 'vertex' attribute}}
func [vertex, kernel] fa6() {}      // expected-error {{attribute 'vertex' cannot be combined with this attribute}}
func [vertex, fragment] fa7() {}    // expected-error {{attribute 'vertex' cannot be combined with this attribute}}
func [fragment] fa8() {}
func [fragment, fragment] fa9() {}  // expected-error {{duplicate 'fragment' attribute}}
func [fragment, kernel] fa10() {}   // expected-error {{attribute 'fragment' cannot be combined with this attribute}}
func [fragment, vertex] fa11() {}   // expected-error {{attribute 'fragment' cannot be combined with this attribute}}
var [kernel] va0 : Int              // expected-error {{kernel attribute only applies to function declarations}}
var [vertex] va1 : Int              // expected-error {{vertex attribute only applies to function declarations}}
var [fragment] va2 : Int            // expected-error {{fragment attribute only applies to function declarations}}
