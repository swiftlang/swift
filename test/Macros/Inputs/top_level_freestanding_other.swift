#anonymousTypes { "hello2" }

var globalVar = #stringify(1 + 1)
var globalVar2 = { #stringify(1 + 1) }()

@available(*, deprecated)
func deprecated() -> Int { 0 }

var globalVar3 = #stringify({ deprecated() })
// expected-note@-1 2{{in expansion of macro 'stringify' here}}
// expected-warning@-2{{'deprecated()' is deprecated}}

var globalVar4 = #stringify({ deprecated() })
// expected-note@-1 2{{in expansion of macro 'stringify' here}}
// expected-warning@-2{{'deprecated()' is deprecated}}
