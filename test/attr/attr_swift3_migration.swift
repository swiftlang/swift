// RUN: %target-parse-verify-swift

@swift3_migration(renamed: "g0(a:b:c:)", message: "it's good for you")
func f0(x: Int, y: Int, z: Int) { }

@swift3_migration(renamed: "Y0")
struct X0 { }



// Parsing diagnostics

@swift3_migration(renamed) // expected-error{{expected ':' following 'renamed' argument of 'swift3_migration'}}
func bad0() { }

@swift3_migration(renamed: blah) // expected-error{{expected string literal for 'renamed' argument of 'swift3_migration'}}
func bad1() { }

@swift3_migration(unknown: "foo") // expected-warning{{expected 'renamed' or 'message' in 'swift3_migration' attribute}}
func bad2() { }

@swift3_migration(renamed: "wibble wonka(") // expected-error{{ill-formed Swift name 'wibble wonka('}}
func bad3() { }
