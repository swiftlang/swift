// RUN: %swift %s -verify

func murderInRoom(room:Int) {}
func murderInRoom(room:Int) withWeapon(weapon:Int) {} // expected-note {{'murderInRoom' previously declared here}}
func murderInRoom(room:Int) withWeapon(weapon:Int)
    framingSuspect(suspect:Int) {}

func murderInRoom(room:Int) withWeapon(w:Int) {} // expected-error {{invalid redeclaration}}

func blah(a:Int) blah(b:Int) {}
func blah(a:Int) blah(b:Int) blah(c:Int) {} // expected-error {{definition conflicts with previous value}} expected-note {{previous definition of 'blah' is here}}
func blah(a:Int) blah(b:Int) bluh(b:Int) {} // expected-error {{definition conflicts with previous value}} expected-note {{previous definition of 'b' is here}}
func blah(a:Int) blah(b:Int) bloh(c:Int) {}

func blah(a:Int) _(b:Int) _(c:Int) {}

func zero() sel(a:Int) {} // expected-error {{selector-style func arguments may only be used with one-argument patterns}}
func two(a:Int, b:Int) sel(c:Int) {} // expected-error {{selector-style func arguments may only be used with one-argument patterns}}
func sel(a:Int) zero() {} // expected-error {{selector-style func arguments may only be used with one-argument patterns}}
func sel(a:Int) two(b:Int, c:Int) {} // expected-error {{selector-style func arguments may only be used with one-argument patterns}}

func curry(a:Int) andSel(b:Int)(c:Int) {} // expected-error {{funcs with selector-style arguments may not be curried}}
