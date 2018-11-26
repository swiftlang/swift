// RUN: %target-typecheck-verify-swift

func no_escape(_ you_say_price_of_my_love_is: () -> ()) {}
func do_escape(_ not_a_price_you_are_willing_to_pay: @escaping () -> ()) {}

struct you_cry_in_your_tea {
  mutating func which_you_hurl_in_the_sea_when_you_see_me_go_by() {
    no_escape { _ = self } // OK
    do_escape { _ = self } // expected-error {{closure cannot implicitly capture a mutating self parameter}}
    do_escape {
      [self] in
      _ = self // OK
      self.other_mutator() // expected-error {{cannot use mutating member on immutable value: 'self' is an immutable capture}}
    }
  }

  mutating func other_mutator() {}
}

func why_so_sad(line: inout String) {
    no_escape { line = "Remember we made an arrangement when you went away" } // OK
    do_escape { line = "now you're making me mad" } // expected-error {{escaping closures can only capture inout parameters explicitly by value}}
    do_escape { [line] in _ = line } // OK
}

func remember(line: inout String) -> () -> Void {
    func despite_our_estrangement() {
        line = "I'm your man"
    }
    no_escape(despite_our_estrangement)
    do_escape(despite_our_estrangement) // expected-error {{nested function with an implicitly captured inout parameter can only be used as a non-escaping argument}}

    return despite_our_estrangement // expected-error {{nested function cannot capture inout parameter and escape}}
}

// This arrangement should be legal, but the type checker does not currently allow it.
//
// TODO: If the type checker is ever fixed, we should enable the corresponding test in
// SILOptimizer/exclusivity_static_diagnostics.swift.
func its_complicated(condition: inout Int) {
  no_escape(condition == 0 ? { condition = 1 } : { condition = 2}) // expected-error 2 {{escaping closures can only capture inout parameters explicitly by value}}
}
