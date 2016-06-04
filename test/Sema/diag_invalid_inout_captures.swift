// RUN: %target-parse-verify-swift

func no_escape(_ you_say_price_of_my_love_is: @noescape () -> ()) {}
func do_escape(_ not_a_price_you_are_willing_to_pay: () -> ()) {}

struct you_cry_in_your_tea {
  mutating func which_you_hurl_in_the_sea_when_you_see_me_go_by() {
    no_escape { _ = self } // OK
    do_escape { _ = self } // expected-error {{closure cannot implicitly capture a mutating self parameter}}
  }
}

func why_so_sad(line: inout String) {
    no_escape { line = "Remember we made an arrangement when you went away" } // OK
    do_escape { line = "now you're making me mad" } // expected-error {{closure cannot implicitly capture an inout parameter unless @noescape}}
}

func remember(line: inout String) -> () -> Void {
    func despite_our_estrangement() {
        line = "I'm your man"
    }
    no_escape(despite_our_estrangement)
    do_escape(despite_our_estrangement) // expected-error {{nested function with an implicitly captured inout parameter can only be used as a @noescape argument}}

    return despite_our_estrangement // expected-error {{nested function cannot capture inout parameter and escape}}
}

