import Seas

@available(Arctic) // expected-error {{unrecognized platform name 'Arctic'}}
func availableInArctic() { }

@available(Mediterranean)
func availableInMediterranean() { }

func testOtherClangDecls() { // expected-note {{add '@available' attribute to enclosing global function}}
  available_in_baltic() // expected-error {{'available_in_baltic()' is only available in Baltic}}
  // expected-note@-1 {{add 'if #available' version check}}
  available_in_bering() // ok, Bering is always available
  unavailable_in_bering() // expected-error {{'unavailable_in_bering()' is unavailable}}
}

@available(Baltic)
func availableInBalticOther() {
  available_in_baltic()
  available_in_bering() // ok, Bering is always available
  unavailable_in_bering() // expected-error {{'unavailable_in_bering()' is unavailable}}
}

@available(Bering)
func availableInBering() { // expected-note {{add '@available' attribute to enclosing global function}}
  available_in_baltic() // expected-error {{'available_in_baltic()' is only available in Baltic}}
  // expected-note@-1 {{add 'if #available' version check}}
  available_in_bering()
  unavailable_in_bering() // expected-error {{'unavailable_in_bering()' is unavailable}}
}

@available(Bering, unavailable)
func unavailableInBering() {
  unavailable_in_bering()
}
