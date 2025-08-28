import Seas

@available(Arctic) // expected-error {{unrecognized platform name 'Arctic'}}
func availableInArctic() { }

@available(Mediterranean)
func availableInMediterranean() { }

func testOtherClangDecls() { // expected-note {{add '@available' attribute to enclosing global function}}
  available_in_baltic() // expected-error {{'available_in_baltic()' is only available in Baltic}}
  // expected-note@-1 {{add 'if #available' version check}}
}
