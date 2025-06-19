import Seas

@available(Arctic) // expected-error {{unrecognized platform name 'Arctic'}}
func availableInArctic() { }

@available(Mediterranean)
func availableInMediterranean() { }

func testOtherClangDecls() {
  available_in_baltic() // expected-error {{'available_in_baltic()' is only available in Baltic}}
}
