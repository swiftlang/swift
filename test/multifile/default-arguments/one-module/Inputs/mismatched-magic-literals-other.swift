func callee(file: String = #file) {} // expected-note {{'file' declared here}}
func callee(optFile: String? = #file) {} // expected-note {{'optFile' declared here}}
func callee(arbitrary: String) {}
