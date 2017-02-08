// RUN: %target-typecheck-verify-swift -swift-version 4

// SR-2505: "Call arguments did not match up" assertion

func sr_2505(_ a: Any) {} // expected-note {{}}
sr_2505()          // expected-error {{missing argument for parameter #1 in call}}
sr_2505(a: 1)      // expected-error {{extraneous argument label 'a:' in call}}
sr_2505(1, 2)      // expected-error {{extra argument in call}}
sr_2505(a: 1, 2)   // expected-error {{extra argument in call}}

struct C_2505 {
  init(_ arg: Any) {
  }
}

protocol P_2505 {
}

extension C_2505 {
  init<T>(from: [T]) where T: P_2505 {
  }
}

class C2_2505: P_2505 {
}

let c_2505 = C_2505(arg: [C2_2505()]) // expected-error {{argument labels '(arg:)' do not match any available overloads}} expected-note {{overloads for 'C_2505' exist}}

