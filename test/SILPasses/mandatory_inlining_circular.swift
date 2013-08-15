// RUN: %swift %s -o /dev/null -verify

func [force_inline] waldo(x : Double) -> Double {
  return fred(x); // expected-error {{forced inlining forms circular loop}} expected-note 1 {{while inlining here}}
}

func [force_inline] fred(x : Double) -> Double {
  return waldo(x); // expected-error {{forced inlining forms circular loop}} expected-note 1 {{while inlining here}}
}

