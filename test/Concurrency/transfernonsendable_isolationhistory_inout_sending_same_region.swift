// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -parse-as-library -sil-region-isolation-emit-isolation-history -verify %s -o /dev/null

// REQUIRES: concurrency

// Swift-source coverage for isolation-history notes on the
// InOutSendingParametersInSameRegion diagnostic, which fires at a function exit
// when two or more 'inout sending' parameters share a region. The caller treats
// each as its own region on return, so it could send them to different isolation
// domains.
//
// Both parameters are disconnected here -- this diagnostic is purely about them
// sharing a region -- so the request is a connect one and every link reads as a
// plain "'a' is connected to 'b'" with no isolated terminus. That is correct for
// this diagnostic: there is no isolated value in the story.
//
// Primary diagnostics use empty-body matchers; chain notes have their full text
// matched.

class NS {}

////////////////////////////////////////////////////////////////////////////////
// One hop — the two parameters are assigned to each other directly.
////////////////////////////////////////////////////////////////////////////////

func one_hop(_ x: inout sending NS, _ y: inout sending NS) {
  x = y // expected-note {{'x' is connected to 'y'}}
} // expected-warning {{}} expected-note {{}}

////////////////////////////////////////////////////////////////////////////////
// Two hops — the two parameters reach each other through a named local.
////////////////////////////////////////////////////////////////////////////////

func two_hop(_ x: inout sending NS, _ y: inout sending NS) {
  let z = y // expected-note {{'z' is connected to 'y'}}
  x = z // expected-note {{'x' is connected to 'z'}}
} // expected-warning {{}} expected-note {{}}

////////////////////////////////////////////////////////////////////////////////
// Three parameters in one region — the multi-pair case, and the reason each
// reported pair is handed its own copy of the partition rather than the error's.
// The walk consumes the partition it rewinds, so if it were moved instead of
// copied only the first pair would get a chain and the rest would silently emit
// nothing. Three pairs are reported here (x/y, x/z, y/z), and the note counts
// below are what catches a regression to moving: 'x is connected to y' is needed
// by the x/y and x/z chains, and 'y is connected to z' by the x/z and y/z ones.
////////////////////////////////////////////////////////////////////////////////

func three_params(_ x: inout sending NS, _ y: inout sending NS,
                  _ z: inout sending NS) {
  x = y // expected-note 2 {{'x' is connected to 'y'}}
  z = y // expected-note 2 {{'y' is connected to 'z'}}
} // expected-warning 3 {{}} expected-note 3 {{}}

////////////////////////////////////////////////////////////////////////////////
// CFG diamond — only the 'if' arm joins the two parameters. The note lands in
// that arm; the 'else' arm's fresh value produces none.
////////////////////////////////////////////////////////////////////////////////

func diamond(_ x: inout sending NS, _ y: inout sending NS, _ cond: Bool) {
  if cond {
    x = y // expected-note {{'x' is connected to 'y'}}
  } else {
    x = NS()
  }
} // expected-warning {{}} expected-note {{}}
