// RUN: %target-typecheck-verify-swift -enable-experimental-feature Reparenting

@reparentable
protocol Q {}

@reparentable
protocol R {}

protocol P: @reparented Q {}

protocol P2: @reparented Q, @reparented R {}

class Whatever: @reparented AnyObject {}
// expected-error @-1 {{'@reparented' only applies in inheritance clauses of protocols}}

protocol Invalid0 {
  associatedtype A: @reparented Q
  // expected-error @-1 {{'@reparented' only applies in inheritance clauses of protocols}}
}

protocol Invalid1: @reparented AnyObject {}
// expected-error @-1 {{'@reparented' cannot apply to non-protocol type 'AnyObject'}}

protocol Invalid2: @reparented Whatever {}
// expected-error @-1 {{'@reparented' cannot apply to non-protocol type 'Whatever'}}

protocol Invalid3: @reparented P {}
// expected-error @-1 {{'@reparented' cannot apply to 'P' unless it is declared to be '@reparentable'}}

protocol Invalid4: @reparented Q & R {}
// expected-error @-1 {{'@reparented' cannot apply to non-protocol type 'Q & R'}}

