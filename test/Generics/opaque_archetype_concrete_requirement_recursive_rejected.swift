// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -enable-requirement-machine-opaque-archetypes

// FIXME: This does not work with -enable-requirement-machine-opaque-archetypes.
// See opaque_archetype_concrete_requirement_recursive.swift for a demonstration
// that it works without the flag (but more involved examples like the above
// won't work).

protocol P {
  associatedtype T

  var t: T { get }
}

protocol RecursiveP {
  associatedtype T : RecursiveP
}

struct S_RecursiveP : RecursiveP {
  typealias T = S_RecursiveP
}

struct DefinesRecursiveP : P {
  var t: some RecursiveP {
    return S_RecursiveP()
  }
}

protocol HasRecursiveP {
  associatedtype T : RecursiveP
}

extension HasRecursiveP where T == DefinesRecursiveP.T {}
// expected-error@-1 {{cannot build rewrite system for generic signature; rule length limit exceeded}}
// expected-note@-2 {{failed rewrite rule is τ_0_0.[HasRecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[concrete: (@_opaqueReturnTypeOf("$s56opaque_archetype_concrete_requirement_recursive_rejected17DefinesRecursivePV1tQrvp", 0) __).T.T.T.T.T.T.T.T.T.T.T.T.T] => τ_0_0.[HasRecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T].[RecursiveP:T]}}

