// RUN: %target-typecheck-verify-swift

// Applying the closure *returned* by a `@dynamicMemberLookup` subscript used to
// crash the type checker while building `ParameterListInfo` for the call. The
// applied closure has more parameters than the subscript's own parameter list,
// so the per-parameter lookup was indexed out of range: `getOrigParamIndex`
// aborted for the generic form, and `ParameterList::get` tripped an ArrayRef
// bounds assert for the non-generic form.

// A generic subscript whose result is a closure over its generic parameters.
@dynamicMemberLookup
struct Generic {
  subscript<A0, A1>(dynamicMember name: String) -> ((A0, A1) -> Int) {
    { _, _ in 0 }
  }
}

func callGeneric(o: Generic) -> Int {
  // `o.foo` resolves to the generic subscript; applying the returned closure to
  // two arguments drives inference of A0/A1 and used to crash in
  // getOrigParamIndex.
  o.foo(1, 2)
}

// A non-generic subscript returning a multi-argument closure triggers the same
// out-of-range indexing through the empty-substitutions path.
@dynamicMemberLookup
struct NonGeneric {
  subscript(dynamicMember name: String) -> ((Int, Int) -> Int) {
    { _, _ in 0 }
  }
}

func callNonGeneric(o: NonGeneric) -> Int {
  o.foo(1, 2)
}
