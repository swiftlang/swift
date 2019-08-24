// RUN: %target-swift-frontend -module-name main -typecheck -swift-version 4 %s

// These tests are split out to ensure that we run the AST verifier's
// special post-type-checked verifications, which don't currently
// happen if any errors occur anywhere during compilation.

func rdar32239354_1(_ fn: () -> Void) {
  var other: (() -> Void) -> Void = { _ in  }

  withoutActuallyEscaping(fn, do: other)
  // Reassign to avoid warning about changing this to a let, since we
  // need this to be a var to trigger the original issue.
  other = { _ in }
}

func rdar32239354_2(_ fn: () -> Void, other: inout (() -> Void) -> Void) {
  withoutActuallyEscaping(fn, do: other)
}

func testVariations(
  _ no_escape: () -> (),
  _ escape: @escaping () -> (),
  _ takesFn: (()->()) -> ()->()
) -> () -> () {
  withoutActuallyEscaping(no_escape) { _ in }
  withoutActuallyEscaping({}) { _ in }
  withoutActuallyEscaping(escape) { _ in }
  _ = withoutActuallyEscaping(no_escape, do: takesFn)
  _ = withoutActuallyEscaping(escape, do: takesFn)
  _ = withoutActuallyEscaping(no_escape) {
    return takesFn($0)
  }
}

func testBlock(f: @convention(block) () -> ()) {
  let escape: (@escaping @convention(block) () -> ()) -> () = { _ in }
  let _: () = withoutActuallyEscaping(f, do: escape)
}
