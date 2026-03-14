// RUN: %target-typecheck-verify-swift

// Make sure we don't end up with a request cycle here, since looking up 'S.Actor'
// requires expanding member attribute macros for 'S'.
@S.Actor
struct S {
  @globalActor actor Actor: GlobalActor {
    public static let shared = S.Actor()
  }
}
