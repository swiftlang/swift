// C shims that call reference counting runtime entrypoints, for tests that need
// to drive an entrypoint directly rather than through the calls IRGen emits.
//
// The call has to come from C, for two separate reasons:
//
// 1. Naming an entrypoint in Swift claims the symbol. The embedded stdlib ships
//    as serialized SIL rather than object code, so the entrypoints are codegen'd
//    into the client's own object file. An `@_extern(c, "swift_foo")`
//    declaration in the client suppresses that emission, and the link then fails
//    with an undefined `swift_foo`.
//
// 2. Even where the symbol resolves, a direct Swift call can be reasoned about
//    by the optimizer, which may fold or delete it. With the definition not
//    visible here, and no attributes promising anything about the result, the
//    call survives as written.
//
// Tests bind to the `test_`-prefixed wrappers with
// `@_extern(c, "test_<name>")`, which needs `-enable-experimental-feature
// Extern`.

void *swift_retain_n(void *object, unsigned n);
void *swift_bridgeObjectRetain(void *object);
void swift_unownedRetainStrongAndRelease(void *object);
void *swift_weakTakeStrong(void *ref);

void *test_retain_n(void *object, unsigned n) {
  return swift_retain_n(object, n);
}

void *test_bridgeObjectRetain(void *object) {
  return swift_bridgeObjectRetain(object);
}

void test_unownedRetainStrongAndRelease(void *object) {
  swift_unownedRetainStrongAndRelease(object);
}

void *test_weakTakeStrong(void *ref) {
  return swift_weakTakeStrong(ref);
}
