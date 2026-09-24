// A caller in a separate translation unit, used by
// bridge-object-retain-returns-argument.swift.
//
// This declaration deliberately carries no `returned` attribute, and the
// definition of swift_bridgeObjectRetain is not visible here, so the optimizer
// cannot replace the call's result with its argument. That is what makes the
// value actually returned by the runtime entry point observable.

extern void *swift_bridgeObjectRetain(void *);

void *call_swift_bridgeObjectRetain(void *object) {
  return swift_bridgeObjectRetain(object);
}
