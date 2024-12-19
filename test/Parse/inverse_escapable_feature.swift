// RUN: %target-typecheck-verify-swift

struct S: ~Escapable {}

func hello(_ t: some Escapable, _ u: any Escapable) {}

protocol Whatever: Escapable {}
