// RUN: %target-swift-frontend-typecheck -verify %s -DILLEGAL \
// RUN:   -enable-experimental-feature NoncopyableGenerics \
// RUN:   -enable-builtin-module \
// RUN:   -verify-additional-prefix illegal-

// RUN: %target-swift-frontend -emit-sil -sil-verify-all -verify %s \
// RUN:   -enable-experimental-feature NoncopyableGenerics \
// RUN:   -enable-builtin-module

import Builtin

struct NC: ~Copyable {}

func checkPointerBuiltins(_ ptr: Builtin.RawPointer, _ value: consuming NC) {
  Builtin.initialize(value, ptr)
}

func checkArrayBuiltins(_ dest: Builtin.RawPointer, src: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.takeArrayFrontToBack(NC.self, dest, src, count)
  Builtin.takeArrayBackToFront(NC.self, dest, src, count)
  Builtin.assignTakeArray(NC.self, dest, src, count)
  Builtin.destroyArray(NC.self, dest, count)

#if ILLEGAL
  Builtin.copyArray(NC.self, dest, src, count) // expected-illegal-error {{noncopyable type 'NC' cannot be substituted for copyable generic parameter 'T' in 'copyArray'}}
  Builtin.assignCopyArrayNoAlias(NC.self, dest, src, count) // expected-illegal-error {{noncopyable type 'NC' cannot be substituted for copyable generic parameter 'T' in 'assignCopyArrayNoAlias'}}
  Builtin.assignCopyArrayFrontToBack(NC.self, dest, src, count) // expected-illegal-error {{noncopyable type 'NC' cannot be substituted for copyable generic parameter 'T' in 'assignCopyArrayFrontToBack'}}
  Builtin.assignCopyArrayBackToFront(NC.self, dest, src, count) // expected-illegal-error {{noncopyable type 'NC' cannot be substituted for copyable generic parameter 'T' in 'assignCopyArrayBackToFront'}}
#endif
}

public func checkIllegal() {
#if ILLEGAL
  _ = Builtin.unsafeCastToNativeObject(NC())
  _ = Builtin.castToNativeObject(NC()) // expected-illegal-error {{noncopyable type 'NC' cannot be substituted for copyable generic parameter 'T' in 'castToNativeObject'}}
  let _: NC = Builtin.zeroInitializer()
#endif
}
