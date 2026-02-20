// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -suppress-notes -I %S/Inputs -cxx-interoperability-mode=default

import CustomIterator

func checkInput<It: UnsafeCxxInputIterator>(_ _: It) {}
func checkRandomAccess<It: UnsafeCxxRandomAccessIterator>(_ _: It) {}
func checkContiguous<It: UnsafeCxxContiguousIterator>(_ _: It) {}
func checkMutableInput<It: UnsafeCxxMutableInputIterator>(_ _: It) {}
func checkMutableRandomAccess<It: UnsafeCxxMutableRandomAccessIterator>(_ _: It) {}
func checkMutableContiguous<It: UnsafeCxxMutableContiguousIterator>(_ _: It) {}

func check(it: ConstIterator) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: ConstRACIterator) {
  checkInput(it)
  checkRandomAccess(it)
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: ConstRACIteratorRefPlusEq) {
  checkInput(it)
  checkRandomAccess(it)
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: ConstIteratorOutOfLineEq) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: MinimalIterator) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: ForwardIterator) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: HasCustomIteratorTag) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: HasCustomRACIteratorTag) {
  checkInput(it)
  checkRandomAccess(it)
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: HasCustomInheritedRACIteratorTag) {
  checkInput(it)
  checkRandomAccess(it)
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: HasCustomIteratorTagInline) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: HasTypedefIteratorTag) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: MutableRACIterator) {
  checkInput(it)
  checkRandomAccess(it)
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)
  checkMutableRandomAccess(it)
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: HasNoIteratorCategory) {
  checkInput(it)                // expected-error {{requires}}
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: HasInvalidIteratorCategory) {
  checkInput(it)                // expected-error {{requires}}
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: HasNoEqualEqual) {
  checkInput(it)                // expected-error {{requires}}
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: HasInvalidEqualEqual) {
  checkInput(it)                // expected-error {{requires}}
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: HasNoIncrementOperator) {
  checkInput(it)                // expected-error {{requires}}
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: HasNoPreIncrementOperator) {
  checkInput(it)                // expected-error {{requires}}
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: HasNoDereferenceOperator) {
  checkInput(it)                // expected-error {{requires}}
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: TemplatedIteratorInt) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}


func check(it: TemplatedIteratorOutOfLineEqInt) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}


func check(it: TemplatedRACIteratorOutOfLineEqInt) {
  checkInput(it)
  checkRandomAccess(it)
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: BaseIntIterator) {
  checkInput(it)                // expected-error {{requires}}
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: InheritedConstIterator) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: InheritedTemplatedConstIteratorInt) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: InheritedTemplatedConstRACIteratorInt) {
  checkInput(it)
  checkRandomAccess(it)
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: InheritedTemplatedConstRACIteratorOutOfLineOpsInt) {
  checkInput(it)
  checkRandomAccess(it)
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)         // expected-error {{requires}}
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: InputOutputIterator) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}

func check(it: InputOutputConstIterator) {
  checkInput(it)
  checkRandomAccess(it)         // expected-error {{requires}}
  checkContiguous(it)           // expected-error {{requires}}
  checkMutableInput(it)
  checkMutableRandomAccess(it)  // expected-error {{requires}}
  checkMutableContiguous(it)    // expected-error {{requires}}
}
