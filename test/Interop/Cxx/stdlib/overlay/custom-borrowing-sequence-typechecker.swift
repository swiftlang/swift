// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop -enable-experimental-cxx-interop -I %swift_src_root/lib/ClangImporter/SwiftBridging -disable-availability-checking

import CustomBorrowingSequence

func checkBorrowingSequence<S>(_ seq: borrowing S) where S: CxxBorrowingSequence, S: ~Copyable {
  // expected-note@-1 {{where 'S' = 'NonReferenceDereferenceOperatorSequence'}}
  var _ = seq.makeBorrowingIterator()
}

// NonReferenceDereferenceOperatorSequence doesn't conform to CxxBorrowingSequence because operator* 
// (the dereference operator) doesn't return a reference
checkBorrowingSequence(NonReferenceDereferenceOperatorSequence()) 
// expected-error@-1 {{global function 'checkBorrowingSequence' requires that 'NonReferenceDereferenceOperatorSequence' conform to 'CxxBorrowingSequence'}}
