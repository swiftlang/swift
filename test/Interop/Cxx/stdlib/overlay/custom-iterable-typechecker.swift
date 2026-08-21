// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop -enable-experimental-cxx-interop -disable-availability-checking

import CustomIterable

func checkIterable<S>(_ seq: borrowing S) where S: CxxIterable, S: ~Copyable {
  // expected-note@-1 {{where 'S' = 'NonInlineDereferenceOperatorSequence'}}
  // expected-note@-2 {{where 'S' = 'NonReferenceDereferenceOperatorSequence'}}
  // expected-note@-3 {{where 'S' = 'NoConstDereferenceOperatorSequence'}}
  // expected-note@-4 {{where 'S' = 'ConstRACButNotBorrowingIteratorSequence'}}
  var _ = seq.makeBorrowingIterator()
}

func checkRandomAccessCollection<S>(_ seq: borrowing S) where S: CxxRandomAccessCollection {}

checkIterable(NonInlineDereferenceOperatorSequence())
// expected-error@-1 {{global function 'checkIterable' requires that 'NonInlineDereferenceOperatorSequence' conform to 'CxxIterable'}}

// NonReferenceDereferenceOperatorSequence doesn't conform to CxxIterable because operator* 
// (the dereference operator) doesn't return a reference
checkIterable(NonReferenceDereferenceOperatorSequence()) 
// expected-error@-1 {{global function 'checkIterable' requires that 'NonReferenceDereferenceOperatorSequence' conform to 'CxxIterable'}}

// NoConstDereferenceOperatorSequence doesn't conform to CxxIterable because there is no const operator* (the dereference operator)
checkIterable(NoConstDereferenceOperatorSequence()) 
// expected-error@-1 {{global function 'checkIterable' requires that 'NoConstDereferenceOperatorSequence' conform to 'CxxIterable'}}

// ConstRACButNotBorrowingIteratorSequence doesn't conform to CxxIterable because operator* 
// (the dereference operator) doesn't return a reference. However, it still conforms to CxxRandomAccessCollection.
checkIterable(ConstRACButNotBorrowingIteratorSequence()) 
// expected-error@-1 {{global function 'checkIterable' requires that 'ConstRACButNotBorrowingIteratorSequence' conform to 'CxxIterable'}}
checkRandomAccessCollection(ConstRACButNotBorrowingIteratorSequence())
