// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop -enable-experimental-cxx-interop -I %swift_src_root/lib/ClangImporter/SwiftBridging -disable-availability-checking

import CustomBorrowingSequence

func checkBorrowingSequence<S>(_ seq: borrowing S) where S: CxxBorrowingSequence, S: ~Copyable {
  // expected-note@-1 {{where 'S' = 'NonInlineDereferenceOperatorSequence'}}
  // expected-note@-2 {{where 'S' = 'NonReferenceDereferenceOperatorSequence'}}
  // expected-note@-3 {{where 'S' = 'NoConstDereferenceOperatorSequence'}}
  // expected-note@-4 {{where 'S' = 'ConstRACButNotBorrowingIteratorSequence'}}
  var _ = seq.makeBorrowingIterator()
}

func checkRandomAccessCollection<S>(_ seq: borrowing S) where S: CxxRandomAccessCollection {}

checkBorrowingSequence(NonInlineDereferenceOperatorSequence())
// expected-error@-1 {{global function 'checkBorrowingSequence' requires that 'NonInlineDereferenceOperatorSequence' conform to 'CxxBorrowingSequence'}}

// NonReferenceDereferenceOperatorSequence doesn't conform to CxxBorrowingSequence because operator* 
// (the dereference operator) doesn't return a reference
checkBorrowingSequence(NonReferenceDereferenceOperatorSequence()) 
// expected-error@-1 {{global function 'checkBorrowingSequence' requires that 'NonReferenceDereferenceOperatorSequence' conform to 'CxxBorrowingSequence'}}

// NoConstDereferenceOperatorSequence doesn't conform to CxxBorrowingSequence because there is no const operator* (the dereference operator)
checkBorrowingSequence(NoConstDereferenceOperatorSequence()) 
// expected-error@-1 {{global function 'checkBorrowingSequence' requires that 'NoConstDereferenceOperatorSequence' conform to 'CxxBorrowingSequence'}}

// ConstRACButNotBorrowingIteratorSequence doesn't conform to CxxBorrowingSequence because operator* 
// (the dereference operator) doesn't return a reference. However, it still conforms to CxxRandomAccessCollection.
checkBorrowingSequence(ConstRACButNotBorrowingIteratorSequence()) 
// expected-error@-1 {{global function 'checkBorrowingSequence' requires that 'ConstRACButNotBorrowingIteratorSequence' conform to 'CxxBorrowingSequence'}}
checkRandomAccessCollection(ConstRACButNotBorrowingIteratorSequence())
