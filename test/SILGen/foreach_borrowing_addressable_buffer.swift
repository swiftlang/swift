// RUN: %target-swift-emit-silgen %s \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -disable-availability-checking \
// RUN:   -module-name test | %FileCheck %s

// RUN: %target-swift-frontend %s -emit-sil -o /dev/null \
// RUN:   -sil-verify-all \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -disable-availability-checking \
// RUN:   -module-name test

// REQUIRES: swift_feature_Lifetimes

// rdar://190797942

// The iterator of a borrowing for-each loop depends on the address of the
// sequence, so SILGen stores the borrowed sequence to memory for the call to
// 'makeBorrowingIterator'. That in-memory borrow has to end before the
// sequence's own borrow does.

struct Rows<Element>: ~Copyable, ~Escapable, Iterable {
    let sample: Element

    @_lifetime(borrow source)
    init(source: borrowing Source, sample: Element) {
        self.sample = sample
    }

    @_lifetime(borrow self)
    func makeBorrowingIterator() -> Span<Element>.BorrowingIterator {
        fatalError()
    }
}

struct Source: ~Copyable {
    @_lifetime(borrow self)
    func rows<Element>(_ sample: Element) -> Rows<Element> {
        Rows(source: self, sample: sample)
    }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test3runyyAA6SourceVF :
// CHECK:         [[BORROW:%.*]] = begin_borrow [var_decl] {{%.*}}
// CHECK:         [[IN_MEMORY:%.*]] = store_borrow [[BORROW]] to
// CHECK:         end_borrow [[IN_MEMORY]]{{ }}
// CHECK:         end_borrow [[BORROW]]{{ }}
// CHECK:       } // end sil function '$s4test3runyyAA6SourceVF'
func run(_ source: borrowing Source) {
    for x in source.rows(1) {
        print(x)
    }
}

// The sequence is a parameter rather than a temporary.

// CHECK-LABEL: sil hidden [ossa] @$s4test12runParameteryyAA4RowsVySiGF :
// CHECK:         [[BORROW:%.*]] = begin_borrow [var_decl] {{%.*}}
// CHECK:         [[IN_MEMORY:%.*]] = store_borrow [[BORROW]] to
// CHECK:         end_borrow [[IN_MEMORY]]{{ }}
// CHECK:         end_borrow [[BORROW]]{{ }}
// CHECK:       } // end sil function '$s4test12runParameteryyAA4RowsVySiGF'
func runParameter(_ rows: borrowing Rows<Int>) {
    for x in rows {
        print(x)
    }
}

// Leaving the loop early takes the cleanups through a different path: first
// the 'return', then the 'break' and the normal exit.

// CHECK-LABEL: sil hidden [ossa] @$s4test12runEarlyExitySiAA6SourceV_SitF :
// CHECK:         [[BORROW:%.*]] = begin_borrow [var_decl] {{%.*}}
// CHECK:         [[IN_MEMORY:%.*]] = store_borrow [[BORROW]] to
// CHECK:         end_borrow [[IN_MEMORY]]{{ }}
// CHECK:         end_borrow [[BORROW]]{{ }}
// CHECK:         end_borrow [[IN_MEMORY]]{{ }}
// CHECK:         end_borrow [[BORROW]]{{ }}
// CHECK:       } // end sil function '$s4test12runEarlyExitySiAA6SourceV_SitF'
func runEarlyExit(_ source: borrowing Source, _ n: Int) -> Int {
    for x in source.rows(1) {
        if x == n { break }
        if x > n { return x }
        print(x)
    }
    return 0
}

// The callee sees its 'self' as a generic parameter, so the sequence needs
// neither to be generic nor to be noncopyable.

protocol Provider: ~Copyable, ~Escapable {}

extension Provider where Self: ~Copyable & ~Escapable {
    @_lifetime(borrow self)
    func makeBorrowingIterator() -> Span<Int>.BorrowingIterator {
        fatalError()
    }
}

struct ProviderRows: ~Copyable, ~Escapable, Provider, Iterable {
    typealias Element = Int
    let sample: Int

    @_lifetime(borrow source)
    init(source: borrowing Source, sample: Int) {
        self.sample = sample
    }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test11runProvideryyAA6SourceVF :
// CHECK:         [[BORROW:%.*]] = begin_borrow [var_decl] {{%.*}}
// CHECK:         [[IN_MEMORY:%.*]] = store_borrow [[BORROW]] to
// CHECK:         end_borrow [[IN_MEMORY]]{{ }}
// CHECK:         end_borrow [[BORROW]]{{ }}
// CHECK:       } // end sil function '$s4test11runProvideryyAA6SourceVF'
func runProvider(_ source: borrowing Source) {
    for x in ProviderRows(source: source, sample: 1) {
        print(x)
    }
}

struct CopyableProviderRows: Provider, Iterable {
    typealias Element = Int
    let sample: [Int]
}

// CHECK-LABEL: sil hidden [ossa] @$s4test19runCopyableProvideryyF :
// CHECK:         [[BORROW:%.*]] = begin_borrow [var_decl] {{%.*}}
// CHECK:         [[IN_MEMORY:%.*]] = store_borrow [[BORROW]] to
// CHECK:         end_borrow [[IN_MEMORY]]{{ }}
// CHECK:         end_borrow [[BORROW]]{{ }}
// CHECK:       } // end sil function '$s4test19runCopyableProvideryyF'
func runCopyableProvider() {
    for x in CopyableProviderRows(sample: [1]) {
        print(x)
    }
}
