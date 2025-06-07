// RUN: %target-swift-frontend -emit-sil -verify %s

struct NC: ~Copyable {
    borrowing func borrow() {}
    mutating func mutate() {}
    consuming func consume() {}
}

func borrow(_: borrowing NC) {}
func consume(_: consuming NC) {}
func mutate(_: inout NC) {}

func unwrapBorrow_Borrow(x: borrowing NC?) {
    x!.borrow()
    borrow(x!)

    x!.borrow()
    borrow(x!)
}

func unwrapConsume_Borrow(x: borrowing NC?) { // expected-error{{cannot be consumed}}
    x!.consume() // expected-note{{consumed here}}
    consume(x!) // expected-note{{consumed here}}

    x!.consume() // expected-note{{consumed here}}
    consume(x!) // expected-note{{consumed here}}
}

func unwrapBorrowMutateConsume_Consume(x: consuming NC?) {
    x!.borrow()
    x!.mutate()

    borrow(x!)
    mutate(&x!)
    
    consume(x!)
}

func unwrapBorrowMutateConsume2_Consume(x: consuming NC?) {
    x!.borrow()
    x!.mutate()

    borrow(x!)
    mutate(&x!)
    
    x!.consume()
}

func unwrapBorrowMutateConsumeBorrow_Consume(x: consuming NC?) { // expected-error {{used after consume}}
    x!.borrow()
    x!.mutate()

    borrow(x!)
    mutate(&x!)
    
    consume(x!) // expected-note{{consumed here}}

    x!.borrow() // expected-note{{used here}}
    borrow(x!)
}

func unwrapBorrowMutateConsumeMutate_Consume(x: consuming NC?) { // expected-error {{used after consume}}
    x!.borrow()
    x!.mutate()

    borrow(x!)
    mutate(&x!)
    
    consume(x!) // expected-note{{consumed here}}

    x!.mutate() // expected-note{{used here}}
    mutate(&x!)
}

func unwrapBorrowMutateConsumeInitBorrow_Consume(x: consuming NC?, y: consuming NC?) {
    x!.borrow()
    x!.mutate()

    borrow(x!)
    mutate(&x!)
    
    consume(x!)

    x = y

    x!.borrow()
    borrow(x!)
}

func unwrapBorrowMutateConsumeInitMutate_Consume(x: consuming NC?, y: consuming NC?) {
    x!.borrow()
    x!.mutate()

    borrow(x!)
    mutate(&x!)
    
    consume(x!)

    x = y

    x!.mutate()
    mutate(&x!)
}

func unwrapBorrowMutateConsumeInitBorrowMutateConsume_Consume(x: consuming NC?, y: consuming NC?) {
    x!.borrow()
    x!.mutate()

    borrow(x!)
    mutate(&x!)
    
    consume(x!)

    x = y

    x!.mutate()
    x!.borrow()
    mutate(&x!)
    borrow(x!)

    consume(x!)
}

func unwrapBorrowMutate_Mutate(x: inout NC?) {
    x!.borrow()
    x!.mutate()

    borrow(x!)
    mutate(&x!)
}

func unwrapBorrowMutateConsume_Mutate(x: inout NC?) { // expected-error {{missing reinitialization}}
    x!.borrow()
    x!.mutate()

    borrow(x!)
    mutate(&x!)

    x!.consume() // expected-note {{consumed here}}
}

func unwrapBorrowMutateConsumeInit_Mutate(x: inout NC?, y: consuming NC) {
    x!.borrow()
    x!.mutate()

    borrow(x!)
    mutate(&x!)

    x!.consume() // expected-note{{consumed here}}

    x! = y // expected-error{{cannot partially reinitialize}}
}
