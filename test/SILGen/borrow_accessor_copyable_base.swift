// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -o /dev/null %s

// https://github.com/swiftlang/swift/issues/93915
//
// A borrowing use of a copyable value read through a borrow accessor of a
// borrowed parameter takes the accessor's result in place, as it does for a
// noncopyable value, rather than a copy of it.

public protocol Priced {
  var price: Int { get }
  consuming func take() -> Int
}

public func use<Row>(_ row: borrowing Row, _ other: Int) {}

public func later() -> Int { 0 }

public struct Box<Row> {
  var _row: Row

  public var row: Row {
    borrow { return _row }
  }
}

public struct Outer<Row> {
  var _box: Box<Row>

  public var box: Box<Row> {
    borrow { return _box }
  }
}

public final class Klass {}

public struct Quote {
  var instrument: Klass
  public func quote() -> Int { 0 }
}

public struct Owner<Row>: ~Copyable {
  var _row: Row

  public var row: Row {
    borrow { return _row }
  }
}

public func destroy<Row>(_ owner: consuming Owner<Row>) -> Int { 0 }

// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_copyable_base9spanPriceySis4SpanVyxGAA6PricedRzlF :
// CHECK:         [[ROW:%.*]] = apply {{%.*}}<Row>({{%.*}}, %0) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @guaranteed Span<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK-NOT:     copy_addr
// CHECK:         [[GETTER:%.*]] = witness_method $Row, #Priced.price!getter
// CHECK-NEXT:    apply [[GETTER]]<Row>([[ROW]])
// CHECK:       } // end sil function '$s29borrow_accessor_copyable_base9spanPriceySis4SpanVyxGAA6PricedRzlF'
public func spanPrice<Row: Priced>(_ rows: Span<Row>) -> Int {
  rows[0].price
}

// The accessor still runs before the arguments that follow it.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_copyable_base13spanBorrowingyys4SpanVyxGlF :
// CHECK:         [[ROW:%.*]] = apply {{%.*}}<Row>({{%.*}}, %0) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @guaranteed Span<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:         [[LATER:%.*]] = function_ref @$s29borrow_accessor_copyable_base5laterSiyF :
// CHECK-NEXT:    [[OTHER:%.*]] = apply [[LATER]]()
// CHECK:         apply {{%.*}}<Row>([[ROW]], [[OTHER]])
// CHECK:       } // end sil function '$s29borrow_accessor_copyable_base13spanBorrowingyys4SpanVyxGlF'
public func spanBorrowing<Row>(_ rows: Span<Row>) {
  use(rows[0], later())
}

// A loadable element is borrowed rather than copied.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_copyable_base12spanLoadableySis4SpanVyAA5QuoteVGF :
// CHECK:         [[ROW:%.*]] = apply {{%.*}}<Quote>({{%.*}}, %0) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @guaranteed Span<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK-NOT:     load [copy]
// CHECK:         [[BORROW:%.*]] = load_borrow [[ROW]]
// CHECK:         apply {{%.*}}([[BORROW]]) : $@convention(method) (@guaranteed Quote) -> Int
// CHECK:       } // end sil function '$s29borrow_accessor_copyable_base12spanLoadableySis4SpanVyAA5QuoteVGF'
public func spanLoadable(_ rows: Span<Quote>) -> Int {
  rows[0].quote()
}

// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_copyable_base10outerPriceySiAA5OuterVyxGAA6PricedRzlF :
// CHECK:         [[BOX:%.*]] = apply {{%.*}}<Row>(%0) : $@convention(method) <τ_0_0> (@in_guaranteed Outer<τ_0_0>) -> @guaranteed_address Box<τ_0_0>
// CHECK:         [[ROW:%.*]] = apply {{%.*}}<Row>([[BOX]]) : $@convention(method) <τ_0_0> (@in_guaranteed Box<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK-NOT:     copy_addr
// CHECK:         [[GETTER:%.*]] = witness_method $Row, #Priced.price!getter
// CHECK-NEXT:    apply [[GETTER]]<Row>([[ROW]])
// CHECK:       } // end sil function '$s29borrow_accessor_copyable_base10outerPriceySiAA5OuterVyxGAA6PricedRzlF'
public func outerPrice<Row: Priced>(_ outer: Outer<Row>) -> Int {
  outer.box.row.price
}

// A loadable base is materialized for the generic accessor, which ends before
// the call, so the element is copied out first.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_copyable_base17materializedQuoteySiAA3BoxVyAA0F0VGF :
// CHECK:         [[BASE:%.*]] = store_borrow %0 to {{%.*}}
// CHECK:         [[ROW:%.*]] = apply {{%.*}}<Quote>([[BASE]]) : $@convention(method) <τ_0_0> (@in_guaranteed Box<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:         copy_addr [[ROW]] to [init] [[COPY:%[0-9]+]]
// CHECK:         [[VALUE:%.*]] = load [take] [[COPY]]
// CHECK:         end_borrow [[BASE]]
// CHECK:         apply {{%.*}}([[VALUE]]) : $@convention(method) (@guaranteed Quote) -> Int
// CHECK:       } // end sil function '$s29borrow_accessor_copyable_base17materializedQuoteySiAA3BoxVyAA0F0VGF'
public func materializedQuote(_ box: Box<Quote>) -> Int {
  box.row.quote()
}

// A consuming use still needs a copy of its own.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_copyable_base8spanTakeySis4SpanVyxGAA6PricedRzlF :
// CHECK:         [[ROW:%.*]] = apply {{%.*}}<Row>({{%.*}}, %0) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @guaranteed Span<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:         [[COPY:%.*]] = alloc_stack $Row
// CHECK:         copy_addr [[ROW]] to [init] [[COPY]]
// CHECK:         [[METHOD:%.*]] = witness_method $Row, #Priced.take
// CHECK-NEXT:    apply [[METHOD]]<Row>([[COPY]])
// CHECK:       } // end sil function '$s29borrow_accessor_copyable_base8spanTakeySis4SpanVyxGAA6PricedRzlF'
public func spanTake<Row: Priced>(_ rows: Span<Row>) -> Int {
  rows[0].take()
}

// Memory reached through an addressor can change during the call.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_copyable_base12pointeePriceySiSPyAA3BoxVyxGGAA6PricedRzlF :
// CHECK:         [[ACCESS:%.*]] = begin_access [read] [unsafe]
// CHECK:         end_access [[ACCESS]]
// CHECK:         [[ROW:%.*]] = apply {{%.*}}<Row>({{%.*}}) : $@convention(method) <τ_0_0> (@in_guaranteed Box<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:         [[COPY:%.*]] = alloc_stack $Row
// CHECK:         copy_addr [[ROW]] to [init] [[COPY]]
// CHECK:         [[GETTER:%.*]] = witness_method $Row, #Priced.price!getter
// CHECK-NEXT:    apply [[GETTER]]<Row>([[COPY]])
// CHECK:       } // end sil function '$s29borrow_accessor_copyable_base12pointeePriceySiSPyAA3BoxVyxGGAA6PricedRzlF'
public func pointeePrice<Row: Priced>(_ pointer: UnsafePointer<Box<Row>>) -> Int {
  unsafe pointer.pointee.row.price
}

// A local can be consumed by a later argument, so the copy has to happen
// before `destroy`.
// CHECK-LABEL: sil [ossa] @$s29borrow_accessor_copyable_base14ownerBorrowingyyxlF :
// CHECK:         [[ROW:%.*]] = apply {{%.*}}<Row>({{%.*}}) : $@convention(method) <τ_0_0> (@in_guaranteed Owner<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:         [[COPY:%.*]] = alloc_stack $Row
// CHECK:         copy_addr [[ROW]] to [init] [[COPY]]
// CHECK:         apply {{%.*}}<Row>({{%.*}}) : $@convention(thin) <τ_0_0> (@in Owner<τ_0_0>) -> Int
// CHECK:         apply {{%.*}}<Row>([[COPY]], {{%.*}})
// CHECK:       } // end sil function '$s29borrow_accessor_copyable_base14ownerBorrowingyyxlF'
public func ownerBorrowing<Row>(_ row: Row) {
  let owner = Owner(_row: row)
  use(owner.row, destroy(owner))
}

// An initializer owns its parameters, which can be consumed as well.
public struct Holder<Row> {
  // CHECK-LABEL: sil [ossa] @$s29borrow_accessor_copyable_base6HolderVyACyxGAA3BoxVyxGcfC :
  // CHECK:         [[ROW:%.*]] = apply {{%.*}}<Row>({{%.*}}) : $@convention(method) <τ_0_0> (@in_guaranteed Box<τ_0_0>) -> @guaranteed_address τ_0_0
  // CHECK:         [[COPY:%.*]] = alloc_stack $Row
  // CHECK:         copy_addr [[ROW]] to [init] [[COPY]]
  // CHECK:       } // end sil function '$s29borrow_accessor_copyable_base6HolderVyACyxGAA3BoxVyxGcfC'
  public init(_ box: Box<Row>) {
    use(box.row, 0)
  }
}
