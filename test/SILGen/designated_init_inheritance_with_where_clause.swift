// RUN: %target-swift-emit-silgen -primary-file %s | %FileCheck %s

public protocol Ungulate {}
public protocol Domesticated {}

public class Horse<U: Ungulate> {
  // CHECK-LABEL: sil [serialized] [exact_self_class] [ossa] @$s45designated_init_inheritance_with_where_clause5HorseCACyxGycfC : $@convention(method) <U where U : Ungulate> (@thick Horse<U>.Type) -> @owned Horse<U> {
  // CHECK-LABEL: sil [ossa] @$s45designated_init_inheritance_with_where_clause5HorseCACyxGycfc : $@convention(method) <U where U : Ungulate> (@owned Horse<U>) -> @owned Horse<U> {
  public init() { }

  // CHECK-LABEL: sil [serialized] [exact_self_class] [ossa] @$s45designated_init_inheritance_with_where_clause5HorseCACyxGycAA12DomesticatedRzrlufC : $@convention(method) <U where U : Domesticated, U : Ungulate> (@thick Horse<U>.Type) -> @owned Horse<U> {
  // CHECK-LABEL: sil [ossa] @$s45designated_init_inheritance_with_where_clause5HorseCACyxGycAA12DomesticatedRzrlufc : $@convention(method) <U where U : Domesticated, U : Ungulate> (@owned Horse<U>) -> @owned Horse<U> {
  public init() where U: Domesticated { }
}

public class Pony<U : Ungulate> : Horse<U> {
  // CHECK-LABEL: sil [serialized] [exact_self_class] [ossa] @$s45designated_init_inheritance_with_where_clause4PonyCACyxGycfC : $@convention(method) <U where U : Ungulate> (@thick Pony<U>.Type) -> @owned Pony<U> {
  // CHECK-LABEL: sil [ossa] @$s45designated_init_inheritance_with_where_clause4PonyCACyxGycfc : $@convention(method) <U where U : Ungulate> (@owned Pony<U>) -> @owned Pony<U> {

  // CHECK-LABEL: sil [serialized] [exact_self_class] [ossa] @$s45designated_init_inheritance_with_where_clause4PonyCACyxGycAA12DomesticatedRzrlufC : $@convention(method) <U where U : Domesticated, U : Ungulate> (@thick Pony<U>.Type) -> @owned Pony<U> {
  // CHECK-LABEL: sil [ossa] @$s45designated_init_inheritance_with_where_clause4PonyCACyxGycAA12DomesticatedRzrlufc : $@convention(method) <U where U : Domesticated, U : Ungulate> (@owned Pony<U>) -> @owned Pony<U> {
}

public class Barn<T> {
  init(_: T) where T : AnyObject {}
  init<U>(_: T, _: U) where T : Domesticated, U : Ungulate {}
}

public class BigBarn : Barn<AnyObject> {
  // CHECK-LABEL: sil hidden [ossa] @$s45designated_init_inheritance_with_where_clause7BigBarnCyACyXlcfc : $@convention(method) (@owned AnyObject, @owned BigBarn) -> @owned BigBarn {
}

public struct Cat : Domesticated {}
public struct Sheep : Ungulate {}

public class SmallBarn : Barn<Cat> {
  // CHECK-LABEL: sil hidden [ossa] @$s45designated_init_inheritance_with_where_clause9SmallBarnCyAcA3CatV_xtcAA8UngulateRzlufc : $@convention(method) <U where U : Ungulate> (Cat, @in U, @owned SmallBarn) -> @owned SmallBarn {
  // CHECK-LABEL: sil private [thunk] [ossa] @$s45designated_init_inheritance_with_where_clause9SmallBarnCyAcA3CatV_xtcAA8UngulateRzlufCAA0H0CyAHyxGx_qd__tcAA12DomesticatedRzAaFRd__lufCTV : $@convention(method) <τ_0_0 where τ_0_0 : Ungulate> (@in Cat, @in τ_0_0, @thick SmallBarn.Type) -> @owned SmallBarn {
}

// CHECK-LABEL: sil_vtable [serialized] BigBarn {
// CHECK-NEXT:     #Barn.init!allocator: <T where T : AnyObject> (Barn<T>.Type) -> (T) -> Barn<T> : @$s45designated_init_inheritance_with_where_clause7BigBarnCyACyXlcfC [override] // BigBarn.__allocating_init(_:)
// CHECK-NEXT:   #Barn.init!allocator: <T where T : Domesticated><U where U : Ungulate> (Barn<T>.Type) -> (T, U) -> Barn<T> : @$s45designated_init_inheritance_with_where_clause4BarnCyACyxGx_qd__tcAA12DomesticatedRzAA8UngulateRd__lufC [inherited]  // Barn.__allocating_init<A>(_:_:)
// CHECK-NEXT:   #BigBarn.deinit!deallocator: @$s45designated_init_inheritance_with_where_clause7BigBarnCfD  // BigBarn.__deallocating_deinit
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable [serialized] SmallBarn {
// CHECK-NEXT:   #Barn.init!allocator: <T where T : AnyObject> (Barn<T>.Type) -> (T) -> Barn<T> : @$s45designated_init_inheritance_with_where_clause4BarnCyACyxGxcRlzClufC [inherited] // Barn.__allocating_init<A>(_:)
// CHECK-NEXT:   <T where T : Domesticated><U where U : Ungulate> (Barn<T>.Type) -> (T, U) -> Barn<T> : @$s45designated_init_inheritance_with_where_clause9SmallBarnCyAcA3CatV_xtcAA8UngulateRzlufCAA0H0CyAHyxGx_qd__tcAA12DomesticatedRzAaFRd__lufCTV [override] // vtable thunk for Barn.__allocating_init<A>(_:_:) dispatching to SmallBarn.__allocating_init<A>(_:_:)
// CHECK-NEXT:   #SmallBarn.deinit!deallocator: @$s45designated_init_inheritance_with_where_clause9SmallBarnCfD  // SmallBarn.__deallocating_deinit
// CHECK-NEXT: }