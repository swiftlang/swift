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
