// RUN: %target-typecheck-verify-swift

struct Article {
  let id: String
}

let keypath = \Article.id
func keypath_generator() -> KeyPath<Article, String> { return \.id }
func const_map(_ map: _const KeyPath<Article, String>) {}
func const_map_in_wrong_position(_const _ map: KeyPath<Article, String>) {} // expected-warning {{'_const' before a parameter name is not allowed, place it before the parameter type instead; this is an error in the Swift 6 language mode}}

const_map(\.id)
const_map(\Article.id)

const_map(keypath_generator()) // expected-error {{expect a compile-time constant literal}}
const_map(keypath) // expected-error {{expect a compile-time constant literal}}
