// RUN: %target-typecheck-verify-swift

struct Article {
  let id: String
}

let keypath = \Article.id
func keypath_generator() -> KeyPath<Article, String> { return \.id }
func const_map(_const _ map: KeyPath<Article, String>) {}

const_map(\.id)
const_map(\Article.id)

const_map(keypath_generator()) // expected-error {{expect a compile-time constant literal}}
const_map(keypath) // expected-error {{expect a compile-time constant literal}}
