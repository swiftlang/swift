// RUN: %target-typecheck-verify-swift

enum Color {
  case grayscale(Int)
  case rgb(r : Int, g : Int, b : Int)
  indirect case inverted(Color)
  case transparent
}

func colorToInt (_ c : Color) -> Int {
  return 0
}

///////// begin tests for SR-13706

// errors that are expected to be accompanied by the let/var note

switch Color.transparent {
  case grayscale: () // expected-error {{cannot find 'grayscale' in scope}} expected-note {{add 'let' or 'var' keyword to bind 'grayscale' in a pattern}}
  default: ()
}

switch Color.transparent {
  case .inverted(.inverted(.inverted(.rgb(🐈, _, _)))): () // expected-error {{cannot find '🐈' in scope}} expected-note {{add 'let' or 'var' keyword to bind '🐈' in a pattern}}
  default: ()
}

switch Color.transparent {
  case .rgb(_, g, // expected-error {{cannot find 'g' in scope}} expected-note {{add 'let' or 'var' keyword to bind 'g' in a pattern}}
               b): () // expected-error {{cannot find 'b' in scope}} expected-note {{add 'let' or 'var' keyword to bind 'b' in a pattern}}
  default: ()
}

// errors that should not have a note

switch Color.transparent {
  case .rgb(5 / 2, y / 3, _): () // expected-error {{cannot find 'y' in scope}}
  default: ()
}

switch Color.transparent {
  case inverted(koala): () // expected-error {{cannot find 'inverted' in scope}} expected-error {{cannot find 'koala' in scope}}
  case inverted(let koala): () // expected-error {{cannot find 'inverted' in scope}}
  case let inverted(koala): () // expected-error {{cannot find 'inverted' in scope}}
  case .inverted(let koala): ()
  case let .inverted(koala): ()
}

switch Color.transparent {
  case .grayscale(colorToInt(y)): () // expected-error {{cannot find 'y' in scope}}
  case .grayscale(mountian(43)): () // expected-error {{cannot find 'mountian' in scope}}
  case .grayscale(hop(y)): () // expected-error {{cannot find 'hop' in scope}} expected-error {{cannot find 'y' in scope}}
  default: ()
}