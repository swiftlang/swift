// rdar://189619132
// RUN: %target-typecheck-verify-swift

class CGImage {}
class CGImageSource {}

func test(cgImageSource: CGImageSource) {
  typealias T = (metadata: Any?, cgImage: CGImage)
  let _ = T(nil, cgImageSource) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
