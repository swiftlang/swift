// RUN: %batch-code-completion

@resultBuilder
struct Builder0 {
  static func buildBlock<T>(_ x: T) -> T { x }
  static func buildArray<T>(_ components: [T]) -> T { components.first! }
}
@resultBuilder
struct Builder1 {
  static func buildBlock<T>(_ x: T) -> T { x }
}
@resultBuilder
struct Builder2 {
  static func buildBlock<T>(_ x: T) -> T { x }
}
@resultBuilder
struct Builder3 {
  static func buildBlock<T>(_ x: T) -> T { x }
}
@resultBuilder
struct Builder4 {
  static func buildBlock<T>(_ x: T) -> T { x }
}
@resultBuilder
struct Builder5 {
  static func buildBlock<T>(_ x: T) -> T { x }
}
@resultBuilder
struct Builder6 {
  static func buildBlock<T>(_ x: T) -> T { x }
}
@resultBuilder
struct Builder7 {
  static func buildBlock<T>(_ x: T) -> T { x }
}
@resultBuilder
struct Builder8 {
  static func buildBlock<T>(_ x: T) -> T { x }
}
@resultBuilder
struct Builder9 {
  static func buildBlock<T>(_ x: T) -> T { x }
}

protocol P0 {}
protocol P1 {}
protocol P2 {}
protocol P3 {}
protocol P4 {}
protocol P5 {}
protocol P6 {}
protocol P7 {}
protocol P8 {}
protocol P9 {}

struct S<T> {}
extension S: P0 where T : P0 { init<U>(@Builder0 fn: () -> U) {} }
extension S: P1 where T : P1 { init<U>(@Builder1 fn: () -> U) {} }
extension S: P2 where T : P2 { init<U>(@Builder2 fn: () -> U) {} }
extension S: P3 where T : P3 { init<U>(@Builder3 fn: () -> U) {} }
extension S: P4 where T : P4 { init<U>(@Builder4 fn: () -> U) {} }
extension S: P5 where T : P5 { init<U>(@Builder5 fn: () -> U) {} }
extension S: P6 where T : P6 { init<U>(@Builder6 fn: () -> U) {} }
extension S: P7 where T : P7 { init<U>(@Builder7 fn: () -> U) {} }
extension S: P8 where T : P8 { init<U>(@Builder8 fn: () -> U) {} }
extension S: P9 where T : P9 { init<U>(@Builder9 fn: () -> U) {} }

struct K {
  var foo: Int
}

// Make sure we ignore all the result builders that cannot handle for loops
// and just use Builder0. There's also no way 'T' can be inferred here but
// that doesn't really matter for this test.
func foo(_ ks: [K]) {
  S {
    for k0 in ks {
      S {
        for k1 in ks {
          S {
            for k2 in ks {
              S {
                for k3 in ks {
                  S {
                    for k4 in ks {
                      S {
                        for k5 in ks {
                          k5.#^COMPLETE^#
                          // COMPLETE: Decl[InstanceVar]/CurrNominal: foo[#Int#]; name=foo
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
