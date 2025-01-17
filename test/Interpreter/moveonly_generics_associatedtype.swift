// RUN: %target-swift-emit-sil %s -DBAD_COPY -verify -sil-verify-all -enable-experimental-feature SuppressedAssociatedTypes
// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all -enable-experimental-feature SuppressedAssociatedTypes) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all -enable-experimental-feature SuppressedAssociatedTypes) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_SuppressedAssociatedTypes


class Backend {
    private var data: [String: String] = [:]
    func read(key: String) -> String? { data[key] }
    func write(key: String, value: String?, author: String) {
        data[key] = "\(author) wrote: \(value ?? "nil")"
    }
}

protocol StringSubscript: ~Copyable {
  subscript(key: String) -> String? { get set }
}

protocol Accessor {
    associatedtype Mutator: StringSubscript & ~Copyable
    func mutator(as author: String) -> Mutator
}

struct SomeAccessor: Accessor {
    private let backend = Backend()

    struct Mutator: StringSubscript, ~Copyable {
        let author: String
        var backend: Backend

        subscript(key: String) -> String? {
            get { backend.read(key: key) }
            set { backend.write(key: key, value: newValue, author: author) }
        }
    }

    func mutator(as author: String) -> Mutator { .init(author: author, backend: backend) }
}


// The major goal of this `Accessor` is that you cannot escape the `Mutator` in the context of the writer closure.
extension Accessor {
    func write(as author: String, body: (inout Mutator) -> ()) {
        var writable = self.mutator(as: author)
        body(&writable)
    }

    func read(key: String) -> String? {
      let immutable = self.mutator(as: "READ_ONLY")
      return immutable[key]
    }
}

func asGeneric<T: Accessor>(_ access: T) {
  #if BAD_COPY
  access.write(as: "Nobody") { // expected-error {{'$0' used after consume}}
    let _ = $0  // expected-note {{consumed}}
    $0["Blah"] = "illegal" // expected-note {{used}}
  }
  #endif

  access.write(as: "Someone") { $0["Foo"] = "Bar" }
  print(access.read(key: "Foo") ?? "nil")
  // CHECK: Someone wrote: Bar
}

defer { check() }
func check() {
  let access = SomeAccessor()
  asGeneric(access)
}
