// RUN: %target-run-simple-swift(-enable-experimental-feature OnewayNowait -Xfrontend -disable-experimental-parser-round-trip -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: swift_feature_OnewayNowait

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// 'oneway' is not part of the runtime type of a function value: the metadata
// of a generic instantiation over a 'oneway' function type is the metadata of
// the plain function type, and its mangled name round-trips through the
// runtime's metadata lookup

actor Worker {
  func work() oneway {}
  func ping() async oneway {}
}

struct Box<T> {
  var value: T
}

func describe<T>(_ value: T) -> String {
  let name = _mangledTypeName(T.self) ?? "<no mangled name>"
  let found = _typeByName(name).map { $0 == T.self } ?? false
  return "\(T.self) mangled: \(name) found: \(found)"
}

func values(_ w: isolated Worker) -> [String] {
  let work = w.work
  let ping = w.ping
  return [
    describe(work),
    describe([work]),
    describe(Box(value: work)),
    describe(Box(value: ping)),
  ]
}

@main struct Main {
  static func main() async {
    for line in await values(Worker()) {
      print(line)
    }
    // CHECK: () -> () mangled: yyc found: true
    // CHECK-NEXT: Array<() -> ()> mangled: SayyycG found: true
    // CHECK-NEXT: Box<() -> ()> mangled: {{.*}}3BoxVyyycG found: true
    // CHECK-NEXT: Box<() async -> ()> mangled: {{.*}}3BoxVyyyYacG found: true
  }
}
