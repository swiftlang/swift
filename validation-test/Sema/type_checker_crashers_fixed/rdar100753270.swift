// RUN: %target-typecheck-verify-swift

struct Location {
  var host: String?
}

struct Data: Hashable {
  func location() -> Location {
    fatalError()
  }
}

struct Test { // expected-note {{to match this opening '{'}}
  private struct Info {
    var data = Set<Data>()
    var desc: String
  }

  static func print(data: [Data]) {
    var infos = [Info]()

    for (index, info) in infos.enumerated() {
      let dataPerHost = Dictionary(grouping: info.data) { data in // expected-note {{'dataPerHost' declared here}}
        let location = data.location()
        guard let host = location.host else {
          return 0
        }

        for character in host {
        // Missing paren!
      }

      for _ in dataPerHost { // expected-error {{use of local variable 'dataPerHost' before its declaration}}
      }
    }
  }
} // expected-error@+1 {{expected '}' in struct}}
