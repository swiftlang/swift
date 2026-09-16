// RUN: %target-swift-frontend -strict-concurrency=complete -swift-version 6 -parse-as-library -emit-sil -verify %s -o /dev/null

nonisolated final class PropertyIsolatedSendable: Sendable {
  @MainActor var value: Int

  nonisolated init() async {
    self.value = 37
    self.value += 5

    let task = Task { @MainActor () -> Void in // expected-note {{after making a copy of 'self', only nonisolated properties of 'self' can be accessed from this init}}
      self.value += 1
    }

    self.value -= 1 // expected-error {{cannot access property 'value' here in nonisolated initializer}}
    _ = await task.value
  }
}

@globalActor
actor AnotherActor {
  static let shared = AnotherActor()
}

nonisolated final class MixedIsolation {
  @MainActor var isolated: Int
  var nonisolated: Int

  nonisolated init() {
    self.isolated = 0
    self.nonisolated = 0

    escape() // expected-note {{after calling instance method 'escape()', only nonisolated properties of 'self' can be accessed from this init}}

    self.nonisolated += 1
    self.isolated += 1 // expected-error {{cannot access property 'isolated' here in nonisolated initializer}}
  }

  @MainActor init(mainActor: ()) {
    self.isolated = 0
    self.nonisolated = 0

    escape()

    self.nonisolated += 1
    self.isolated += 1
  }

  @AnotherActor init(anotherActor: ()) {
    self.isolated = 0
    self.nonisolated = 0

    escape() // expected-note {{after calling instance method 'escape()', only global actor 'AnotherActor'-isolated properties of 'self' can be accessed from this init}}

    self.nonisolated += 1
    self.isolated += 1 // expected-error {{cannot access property 'isolated' here in global actor 'AnotherActor'-isolated initializer}}
  }

  nonisolated func escape() {}
}
