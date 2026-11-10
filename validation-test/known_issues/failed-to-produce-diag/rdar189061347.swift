// rdar://189061347
// RUN: %target-typecheck-verify-swift

struct Configuration { var isPlaybackEnabled = false }
struct Status { var isPlaybackLikelyToKeepUp = false }

@dynamicMemberLookup
final class Player<Content> {
  var configuration = Configuration()
  var status = Status()

  subscript<Value>(dynamicMember keyPath: WritableKeyPath<Configuration, Value>) -> Value {
    get { configuration[keyPath: keyPath] }
    set { configuration[keyPath: keyPath] = newValue }
  }
  subscript<Value>(dynamicMember keyPath: KeyPath<Status, Value>) -> Value {
    status[keyPath: keyPath]
  }
}

func test(player: Player<Int>) {
  player.play() // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  _ = player.nonexistentProperty // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
