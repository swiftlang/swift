// RUN: %target-swift-frontend -emit-sil -verify %s

struct PinnedInfo {
    var hasPinnedCells: Bool {
        return !allPins.isEmpty
    }

    var allPins: ContiguousArray<Int>

    init(allPins: ContiguousArray<Int> = []) {
        self.allPins = allPins
    }
}

enum ModelState: Hashable, CaseIterable {
    case initial
    case modified
}

struct Storage<Model> {
    var initial: Model?
    var modified: Model?

    mutating func setStorage(_ storage: Model, at state: ModelState) {
            switch state {
            case .initial:
                initial = storage
            case .modified:
                modified = storage
            }
        }
}

func test() {
  var storagePinInfo = Storage<PinnedInfo>()
  var storageAllPins = Storage<Array<Int>>()

  var pinInfo = PinnedInfo(allPins: []) // expected-warning{{variable 'pinInfo' was never mutated; consider changing to 'let' constant}}
  var allPins = Array(repeating: 0, count: 100) // expected-warning{{variable 'allPins' was never mutated; consider changing to 'let' constant}}

  storagePinInfo.setStorage(consume pinInfo, at: .initial)
  storageAllPins.setStorage(consume allPins, at: .initial)
}

var storagePinInfo = Storage<PinnedInfo>()
var storageAllPins = Storage<Array<Int>>()

var pinInfo = PinnedInfo(allPins: [])
var allPins = Array(repeating: 0, count: 100)

storagePinInfo.setStorage(consume pinInfo, at: .initial) // expected-error{{'consume' cannot be applied to globals}}
storageAllPins.setStorage(consume allPins, at: .initial) // expected-error{{'consume' cannot be applied to globals}}
