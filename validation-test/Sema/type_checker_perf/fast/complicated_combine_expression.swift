// RUN: %target-typecheck-verify-swift -solver-scope-threshold=15000

// REQUIRES: OS=macosx

import Combine
import Foundation

struct Horse: Hashable {}
struct Sheep: Hashable {}

struct Barn: Hashable {
    var material: BuildingMaterial
}

enum BuildingMaterial: Hashable {
    case wood
    case metal
}

enum BarnStyle {
    case historic
    case modern
}

enum Flags: Int {
    case warpSpeed = 0
}

struct HorseMagazine {
    static func photoshoot(string: String, barnStyle: BarnStyle) -> [HorseMagazine] {
        fatalError()
    }
}

class Farm {
    static let farm = Farm()

    var sheep: Set<Sheep> = []
    var name: String = ""

    init() {}
}

extension Notification.Name {
    static let horseChanged = Notification.Name(rawValue: "")
    static let sheepChanged = Notification.Name(rawValue: "")
}

final class FarmModel: ObservableObject {
    @Published var firstString: String = ""
    @Published var secondString: String = ""
    @Published var flags: Int = 0
    @Published var horses: Set<Horse> = []
    @Published var barn: Barn = .init(material: .wood)

    private var cancellable: (any Cancellable)?

    init() {
        cancellable = NotificationCenter.default.publisher(for: Notification.Name.horseChanged)
            .map { _ in Farm.farm.sheep }
            .prepend(Farm.farm.sheep)
            .combineLatest(
                $firstString.prepend("").removeDuplicates()
                    .combineLatest($barn.prepend(.init(material: .wood)).removeDuplicates())
                    .map { tuple -> [HorseMagazine] in
                        let (firstString, barn) = tuple
                        return HorseMagazine.photoshoot(string: firstString,
                                                        barnStyle: barn.material == .wood ? .historic : .modern)
                    },
                $secondString.prepend(Farm.farm.name).removeDuplicates()
                    .combineLatest(
                        NotificationCenter.default.publisher(for: Notification.Name.horseChanged).map { _ in () }.prepend(()),
                        NotificationCenter.default.publisher(for: Notification.Name.sheepChanged).map { _ in () }.prepend(())
                    ) { x, _, _ in x }
            )
            .combineLatest($horses.prepend([]).removeDuplicates()) { ($0, $1) }
            .map { x, y -> ([Horse: Set<Sheep>], [Horse], Set<Sheep>) in
                fatalError()
            }
            .sink { (x, y, z) -> Void in
                fatalError()
            }
    }
}
