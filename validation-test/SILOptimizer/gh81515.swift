// RUN: %target-build-swift %s

typealias LeagueDayOfWeek = UInt8

let settings = LeagueSettings(divisions: [.init(id: 0, dayOfWeek: 0), .init(id: 1, dayOfWeek: 0)])
let dummy = Dummy()
dummy.dummy(settings: settings)

struct LeagueSettings: Codable, Sendable {
    let divisions:ContiguousArray<LeagueDivision>
}
struct LeagueEntry: Codable, Sendable {
    typealias IDValue = Int
}
struct LeagueDivision: Codable, Hashable, Sendable {
    typealias IDValue = Int
    let id:IDValue
    let dayOfWeek:LeagueDayOfWeek
}
struct Dummy: ~Copyable {
}
extension Dummy {
    func dummy(settings: LeagueSettings) {
        var divisionEntries:ContiguousArray<Set<LeagueEntry.IDValue>> = .init(repeating: Set(), count: settings.divisions.count)
        var grouped:[LeagueDayOfWeek:Set<LeagueEntry.IDValue>] = [:]
        for division in settings.divisions {
            /* // OK
            if grouped[division.dayOfWeek] == nil {
                grouped[division.dayOfWeek] = divisionEntries[division.id]
            } else {
                grouped[division.dayOfWeek]!.formUnion(divisionEntries[division.id])
            }*/
            grouped[division.dayOfWeek, default: []].formUnion(divisionEntries[division.id]) // crashes
        }
    }
}
