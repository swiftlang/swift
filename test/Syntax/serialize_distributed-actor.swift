// RUN: %swift-syntax-test -input-source-filename %s -serialize-raw-tree > %t
// RUN: diff %t %S/Inputs/serialize_distributed_actor.json -u

distributed actor DA {
    distributed func hello(name: String) -> String {
        "Hello \(name)!"
    }
}