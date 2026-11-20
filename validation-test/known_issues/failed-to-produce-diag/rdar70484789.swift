// rdar://70484789
// RUN: %target-typecheck-verify-swift

class Group {
  var managerID: Int?
  var members: [Any]?
}

class Person {}

func test(group: Group) {
  let managerID = group.managerID
  let _: Person? = group.members?.first(where: { ($0 as! Group).managerID == managerID }) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
