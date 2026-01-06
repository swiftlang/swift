// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

// https://github.com/swiftlang/swift/issues/48706

struct IssueAssignee {
    let issueUid: String
    let assigneeUid: String
}

struct IssueFollower {
    let issueUid: String
    let followerUid: String
}

struct IssueModel {
    let uid: String
    let title: String
    var assignees: [IssueAssignee] = []
    var followers: [IssueFollower] = []

    static let joins: [AnyRelationship<IssueModel>] = [
        OneToMany(\IssueModel.assignees, \IssueAssignee.issueUid, \IssueModel.uid),
        OneToMany(\IssueModel.followers, \IssueFollower.issueUid, \IssueModel.uid)
    ]
}

class AnyRelationship<Base> {
    func query(withModel model: inout Base) {
        fatalError("Needs to be overriden!")
    }
}

final class OneToMany<One, Many, SharedKey: Equatable>: AnyRelationship<One> {
    let oneKeyPath: WritableKeyPath<One, [Many]>
    let manyKeyPath: KeyPath<Many, SharedKey>
    let joinKeyPath: KeyPath<One, SharedKey>

    init(_ oneKeyPath: WritableKeyPath<One, [Many]>, _ manyKeyPath: KeyPath<Many, SharedKey>, _ joinKeyPath: KeyPath<One, SharedKey>) {
        self.oneKeyPath = oneKeyPath
        self.manyKeyPath = manyKeyPath
        self.joinKeyPath = joinKeyPath
    }

    override func query(withModel model: inout One) {
        // Force casts here simulate generic storage that allows us to get values by metatype.
        // For this playground we only support hardcoded `IssueAssignee` type, but that doesn't change
        // anything about overall API.
        if Many.self == IssueAssignee.self {
            model[keyPath: self.oneKeyPath] = assignees.filter { ($0 as! Many)[keyPath: self.manyKeyPath] == model[keyPath: self.joinKeyPath] } as! [Many]
        } else if Many.self == IssueFollower.self {
            model[keyPath: self.oneKeyPath] = followers.filter { ($0 as! Many)[keyPath: self.manyKeyPath] == model[keyPath: self.joinKeyPath] } as! [Many]
        } else {
            fatalError()
        }
    }
}

var issue = IssueModel(uid: "1", title: "ABC", assignees: [], followers: [])
let assignees = [
    IssueAssignee(issueUid: "1", assigneeUid: "Assigned to Issue 1"),
    IssueAssignee(issueUid: "2", assigneeUid: "Assigned to Issue 2"),
    IssueAssignee(issueUid: "1", assigneeUid: "Also assigned to issue 1!!!!")
]

let followers = [
    IssueFollower(issueUid: "1", followerUid: "Following Issue 1"),
    IssueFollower(issueUid: "2", followerUid: "Following Issue 2"),
    IssueFollower(issueUid: "1", followerUid: "Also following issue 1!!!!")
]

for join in IssueModel.joins {
    join.query(withModel: &issue)
}

print(issue)
