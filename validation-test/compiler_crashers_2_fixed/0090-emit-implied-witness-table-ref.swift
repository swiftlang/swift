// RUN: %target-swift-frontend -primary-file %s -emit-ir

// From https://twitter.com/Dimillian/status/854436731894018048

class User: Equatable {
    var id: String = ""
    var username: String = ""
    var name: String = ""
    var avatar: String?
    var friends: [String]?
    var followers: [String]?
    var library: [String]?
    var libraryCount: Int = 0
    var friendsCount: Int = 0
    var followersCount: Int = 0

    var loadMoreFriends = false
    var loadMoreFollowers = false
    var loadMoreLibrary = false
}

func ==<T: User>(lhs: T?, rhs: T?) -> Bool {
    guard let lhs = lhs, let rhs = rhs else {
        return false
    }
    return lhs == rhs
}

func ==<T: User>(lhs: T, rhs: T) -> Bool {
    return lhs.id == rhs.id &&
        lhs.username == rhs.username &&
        lhs.name == rhs.name &&
        lhs.libraryCount == rhs.libraryCount &&
        lhs.friendsCount == rhs.friendsCount &&
        lhs.followersCount == rhs.followersCount &&
        lhs.friends == rhs.friends &&
        lhs.followers == rhs.followers &&
        lhs.library == rhs.library
}

func ==<T: Equatable>(lhs: [T]?, rhs: [T]?) -> Bool {
    switch (lhs, rhs) {
    case (.some(let lhs), .some(let rhs)):
        return lhs == rhs
    case (.none, .none):
        return true
    default:
        return false
    }
}
