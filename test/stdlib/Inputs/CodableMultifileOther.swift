class BaseFirst: Codable {
  var baseMember = 1

  init() {}
}

class DerivedFirst: BaseFirst {
  var derivedMember = false
}

class BaseSecond: Codable {
  var baseMember = 2

  init() {}
}

class BaseThird: Codable {
  var baseMember = 3

  init() {}
}
