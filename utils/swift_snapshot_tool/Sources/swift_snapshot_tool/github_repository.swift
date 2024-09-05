import ArgumentParser

enum Platform: String, EnumerableFlag {
  case osx
  case ubuntu1404
  case ubuntu1604
  case ubuntu1804

  var fileType: String {
    switch self {
    case .osx:
      return "pkg"
    case .ubuntu1404,
      .ubuntu1604,
      .ubuntu1804:
      return "tar.gz"
    }
  }

  var toolchainType: String {
    switch self {
    case .osx:
      return "xcode"
    case .ubuntu1404,
      .ubuntu1604,
      .ubuntu1804:
      return self.rawValue
    }
  }
}

enum Branch: String, EnumerableFlag {
  case development
  case release50 = "5.0"
  case release60 = "6.0"
}
