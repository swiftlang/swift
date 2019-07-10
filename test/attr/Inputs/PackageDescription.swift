public enum SwiftVersion {
    // CHECK: @available(_PackageDescription, introduced: 3.0, deprecated: 4.2, obsoleted: 5.0)
    @available(_PackageDescription, introduced: 3.0, deprecated: 4.2, obsoleted: 5.0)
    case v3

    case v4

    // CHECK: @available(_PackageDescription 5.0)
    // CHECK-NEXT: @available(OSX 10.1, *)
    // CHECK-NEXT: v5
    @available(_PackageDescription, introduced: 5.0)
    @available(macOS, introduced: 10.1)
    case v5
}

public class Package {

    public var swiftVersion: [SwiftVersion]

    @available(_PackageDescription 4.3)
    public var buildSettings: [String: String] {
        get {
            return _buildSettings
        }
        set {
            _buildSettings = newValue
        }
    }
    private var _buildSettings: [String: String]

    @available(_PackageDescription 5)
    public init(
        swiftVersion: [SwiftVersion] = [],
        buildSettings: [String: String] = [:]
    ) {
        self._buildSettings = buildSettings
        self.swiftVersion = swiftVersion
    }

    @available(_PackageDescription, introduced: 3.0, obsoleted: 5.0)
    public init(
        swiftVersion: [SwiftVersion] = []
    ) {
        self._buildSettings = [:]
        self.swiftVersion = swiftVersion
    }

    public func serialize() {
        for version in swiftVersion {
            print(version)
        }
        print(_buildSettings)
    }
}
