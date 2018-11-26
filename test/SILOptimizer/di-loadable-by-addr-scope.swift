// Make sure we don't crash when verifying.
// RUN: %target-swift-frontend -emit-ir %s -Onone -sil-verify-all \
// RUN:   -Xllvm -sil-print-debuginfo -o - 2>&1 | %FileCheck %s

// CHECK: define hidden swiftcc {{.*}} @"$S4main18DependencyResolverC14resolveSubtree_9excludings11AnySequenceVyAA20VersionAssignmentSetVy9ContainerQzGGAK_SDyAJ_10IdentifierQZShyAA0I0VGGtF"

public struct Version {
    public let major: Int
    public let minor: Int
    public let patch: Int
    public let prereleaseIdentifiers: [String]
    public let buildMetadataIdentifiers: [String]
    public init(
        _ major: Int,
        _ minor: Int,
        _ patch: Int,
        prereleaseIdentifiers: [String] = [],
        buildMetadataIdentifiers: [String] = []
    ) {
        self.major = major
        self.minor = minor
        self.patch = patch
        self.prereleaseIdentifiers = prereleaseIdentifiers
        self.buildMetadataIdentifiers = buildMetadataIdentifiers
    }
}
extension Version: Hashable {
    static public func == (lhs: Version, rhs: Version) -> Bool {
        return lhs.major == rhs.major &&
               lhs.buildMetadataIdentifiers == rhs.buildMetadataIdentifiers
    }
    public var hashValue: Int {
        var result: UInt64 = 0
        return Int(truncatingIfNeeded: result)
    }
}

public protocol PackageContainerIdentifier: Hashable { }
public protocol PackageContainer {
    associatedtype Identifier: PackageContainerIdentifier
    var identifier: Identifier { get }
}
public protocol PackageContainerProvider {
    associatedtype Container: PackageContainer
    func getContainer(
    )
}
public struct PackageContainerConstraint<T: PackageContainerIdentifier> {
    public enum Requirement: Hashable {
    }
}

public enum BoundVersion {
    case version(Version)
}

struct VersionAssignmentSet<C: PackageContainer> {
    typealias Container = C
    typealias Identifier = Container.Identifier
    fileprivate var assignments: [Identifier: (container: Container, binding: BoundVersion)]
    init() {
        assignments = [:]
    }
    subscript(identifier: Identifier) -> BoundVersion? {
        get {
            return assignments[identifier]?.binding
        }
    }
    subscript(container: Container) -> BoundVersion? {
        get {
            return self[container.identifier]
        }
        set {
        }
    }
}
public class DependencyResolver<P: PackageContainerProvider> {
    public typealias Provider = P
    public typealias Container = Provider.Container
    public typealias Identifier = Container.Identifier
    public typealias Constraint = PackageContainerConstraint<Identifier>
    typealias AssignmentSet = VersionAssignmentSet<Container>
    public let provider: Provider
    private let isPrefetchingEnabled: Bool
    private let skipUpdate: Bool
    public init(
        _ provider: Provider,
        isPrefetchingEnabled: Bool = false,
        skipUpdate: Bool = false
    ) {
        self.provider = provider
        self.isPrefetchingEnabled = isPrefetchingEnabled
        self.skipUpdate = skipUpdate
    }
    func resolveSubtree(
        _ container: Container,
        excluding allExclusions: [Identifier: Set<Version>]
    ) -> AnySequence<AssignmentSet> {
        func merge(constraints: [Constraint], binding: BoundVersion) -> AnySequence<AssignmentSet> {
            var assignment = AssignmentSet()
            assignment[container] = binding
            return AnySequence([])
        }
        return AnySequence([])
    }
}
