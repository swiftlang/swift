//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2014-2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/*
 An extemely simple rendition of the Xcode project model into a plist.  There
 is only enough functionality to allow serialization of Xcode projects.
 */

extension Xcode.Project {
  fileprivate enum MinVersion {
    case xcode8, xcode16

    var objectVersion: Int {
      switch self {
      case .xcode8: 46
      case .xcode16: 77
      }
    }
  }

  fileprivate var hasBuildableFolders: Bool {
    var worklist: [Xcode.Reference] = []
    worklist.append(mainGroup)
    while let ref = worklist.popLast() {
      if let fileRef = ref as? Xcode.FileReference, fileRef.isBuildableFolder {
        return true
      }
      if let group = ref as? Xcode.Group {
        worklist += group.subitems
      }
    }
    return false
  }
}

extension Xcode.Project: PropertyListSerializable {
  fileprivate var xcodeClassName: String { "PBXProject" }

  /// Generates and returns the contents of a `project.pbxproj` plist.  Does
  /// not generate any ancillary files, such as a set of schemes.
  ///
  /// Many complexities of the Xcode project model are not represented; we
  /// should not add functionality to this model unless it's needed, since
  /// implementation of the full Xcode project model would be unnecessarily
  /// complex.
  public func generatePlist() throws -> PropertyList {
    // The project plist is a bit special in that it's the archive for the
    // whole file.  We create a plist serializer and serialize the entire
    // object graph to it, and then return an archive dictionary containing
    // the serialized object dictionaries.
    let serializer = PropertyListSerializer()
    try serializer.serialize(object: self)
    var minVersion = MinVersion.xcode8
    if hasBuildableFolders {
      minVersion = .xcode16
    }
    return .dictionary([
      "archiveVersion": .string("1"),
      "objectVersion": .string(String(minVersion.objectVersion)),
      "rootObject": .identifier(serializer.id(of: self)),
      "objects": .dictionary(serializer.idsToDicts),
    ])
  }

  /// Called by the Serializer to serialize the Project.
  fileprivate func serialize(to serializer: PropertyListSerializer) throws -> [String: PropertyList] {
    // Create a `PBXProject` plist dictionary.
    // Note: we skip things like the `Products` group; they get autocreated
    // by Xcode when it opens the project and notices that they are missing.
    // Note: we also skip schemes, since they are not in the project plist.
    var dict = [String: PropertyList]()
    dict["isa"] = .string(xcodeClassName)
    // Since the project file is generated, we opt out of upgrade-checking.
    // FIXME: Should we really?  Why would we not want to get upgraded?
    dict["attributes"] = .dictionary([
      "LastUpgradeCheck": .string("9999"),
      "LastSwiftMigration": .string("9999"),
    ])
    dict["compatibilityVersion"] = .string("Xcode 3.2")
    dict["developmentRegion"] = .string("en")
    // Build settings are a bit tricky; in Xcode, each is stored in a named
    // XCBuildConfiguration object, and the list of build configurations is
    // in turn stored in an XCConfigurationList.  In our simplified model,
    // we have a BuildSettingsTable, with three sets of settings:  one for
    // the common settings, and one each for the Debug and Release overlays.
    // So we consider the BuildSettingsTable to be the configuration list.
    dict["buildConfigurationList"] = try .identifier(serializer.serialize(object: buildSettings))
    dict["mainGroup"] = try .identifier(serializer.serialize(object: mainGroup))
    dict["hasScannedForEncodings"] = .string("0")
    dict["knownRegions"] = .array([.string("en")])
    if let productGroup = productGroup {
      dict["productRefGroup"] = .identifier(serializer.id(of: productGroup))
    }
    dict["projectDirPath"] = .string(projectDir)
    // Ensure that targets are output in a sorted order.
    let sortedTargets = targets.sorted(by: { $0.name < $1.name })
    dict["targets"] = try .array(
      sortedTargets.map({ target in
        try .identifier(serializer.serialize(object: target))
      })
    )
    return dict
  }
}

extension Xcode.BuildableFolder.TargetException: PropertyListSerializable {
  var xcodeClassName: String { "PBXFileSystemSynchronizedBuildFileExceptionSet" }

  fileprivate func serialize(
    to serializer: PropertyListSerializer
  ) throws -> [String: PropertyList] {
    var dict = [String: PropertyList]()
    dict["isa"] = .string(xcodeClassName)
    dict["additionalCompilerFlagsByRelativePath"] = .dictionary(
      Dictionary(
        uniqueKeysWithValues:
          extraCompilerArgs.map {
            ($0.rawPath, PropertyList.string($1.joined(separator: " ")))
          }
      )
    )
    dict["membershipExceptions"] = .array(sources.map { .string($0.rawPath) })
    dict["target"] = .identifier(serializer.id(of: target))
    return dict
  }
}

extension Xcode.Reference: PropertyListSerializable {
  fileprivate var xcodeClassName: String {
    switch self {
    case is Xcode.Group:
      "PBXGroup"
    case let fileRef as Xcode.FileReference:
      fileRef.isBuildableFolder
        ? "PBXFileSystemSynchronizedRootGroup"
        : "PBXFileReference"
    default:
      fatalError("Unhandled subclass")
    }
  }

  fileprivate func serialize(
    to serializer: PropertyListSerializer
  ) throws -> [String: PropertyList] {
    var dict = [String: PropertyList]()
    dict["isa"] = .string(xcodeClassName)
    dict["path"] = .string(path)
    if let name = name {
      dict["name"] = .string(name)
    }
    dict["sourceTree"] = .string(pathBase.rawValue)

    switch self {
    case let group as Xcode.Group:
      dict["children"] = try .array(
        group.subitems.map({ reference in
          try .identifier(serializer.serialize(object: reference))
        })
      )
    case let fileRef as Xcode.FileReference:
      if let buildableFolder = fileRef.buildableFolder {
        dict["exceptions"] = try .array(
          buildableFolder.makeTargetExceptions().map {
            try .identifier(serializer.serialize(object: $0))
          }
        )
      }
      if let fileType = fileRef.fileType {
        dict["explicitFileType"] = .string(fileType)
      }
      // FileReferences don't need to store a name if it's the same as the path.
      if name == path {
        dict["name"] = nil
      }
    default:
      fatalError("Unhandled subclass")
    }
    return dict
  }
}

extension Xcode.Target: PropertyListSerializable {
  fileprivate var xcodeClassName: String {
    productType == nil ? "PBXAggregateTarget" : "PBXNativeTarget"
  }

  /// Called by the Serializer to serialize the Target.
  fileprivate func serialize(to serializer: PropertyListSerializer) throws -> [String: PropertyList] {
    // Create either a `PBXNativeTarget` or an `PBXAggregateTarget` plist
    // dictionary (depending on whether or not we have a product type).
    var dict = [String: PropertyList]()
    dict["isa"] = .string(xcodeClassName)
    dict["name"] = .string(name)
    // Build settings are a bit tricky; in Xcode, each is stored in a named
    // XCBuildConfiguration object, and the list of build configurations is
    // in turn stored in an XCConfigurationList.  In our simplified model,
    // we have a BuildSettingsTable, with three sets of settings:  one for
    // the common settings, and one each for the Debug and Release overlays.
    // So we consider the BuildSettingsTable to be the configuration list.
    // This is the same situation as for Project.
    dict["buildConfigurationList"] = try .identifier(serializer.serialize(object: buildSettings))
    dict["buildPhases"] = try .array(
      buildPhases.map({ phase in
        // Here we have the same problem as for Reference; we cannot inherit
        // functionality since we're in an extension.
        try .identifier(serializer.serialize(object: phase as! PropertyListSerializable))
      })
    )
    /// Private wrapper class for a target dependency relation.  This is
    /// glue between our value-based settings structures and the Xcode
    /// project model's identity-based TargetDependency objects.
    class TargetDependency: PropertyListSerializable {
      var xcodeClassName: String { "PBXTargetDependency" }

      var target: Xcode.Target
      init(target: Xcode.Target) {
        self.target = target
      }
      func serialize(to serializer: PropertyListSerializer) -> [String: PropertyList] {
        // Create a `PBXTargetDependency` plist dictionary.
        var dict = [String: PropertyList]()
        dict["isa"] = .string(xcodeClassName)
        dict["target"] = .identifier(serializer.id(of: target))
        return dict
      }
    }
    dict["dependencies"] = try .array(
      dependencies.map({ dep in
        // In the Xcode project model, target dependencies are objects,
        // so we need a helper class here.
        try .identifier(serializer.serialize(object: TargetDependency(target: dep.target)))
      })
    )
    if !buildableFolders.isEmpty {
      dict["fileSystemSynchronizedGroups"] = .array(
        buildableFolders.map { .identifier(serializer.id(of: $0.ref)) }
      )
    }
    dict["productName"] = .string(productName)
    if let productType = productType {
      dict["productType"] = .string(productType.rawValue)
    }
    if let productReference = productReference {
      dict["productReference"] = .identifier(serializer.id(of: productReference))
    }
    return dict
  }
}

extension PropertyListSerializable where Self: Xcode.BuildPhase {
  /// Helper function that constructs and returns the base property list
  /// dictionary for build phases.
  fileprivate func makeBuildPhaseDict(
    serializer: PropertyListSerializer
  ) throws -> [String: PropertyList] {
    var dict = [String: PropertyList]()
    dict["isa"] = .string(xcodeClassName)
    dict["files"] = try .array(
      files.map({ file in
        try .identifier(serializer.serialize(object: file))
      })
    )
    return dict
  }

  /// Default serialization implementation.
  fileprivate func serialize(
    to serializer: PropertyListSerializer
  ) throws -> [String: PropertyList] {
    try makeBuildPhaseDict(serializer: serializer)
  }
}

extension Xcode.HeadersBuildPhase: PropertyListSerializable {
  fileprivate var xcodeClassName: String { "PBXHeadersBuildPhase" }
}

extension Xcode.SourcesBuildPhase: PropertyListSerializable {
  fileprivate var xcodeClassName: String { "PBXSourcesBuildPhase" }
}

extension Xcode.FrameworksBuildPhase: PropertyListSerializable {
  fileprivate var xcodeClassName: String { "PBXFrameworksBuildPhase" }
}

extension Xcode.CopyFilesBuildPhase: PropertyListSerializable {
  fileprivate var xcodeClassName: String { "PBXCopyFilesBuildPhase" }

  fileprivate func serialize(
    to serializer: PropertyListSerializer
  ) throws -> [String: PropertyList] {
    var dict = try makeBuildPhaseDict(serializer: serializer)
    dict["dstPath"] = .string("")  // FIXME: needs to be real
    dict["dstSubfolderSpec"] = .string("")  // FIXME: needs to be real
    return dict
  }
}

extension Xcode.ShellScriptBuildPhase: PropertyListSerializable {
  fileprivate var xcodeClassName: String { "PBXShellScriptBuildPhase" }

  fileprivate func serialize(
    to serializer: PropertyListSerializer
  ) throws -> [String: PropertyList] {
    var dict = try makeBuildPhaseDict(serializer: serializer)
    dict["shellPath"] = .string("/bin/sh")  // FIXME: should be settable
    dict["shellScript"] = .string(script)
    dict["inputPaths"] = .array(inputs.map { .string($0) })
    dict["outputPaths"] = .array(outputs.map { .string($0) })
    dict["alwaysOutOfDate"] = .string(alwaysRun ? "1" : "0")
    return dict
  }
}

extension Xcode.BuildFile: PropertyListSerializable {
  fileprivate var xcodeClassName: String { "PBXBuildFile" }

  /// Called by the Serializer to serialize the BuildFile.
  fileprivate func serialize(to serializer: PropertyListSerializer) throws -> [String: PropertyList] {
    // Create a `PBXBuildFile` plist dictionary.
    var dict = [String: PropertyList]()
    dict["isa"] = .string(xcodeClassName)
    if let fileRef = fileRef {
      dict["fileRef"] = .identifier(serializer.id(of: fileRef))
    }

    let settingsDict = try PropertyList.encode(settings)
    if !settingsDict.isEmpty {
      dict["settings"] = settingsDict
    }

    return dict
  }
}

extension Xcode.BuildSettingsTable: PropertyListSerializable {
  fileprivate var xcodeClassName: String { "XCConfigurationList" }

  /// Called by the Serializer to serialize the BuildFile.  It is serialized
  /// as an XCBuildConfigurationList and two additional XCBuildConfiguration
  /// objects (one for debug and one for release).
  fileprivate func serialize(to serializer: PropertyListSerializer) throws -> [String: PropertyList] {
    /// Private wrapper class for BuildSettings structures.  This is glue
    /// between our value-based settings structures and the Xcode project
    /// model's identity-based XCBuildConfiguration objects.
    class BuildSettingsDictWrapper: PropertyListSerializable {
      let name: String
      var baseSettings: BuildSettings
      var overlaySettings: BuildSettings
      let xcconfigFileRef: Xcode.FileReference?

      var xcodeClassName: String { "XCBuildConfiguration" }

      init(
        name: String,
        baseSettings: BuildSettings,
        overlaySettings: BuildSettings,
        xcconfigFileRef: Xcode.FileReference?
      ) {
        self.name = name
        self.baseSettings = baseSettings
        self.overlaySettings = overlaySettings
        self.xcconfigFileRef = xcconfigFileRef
      }

      func serialize(to serializer: PropertyListSerializer) throws -> [String: PropertyList] {
        // Create a `XCBuildConfiguration` plist dictionary.
        var dict = [String: PropertyList]()
        dict["isa"] = .string(xcodeClassName)
        dict["name"] = .string(name)
        // Combine the base settings and the overlay settings.
        dict["buildSettings"] = try combineBuildSettingsPropertyLists(
          baseSettings: .encode(baseSettings),
          overlaySettings: .encode(overlaySettings)
        )
        // Add a reference to the base configuration, if there is one.
        if let xcconfigFileRef = xcconfigFileRef {
          dict["baseConfigurationReference"] = .identifier(serializer.id(of: xcconfigFileRef))
        }
        return dict
      }
    }

    // Create a `XCConfigurationList` plist dictionary.
    var dict = [String: PropertyList]()
    dict["isa"] = .string(xcodeClassName)
    dict["buildConfigurations"] = .array([
      // We use a private wrapper to "objectify" our two build settings
      // structures (which, being structs, are value types).
      try .identifier(
        serializer.serialize(
          object: BuildSettingsDictWrapper(
            name: "Debug",
            baseSettings: common,
            overlaySettings: debug,
            xcconfigFileRef: xcconfigFileRef
          )
        )
      ),
      try .identifier(
        serializer.serialize(
          object: BuildSettingsDictWrapper(
            name: "Release",
            baseSettings: common,
            overlaySettings: release,
            xcconfigFileRef: xcconfigFileRef
          )
        )
      ),
    ])
    // FIXME: What is this, and why are we setting it?
    dict["defaultConfigurationIsVisible"] = .string("0")
    // FIXME: Should we allow this to be set in the model?
    dict["defaultConfigurationName"] = .string("Release")
    return dict
  }
}

fileprivate struct InternalError: Error {
  private let description: String
  public init(_ description: String) {
    assertionFailure(description)
    self.description = description
  }
}

/// Private helper function that combines a base property list and an overlay
/// property list, respecting the semantics of `$(inherited)` as we go.
fileprivate func combineBuildSettingsPropertyLists(
  baseSettings: PropertyList,
  overlaySettings: PropertyList
) throws -> PropertyList {
  // Extract the base and overlay dictionaries.
  guard case let .dictionary(baseDict) = baseSettings else {
    throw InternalError("base settings plist must be a dictionary")
  }
  guard case let .dictionary(overlayDict) = overlaySettings else {
    throw InternalError("overlay settings plist must be a dictionary")
  }

  // Iterate over the overlay values and apply them to the base.
  var resultDict = baseDict
  for (name, value) in overlayDict {
    if let array = baseDict[name]?.array, let overlayArray = value.array, overlayArray.first?.string == "$(inherited)" {
      resultDict[name] = .array(array + overlayArray.dropFirst())
    } else {
      resultDict[name] = value
    }
  }
  return .dictionary(resultDict)
}

/// A simple property list serializer with the same semantics as the Xcode
/// property list serializer.  Not generally reusable at this point, but only
/// because of implementation details (architecturally it isn't tied to Xcode).
fileprivate class PropertyListSerializer {

  /// Private struct that represents a strong reference to a serializable
  /// object.  This prevents any temporary objects from being deallocated
  /// during the serialization and replaced with other objects having the
  /// same object identifier (a violation of our assumptions)
  struct SerializedObjectRef: Hashable, Equatable {
    let object: PropertyListSerializable

    init(_ object: PropertyListSerializable) {
      self.object = object
    }

    func hash(into hasher: inout Hasher) {
      hasher.combine(ObjectIdentifier(object))
    }

    static func == (lhs: SerializedObjectRef, rhs: SerializedObjectRef) -> Bool {
      return lhs.object === rhs.object
    }
  }

  /// Maps objects to the identifiers that have been assigned to them.  The
  /// next identifier to be assigned is always one greater than the number
  /// of entries in the mapping.
  var objsToIds = [SerializedObjectRef: String]()

  /// Maps serialized objects ids to dictionaries.  This may contain fewer
  /// entries than `objsToIds`, since ids are assigned upon reference, but
  /// plist dictionaries are created only upon actual serialization.  This
  /// dictionary is what gets written to the property list.
  var idsToDicts = [String: PropertyList]()

  /// Returns the quoted identifier for the object, assigning one if needed.
  func id(of object: PropertyListSerializable) -> String {
    // We need a "serialized object ref" wrapper for the `objsToIds` map.
    let serObjRef = SerializedObjectRef(object)
    if let id = objsToIds[serObjRef] {
      return "\"\(id)\""
    }
    // We currently always assign identifiers starting at 1 and going up.
    // FIXME: This is a suboptimal format for object identifier strings;
    // for debugging purposes they should at least sort in numeric order.
    let id = object.objectID ?? "OBJ_\(objsToIds.count + 1)"
    objsToIds[serObjRef] = id
    return "\"\(id)\""
  }

  /// Serializes `object` by asking it to construct a plist dictionary and
  /// then adding that dictionary to the serializer.  This may in turn cause
  /// recursive invocations of `serialize(object:)`; the closure of these
  /// invocations end up serializing the whole object graph.
  @discardableResult
  func serialize(object: PropertyListSerializable) throws -> String {
    // Assign an id for the object, if it doesn't already have one.
    let id = self.id(of: object)

    // If that id is already in `idsToDicts`, we've detected recursion or
    // repeated serialization.
    guard idsToDicts[id] == nil else {
      throw InternalError("tried to serialize \(object) twice")
    }

    // Set a sentinel value in the `idsToDicts` mapping to detect recursion.
    idsToDicts[id] = .dictionary([:])

    // Now recursively serialize the object, and store the result (replacing
    // the sentinel).
    idsToDicts[id] = try .dictionary(object.serialize(to: self))

    // Finally, return the identifier so the caller can store it (usually in
    // an attribute in its own serialization dictionary).
    return id
  }
}

fileprivate protocol PropertyListSerializable: AnyObject {
  /// Called by the Serializer to construct and return a dictionary for a
  /// serializable object.  The entries in the dictionary should represent
  /// the receiver's attributes and relationships, as PropertyList values.
  ///
  /// Every object that is written to the Serializer is assigned an id (an
  /// arbitrary but unique string).  Forward references can use `id(of:)`
  /// of the Serializer to assign and access the id before the object is
  /// actually written.
  ///
  /// Implementations can use the Serializer's `serialize(object:)` method
  /// to serialize owned objects (getting an id to the serialized object,
  /// which can be stored in one of the attributes) or can use the `id(of:)`
  /// method to store a reference to an unowned object.
  ///
  /// The implementation of this method for each serializable objects looks
  /// something like this:
  ///
  ///   // Create a `PBXSomeClassOrOther` plist dictionary.
  ///   var dict = [String: PropertyList]()
  ///   dict["isa"] = .string("PBXSomeClassOrOther")
  ///   dict["name"] = .string(name)
  ///   if let path = path { dict["path"] = .string(path) }
  ///   dict["mainGroup"] = .identifier(serializer.serialize(object: mainGroup))
  ///   dict["subitems"] = .array(subitems.map({ .string($0.id) }))
  ///   dict["cross-ref"] = .identifier(serializer.id(of: unownedObject))
  ///   return dict
  ///
  /// FIXME: I'm not totally happy with how this looks.  It's far too clunky
  /// and could be made more elegant.  However, since the Xcode project model
  /// is static, this is not something that will need to evolve over time.
  /// What does need to evolve, which is how the project model is constructed
  /// from the package contents, is where the elegance and simplicity really
  /// matters.  So this is acceptable for now in the interest of getting it
  /// done.

  /// The ID for the `isa` field of the object.
  var xcodeClassName: String { get }

  /// A custom ID to use for the instance, if enabled.
  ///
  /// This ID must be unique across the entire serialized graph.
  var objectID: String? { get }

  /// Should create and return a property list dictionary of the object's
  /// attributes.  This function may also use the serializer's `serialize()`
  /// function to serialize other objects, and may use `id(of:)` to access
  /// ids of objects that either have or will be serialized.
  func serialize(to serializer: PropertyListSerializer) throws -> [String: PropertyList]
}

extension PropertyListSerializable {
  var objectID: String? {
    return nil
  }
}

extension PropertyList {
  var isEmpty: Bool {
    switch self {
    case let .identifier(string): return string.isEmpty
    case let .string(string): return string.isEmpty
    case let .array(array): return array.isEmpty
    case let .dictionary(dictionary): return dictionary.isEmpty
    }
  }
}
