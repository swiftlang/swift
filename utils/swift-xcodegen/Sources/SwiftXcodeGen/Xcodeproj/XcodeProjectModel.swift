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
 A very simple rendition of the Xcode project model.  There is only sufficient
 functionality to allow creation of Xcode projects in a somewhat readable way,
 and serialization to .xcodeproj plists.  There is no consistency checking to
 ensure, for example, that build settings have valid values, dependency cycles
 are not created, etc.

 Everything here is geared toward supporting project generation.  The intended
 usage model is for custom logic to build up a project using Xcode terminology
 (e.g. "group", "reference", "target", "build phase"), but there is almost no
 provision for modifying the model after it has been built up.  The intent is
 to create it as desired from the start.

 Rather than try to represent everything that Xcode's project model supports,
 the approach is to start small and to add functionality as needed.

 Note that this API represents only the project model — there is no notion of
 workspaces, schemes, etc (although schemes are represented individually in a
 separate API).  The notion of build settings is also somewhat different from
 what it is in Xcode:  instead of an open-ended mapping of build configuration
 names to dictionaries of build settings, here there is a single set of common
 build settings plus two overlay sets for debug and release.  The generated
 project has just the two Debug and Release configurations, created by merging
 the common set into the release and debug sets.  This allows a more natural
 configuration of the settings, since most values are the same between Debug
 and Release.  Also, the build settings themselves are represented as structs
 of named fields, instead of dictionaries with arbitrary name strings as keys.

 It is expected that some of these simplifications will need to be lifted over
 time, based on need.  That should be done carefully, however, to avoid ending
 up with an overly complicated model.

 Some things that are incomplete in even this first model:
 - copy files build phases are incomplete
 - shell script build phases are incomplete
 - file types in file references are specified using strings; should be enums
 so that the client doesn't have to hardcode the mapping to Xcode file type
 identifiers
 - debug and release settings override common settings; they should be merged
 in a way that respects `$(inhertied)` when the same setting is defined in
 common and in debug or release
 - there is no good way to control the ordering of the `Products` group in the
 main group; it needs to be added last in order to appear after the other
 references
 */

public struct Xcode {

  /// An Xcode project, consisting of a tree of groups and file references,
  /// a list of targets, and some additional information.  Note that schemes
  /// are outside of the project data model.
  public class Project {
    public let mainGroup: Group
    public var buildSettings: BuildSettingsTable
    public var productGroup: Group?
    public var projectDir: String
    public var targets: [Target]
    public init() {
      self.mainGroup = Group(path: "")
      self.buildSettings = BuildSettingsTable()
      self.productGroup = nil
      self.projectDir = ""
      self.targets = []
    }

    /// Creates and adds a new target (which does not initially have any
    /// build phases).
    public func addTarget(objectID: String? = nil, productType: Target.ProductType? = nil, name: String) -> Target {
      let target = Target(objectID: objectID ?? "TARGET_\(name)", productType: productType, name: name)
      targets.append(target)
      return target
    }
  }

  /// Abstract base class for all items in the group hierarchy.
  public class Reference {
    /// Relative path of the reference.  It is usually a literal, but may
    /// in fact contain build settings.
    public var path: String
    /// Determines the base path for the reference's relative path.
    public var pathBase: RefPathBase
    /// Name of the reference, if different from the last path component
    /// (if not set, Xcode will use the last path component as the name).
    public var name: String?

    /// Determines the base path for a reference's relative path (this is
    /// what for some reason is called a "source tree" in Xcode).
    public enum RefPathBase: String {
      /// An absolute path
      case absolute = "<absolute>"
      /// Indicates that the path is relative to the source root (i.e.
      /// the "project directory").
      case projectDir = "SOURCE_ROOT"
      /// Indicates that the path is relative to the path of the parent
      /// group.
      case groupDir = "<group>"
      /// Indicates that the path is relative to the effective build
      /// directory (which varies depending on active scheme, active run
      /// destination, or even an overridden build setting.
      case buildDir = "BUILT_PRODUCTS_DIR"
    }

    init(path: String, pathBase: RefPathBase = .groupDir, name: String? = nil) {
      self.path = path
      self.pathBase = pathBase
      self.name = name
    }

    /// Whether this is either a group or directory reference (blue folder).
    public var isDirectoryLike: Bool {
      if self is Xcode.Group {
        return true
      }
      if let ref = self as? Xcode.FileReference {
        return ref.isDirectory
      }
      return false
    }
  }

  /// A reference to a file system entity (a file, folder, etc).
  public final class FileReference: Reference {
    public var objectID: String?
    public var fileType: String?
    public var isDirectory: Bool
    public private(set) unowned var buildableFolder: BuildableFolder?

    public var isBuildableFolder: Bool {
      buildableFolder != nil
    }

    init(
      path: String,
      isDirectory: Bool,
      pathBase: RefPathBase = .groupDir,
      name: String? = nil,
      fileType: String? = nil,
      objectID: String? = nil
    ) {
      self.isDirectory = isDirectory
      super.init(path: path, pathBase: pathBase, name: name)
      self.objectID = objectID
      self.fileType = fileType
    }

    /// Turn a folder reference into a buildable folder.
    func getOrCreateBuildableFolder(at path: RelativePath) -> BuildableFolder {
      precondition(isDirectory)
      if let buildableFolder {
        precondition(buildableFolder.path == path)
        return buildableFolder
      }
      let folder = BuildableFolder(for: self, at: path)
      buildableFolder = folder
      return folder
    }
  }

  public final class BuildableFolder {
    public let ref: FileReference
    public let path: RelativePath

    /// The "primary" targets that are directly associated with the buildable
    /// folder and are automatically inferred for every child source file.
    private(set) var primaryTargets: Set<Xcode.Target> = []

    /// Any source file specific compiler arguments.
    private(set) var fileArgs: [FileInTarget: [String]] = [:]

    /// Any non-default target memberships.
    private(set) var targetMemberships: [RelativePath: Set<Xcode.Target>] = [:]

    fileprivate init(for ref: FileReference, at path: RelativePath) {
      self.ref = ref
      self.path = path
    }

    /// Add a primary target to the buildable folder.
    fileprivate func addPrimaryTarget(_ target: Target) {
      primaryTargets.insert(target)
    }

    /// Map the given source file path such that it's relative to the
    /// buildable folder itself.
    private func mapSourcePath(_ sourcePath: RelativePath) -> RelativePath {
      sourcePath.removingPrefix(path)!
    }

    /// Create the corresponding target exceptions for the buildable folder.
    public func makeTargetExceptions() -> [TargetException] {
      var result: [Xcode.Target: TargetException] = [:]
      func getOrCreateException(for target: Xcode.Target) -> TargetException {
        if let exception = result[target] {
          return exception
        }
        let exception = TargetException(for: target)
        result[target] = exception
        return exception
      }

      // Populate target memberships.
      for (path, targets) in targetMemberships.sorted(by: \.key.rawPath) {
        // If it's missing from primary targets, it needs adding to the
        // corresponding exception.
        for missingPrimary in primaryTargets.subtracting(targets) {
          getOrCreateException(for: missingPrimary).sources.append(path)
        }
        // Add to any non-primary targets.
        for inSecondary in targets.subtracting(primaryTargets) {
          getOrCreateException(for: inSecondary).sources.append(path)
        }
      }

      // Populate per-file args.
      for (fileInTarget, args) in fileArgs {
        getOrCreateException(for: fileInTarget.target)
          .extraCompilerArgs[fileInTarget.path] = args
      }

      return result.values.sorted(by: \.target.name)
    }

    /// Change the target membership for a given set of paths.
    public func setTargets(
      _ targets: Set<Xcode.Target>,
      for sourcePaths: [RelativePath]
    ) {
      for source in sourcePaths.map(mapSourcePath) {
        targetMemberships[source] = targets
      }
    }

    /// Set additional per-file arguments for a given path.
    public func setExtraCompilerArgs(
      _ args: [String],
      for sourcePath: RelativePath,
      in target: Xcode.Target
    ) {
      let key = FileInTarget(path: mapSourcePath(sourcePath), target: target)
      fileArgs[key] = args
    }
  }

  /// A group that can contain References (FileReferences and other Groups).
  /// The resolved path of a group is used as the base path for any child
  /// references whose source tree type is GroupRelative.
  public final class Group: Reference {
    public var subitems = [Reference]()

    /// Creates and appends a new Group to the list of subitems.
    /// The new group is returned so that it can be configured.
    @discardableResult
    public func addGroup(
      path: String,
      pathBase: RefPathBase = .groupDir,
      name: String? = nil
    ) -> Group {
      let group = Group(path: path, pathBase: pathBase, name: name)
      subitems.append(group)
      return group
    }

    /// Creates and appends a new FileReference to the list of subitems.
    @discardableResult
    public func addFileReference(
      path: String,
      isDirectory: Bool,
      pathBase: RefPathBase = .groupDir,
      name: String? = nil,
      fileType: String? = nil,
      objectID: String? = nil
    ) -> FileReference {
      let fref = FileReference(
        path: path,
        isDirectory: isDirectory,
        pathBase: pathBase,
        name: name,
        fileType: fileType,
        objectID: objectID
      )
      subitems.append(fref)
      return fref
    }
  }

  /// An Xcode target, representing a single entity to build.
  public final class Target {
    public var objectID: String?
    public var name: String
    public var productName: String
    public var productType: ProductType?
    public var buildSettings: BuildSettingsTable
    public var buildPhases: [BuildPhase]
    public var productReference: FileReference?
    public var dependencies: [TargetDependency]
    public private(set) var buildableFolders: [BuildableFolder]
    public enum ProductType: String {
      case application = "com.apple.product-type.application"
      case staticArchive = "com.apple.product-type.library.static"
      case dynamicLibrary = "com.apple.product-type.library.dynamic"
      case framework = "com.apple.product-type.framework"
      case executable = "com.apple.product-type.tool"
      case unitTest = "com.apple.product-type.bundle.unit-test"
    }
    init(objectID: String?, productType: ProductType?, name: String) {
      self.objectID = objectID
      self.name = name
      self.productType = productType
      self.productName = name
      self.buildSettings = BuildSettingsTable()
      self.buildPhases = []
      self.dependencies = []
      self.buildableFolders = []
    }

    // FIXME: There's a lot repetition in these methods; using generics to
    // try to avoid that raised other issues in terms of requirements on
    // the Reference class, though.

    /// Adds a "headers" build phase, i.e. one that copies headers into a
    /// directory of the product, after suitable processing.
    @discardableResult
    public func addHeadersBuildPhase() -> HeadersBuildPhase {
      let phase = HeadersBuildPhase()
      buildPhases.append(phase)
      return phase
    }

    /// Adds a "sources" build phase, i.e. one that compiles sources and
    /// provides them to be linked into the executable code of the product.
    @discardableResult
    public func addSourcesBuildPhase() -> SourcesBuildPhase {
      let phase = SourcesBuildPhase()
      buildPhases.append(phase)
      return phase
    }

    /// Adds a "frameworks" build phase, i.e. one that links compiled code
    /// and libraries into the executable of the product.
    @discardableResult
    public func addFrameworksBuildPhase() -> FrameworksBuildPhase {
      let phase = FrameworksBuildPhase()
      buildPhases.append(phase)
      return phase
    }

    /// Adds a "copy files" build phase, i.e. one that copies files to an
    /// arbitrary location relative to the product.
    @discardableResult
    public func addCopyFilesBuildPhase(dstDir: String) -> CopyFilesBuildPhase {
      let phase = CopyFilesBuildPhase(dstDir: dstDir)
      buildPhases.append(phase)
      return phase
    }

    /// Adds a "shell script" build phase, i.e. one that runs a custom
    /// shell script as part of the build.
    @discardableResult
    public func addShellScriptBuildPhase(
      script: String,
      inputs: [String],
      outputs: [String],
      alwaysRun: Bool
    ) -> ShellScriptBuildPhase {
      let phase = ShellScriptBuildPhase(
        script: script,
        inputs: inputs,
        outputs: outputs,
        alwaysRun: alwaysRun
      )
      buildPhases.append(phase)
      return phase
    }

    /// Adds a dependency on another target.
    /// FIXME: We do not check for cycles.  Should we?  This is an extremely
    /// minimal API so it's not clear that we should.
    public func addDependency(on target: Target) {
      dependencies.append(TargetDependency(target: target))
    }

    /// Add an associated buildable folder to the target.
    public func addBuildableFolder(_ folder: BuildableFolder) {
      folder.addPrimaryTarget(self)
      buildableFolders.append(folder)
    }

    /// A simple wrapper to prevent ownership cycles in the `dependencies`
    /// property.
    public struct TargetDependency {
      public unowned var target: Target
    }
  }

  /// Abstract base class for all build phases in a target.
  public class BuildPhase {
    public var files: [BuildFile] = []

    /// Adds a new build file that refers to `fileRef`.
    @discardableResult
    public func addBuildFile(fileRef: FileReference) -> BuildFile {
      let buildFile = BuildFile(fileRef: fileRef)
      files.append(buildFile)
      return buildFile
    }
  }

  /// A "headers" build phase, i.e. one that copies headers into a directory
  /// of the product, after suitable processing.
  public final class HeadersBuildPhase: BuildPhase {
    // Nothing extra yet.
  }

  /// A "sources" build phase, i.e. one that compiles sources and provides
  /// them to be linked into the executable code of the product.
  public final class SourcesBuildPhase: BuildPhase {
    // Nothing extra yet.
  }

  /// A "frameworks" build phase, i.e. one that links compiled code and
  /// libraries into the executable of the product.
  public final class FrameworksBuildPhase: BuildPhase {
    // Nothing extra yet.
  }

  /// A "copy files" build phase, i.e. one that copies files to an arbitrary
  /// location relative to the product.
  public final class CopyFilesBuildPhase: BuildPhase {
    public var dstDir: String
    init(dstDir: String) {
      self.dstDir = dstDir
    }
  }

  /// A "shell script" build phase, i.e. one that runs a custom shell script.
  public final class ShellScriptBuildPhase: BuildPhase {
    public var script: String
    public var inputs: [String]
    public var outputs: [String]
    public var alwaysRun: Bool
    init(script: String, inputs: [String], outputs: [String], alwaysRun: Bool) {
      self.script = script
      self.inputs = inputs
      self.outputs = outputs
      self.alwaysRun = alwaysRun
    }
  }

  /// A build file, representing the membership of a file reference in a
  /// build phase of a target.
  public final class BuildFile {
    public var fileRef: FileReference?
    init(fileRef: FileReference) {
      self.fileRef = fileRef
    }

    public var settings = Settings()

    /// A set of file settings.
    public struct Settings: Encodable {
      public var ATTRIBUTES: [String]?
      public var COMPILER_FLAGS: String?

      public init() {
      }
    }
  }

  /// A table of build settings, which for the sake of simplicity consists
  /// (in this simplified model) of a set of common settings, and a set of
  /// overlay settings for Debug and Release builds.  There can also be a
  /// file reference to an .xcconfig file on which to base the settings.
  public final class BuildSettingsTable {
    /// Common build settings are in both generated configurations (Debug
    /// and Release).
    public var common = BuildSettings()

    /// Debug build settings are overlaid over the common settings in the
    /// generated Debug configuration.
    public var debug = BuildSettings()

    /// Release build settings are overlaid over the common settings in the
    /// generated Release configuration.
    public var release = BuildSettings()

    /// An optional file reference to an .xcconfig file.
    public var xcconfigFileRef: FileReference?

    public init() {
    }

    /// A set of build settings, which is represented as a struct of optional
    /// build settings.  This is not optimally efficient, but it is great for
    /// code completion and type-checking.
    public struct BuildSettings: Encodable {
      // Note: although some of these build settings sound like booleans,
      // they are all either strings or arrays of strings, because even
      // a boolean may be a macro reference expression.
      public var BUILT_PRODUCTS_DIR: String?
      public var CLANG_CXX_LANGUAGE_STANDARD: String?
      public var CLANG_ENABLE_MODULES: String?
      public var CLANG_ENABLE_OBJC_ARC: String?
      public var COMBINE_HIDPI_IMAGES: String?
      public var COPY_PHASE_STRIP: String?
      public var CURRENT_PROJECT_VERSION: String?
      public var DEBUG_INFORMATION_FORMAT: String?
      public var DEFINES_MODULE: String?
      public var DYLIB_INSTALL_NAME_BASE: String?
      public var EMBEDDED_CONTENT_CONTAINS_SWIFT: String?
      public var ENABLE_NS_ASSERTIONS: String?
      public var ENABLE_TESTABILITY: String?
      public var FRAMEWORK_SEARCH_PATHS: [String]?
      public var GCC_C_LANGUAGE_STANDARD: String?
      public var GCC_OPTIMIZATION_LEVEL: String?
      public var GCC_PREPROCESSOR_DEFINITIONS: [String]?
      public var GCC_GENERATE_DEBUGGING_SYMBOLS: String?
      public var GCC_WARN_64_TO_32_BIT_CONVERSION: String?
      public var HEADER_SEARCH_PATHS: [String]?
      public var INFOPLIST_FILE: String?
      public var LD_RUNPATH_SEARCH_PATHS: [String]?
      public var LIBRARY_SEARCH_PATHS: [String]?
      public var MACOSX_DEPLOYMENT_TARGET: String?
      public var IPHONEOS_DEPLOYMENT_TARGET: String?
      public var TVOS_DEPLOYMENT_TARGET: String?
      public var WATCHOS_DEPLOYMENT_TARGET: String?
      public var DRIVERKIT_DEPLOYMENT_TARGET: String?
      public var MODULEMAP_FILE: String?
      public var ONLY_ACTIVE_ARCH: String?
      public var OTHER_CFLAGS: [String]?
      public var OTHER_CPLUSPLUSFLAGS: [String]?
      public var OTHER_LDFLAGS: [String]?
      public var OTHER_SWIFT_FLAGS: [String]?
      public var PRODUCT_BUNDLE_IDENTIFIER: String?
      public var PRODUCT_MODULE_NAME: String?
      public var PRODUCT_NAME: String?
      public var PROJECT_DIR: String?
      public var PROJECT_NAME: String?
      public var SDKROOT: String?
      public var SKIP_INSTALL: String?
      public var SUPPORTED_PLATFORMS: [String]?
      public var SUPPORTS_MACCATALYST: String?
      public var SWIFT_ACTIVE_COMPILATION_CONDITIONS: [String]?
      public var SWIFT_COMPILATION_MODE: String?
      public var SWIFT_ENABLE_EXPLICIT_MODULES: String?
      public var SWIFT_FORCE_STATIC_LINK_STDLIB: String?
      public var SWIFT_FORCE_DYNAMIC_LINK_STDLIB: String?
      public var SWIFT_INCLUDE_PATHS: [String]?
      public var SWIFT_MODULE_ALIASES: [String: String]?
      public var SWIFT_OBJC_INTERFACE_HEADER_NAME: String?
      public var SWIFT_OPTIMIZATION_LEVEL: String?
      public var SWIFT_VERSION: String?
      public var TARGET_NAME: String?
      public var TARGET_BUILD_DIR: String?
      public var USE_HEADERMAP: String?
      public var LD: String?
    }
  }
}

extension Xcode.BuildableFolder {
  /// A "target exception" for a buildable folder stores information about
  /// source files in the folder that are augmented in some way for a given
  /// target.
  public final class TargetException {
    unowned let target: Xcode.Target

    /// The sources that are either excluded from the buildable folder
    /// (for a primary exception), or are members of another target.
    fileprivate(set) var sources: [RelativePath] = []

    /// A set of extra compiler arguments for a given source.
    fileprivate(set) var extraCompilerArgs: [RelativePath: [String]] = [:]

    init(for target: Xcode.Target) {
      self.target = target
    }
  }

  struct FileInTarget: Hashable {
    var path: RelativePath
    var target: Xcode.Target
  }
}

extension Xcode.Target: Hashable {
  public static func == (lhs: Xcode.Target, rhs: Xcode.Target) -> Bool {
    // Identity is a key part of Target, so there's no useful value comparison
    // to do.
    lhs === rhs
  }
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }
}
