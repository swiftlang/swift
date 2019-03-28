
# Each tuple in this array consists of:
# - property name
# - property kind, one of:
#   - stored (only makes sense in structs or classes)
#   - mutating (only makes sense in structs or enums)
#   - nonmutating (makes sense in classes, structs, and enums)
# - "before" definition
# - "after" definition
# - whether the "after" definition adds API
# - Suffix added to type name (usually "" or ".Type")
#
# The definition strings are formatted with the following substitutions:
# - {name}: property name
# - {nonmutating}: nonmutating modifier spelling, if needed for context

AddsAPI = True
DoesntAddAPI = False

testCases = [
    (
        "addsPrivateSetter",
        "mutating",
        """
          public var {name}: Int {{ return 0 }}
        """,
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            mutating set {{ self.sink = newValue }}
          }}
        """,
        DoesntAddAPI,
        "",
    ),
    (
        "addsPublicSetter",
        "mutating",
        """
          public var {name}: Int {{ return 0 }}
        """,
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            mutating set {{ self.sink = newValue }}
          }}
        """,
        AddsAPI,
        "",
    ),
    (
        "makesPrivateSetterPublic",
        "mutating",
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            mutating set {{ self.sink = newValue }}
          }}
        """,
        """
          public var {name}: Int {{
            get {{ return 0 }}
            mutating set {{ self.sink = newValue }}
          }}
        """,
        AddsAPI,
        "",
    ),
    (
        "dropsPrivateSetter",
        "mutating",
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            mutating set {{ self.sink = newValue }}
          }}
        """,
        """
          public var {name}: Int {{
            get {{ return 0 }}
          }}
        """,
        DoesntAddAPI,
        "",
    ),
    (
        "makesPrivateSetterNonmutating",
        "mutating",
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            mutating set {{ self.sink = newValue }}
          }}
        """,
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            {nonmutating} set {{ globalSink = newValue }}
          }}
        """,
        DoesntAddAPI,
        "",
    ),
    (
        "makesPrivateSetterMutating",
        "mutating",
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            {nonmutating} set {{ globalSink = newValue }}
          }}
        """,
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            set {{ self.sink = newValue }}
          }}
        """,
        DoesntAddAPI,
        "",
    ),
    (
        "addsPrivateNonmutatingSetter",
        "nonmutating",
        """
          public var {name}: Int {{ return 0 }}
        """,
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            {nonmutating} set {{ globalSink = newValue }}
          }}
        """,
        DoesntAddAPI,
        "",
    ),
    (
        "addsPublicNonmutatingSetter",
        "nonmutating",
        """
          public var {name}: Int {{ return 0 }}
        """,
        """
          public var {name}: Int {{
            get {{ return 0 }}
            {nonmutating} set {{ globalSink = newValue }}
        }}
        """,
        AddsAPI,
        "",
    ),
    (
        "makesPrivateNonmutatingSetterPublic",
        "nonmutating",
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            {nonmutating} set {{ globalSink = newValue }}
          }}
        """,
        """
          public var {name}: Int {{
            get {{ return 0 }}
            {nonmutating} set {{ globalSink = newValue }}
          }}
        """,
        AddsAPI,
        "",
    ),
    (
        "makesPrivateNonmutatingSetterPublicMutating",
        "mutating",
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            {nonmutating} set {{ globalSink = newValue }}
          }}
        """,
        """
          public var {name}: Int {{
            get {{ return 0 }}
            set {{ self.sink = newValue }}
          }}
        """,
        AddsAPI,
        "",
    ),
    (
        "makesPrivateMutatingSetterPublicNonmutating",
        "mutating",
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            set {{ self.sink = newValue }}
          }}
        """,
        """
          public var {name}: Int {{
            get {{ return 0 }}
            {nonmutating} set {{ globalSink = newValue }}
          }}
        """,
        AddsAPI,
        "",
    ),
    (
        "storedToComputed",
        "stored",
        """
          public var {name}: Int = 0
        """,
        """
          public var {name}: Int {{
            get {{ return 0 }}
            set {{ self.sink = newValue }}
          }}
        """,
        DoesntAddAPI,
        "",
    ),
    (
        "computedToStored",
        "stored",
        """
          public var {name}: Int {{
            get {{ return 0 }}
            set {{ self.sink = newValue }}
          }}
        """,
        """
          public var {name}: Int = 0
        """,
        DoesntAddAPI,
        "",
    ),
    (
        "storedToComputedPrivateSet",
        "stored",
        """
          public private(set) var {name}: Int = 0
        """,
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            set {{ self.sink = newValue }}
          }}
        """,
        DoesntAddAPI,
        "",
    ),
    (
        "storedToComputedDroppingPrivateSet",
        "stored",
        """
          public private(set) var {name}: Int = 0
        """,
        """
          public var {name}: Int {{
            get {{ return 0 }}
          }}
        """,
        DoesntAddAPI,
        "",
    ),
    (
        "getOnlyComputedToSettableStored",
        "stored",
        """
          public var {name}: Int {{
            return 0
          }}
        """,
        """
          public var {name}: Int = 0
        """,
        AddsAPI,
        "",
    ),
    (
        "getOnlyComputedToPrivateSettableStored",
        "stored",
        """
          public var {name}: Int {{
            return 0
          }}
        """,
        """
          public private(set) var {name}: Int = 0
        """,
        DoesntAddAPI,
        "",
    ),
    (
        "storedMakesPrivateSetPublic",
        "stored",
        """
          public private(set) var {name}: Int = 0
        """,
        """
          public var {name}: Int = 0
        """,
        AddsAPI,
        "",
    ),
    # In the staticBackDeploy test, the "after" case simulates Swift 5.0, which
    # did not support static key paths and did not emit property descriptors
    # for them; the "before" case simulates Swift 5.1, which does support static
    # key paths and does emit property descriptors for them. A Swift 5.1 client
    # can form key paths to static properties in a Swift 5.0 library, but
    # Equatable will return incorrect results.
    # (I'm not sure why the before and after cases need to be backwards, but
    # they do.)
    (
        "staticBackDeploy",
        "nonmutating",
        """
          public static var {name}: Int {{
            @_semantics("keypath.no_property_descriptor")
            get {{ return 0 }}
          }}
          
          // Second property used only in staticBackDeployEquality test
          public static var {name}Other: Int {{
            @_semantics("keypath.no_property_descriptor")
            get {{ return 0 }}
          }}
          public static var keyPath_{name}Other: PartialKeyPath<{type}.Type> {{
            return \.{name}Other
          }}
        """,
        """
          public static let {name}: Int = 0
          
          // Second property used only in staticBackDeployEquality test
          public static let {name}Other: Int = 0
          
          public static var keyPath_{name}Other: PartialKeyPath<{type}.Type> {{
            return \.{name}Other
          }}
        """,
        AddsAPI,      # Because Swift 5.0 doesn't support static keypaths
        ".Type",
    ),
    # TODO: Turning computed gets into lets without annotation drops method
    # dispatch thunks and other ABI artifacts currently.
    # TODO # (
    # TODO #     "computedGetOnlyToLet",
    # TODO #     "stored",
    # TODO #     """
    # TODO #       public var {name}: Int {{
    # TODO #         return 0
    # TODO #       }}
    # TODO #     """,
    # TODO #     """
    # TODO #       public let {name}: Int = 0
    # TODO #     """,
    # TODO #     DoesntAddAPI,
    # TODO # ),
    # TODO # (
    # TODO #     "computedPrivateSetToLet",
    # TODO #     "stored",
    # TODO #     """
    # TODO #       public private(set) var {name}: Int {{
    # TODO #         get {{ return 0 }}
    # TODO #         set {{ self.sink = newValue }}
    # TODO #       }}
    # TODO #     """,
    # TODO #     """
    # TODO #       public let {name}: Int = 0
    # TODO #     """,
    # TODO #     DoesntAddAPI,
    # TODO # ),
    # TODO # (
    # TODO #     "computedPrivateNonmutatingSetToLet",
    # TODO #     "stored",
    # TODO #     """
    # TODO #       public private(set) var {name}: Int {{
    # TODO #         get {{ return 0 }}
    # TODO #         {nonmutating} set {{ globalSink = newValue }}
    # TODO #       }}
    # TODO #     """,
    # TODO #     """
    # TODO #       public let {name}: Int = 0
    # TODO #     """,
    # TODO #     DoesntAddAPI,
    # TODO # ),
    # TODO # (
    # TODO #     "storedPrivateSetToLet",
    # TODO #     "stored",
    # TODO #     """
    # TODO #       public private(set) var {name}: Int = 0
    # TODO #     """,
    # TODO #     """
    # TODO #       public let {name}: Int = 0
    # TODO #     """,
    # TODO #     DoesntAddAPI,
    # TODO # ),
]
