
# Each tuple in this array consists of:
# - property name
# - property kind, one of:
#   - stored (only makes sense in structs or classes)
#   - mutating (only makes sense in structs or enums)
#   - nonmutating (makes sense in classes, structs, and enums)
# - "before" definition
# - "after" definition
# - whether the "after" definition adds API
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
