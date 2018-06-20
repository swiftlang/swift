
# Each tuple in this array consists of:
# - property name
# - property kind, one of:
#   - stored (only makes sense in structs or classes)
#   - mutating (only makes sense in structs or enums)
#   - struct (only makes sense in structs)
#   - nonmutating (makes sense in classes, structs, and enums)
# - "before" definition
# - "after" definition
#
# The definition strings are formatted with the following substitutions:
# - {name}: property name
# - {nonmutating}: nonmutating modifier spelling, if needed for context
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
        """
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
        """
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
        """
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
        """
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
        """
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
        """
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
        """
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
        """
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
        """
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
        """
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
        """
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
        """
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
        """
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
        """
    ),
    (
        "storedMakesPrivateSetPublic",
        "stored",
        """
          public private(set) var {name}: Int = 0
        """,
        """
          public var {name}: Int = 0
        """
    ),
    (
        "computedGetOnlyToLet",
        "stored",
        """
          public var {name}: Int {{
            return 0
          }}
        """,
        """
          public let {name}: Int = 0
        """
    ),
    (
        "computedPrivateSetToLet",
        "stored",
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            set {{ self.sink = newValue }}
          }}
        """,
        """
          public let {name}: Int = 0
        """
    ),
    (
        "computedPrivateNonmutatingSetToLet",
        "stored",
        """
          public private(set) var {name}: Int {{
            get {{ return 0 }}
            {nonmutating} set {{ globalSink = newValue }}
          }}
        """,
        """
          public let {name}: Int = 0
        """
    ),
    (
        "storedPrivateSetToLet",
        "stored",
        """
          public private(set) var {name}: Int = 0
        """,
        """
          public let {name}: Int = 0
        """
    ),
]
