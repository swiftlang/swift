import sys

def error(msg):
    print('error: ' + msg, file=sys.stderr)
    sys.exit(-1)

class Attribute(object):
    def __init__(self, name, swift_name=None):
        self.name = name
        self.swift_name = swift_name or name

class TypeAttribute(Attribute):
    def __init__(self, name):
        super().__init__(name, name)

class DeclAttribute(Attribute):
    def __init__(self, name, class_name, *options, code, swift_name=None):
        super().__init__(name, swift_name)
        self.class_name = class_name
        self.options = options
        self.code = code

class SimpleDeclAttribute(DeclAttribute):
    def __init__(self, name, class_name, *options, code, swift_name=None):
        super().__init__(name, class_name, *options, code=code, swift_name=swift_name)

class ContextualDeclAttribute(DeclAttribute):
    def __init__(self, name, class_name, *options, code):
        super().__init__(name, class_name, *options, code=code)

class ContextualSimpleDeclAttribute(SimpleDeclAttribute):
    def __init__(self, name, class_name, *options, code):
        super().__init__(name, class_name, *options, code=code)

class DeclAttributeAlias(Attribute):
    def __init__(self, name, class_name, swift_name=None):
        super().__init__(name, swift_name)
        self.class_name = class_name

class ContextualDeclAttributeAlias(DeclAttributeAlias):
    def __init__(self, name, class_name, swift_name=None):
        super().__init__(name, class_name, swift_name)

class BuiltinDeclModifier(Attribute):
    def __init__(self, name, swift_name=None):
        super().__init__(name, swift_name)

# Abstract class aggregations for use in Attr.def.
OnValue = 'OnValue'
OnTypeAlias = 'OnTypeAlias'
OnEnumElement = 'OnEnumElement'
OnMacro = 'OnMacro'
OnSubscript = 'OnSubscript'
OnVar = 'OnVar'
OnExtension = 'OnExtension'
OnClass = 'OnClass'
OnFunc = 'OnFunc'
OnAccessor = 'OnAccessor'
OnEnum = 'OnEnum'
OnConstructor = 'OnConstructor'
OnStruct = 'OnStruct'
OnImport = 'OnImport'
OnAssociatedType = 'OnAssociatedType'
OnGenericTypeParam = 'OnGenericTypeParam'
OnParam = 'OnParam'
OnNominalType = 'OnNominalType'
OnProtocol = 'OnProtocol'
OnConcreteNominalType = 'OnConcreteNominalType'
OnGenericType = 'OnGenericType'
OnAbstractFunction = 'OnAbstractFunction'
OnOperator = 'OnOperator'
OnAnyDecl = 'OnAnyDecl'

# True if multiple instances of this attribute are allowed on a single
# declaration.
AllowMultipleAttributes = 'AllowMultipleAttributes'

# True if this is a decl modifier - i.e., that it should not be spelled
# with an @.
DeclModifier = 'DeclModifier'

# True if this is a long attribute that should be printed on its own line.
#
# Currently has no effect on DeclModifier attributes.
LongAttribute = 'LongAttribute'

# True if this shouldn't be serialized.
NotSerialized = 'NotSerialized'

# True if this attribute is only valid when parsing a .sil file.
SILOnly = 'SILOnly'

# The attribute should be reported by parser as unknown.
RejectByParser = 'RejectByParser'

# Whether client code cannot use the attribute.
UserInaccessible = 'UserInaccessible'

# Whether adding this attribute can break API
APIBreakingToAdd = 'APIBreakingToAdd'

# Whether removing this attribute can break API
APIBreakingToRemove = 'APIBreakingToRemove'

# Whether adding this attribute can break ABI
ABIBreakingToAdd = 'ABIBreakingToAdd'

# Whether removing this attribute can break ABI
ABIBreakingToRemove = 'ABIBreakingToRemove'

# The opposite of APIBreakingToAdd
APIStableToAdd = 'APIStableToAdd'

# The opposite of APIBreakingToRemove
APIStableToRemove = 'APIStableToRemove'

# The opposite of ABIBreakingToAdd
ABIStableToAdd = 'ABIStableToAdd'

# The opposite of ABIBreakingToRemove
ABIStableToRemove = 'ABIStableToRemove'

# Whether this attribute is only valid when concurrency is enabled.
ConcurrencyOnly = 'ConcurrencyOnly'

# Whether this attribute is valid on additional decls in ClangImporter.
OnAnyClangDecl = 'OnAnyClangDecl'

# Type attributes
TYPE_ATTR_KINDS = [
    TypeAttribute('autoclosure'),
    TypeAttribute('convention'),
    TypeAttribute('noescape'),
    TypeAttribute('escaping'),
    TypeAttribute('differentiable'),
    TypeAttribute('noDerivative'),
    TypeAttribute('async'),
    TypeAttribute('Sendable'),
    TypeAttribute('unchecked'),
    TypeAttribute('_local'),
    TypeAttribute('_noMetadata'),

    # Generated interface attributes
    TypeAttribute('_opaqueReturnTypeOf'),
]

# Schema for `DeclAttribute`s:
#
# - Attribute name.
# - C++ class name without the 'Attr' suffix
# - Options for the attribute, including:
#    * the declarations the attribute can appear on
#    * whether duplicates are allowed
# - Unique attribute identifier used for serialization. This
#   can never be changed.
#
# SimpleDeclAttribute is the same, but the class becomes
# SimpleDeclAttr<DAK_##NAME> on the C++ side.
#
# Please help ease code review/audits:
# - Please place the "OnXYZ" flags together on the next line.
# - Please place the non-OnXYZ flags together on the next to last line.
# - Please place the unique code number on the last line.
# - Please sort attributes by serialization number.
# - Please create a "NOTE" comment if a unique number is skipped.
#
# If you're adding a new kind of "attribute" that is spelled without a leading
# '@' symbol, add an entry to the `DECL_MODIFIER_KINDS` array instead.
#
# If you're adding a new underscored attribute here, please document it in
# docs/ReferenceGuides/UnderscoredAttributes.md.
DECL_ATTR_KINDS = [
    DeclAttribute('_silgen_name', 'SILGenName',
                  OnAbstractFunction,
                  LongAttribute,  UserInaccessible,  ABIStableToAdd,  ABIStableToRemove,
                  APIStableToAdd,  APIStableToRemove,
                  code=0),
    DeclAttribute('available', 'Available',
                  OnAbstractFunction,  OnGenericType,  OnVar,  OnSubscript,  OnEnumElement, OnMacro,
                  OnExtension,  AllowMultipleAttributes,  LongAttribute,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=1),
    DeclAttribute('objc', 'ObjC',
                  OnAbstractFunction,  OnClass,  OnProtocol,  OnExtension,  OnVar,
                  OnSubscript,  OnEnum,  OnEnumElement,
                  ABIBreakingToAdd,  ABIBreakingToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=3),
    SimpleDeclAttribute('dynamicCallable', 'DynamicCallable',
                        OnNominalType,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=6),
    DeclAttribute('main', 'MainType',
                  OnClass,  OnStruct,  OnEnum,  OnExtension,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=7),
    SimpleDeclAttribute('_exported', 'Exported',
                        OnImport,
                        UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=8),
    SimpleDeclAttribute('dynamicMemberLookup', 'DynamicMemberLookup',
                        OnNominalType,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=9),
    SimpleDeclAttribute('NSCopying', 'NSCopying',
                        OnVar,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=10),
    SimpleDeclAttribute('IBAction', 'IBAction',
                        OnFunc,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=11),
    SimpleDeclAttribute('IBDesignable', 'IBDesignable',
                        OnClass,  OnExtension,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=12),
    SimpleDeclAttribute('IBInspectable', 'IBInspectable',
                        OnVar,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=13),
    SimpleDeclAttribute('IBOutlet', 'IBOutlet',
                        OnVar,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=14),
    SimpleDeclAttribute('NSManaged', 'NSManaged',
                        OnVar,  OnFunc,  OnAccessor,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=15),
    SimpleDeclAttribute('LLDBDebuggerFunction', 'LLDBDebuggerFunction',
                        OnFunc,
                        UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=17),
    SimpleDeclAttribute('UIApplicationMain', 'UIApplicationMain',
                        OnClass,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=18),
    SimpleDeclAttribute('unsafe_no_objc_tagged_pointer', 'UnsafeNoObjCTaggedPointer',
                        OnProtocol,
                        UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=19),
    DeclAttribute('inline', 'Inline',
                  OnVar,  OnSubscript,  OnAbstractFunction,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=20),
    DeclAttribute('_semantics', 'Semantics',
                  OnAbstractFunction,  OnSubscript,  OnNominalType,  OnVar,
                  AllowMultipleAttributes,  UserInaccessible,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=21),
    SimpleDeclAttribute('_transparent', 'Transparent',
                        OnFunc,  OnAccessor,  OnConstructor,  OnVar,  UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=26),
    SimpleDeclAttribute('requires_stored_property_inits', 'RequiresStoredPropertyInits',
                        OnClass,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=27),
    SimpleDeclAttribute('nonobjc', 'NonObjC',
                        OnExtension,  OnFunc,  OnAccessor,  OnVar,  OnSubscript,  OnConstructor,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=30),
    SimpleDeclAttribute('_fixed_layout', 'FixedLayout',
                        OnVar,  OnClass,  OnStruct,  OnProtocol,
                        UserInaccessible,  ABIBreakingToAdd,  ABIBreakingToRemove,
                        APIStableToAdd,  APIStableToRemove,
                        code=31),
    SimpleDeclAttribute('inlinable', 'Inlinable',
                        OnVar,  OnSubscript,  OnAbstractFunction,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=32),
    DeclAttribute('_specialize', 'Specialize',
                  OnConstructor,  OnFunc,  OnAccessor,
                  AllowMultipleAttributes,  LongAttribute,  UserInaccessible,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=33),
    SimpleDeclAttribute('objcMembers', 'ObjCMembers',
                        OnClass,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=34),
    ContextualSimpleDeclAttribute('_compilerInitialized', 'CompilerInitialized',
                                  OnVar,
                                  UserInaccessible,
                                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=35),
    SimpleDeclAttribute('_hasStorage', 'HasStorage',
                        OnVar,
                        UserInaccessible,
                        NotSerialized,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=45),
    DeclAttribute('__raw_doc_comment', 'RawDocComment',
                  OnAnyDecl,
                  UserInaccessible,
                  RejectByParser,
                  NotSerialized,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=48),
    DeclAttribute('_effects', 'Effects',
                  OnAbstractFunction,
                  AllowMultipleAttributes,  UserInaccessible,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=50),
    DeclAttribute('__objc_bridged', 'ObjCBridged',
                  OnClass,
                  UserInaccessible,
                  RejectByParser,
                  NotSerialized,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=51),
    SimpleDeclAttribute('NSApplicationMain', 'NSApplicationMain',
                        OnClass,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=52),
    SimpleDeclAttribute('_objc_non_lazy_realization', 'ObjCNonLazyRealization',
                        OnClass,
                        UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=53),
    DeclAttribute('__synthesized_protocol', 'SynthesizedProtocol',
                  OnConcreteNominalType,
                  UserInaccessible,
                  RejectByParser,
                  NotSerialized,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=54),
    SimpleDeclAttribute('testable', 'Testable',
                        OnImport,
                        UserInaccessible,
                        NotSerialized,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=55),
    DeclAttribute('_alignment', 'Alignment',
                  OnStruct,  OnEnum,
                  UserInaccessible,
                  ABIBreakingToAdd,  ABIBreakingToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=56),
    SimpleDeclAttribute('rethrows', 'AtRethrows',
                        OnProtocol,
                        ABIBreakingToAdd,  ABIBreakingToRemove,  APIBreakingToAdd,  APIBreakingToRemove,
                        swift_name='atRethrows',
                        code=58),
    DeclAttribute('_swift_native_objc_runtime_base', 'SwiftNativeObjCRuntimeBase',
                  OnClass,
                  UserInaccessible,
                  ABIBreakingToAdd,  ABIBreakingToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=59),
    SimpleDeclAttribute('warn_unqualified_access', 'WarnUnqualifiedAccess',
                        OnFunc,  OnAccessor,  #| OnVar
                        LongAttribute,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=61),
    SimpleDeclAttribute('_show_in_interface', 'ShowInInterface',
                        OnProtocol,
                        UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=62),
    DeclAttribute('_cdecl', 'CDecl',
                  OnFunc,  OnAccessor,
                  LongAttribute,  UserInaccessible,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=63),
    SimpleDeclAttribute('usableFromInline', 'UsableFromInline',
                        OnAbstractFunction,  OnVar,  OnSubscript,  OnNominalType,  OnTypeAlias,
                        LongAttribute,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=64),
    SimpleDeclAttribute('discardableResult', 'DiscardableResult',
                        OnFunc,  OnAccessor,  OnConstructor,
                        LongAttribute,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=65),
    SimpleDeclAttribute('GKInspectable', 'GKInspectable',
                        OnVar,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=66),
    DeclAttribute('_implements', 'Implements',
                  OnFunc,  OnAccessor,  OnVar,  OnSubscript,  OnTypeAlias,
                  UserInaccessible,
                  NotSerialized,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=67),
    DeclAttribute('_objcRuntimeName', 'ObjCRuntimeName',
                  OnClass,
                  UserInaccessible,
                  NotSerialized,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=68),
    SimpleDeclAttribute('_staticInitializeObjCMetadata', 'StaticInitializeObjCMetadata',
                        OnClass,
                        UserInaccessible,
                        LongAttribute,  RejectByParser,
                        NotSerialized,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=69),
    DeclAttribute('_restatedObjCConformance', 'RestatedObjCConformance',
                  OnProtocol,
                  UserInaccessible,
                  LongAttribute,  RejectByParser,
                  NotSerialized,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=70),
  # NOTE: 71 is unused
    DeclAttribute('_objcImplementation', 'ObjCImplementation',
                  OnExtension,
                  UserInaccessible,
                  ABIBreakingToAdd, ABIBreakingToRemove, APIBreakingToAdd, APIBreakingToRemove,
                  code=72),
    DeclAttribute('_optimize', 'Optimize',
                  OnAbstractFunction,  OnSubscript,  OnVar,
                  UserInaccessible,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=73),
    DeclAttribute('_clangImporterSynthesizedType', 'ClangImporterSynthesizedType',
                  OnGenericType,
                  LongAttribute,  RejectByParser,  UserInaccessible,
                  NotSerialized,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=74),
    SimpleDeclAttribute('_weakLinked', 'WeakLinked',
                        OnNominalType,  OnAssociatedType,  OnFunc,  OnAccessor,  OnVar,
                        OnSubscript,  OnConstructor,  OnEnumElement,  OnExtension,  OnImport,
                        UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=75),
    SimpleDeclAttribute('frozen', 'Frozen',
                        OnEnum,  OnStruct,  ABIBreakingToAdd,  ABIBreakingToRemove,  APIBreakingToRemove,  APIStableToAdd,
                        code=76),
    DeclAttributeAlias('_frozen', 'Frozen'),
    SimpleDeclAttribute('_forbidSerializingReference', 'ForbidSerializingReference',
                        OnAnyDecl,
                        LongAttribute,  RejectByParser,  UserInaccessible,  NotSerialized,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=77),
    SimpleDeclAttribute('_hasInitialValue', 'HasInitialValue',
                        OnVar,
                        UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=78),
    SimpleDeclAttribute('_nonoverride', 'NonOverride',
                        OnFunc,  OnAccessor,  OnVar,  OnSubscript,  OnConstructor,  OnAssociatedType,
                        UserInaccessible,  NotSerialized,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=79),
    DeclAttribute('_dynamicReplacement', 'DynamicReplacement',
                  OnAbstractFunction,  OnVar,  OnSubscript,  UserInaccessible,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=80),
    SimpleDeclAttribute('_borrowed', 'Borrowed',
                        OnVar,  OnSubscript,  UserInaccessible,
                        NotSerialized,
                        ABIBreakingToAdd,  ABIBreakingToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=81),
    DeclAttribute('_private', 'PrivateImport',
                  OnImport,
                  UserInaccessible,
                  NotSerialized,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=82),
    SimpleDeclAttribute('_alwaysEmitIntoClient', 'AlwaysEmitIntoClient',
                        OnVar,  OnSubscript,  OnAbstractFunction,  UserInaccessible,
                        ABIBreakingToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=83),

    SimpleDeclAttribute('_implementationOnly', 'ImplementationOnly',
                        OnImport,  OnFunc,  OnConstructor,  OnVar,  OnSubscript,  UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=84),
    DeclAttribute('_custom', 'Custom',
                  OnAnyDecl,  RejectByParser,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=85),
    SimpleDeclAttribute('propertyWrapper', 'PropertyWrapper',
                        OnStruct,  OnClass,  OnEnum,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=86),
    SimpleDeclAttribute('_disfavoredOverload', 'DisfavoredOverload',
                        OnAbstractFunction,  OnVar,  OnSubscript,  UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=87),
    SimpleDeclAttribute('resultBuilder', 'ResultBuilder',
                        OnNominalType,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=88),
    DeclAttribute('_projectedValueProperty', 'ProjectedValueProperty',
                  OnVar,  UserInaccessible,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=89),
    SimpleDeclAttribute('_nonEphemeral', 'NonEphemeral',
                        OnParam,  UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIBreakingToAdd,  APIStableToRemove,
                        code=90),

    DeclAttribute('differentiable', 'Differentiable',
                  OnAccessor,  OnConstructor,  OnFunc,  OnVar,  OnSubscript,  LongAttribute,
                  AllowMultipleAttributes,
                  ABIStableToAdd,  ABIBreakingToRemove,  APIStableToAdd,  APIBreakingToRemove,
                  code=91),

    SimpleDeclAttribute('_hasMissingDesignatedInitializers',
                        'HasMissingDesignatedInitializers',
                        OnClass,  UserInaccessible,  NotSerialized,
                        APIBreakingToAdd,  ABIBreakingToAdd,  APIStableToRemove,  ABIStableToRemove,
                        code=92),

    SimpleDeclAttribute('_inheritsConvenienceInitializers',
                        'InheritsConvenienceInitializers',
                        OnClass,  UserInaccessible,  NotSerialized,
                        APIStableToAdd,  ABIStableToAdd,  APIBreakingToRemove,  ABIBreakingToRemove,
                        code=93),

    DeclAttribute('_typeEraser', 'TypeEraser',
                  OnProtocol,  UserInaccessible,
                  ABIStableToAdd,  ABIBreakingToRemove,  APIStableToAdd,  APIBreakingToRemove,
                  code=94),

    SimpleDeclAttribute('IBSegueAction', 'IBSegueAction',
                        OnFunc,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=95),

    DeclAttribute('_originallyDefinedIn', 'OriginallyDefinedIn',
                  OnNominalType,  OnFunc,  OnVar,  OnExtension,  UserInaccessible,
                  AllowMultipleAttributes,  LongAttribute,
                  ABIBreakingToAdd,  ABIBreakingToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=96),

    DeclAttribute('derivative', 'Derivative',
                  OnFunc,  LongAttribute,  AllowMultipleAttributes,
                  ABIStableToAdd,  ABIBreakingToRemove,  APIStableToAdd,  APIBreakingToRemove,
                  code=97),

    DeclAttribute('_spi', 'SPIAccessControl',
                  OnAbstractFunction,  OnExtension,  OnGenericType,  OnVar,  OnSubscript,
                  OnImport,  OnAccessor,  OnEnumElement, OnMacro,
                  AllowMultipleAttributes,  UserInaccessible,
                  ABIStableToAdd,  ABIStableToRemove,  APIBreakingToAdd,  APIStableToRemove,
                  code=98),

    DeclAttribute('transpose', 'Transpose',
                  OnFunc,  LongAttribute,  AllowMultipleAttributes,
                  ABIStableToAdd,  ABIBreakingToRemove,  APIStableToAdd,  APIBreakingToRemove,
                  code=99),

    SimpleDeclAttribute('noDerivative', 'NoDerivative',
                        OnAbstractFunction,  OnVar,  OnSubscript,
                        ABIBreakingToAdd,  ABIBreakingToRemove,  APIBreakingToAdd,  APIBreakingToRemove,
                        code=100),

    # 101 was @asyncHandler and is now unused

    SimpleDeclAttribute('globalActor', 'GlobalActor',
                        OnClass,  OnStruct,  OnEnum,
                        ABIStableToAdd,  ABIBreakingToRemove,
                        APIStableToAdd,  APIBreakingToRemove,
                        code=104),

    SimpleDeclAttribute('_specializeExtension', 'SpecializeExtension',
                        OnExtension,  UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=105),

    SimpleDeclAttribute('Sendable', 'Sendable',
                        OnFunc,  OnConstructor,  OnAccessor,  OnAnyClangDecl,
                        ABIBreakingToAdd,  ABIBreakingToRemove,
                        APIBreakingToAdd,  APIBreakingToRemove,
                        code=107),

    SimpleDeclAttribute('_marker', 'Marker',
                        OnProtocol,  UserInaccessible,
                        ABIBreakingToAdd,  ABIBreakingToRemove,
                        APIBreakingToAdd,  APIBreakingToRemove,
                        code=108),

    SimpleDeclAttribute('reasync', 'AtReasync',
                        OnProtocol,  ConcurrencyOnly,
                        ABIBreakingToAdd,  ABIBreakingToRemove,  APIBreakingToAdd,  APIBreakingToRemove,
                        swift_name='atReasync',
                        code=110),

    # 111 was an experimental @completionHandlerAsync and is now unused

    # 113 was experimental _unsafeSendable and is now unused

    SimpleDeclAttribute('_unsafeInheritExecutor', 'UnsafeInheritExecutor',
                        OnFunc,  UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIBreakingToRemove,
                        code=114), # previously experimental _unsafeMainActor

    SimpleDeclAttribute('_implicitSelfCapture', 'ImplicitSelfCapture',
                        OnParam,  UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIBreakingToRemove,
                        code=115),

    SimpleDeclAttribute('_inheritActorContext', 'InheritActorContext',
                        OnParam,  UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIBreakingToAdd,  APIBreakingToRemove,
                        code=116),

    SimpleDeclAttribute('_eagerMove', 'EagerMove',
                        UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,
                        APIStableToAdd,  APIStableToRemove,
                        OnFunc,  OnParam,  OnVar,  OnNominalType,
                        code=117),

    SimpleDeclAttribute('_noEagerMove', 'NoEagerMove',
                        UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,
                        APIStableToAdd,  APIStableToRemove,
                        OnFunc,  OnParam,  OnVar,  OnNominalType,
                        code=119),

    SimpleDeclAttribute('_assemblyVision', 'EmitAssemblyVisionRemarks',
                        OnFunc,  UserInaccessible,  NotSerialized,  OnNominalType,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=120),

    DeclAttribute('_nonSendable', 'NonSendable',
                  OnNominalType,
                  UserInaccessible,  AllowMultipleAttributes,
                  ABIStableToAdd,  ABIBreakingToRemove,
                  APIStableToAdd,  APIBreakingToRemove,
                  code=121),

    SimpleDeclAttribute('_noImplicitCopy', 'NoImplicitCopy',
                        UserInaccessible,
                        ABIStableToAdd,  ABIBreakingToRemove,
                        APIStableToAdd,  APIBreakingToRemove,
                        OnFunc,  OnParam,  OnVar,
                        code=122),

    SimpleDeclAttribute('_noLocks', 'NoLocks',
                        OnAbstractFunction,  OnSubscript,  UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=123),

    SimpleDeclAttribute('_noAllocation', 'NoAllocation',
                        OnAbstractFunction,  OnSubscript,  UserInaccessible,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=124),

    SimpleDeclAttribute('preconcurrency', 'Preconcurrency',
                        OnFunc,  OnConstructor,  OnProtocol,  OnGenericType,  OnVar,  OnSubscript,
                        OnEnumElement,  OnImport,
                        ABIBreakingToAdd,  ABIBreakingToRemove,  APIBreakingToAdd,  APIBreakingToRemove,
                        code=125),

    DeclAttribute('_unavailableFromAsync', 'UnavailableFromAsync',
                  OnFunc,  OnConstructor,  OnMacro, UserInaccessible,
                  ABIStableToAdd,  ABIStableToRemove,
                  APIBreakingToAdd,  APIStableToRemove,
                  code=127),

    DeclAttribute('exclusivity', 'Exclusivity',
                  OnVar,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=128),

    DeclAttribute('_backDeploy', 'BackDeploy',
                  OnAbstractFunction,  OnAccessor,  OnSubscript,  OnVar,
                    AllowMultipleAttributes,  LongAttribute,  UserInaccessible,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIBreakingToRemove,
                  code=129),

    SimpleDeclAttribute('_moveOnly', 'MoveOnly',
                        OnNominalType,
                        UserInaccessible,
                        ABIBreakingToAdd,  ABIBreakingToRemove,  APIBreakingToAdd,  APIBreakingToRemove,
                        code=131),

    SimpleDeclAttribute('_alwaysEmitConformanceMetadata', 'AlwaysEmitConformanceMetadata',
                        OnProtocol,  UserInaccessible,  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=132),

    DeclAttribute('_expose', 'Expose',
                  OnFunc,  OnNominalType,  OnVar,  OnConstructor,
                  LongAttribute,  UserInaccessible,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=133),

    SimpleDeclAttribute('typeWrapper', 'TypeWrapper',
                        OnStruct,  OnClass,  OnEnum,
                        ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                        code=134),

    SimpleDeclAttribute('_spiOnly', 'SPIOnly',
                        OnImport, UserInaccessible,
                        ABIStableToAdd, ABIStableToRemove, APIStableToAdd, APIStableToRemove,
                        code=135),
    DeclAttribute('_documentation', 'Documentation',
                  OnAnyDecl, UserInaccessible,
                  APIBreakingToAdd, APIStableToRemove, ABIStableToAdd, ABIStableToRemove,
                  code=136),

    SimpleDeclAttribute('typeWrapperIgnored', 'TypeWrapperIgnored',
                        OnVar,
                        ABIStableToAdd, ABIStableToRemove, APIStableToAdd, APIStableToRemove,
                        code=137),

    SimpleDeclAttribute('_noMetadata', 'NoMetadata',
                        OnGenericTypeParam,
                        UserInaccessible,
                        NotSerialized,
                        ABIStableToAdd, ABIBreakingToRemove, APIStableToAdd, APIStableToRemove,
                        code=138),

    SimpleDeclAttribute('runtimeMetadata', 'RuntimeMetadata',
                        OnStruct,  OnClass,
                        ABIBreakingToAdd,  ABIBreakingToRemove,  APIBreakingToAdd,  APIBreakingToRemove,
                        code=139)
]

# Schema for declaration modifiers:
#
# - Modifier name.
# - C++ class name without the 'Attr' suffix
# - Options for the attribute, including:
#    * the declarations the attribute can appear on
#    * whether duplicates are allowed
# - Unique attribute identifier used for serialization. This
#   can never be changed.
#
# SimpleDeclAttribute is the same, but the class becomes
# SimpleDeclAttr<DAK_##NAME> on the C++ side.
#
# Please help ease code review/audits:
# - Please place the "OnXYZ" flags together on the next line.
# - Please place the non-OnXYZ flags together on the next to last line.
# - Please place the unique code number on the last line.
# - Please sort attributes by serialization number.
# - Please create a "NOTE" comment if a unique number is skipped.
#
# If you're adding a new kind of attribute that is spelled with a leading
# '@' symbol, add an entry to the `DECL_ATTR_KINDS` array instead.
DECL_MODIFIER_KINDS = [
    # These are not really attributes or modifiers in the C++ AST and they are
    # serialized directly into the ASTs they are attached to rather than using
    # the generic attribute serialization infrastructure.
    BuiltinDeclModifier('static', swift_name='staticKeyword'),
    BuiltinDeclModifier('class', swift_name='classKeyword'),

    ContextualSimpleDeclAttribute('final', 'Final',
                                  OnClass,  OnFunc,  OnAccessor,  OnVar,  OnSubscript,
                                  DeclModifier,
                                  ABIBreakingToAdd,  ABIBreakingToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=2),
    ContextualSimpleDeclAttribute('required', 'Required',
                                  OnConstructor,
                                  DeclModifier,
                                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=4),
    ContextualSimpleDeclAttribute('optional', 'Optional',
                                  OnConstructor,  OnFunc,  OnAccessor,  OnVar,  OnSubscript,
                                  DeclModifier,
                                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=5),
    ContextualSimpleDeclAttribute('lazy', 'Lazy',  DeclModifier,
                                  OnVar,
                                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=16),
    ContextualSimpleDeclAttribute('dynamic', 'Dynamic',
                                  OnFunc,  OnAccessor,  OnVar,  OnSubscript,  OnConstructor,
                                  DeclModifier,  ABIBreakingToAdd,  ABIBreakingToRemove,
                                  APIStableToAdd,  APIStableToRemove,
                                  code=22),
    ContextualSimpleDeclAttribute('infix', 'Infix',
                                  OnFunc,  OnOperator,
                                  DeclModifier,
                                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=23),
    ContextualSimpleDeclAttribute('prefix', 'Prefix',
                                  OnFunc,  OnOperator,
                                  DeclModifier,
                                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=24),
    ContextualSimpleDeclAttribute('postfix', 'Postfix',
                                  OnFunc,  OnOperator,
                                  DeclModifier,
                                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=25),
    ContextualSimpleDeclAttribute('__consuming', 'Consuming',
                                  OnFunc,  OnAccessor,
                                  DeclModifier,
                                  UserInaccessible,
                                  NotSerialized,
                                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=40),
    ContextualSimpleDeclAttribute('mutating', 'Mutating',
                                  OnFunc,  OnAccessor,
                                  DeclModifier,
                                  NotSerialized,
                                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=41),
    ContextualSimpleDeclAttribute('nonmutating', 'NonMutating',
                                  OnFunc,  OnAccessor,
                                  DeclModifier,
                                  NotSerialized,
                                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=42),
    ContextualSimpleDeclAttribute('convenience', 'Convenience',
                                  OnConstructor,
                                  DeclModifier,
                                  NotSerialized,
                                  ABIBreakingToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=43),
    ContextualSimpleDeclAttribute('override', 'Override',
                                  OnFunc,  OnAccessor,  OnVar,  OnSubscript,  OnConstructor,  OnAssociatedType,
                                  DeclModifier,
                                  NotSerialized,
                                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=44),
    DeclAttribute('private', 'AccessControl',
                  OnFunc,  OnAccessor,  OnExtension,  OnGenericType,  OnVar,  OnSubscript,
                  OnConstructor, OnMacro,
                  DeclModifier,
                  NotSerialized,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  swift_name='privateKeyword',
                  code=46),
    DeclAttributeAlias('fileprivate', 'AccessControl', swift_name='fileprivateKeyword'),
    DeclAttributeAlias('internal', 'AccessControl', swift_name='internalKeyword'),
    DeclAttributeAlias('public', 'AccessControl', swift_name='publicKeyword'),
    ContextualDeclAttributeAlias('open', 'AccessControl'),
    DeclAttribute('__setter_access', 'SetterAccess',
                  OnVar,  OnSubscript,
                  DeclModifier,  RejectByParser,
                  NotSerialized,
                  ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                  code=47),
    ContextualDeclAttribute('weak', 'ReferenceOwnership',
                            OnVar,
                            DeclModifier,
                            NotSerialized,
                            ABIStableToAdd,  ABIStableToRemove,  APIStableToAdd,  APIStableToRemove,
                            code=49),
    ContextualDeclAttributeAlias('unowned', 'ReferenceOwnership', swift_name='unowned'),

    SimpleDeclAttribute('rethrows', 'Rethrows',
                        OnFunc,  OnConstructor,
                        RejectByParser,
                        ABIBreakingToAdd,  ABIBreakingToRemove,  APIBreakingToAdd,  APIBreakingToRemove,
                        code=57,
                        swift_name='`rethrows`'),
                        
    ContextualSimpleDeclAttribute('indirect', 'Indirect',  DeclModifier,
                                  OnEnum,  OnEnumElement,
                                  ABIBreakingToAdd,  ABIBreakingToRemove,  APIStableToAdd,  APIStableToRemove,
                                  code=60),

    ContextualSimpleDeclAttribute('isolated', 'Isolated',
                                  DeclModifier,  OnParam,
                                  ABIBreakingToAdd,  ABIBreakingToRemove,
                                  APIBreakingToAdd,  APIBreakingToRemove,
                                  code=103),

    ContextualSimpleDeclAttribute('async', 'Async',
                                  DeclModifier,  OnVar,  OnFunc,
                                  ABIBreakingToAdd,  ABIBreakingToRemove,
                                  APIBreakingToAdd,  APIBreakingToRemove,
                                  code=106),

    SimpleDeclAttribute('reasync', 'Reasync',
                        OnFunc,  OnConstructor,
                        RejectByParser,
                        ABIBreakingToAdd,  ABIBreakingToRemove,  APIBreakingToAdd,  APIBreakingToRemove,
                        code=109),

    ContextualSimpleDeclAttribute('nonisolated', 'Nonisolated',
                                  DeclModifier,  OnFunc,  OnConstructor,  OnVar,  OnSubscript,
                                  ABIStableToAdd,  ABIStableToRemove,
                                  APIBreakingToAdd,  APIStableToRemove,
                                  code=112),

    ContextualSimpleDeclAttribute('distributed', 'DistributedActor',
                                  DeclModifier,  OnClass,  OnFunc,  OnAccessor,  OnVar,
                                  ABIBreakingToAdd,  ABIBreakingToRemove,
                                  APIBreakingToAdd,  APIBreakingToRemove,
                                  code=118),

    ContextualSimpleDeclAttribute('_const', 'CompileTimeConst',
                                  DeclModifier,  OnParam,  OnVar,
                                  UserInaccessible,
                                  ABIStableToAdd,  ABIStableToRemove,  APIBreakingToAdd,  APIStableToRemove,
                                  code=126),

    ContextualSimpleDeclAttribute('_local', 'KnownToBeLocal',
                                  DeclModifier,  OnFunc,  OnParam,  OnVar,
                                  UserInaccessible,
                                  ABIBreakingToAdd,  ABIBreakingToRemove,
                                  APIBreakingToAdd,  APIBreakingToRemove,
                                  code=130),
]

DEPRECATED_MODIFIER_KINDS = [
    # TODO: Remove this once we don't need to support 'actor' as a modifier
    ContextualSimpleDeclAttribute('actor', 'Actor',
                                  DeclModifier,  OnClass,  ConcurrencyOnly,
                                  ABIBreakingToAdd,  ABIBreakingToRemove,
                                  APIBreakingToAdd,  APIBreakingToRemove,
                                  code=102),
]

def verify_attribute_serialization_codes(nodes):
    # Verify that no serialization code is used twice
    used_codes = set()
    for node in nodes:
        if isinstance(node, DeclAttribute):
            if node.code in used_codes:
                error("Serialization code %d used twice" % node.code)
            used_codes.add(node.code)
