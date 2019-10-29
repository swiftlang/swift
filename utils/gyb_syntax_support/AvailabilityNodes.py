from Child import Child
from Node import Node  # noqa: I201

AVAILABILITY_NODES = [
    # availability-spec-list -> availability-entry availability-spec-list?
    Node('AvailabilitySpecList', kind='SyntaxCollection',
         element='AvailabilityArgument'),

    # Wrapper for all the different entries that may occur inside @available
    # availability-entry -> '*' ','?
    #                     | identifier ','?
    #                     | availability-version-restriction ','?
    #                     | availability-versioned-argument ','?
    Node('AvailabilityArgument', kind='Syntax',
         description='''
         A single argument to an `@available` argument like `*`, `iOS 10.1`,
         or `message: "This has been deprecated"`.
         ''',
         children=[
             Child('Entry', kind='Syntax',
                   description='The actual argument',
                   node_choices=[
                       Child('Star', kind='SpacedBinaryOperatorToken',
                             text_choices=['*']),
                       Child('IdentifierRestriction',
                             kind='IdentifierToken'),
                       Child('AvailabilityVersionRestriction',
                             kind='AvailabilityVersionRestriction'),
                       Child('AvailabilityLabeledArgument',
                             kind='AvailabilityLabeledArgument'),
                   ]),
             Child('TrailingComma', kind='CommaToken', is_optional=True,
                   description='''
                   A trailing comma if the argument is followed by another
                   argument
                   '''),
         ]),

    # Representation of 'deprecated: 2.3', 'message: "Hello world"' etc.
    # availability-versioned-argument -> identifier ':' version-tuple
    Node('AvailabilityLabeledArgument', kind='Syntax',
         description='''
         A argument to an `@available` attribute that consists of a label and
         a value, e.g. `message: "This has been deprecated"`.
         ''',
         children=[
             Child('Label', kind='IdentifierToken',
                   description='The label of the argument'),
             Child('Colon', kind='ColonToken',
                   description='The colon separating label and value'),
             Child('Value', kind='Syntax',
                   node_choices=[
                       Child('String', 'StringLiteralToken'),
                       Child('Version', 'VersionTuple'),
                   ], description='The value of this labeled argument',),
         ]),

    # Representation for 'iOS 10', 'swift 3.4' etc.
    # availability-version-restriction -> identifier version-tuple
    Node('AvailabilityVersionRestriction', kind='Syntax',
         description='''
         An argument to `@available` that restricts the availability on a
         certain platform to a version, e.g. `iOS 10` or `swift 3.4`.
         ''',
         children=[
             Child('Platform', kind='IdentifierToken',
                   classification='Keyword',
                   description='''
                   The name of the OS on which the availability should be
                   restricted or 'swift' if the availability should be
                   restricted based on a Swift version.
                   '''),
             Child('Version', kind='VersionTuple'),
         ]),

    # version-tuple -> integer-literal
    #                | float-literal
    #                | float-literal '.' integer-literal
    Node('VersionTuple', kind='Syntax',
         description='''
         A version number of the form major.minor.patch in which the minor
         and patch part may be ommited.
         ''',
         children=[
             Child('MajorMinor', kind='Syntax',
                   node_choices=[
                       Child('Major', kind='IntegerLiteralToken'),
                       Child('MajorMinor', kind='FloatingLiteralToken')
                   ], description='''
                   In case the version consists only of the major version, an
                   integer literal that specifies the major version. In case
                   the version consists of major and minor version number, a
                   floating literal in which the decimal part is interpreted
                   as the minor version.
                   '''),
             Child('PatchPeriod', kind='PeriodToken', is_optional=True,
                   description='''
                   If the version contains a patch number, the period
                   separating the minor from the patch number.
                   '''),
             Child('PatchVersion', kind='IntegerLiteralToken',
                   is_optional=True, description='''
                   The patch version if specified.
                   '''),
         ]),
]
