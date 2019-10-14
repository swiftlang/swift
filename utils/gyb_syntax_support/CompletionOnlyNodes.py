from Child import Child
from Node import Node  # noqa: I201

# These nodes are used only in code completion.

COMPLETIONONLY_NODES = [
    # type
    Node('CodeCompletionType', kind='Type',
         children=[
             Child('Base', kind='Type', is_optional=True),
             Child('Period', kind='Token',
                   token_choices=[
                       'PeriodToken',
                       'PrefixPeriodToken',
                   ],
                   is_optional=True),
             Child('CodeCompletionToken', kind='Token'),
         ]),
]
