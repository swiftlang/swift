from kinds import lowercase_first_word


class Trivia(object):
    def __init__(self, name, comment, characters=[], swift_characters=[],
                 is_new_line=False, is_comment=False):
        self.name = name
        self.comment = comment
        self.characters = characters
        self.lower_name = lowercase_first_word(name)
        self.is_new_line = is_new_line
        self.is_comment = is_comment

        # Swift sometimes doesn't support escaped characters like \f or \v;
        # we should allow specifying alternatives explicitly.
        self.swift_characters = swift_characters if swift_characters else\
            characters
        assert len(self.swift_characters) == len(self.characters)

    def characters_len(self):
        return len(self.characters)

    def is_collection(self):
        return self.characters_len() > 0


TRIVIAS = [
    Trivia('Space', 'A space \' \' character.', characters=[' ']),
    Trivia('Tab', 'A tab \'\\t\' character.', characters=['\\t']),
    Trivia('VerticalTab', 'A vertical tab \'\\v\' character.',
           characters=['\\v'], swift_characters=['\\u{2B7F}']),
    Trivia('Formfeed', 'A form-feed \'f\' character.', characters=['\\f'],
           swift_characters=['\\u{240C}']),
    Trivia('Newline', 'A newline \'\\n\' character.', characters=['\\n'],
           is_new_line=True),
    Trivia('CarriageReturn', 'A newline \'\\r\' character.',
           characters=['\\r'],
           is_new_line=True),

    Trivia('CarriageReturnLineFeed',
           'A newline consists of contiguous \'\\r\' and \'\\n\' characters.',
           characters=['\\r', '\\n'], is_new_line=True),

    Trivia('Backtick',
           'A backtick \'`\' character, used to escape identifiers.',
           characters=['`']),

    Trivia('LineComment', 'A developer line comment, starting with \'//\'',
           is_comment=True),
    Trivia('BlockComment',
           'A developer block comment, starting with \'/*\' and ending with'
           ' \'*/\'.',
           is_comment=True),
    Trivia('DocLineComment',
           'A documentation line comment, starting with \'///\'.',
           is_comment=True),
    Trivia('DocBlockComment',
           'A documentation block comment, starting with \'/**\' and ending '
           'with \'*/\'.',
           is_comment=True),
    Trivia('GarbageText', 'Any skipped garbage text.'),
]
