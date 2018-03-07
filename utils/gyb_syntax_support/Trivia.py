from kinds import lowercase_first_word


class Trivia(object):
    def __init__(self, name, comment, characters=[], is_new_line=False):
        self.name = name
        self.comment = comment
        self.characters = characters
        self.lower_name = lowercase_first_word(name)
        self.is_new_line = is_new_line

    def characters_len(self):
        return len(self.characters)

    def is_collection(self):
        return self.characters_len() > 0


TRIVIAS = [
    Trivia('Space', 'A space \' \' character.', characters=[' ']),
    Trivia('Tab', 'A tab \'\\t\' character.', characters=['\\t']),
    Trivia('VerticalTab', 'A vertical tab \'\\v\' character.',
           characters=['\\v']),
    Trivia('Formfeed', 'A form-feed \'f\' character.', characters=['\\f']),
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

    Trivia('LineComment', 'A developer line comment, starting with \'//\''),
    Trivia('BlockComment',
           'A developer block comment, starting with \'/*\' and ending with'
           ' \'*/\'.'),
    Trivia('DocLineComment',
           'A documentation line comment, starting with \'///\'.'),
    Trivia('DocBlockComment',
           'A documentation block comment, starting with \'/**\' and ending '
           'with \'*/\'.'),
    Trivia('GarbageText', 'Any skipped garbage text.'),
]
