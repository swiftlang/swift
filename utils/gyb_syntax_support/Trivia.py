from Node import error
from kinds import lowercase_first_word


class Trivia(object):
    def __init__(self, name, comment, serialization_code, characters=[],
                 swift_characters=[], is_new_line=False, is_comment=False):
        self.name = name
        self.comment = comment
        self.serialization_code = serialization_code
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
    Trivia('Space', 'A space \' \' character.', characters=[' '],
           serialization_code=0),
    Trivia('Tab', 'A tab \'\\t\' character.', characters=['\\t'],
           serialization_code=1),
    Trivia('VerticalTab', 'A vertical tab \'\\v\' character.',
           characters=['\\v'], swift_characters=['\\u{2B7F}'],
           serialization_code=2),
    Trivia('Formfeed', 'A form-feed \'f\' character.', characters=['\\f'],
           swift_characters=['\\u{240C}'], serialization_code=3),
    Trivia('Newline', 'A newline \'\\n\' character.', characters=['\\n'],
           is_new_line=True, serialization_code=4),
    Trivia('CarriageReturn', 'A newline \'\\r\' character.',
           characters=['\\r'], is_new_line=True, serialization_code=5),
    Trivia('CarriageReturnLineFeed',
           'A newline consists of contiguous \'\\r\' and \'\\n\' characters.',
           characters=['\\r', '\\n'], is_new_line=True, serialization_code=6),
    Trivia('Backtick',
           'A backtick \'`\' character, used to escape identifiers.',
           characters=['`'], serialization_code=7),
    Trivia('LineComment', 'A developer line comment, starting with \'//\'',
           is_comment=True, serialization_code=8),
    Trivia('BlockComment',
           'A developer block comment, starting with \'/*\' and ending with'
           ' \'*/\'.',
           is_comment=True, serialization_code=9),
    Trivia('DocLineComment',
           'A documentation line comment, starting with \'///\'.',
           is_comment=True, serialization_code=10),
    Trivia('DocBlockComment',
           'A documentation block comment, starting with \'/**\' and ending '
           'with \'*/\'.',
           is_comment=True, serialization_code=11),
    Trivia('GarbageText', 'Any skipped garbage text.', serialization_code=12),
]


def verify_no_duplicate_serialization_codes(trivias):
    used_codes = set()
    for trivia in trivias:
        if trivia.serialization_code in used_codes:
            error("Serialization code %d used twice for trivia" %
                  trivia.serialization_code)
        used_codes.add(trivia.serialization_code)


verify_no_duplicate_serialization_codes(TRIVIAS)
