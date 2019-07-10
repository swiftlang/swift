
# [0xC2] is utf8 2 byte character start byte.
# 0xC2 without second byte is invalid UTF-8 sequence.
# It becomes garbage text trivia.
# Marker(1) is replaced to this sequence.
s/Z1/�/g

# [0xCC, 0x82] in UTF-8 is U+0302.
# This character is invalid for identifier start, but valid for identifier body.
# It becomes unknown token.
# If this type characters are conitguous, they are concatenated to one long unknown token.
# Marker(2) is replaced to this sequence.
s/Z2/̂/g

# [0xE2, 0x80, 0x9C] in UTF-8 is U+201C, left quote.
# It becomes single character unknown token.
# If this left quote and right quote enclosure text,
# they become one long unknown token.
# Marker(3) is replaced to this sequence.
s/Z3/“/g

# [0xE2, 0x80, 0x9D] in UTF-8 is U+201D, right quote.
# It becomes single character unknown token.
# Marker(4) is replaced to this sequence.
s/Z4/”/g

# [0xE1, 0x9A, 0x80] in UTF-8 is U+1680.
# This character is invalid for swift source.
# It becomes garbage trivia.
# Marker(5) is replaced to this sequence.
s/Z5/ /g

