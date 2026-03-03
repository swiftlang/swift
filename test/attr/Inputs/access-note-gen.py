#!/usr/bin/env python3
# coding=utf8

"""
Convert selected @objc attributes in a source file into access notes, removing
the originals in the process.
"""

import io
import re
import sys

#
# Entry point
#


def main():
    if len(sys.argv) != 4:
        print('Too few args to ' + sys.argv[0])
        print('Usage: access-note-gen.py <input-file> <output-source-file> ' +
              '<output-access-notes-file>')
        sys.exit(1)

    with io.open(sys.argv[1], mode='r', encoding='utf8') as input_file, \
            io.open(sys.argv[2], mode='w', encoding='utf8') as output_file, \
            io.open(sys.argv[3], mode='w', encoding='utf8') as access_notes_file:

        # Add header to access notes file
        access_notes_file.write(u"""\
Reason: 'fancy tests'
Notes:""")

        # Loop over input lines, transforming them into output lines, writing access
        # notes as a side effect.
        for input_line in input_file:
            # Look for access-note-move comments.
            input_line = access_note_move_re.sub(replacer(move_at_objc_to_access_note,
                                                          access_notes_file),
                                                 input_line, count=1)

            # Look for access-note-adjust comments.
            input_line = adjust_comment_re.sub(replacer(adjust_comments),
                                               input_line, count=1)

            output_file.write(input_line)


#
# Offsets
#

"""Matches an @±N offset."""
offset_re_fragment = r'[ \t]*(?:@([+-]\d+))?[ \t]*'


def offsetify(*offsets):
    """Sum line offsets matched by offset_re_fragment and convert them to strings
       like @+3 or @-2."""

    offset = sum([int(o) for o in offsets if o is not None])
    if offset < 0:
        return u"@-" + str(-offset)
    elif offset > 0:
        return u"@+" + str(offset)
    else:
        return u""


#
# Adjusting comments
#

"""Matches expected-warning/note/remark and its offset."""
expected_other_diag_re = re.compile(r'expected-(warning|note|remark)' +
                                    offset_re_fragment)

"""Matches expected-error and its offset."""
expected_error_re = re.compile(r'expected-error' + offset_re_fragment +
                               r'\s*(\d*\s*)\{\{' +
                               r'([^}\\]*(?:(?:\}?\\.|\}[^}])[^}\\]*)*)' +
                               r'\}\}')

"""Matches the string "marked '@objc'"."""
marked_objc_re = re.compile(r"marked '@objc'")

"""Matches any non-none fix-it expectation."""
fixit_re = re.compile(r'{{\d+-\d+=[^}]*}}')


def adjust_comments(offset, inserted_attr, comment_str):
    """Replace expected-errors with expected-remarks, and make other adjustments
       to diagnostics so that they reflect access notes."""

    prefix = u"{{ignored access note: "
    suffix = u"; did not implicitly add '" + inserted_attr + "' to this }}"

    adjusted = expected_other_diag_re.sub(lambda m: u"expected-" + m.group(1) +
                                                    offsetify(offset, m.group(2)),
                                          comment_str)
    adjusted = expected_error_re.sub(lambda m: u"expected-remark" +
                                               offsetify(offset, m.group(1)) + " " +
                                               m.group(2) + prefix + m.group(3) +
                                               suffix,
                                     adjusted)
    adjusted = marked_objc_re.sub(u"marked '@objc' by an access note", adjusted)
    adjusted = fixit_re.sub(u"{{none}}", adjusted)

    return u"// [expectations adjusted] " + adjusted


#
# Writing attrs to access notes
#

def move_at_objc_to_access_note(access_notes_file, arg, maybe_bad, offset,
                                access_note_name):
    """Write an @objc attribute into an access notes file, then return the
       string that will replace the attribute and trailing comment."""

    is_bad = (maybe_bad == "bad-")

    access_notes_file.write(u"""
- Name: '{}'
  ObjC: true""".format(access_note_name))
    if arg:
        access_notes_file.write(u"""
  ObjCName: '{}'""".format(arg))

    # Default to shifting expected diagnostics down 1 line.
    if offset is None:
        offset = 1

    inserted_attr = u"@objc"
    if arg:
        inserted_attr += u"(" + arg + u")"

    replacement = u"// access-note-adjust" + offsetify(offset) + \
                  u"{{" + inserted_attr + "}} [attr moved] "

    if not is_bad:
        replacement += u"expected-remark{{implicitly added '" + inserted_attr + \
                       u"' to this }} expected-note{{add '" + inserted_attr + \
                       u"' explicitly to silence this warning}}"

    return replacement


#
# Matching lines
#

"""Matches '@objc(foo) // access-note-move{{access-note-name}}'
   or '@objc // bad-access-note-move{{access-note-name}}'"""
access_note_move_re = re.compile(r'@objc(?:\(([\w:]+)\))?[ \t]*' +
                                 r'//[ \t]*(bad-)?access-note-move' +
                                 offset_re_fragment +
                                 r'\{\{([^}]*)\}\}')

"""Matches // access-note-adjust{{@objc}} <comment>"""
adjust_comment_re = re.compile(r'//[ \t]*access-note-adjust' + offset_re_fragment +
                               r'\{\{([^}]*)\}\}[ \t]*(.*)')


def replacer(fn, *args):
    """Returns a lambda which calls fn with args, followed by the groups from
       the match passed to the lambda."""

    return lambda m: fn(*(args + m.groups()))


main()
