"""
LLDB Formatters for LLVM data types for use in the swift project.

Load into LLDB with 'command script import /path/to/lldbDataFormatters.py'
"""

import sys


def __lldb_init_module(debugger, internal_dict):
    tName = 'lldbSwiftDataFormatters.SmallBitVectorSummaryProvider'
    debugger.HandleCommand('type summary add -w llvm '
                           '-F %s -x "^llvm::SmallBitVector$"' % tName)


def SmallBitVectorSummaryProvider(valobj, internal_dict):
    underlyingValue = valobj.GetChildMemberWithName('X').GetValueAsUnsigned()
    numBaseBits = 32
    is64Bit = sys.maxsize > 2**32
    if is64Bit:
        numBaseBits = 64
    smallNumRawBits = numBaseBits - 1
    smallNumSizeBits = None
    if numBaseBits == 32:
        smallNumSizeBits = 5
    elif numBaseBits == 64:
        smallNumSizeBits = 6
    else:
        smallNumSizeBits = smallNumRawBits
    smallNumDataBits = smallNumRawBits - smallNumSizeBits

    # If our underlying value is not small, print we can not dump large values.
    isSmallMask = 1
    if (underlyingValue & isSmallMask) == 0:
        return '<can not read large SmallBitVector>'

    smallRawBits = underlyingValue >> 1
    smallSize = smallRawBits >> smallNumDataBits
    bits = smallRawBits & ((1 << (smallSize + 1)) - 1)
    res = "["
    for i in reversed(range(0, smallSize)):
        if bool(bits & (1 << i)):
            res += '1'
        else:
            res += '0'
    res += "]"
    return res
