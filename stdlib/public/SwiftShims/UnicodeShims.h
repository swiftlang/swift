//===--- UnicodeShims.h - Access to Unicode data for the core stdlib ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Data structures required for Unicode support in Swift that are
//  statically initialized in its runtime's C++ source.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_STDLIB_SHIMS_UNICODESHIMS_H_
#define SWIFT_STDLIB_SHIMS_UNICODESHIMS_H_

#include "SwiftStdint.h"
#include "SwiftStdbool.h"
#include "Visibility.h"

#if __has_feature(nullability)
#pragma clang assume_nonnull begin
#endif

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

SWIFT_RUNTIME_STDLIB_INTERFACE
const __swift_uint8_t *_swift_stdlib_GraphemeClusterBreakPropertyTrie;

struct _swift_stdlib_GraphemeClusterBreakPropertyTrieMetadataTy {
  unsigned BMPFirstLevelIndexBits;
  unsigned BMPDataOffsetBits;
  unsigned SuppFirstLevelIndexBits;
  unsigned SuppSecondLevelIndexBits;
  unsigned SuppDataOffsetBits;

  unsigned BMPLookupBytesPerEntry;
  unsigned BMPDataBytesPerEntry;
  unsigned SuppLookup1BytesPerEntry;
  unsigned SuppLookup2BytesPerEntry;
  unsigned SuppDataBytesPerEntry;

  unsigned TrieSize;

  unsigned BMPLookupBytesOffset;
  unsigned BMPDataBytesOffset;
  unsigned SuppLookup1BytesOffset;
  unsigned SuppLookup2BytesOffset;
  unsigned SuppDataBytesOffset;
};

SWIFT_RUNTIME_STDLIB_INTERFACE
const struct _swift_stdlib_GraphemeClusterBreakPropertyTrieMetadataTy
_swift_stdlib_GraphemeClusterBreakPropertyTrieMetadata;

SWIFT_RUNTIME_STDLIB_INTERFACE
const __swift_uint16_t *
_swift_stdlib_ExtendedGraphemeClusterNoBoundaryRulesMatrix;

SWIFT_RUNTIME_STDLIB_INTERFACE
SWIFT_READONLY __swift_int32_t
_swift_stdlib_unicode_compare_utf16_utf16(const __swift_uint16_t *Left,
                                          __swift_int32_t LeftLength,
                                          const __swift_uint16_t *Right,
                                          __swift_int32_t RightLength);

SWIFT_RUNTIME_STDLIB_INTERFACE
SWIFT_READONLY __swift_int32_t
_swift_stdlib_unicode_compare_utf8_utf16(const unsigned char *Left,
                                         __swift_int32_t LeftLength,
                                         const __swift_uint16_t *Right,
                                         __swift_int32_t RightLength);

SWIFT_RUNTIME_STDLIB_INTERFACE
SWIFT_READONLY __swift_int32_t
_swift_stdlib_unicode_compare_utf8_utf8(const unsigned char *Left,
                                        __swift_int32_t LeftLength,
                                        const unsigned char *Right,
                                        __swift_int32_t RightLength);

SWIFT_RUNTIME_STDLIB_INTERFACE
void *_swift_stdlib_unicodeCollationIterator_create(
    const __swift_uint16_t *Str,
    __swift_uint32_t Length);

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t _swift_stdlib_unicodeCollationIterator_next(
    void *CollationIterator, __swift_bool *HitEnd);

SWIFT_RUNTIME_STDLIB_INTERFACE
void _swift_stdlib_unicodeCollationIterator_delete(
    void *CollationIterator);

SWIFT_RUNTIME_STDLIB_INTERFACE
const __swift_int32_t *_swift_stdlib_unicode_getASCIICollationTable();

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t _swift_stdlib_unicode_strToUpper(
  __swift_uint16_t *Destination, __swift_int32_t DestinationCapacity,
  const __swift_uint16_t *Source, __swift_int32_t SourceLength);

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t _swift_stdlib_unicode_strToLower(
  __swift_uint16_t *Destination, __swift_int32_t DestinationCapacity,
  const __swift_uint16_t *Source, __swift_int32_t SourceLength);

typedef enum __swift_stdlib_UErrorCode {
    __swift_stdlib_U___swift_stdlib_USING_FALLBACK_WARNING  = -128,   
    __swift_stdlib_U_ERROR_WARNING_START     = -128,   
    __swift_stdlib_U___swift_stdlib_USING_DEFAULT_WARNING   = -127,   
    __swift_stdlib_U_SAFECLONE_ALLOCATED_WARNING = -126, 
    __swift_stdlib_U_STATE_OLD_WARNING       = -125,   
    __swift_stdlib_U_STRING_NOT_TERMINATED_WARNING = -124,
    __swift_stdlib_U_SORT_KEY_TOO_SHORT_WARNING = -123, 
    __swift_stdlib_U_AMBIGUOUS_ALIAS_WARNING = -122,   
    __swift_stdlib_U_DIFFERENT___swift_stdlib_UCA_VERSION = -121,     
    __swift_stdlib_U_PLUGIN_CHANGED_LEVEL_WARNING = -120, 
    __swift_stdlib_U_ERROR_WARNING_LIMIT,              
    __swift_stdlib_U_ZERO_ERROR              =  0,     
    __swift_stdlib_U_ILLEGAL_ARGUMENT_ERROR  =  1,     
    __swift_stdlib_U_MISSING_RESOURCE_ERROR  =  2,     
    __swift_stdlib_U_INVALID_FORMAT_ERROR    =  3,     
    __swift_stdlib_U_FILE_ACCESS_ERROR       =  4,     
    __swift_stdlib_U_INTERNAL_PROGRAM_ERROR  =  5,     
    __swift_stdlib_U_MESSAGE_PARSE_ERROR     =  6,     
    __swift_stdlib_U_MEMORY_ALLOCATION_ERROR =  7,     
    __swift_stdlib_U_INDEX_OUTOFBOUNDS_ERROR =  8,     
    __swift_stdlib_U_PARSE_ERROR             =  9,     
    __swift_stdlib_U_INVALID_CHAR_FOUND      = 10,     
    __swift_stdlib_U_TRUNCATED_CHAR_FOUND    = 11,     
    __swift_stdlib_U_ILLEGAL_CHAR_FOUND      = 12,     
    __swift_stdlib_U_INVALID_TABLE_FORMAT    = 13,     
    __swift_stdlib_U_INVALID_TABLE_FILE      = 14,     
    __swift_stdlib_U_BUFFER_OVERFLOW_ERROR   = 15,     
    __swift_stdlib_U___swift_stdlib_UNSUPPORTED_ERROR       = 16,     
    __swift_stdlib_U_RESOURCE_TYPE_MISMATCH  = 17,     
    __swift_stdlib_U_ILLEGAL_ESCAPE_SEQUENCE = 18,     
    __swift_stdlib_U___swift_stdlib_UNSUPPORTED_ESCAPE_SEQUENCE = 19, 
    __swift_stdlib_U_NO_SPACE_AVAILABLE      = 20,     
    __swift_stdlib_U_CE_NOT_FOUND_ERROR      = 21,     
    __swift_stdlib_U_PRIMARY_TOO_LONG_ERROR  = 22,     
    __swift_stdlib_U_STATE_TOO_OLD_ERROR     = 23,     
    __swift_stdlib_U_TOO_MANY_ALIASES_ERROR  = 24,     
    __swift_stdlib_U_ENUM_OUT_OF_SYNC_ERROR  = 25,     
    __swift_stdlib_U_INVARIANT_CONVERSION_ERROR = 26,  
    __swift_stdlib_U_INVALID_STATE_ERROR     = 27,     
    __swift_stdlib_U_COLLATOR_VERSION_MISMATCH = 28,   
    __swift_stdlib_U___swift_stdlib_USELESS_COLLATOR_ERROR  = 29,     
    __swift_stdlib_U_NO_WRITE_PERMISSION     = 30,     
    __swift_stdlib_U_STANDARD_ERROR_LIMIT,             
    __swift_stdlib_U_BAD_VARIABLE_DEFINITION=0x10000,
    __swift_stdlib_U_PARSE_ERROR_START = 0x10000,    
    __swift_stdlib_U_MALFORMED_RULE,                 
    __swift_stdlib_U_MALFORMED_SET,                  
    __swift_stdlib_U_MALFORMED_SYMBOL_REFERENCE,     
    __swift_stdlib_U_MALFORMED___swift_stdlib_UNICODE_ESCAPE,       
    __swift_stdlib_U_MALFORMED_VARIABLE_DEFINITION,  
    __swift_stdlib_U_MALFORMED_VARIABLE_REFERENCE,   
    __swift_stdlib_U_MISMATCHED_SEGMENT_DELIMITERS,  
    __swift_stdlib_U_MISPLACED_ANCHOR_START,         
    __swift_stdlib_U_MISPLACED_CURSOR_OFFSET,        
    __swift_stdlib_U_MISPLACED_QUANTIFIER,           
    __swift_stdlib_U_MISSING_OPERATOR,               
    __swift_stdlib_U_MISSING_SEGMENT_CLOSE,          
    __swift_stdlib_U_MULTIPLE_ANTE_CONTEXTS,         
    __swift_stdlib_U_MULTIPLE_CURSORS,               
    __swift_stdlib_U_MULTIPLE_POST_CONTEXTS,         
    __swift_stdlib_U_TRAILING_BACKSLASH,             
    __swift_stdlib_U___swift_stdlib_UNDEFINED_SEGMENT_REFERENCE,    
    __swift_stdlib_U___swift_stdlib_UNDEFINED_VARIABLE,             
    __swift_stdlib_U___swift_stdlib_UNQUOTED_SPECIAL,               
    __swift_stdlib_U___swift_stdlib_UNTERMINATED_QUOTE,             
    __swift_stdlib_U_RULE_MASK_ERROR,                
    __swift_stdlib_U_MISPLACED_COMPOUND_FILTER,      
    __swift_stdlib_U_MULTIPLE_COMPOUND_FILTERS,      
    __swift_stdlib_U_INVALID_RBT_SYNTAX,             
    __swift_stdlib_U_INVALID_PROPERTY_PATTERN,       
    __swift_stdlib_U_MALFORMED_PRAGMA,               
    __swift_stdlib_U___swift_stdlib_UNCLOSED_SEGMENT,               
    __swift_stdlib_U_ILLEGAL_CHAR_IN_SEGMENT,        
    __swift_stdlib_U_VARIABLE_RANGE_EXHAUSTED,       
    __swift_stdlib_U_VARIABLE_RANGE_OVERLAP,         
    __swift_stdlib_U_ILLEGAL_CHARACTER,              
    __swift_stdlib_U_INTERNAL_TRANSLITERATOR_ERROR,  
    __swift_stdlib_U_INVALID_ID,                     
    __swift_stdlib_U_INVALID_FUNCTION,               
    __swift_stdlib_U_PARSE_ERROR_LIMIT,              
    __swift_stdlib_U___swift_stdlib_UNEXPECTED_TOKEN=0x10100,       
    __swift_stdlib_U_FMT_PARSE_ERROR_START=0x10100,  
    __swift_stdlib_U_MULTIPLE_DECIMAL_SEPARATORS,    
    __swift_stdlib_U_MULTIPLE_DECIMAL_SEPERATORS = __swift_stdlib_U_MULTIPLE_DECIMAL_SEPARATORS, 
    __swift_stdlib_U_MULTIPLE_EXPONENTIAL_SYMBOLS,   
    __swift_stdlib_U_MALFORMED_EXPONENTIAL_PATTERN,  
    __swift_stdlib_U_MULTIPLE_PERCENT_SYMBOLS,       
    __swift_stdlib_U_MULTIPLE_PERMILL_SYMBOLS,       
    __swift_stdlib_U_MULTIPLE_PAD_SPECIFIERS,        
    __swift_stdlib_U_PATTERN_SYNTAX_ERROR,           
    __swift_stdlib_U_ILLEGAL_PAD_POSITION,           
    __swift_stdlib_U___swift_stdlib_UNMATCHED_BRACES,               
    __swift_stdlib_U___swift_stdlib_UNSUPPORTED_PROPERTY,           
    __swift_stdlib_U___swift_stdlib_UNSUPPORTED_ATTRIBUTE,          
    __swift_stdlib_U_ARGUMENT_TYPE_MISMATCH,         
    __swift_stdlib_U_DUPLICATE_KEYWORD,              
    __swift_stdlib_U___swift_stdlib_UNDEFINED_KEYWORD,              
    __swift_stdlib_U_DEFAULT_KEYWORD_MISSING,        
    __swift_stdlib_U_DECIMAL_NUMBER_SYNTAX_ERROR,    
    __swift_stdlib_U_FORMAT_INEXACT_ERROR,           
    __swift_stdlib_U_FMT_PARSE_ERROR_LIMIT,          
    __swift_stdlib_U_BRK_INTERNAL_ERROR=0x10200,          
    __swift_stdlib_U_BRK_ERROR_START=0x10200,             
    __swift_stdlib_U_BRK_HEX_DIGITS_EXPECTED,             
    __swift_stdlib_U_BRK_SEMICOLON_EXPECTED,              
    __swift_stdlib_U_BRK_RULE_SYNTAX,                     
    __swift_stdlib_U_BRK___swift_stdlib_UNCLOSED_SET,                    
    __swift_stdlib_U_BRK_ASSIGN_ERROR,                    
    __swift_stdlib_U_BRK_VARIABLE_REDFINITION,            
    __swift_stdlib_U_BRK_MISMATCHED_PAREN,                
    __swift_stdlib_U_BRK_NEW_LINE_IN_QUOTED_STRING,       
    __swift_stdlib_U_BRK___swift_stdlib_UNDEFINED_VARIABLE,              
    __swift_stdlib_U_BRK_INIT_ERROR,                      
    __swift_stdlib_U_BRK_RULE_EMPTY_SET,                  
    __swift_stdlib_U_BRK___swift_stdlib_UNRECOGNIZED_OPTION,             
    __swift_stdlib_U_BRK_MALFORMED_RULE_TAG,              
    __swift_stdlib_U_BRK_ERROR_LIMIT,                     
    __swift_stdlib_U_REGEX_INTERNAL_ERROR=0x10300,       
    __swift_stdlib_U_REGEX_ERROR_START=0x10300,          
    __swift_stdlib_U_REGEX_RULE_SYNTAX,                  
    __swift_stdlib_U_REGEX_INVALID_STATE,                
    __swift_stdlib_U_REGEX_BAD_ESCAPE_SEQUENCE,          
    __swift_stdlib_U_REGEX_PROPERTY_SYNTAX,              
    __swift_stdlib_U_REGEX___swift_stdlib_UNIMPLEMENTED,                
    __swift_stdlib_U_REGEX_MISMATCHED_PAREN,             
    __swift_stdlib_U_REGEX_NUMBER_TOO_BIG,               
    __swift_stdlib_U_REGEX_BAD_INTERVAL,                 
    __swift_stdlib_U_REGEX_MAX_LT_MIN,                   
    __swift_stdlib_U_REGEX_INVALID_BACK_REF,             
    __swift_stdlib_U_REGEX_INVALID_FLAG,                 
    __swift_stdlib_U_REGEX_LOOK_BEHIND_LIMIT,            
    __swift_stdlib_U_REGEX_SET_CONTAINS_STRING,          
#ifndef __swift_stdlib_U_HIDE_DEPRECATED_API
    __swift_stdlib_U_REGEX_OCTAL_TOO_BIG,                
#endif  
    __swift_stdlib_U_REGEX_MISSING_CLOSE_BRACKET=__swift_stdlib_U_REGEX_SET_CONTAINS_STRING+2, 
    __swift_stdlib_U_REGEX_INVALID_RANGE,                
    __swift_stdlib_U_REGEX_STACK_OVERFLOW,               
    __swift_stdlib_U_REGEX_TIME_OUT,                     
    __swift_stdlib_U_REGEX_STOPPED_BY_CALLER,            
#ifndef __swift_stdlib_U_HIDE_DRAFT_API
    __swift_stdlib_U_REGEX_PATTERN_TOO_BIG,              
    __swift_stdlib_U_REGEX_INVALID_CAPTURE_GROUP_NAME,   
#endif  
    __swift_stdlib_U_REGEX_ERROR_LIMIT=__swift_stdlib_U_REGEX_STOPPED_BY_CALLER+3, 
    __swift_stdlib_U_IDNA_PROHIBITED_ERROR=0x10400,
    __swift_stdlib_U_IDNA_ERROR_START=0x10400,
    __swift_stdlib_U_IDNA___swift_stdlib_UNASSIGNED_ERROR,
    __swift_stdlib_U_IDNA_CHECK_BIDI_ERROR,
    __swift_stdlib_U_IDNA_STD3_ASCII_RULES_ERROR,
    __swift_stdlib_U_IDNA_ACE_PREFIX_ERROR,
    __swift_stdlib_U_IDNA_VERIFICATION_ERROR,
    __swift_stdlib_U_IDNA_LABEL_TOO_LONG_ERROR,
    __swift_stdlib_U_IDNA_ZERO_LENGTH_LABEL_ERROR,
    __swift_stdlib_U_IDNA_DOMAIN_NAME_TOO_LONG_ERROR,
    __swift_stdlib_U_IDNA_ERROR_LIMIT,
    __swift_stdlib_U_STRINGPREP_PROHIBITED_ERROR = __swift_stdlib_U_IDNA_PROHIBITED_ERROR,
    __swift_stdlib_U_STRINGPREP___swift_stdlib_UNASSIGNED_ERROR = __swift_stdlib_U_IDNA___swift_stdlib_UNASSIGNED_ERROR,
    __swift_stdlib_U_STRINGPREP_CHECK_BIDI_ERROR = __swift_stdlib_U_IDNA_CHECK_BIDI_ERROR,
    __swift_stdlib_U_PLUGIN_ERROR_START=0x10500,         
    __swift_stdlib_U_PLUGIN_TOO_HIGH=0x10500,            
    __swift_stdlib_U_PLUGIN_DIDNT_SET_LEVEL,             
    __swift_stdlib_U_PLUGIN_ERROR_LIMIT,                 
    __swift_stdlib_U_ERROR_LIMIT=__swift_stdlib_U_PLUGIN_ERROR_LIMIT      
} __swift_stdlib_UErrorCode;
    
enum {
    __swift_stdlib_UTEXT_PROVIDER_LENGTH_IS_EXPENSIVE = 1,
    __swift_stdlib_UTEXT_PROVIDER_STABLE_CHUNKS = 2,
    __swift_stdlib_UTEXT_PROVIDER_WRITABLE = 3,
    __swift_stdlib_UTEXT_PROVIDER_HAS_META_DATA = 4,
    __swift_stdlib_UTEXT_PROVIDER_OWNS_TEXT = 5
};
typedef __swift_uint32_t __swift_stdlib_UChar32;
typedef __swift_uint16_t __swift_stdlib_UChar;
typedef __swift_int8_t __swift_stdlib_UBool;
typedef struct __swift_stdlib_UText __swift_stdlib_UText;
typedef __swift_stdlib_UText * 
__swift_stdlib_UTextClone(__swift_stdlib_UText *dest, const __swift_stdlib_UText *src, __swift_stdlib_UBool deep, __swift_stdlib_UErrorCode *status);
typedef __swift_int64_t 
__swift_stdlib_UTextNativeLength(__swift_stdlib_UText *ut);
typedef __swift_stdlib_UBool 
__swift_stdlib_UTextAccess(__swift_stdlib_UText *ut, __swift_int64_t nativeIndex, __swift_stdlib_UBool forward);
typedef __swift_int32_t 
__swift_stdlib_UTextExtract(__swift_stdlib_UText *ut,
             __swift_int64_t nativeStart, __swift_int64_t nativeLimit,
             __swift_stdlib_UChar *dest, __swift_int32_t destCapacity,
             __swift_stdlib_UErrorCode *status);
typedef __swift_int32_t 
__swift_stdlib_UTextReplace(__swift_stdlib_UText *ut,
             __swift_int64_t nativeStart, __swift_int64_t nativeLimit,
             const __swift_stdlib_UChar *replacementText, __swift_int32_t replacmentLength,
             __swift_stdlib_UErrorCode *status);
typedef void 
__swift_stdlib_UTextCopy(__swift_stdlib_UText *ut,
          __swift_int64_t nativeStart, __swift_int64_t nativeLimit,
          __swift_int64_t nativeDest,
          __swift_stdlib_UBool move,
          __swift_stdlib_UErrorCode *status);
typedef __swift_int64_t 
__swift_stdlib_UTextMapOffsetToNative(const __swift_stdlib_UText *ut);
typedef __swift_int32_t 
__swift_stdlib_UTextMapNativeIndexToUTF16(const __swift_stdlib_UText *ut, __swift_int64_t nativeIndex);
typedef void 
__swift_stdlib_UTextClose(__swift_stdlib_UText *ut);

struct __swift_stdlib_UTextFuncs {
    __swift_int32_t       tableSize;
    __swift_int32_t       reserved1,  reserved2,  reserved3;
    __swift_stdlib_UTextClone *clone;
    __swift_stdlib_UTextNativeLength *nativeLength;
    __swift_stdlib_UTextAccess *access;
    __swift_stdlib_UTextExtract *extract;
    __swift_stdlib_UTextReplace *__null_unspecified replace;
    __swift_stdlib_UTextCopy *__null_unspecified copy;
    __swift_stdlib_UTextMapOffsetToNative *mapOffsetToNative;
    __swift_stdlib_UTextMapNativeIndexToUTF16 *mapNativeIndexToUTF16;
    __swift_stdlib_UTextClose  *__null_unspecified close;
    __swift_stdlib_UTextClose  *__null_unspecified spare1;
    __swift_stdlib_UTextClose  *__null_unspecified spare2;
    __swift_stdlib_UTextClose  *__null_unspecified spare3;
};
typedef struct __swift_stdlib_UTextFuncs __swift_stdlib_UTextFuncs;
enum {
    __swift_stdlib_UTEXT_MAGIC = 0x345ad82c
};
struct __swift_stdlib_UText {
    __swift_uint32_t       magic;
    __swift_int32_t        flags;
    __swift_int32_t         providerProperties;
    __swift_int32_t         sizeOfStruct;
    __swift_int64_t         chunkNativeLimit;
    __swift_int32_t        extraSize;
    __swift_int32_t         nativeIndexingLimit;
    __swift_int64_t         chunkNativeStart;
    __swift_int32_t         chunkOffset;
    __swift_int32_t         chunkLength;
    const __swift_stdlib_UChar    *chunkContents;
    const __swift_stdlib_UTextFuncs     *pFuncs;
    void          *pExtra;
    const void   *context;
    const void     *p; 
    const void     *q;
    const void     *r;
    void           *privP;
    __swift_int64_t         a;
    __swift_int32_t         b;
    __swift_int32_t         c;
    __swift_int64_t         privA;
    __swift_int32_t         privB;
    __swift_int32_t         privC;
};

typedef enum __swift_stdlib_UBreakIteratorType {
  __swift_stdlib_UBRK_CHARACTER = 0,
  __swift_stdlib_UBRK_WORD = 1,
  __swift_stdlib_UBRK_LINE = 2,
  __swift_stdlib_UBRK_SENTENCE = 3,
#ifndef U_HIDE_DEPRECATED_API
  __swift_stdlib_UBRK_TITLE = 4,
#endif 
  __swift_stdlib_UBRK_COUNT = 5
} __swift_stdlib_UBreakIteratorType;

typedef struct __swift_stdlib_UBreakIterator
  __swift_stdlib_UBreakIterator;

SWIFT_RUNTIME_STDLIB_INTERFACE
void __swift_stdlib_ubrk_close(__swift_stdlib_UBreakIterator *bi);
    
SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_stdlib_UBreakIterator *__swift_stdlib_ubrk_open(
    __swift_stdlib_UBreakIteratorType type,
    const char *__null_unspecified locale,
    const __swift_stdlib_UChar *__null_unspecified text,
    __swift_int32_t textLength,
    __swift_stdlib_UErrorCode *status
    );
    
SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t
__swift_stdlib_ubrk_preceding(__swift_stdlib_UBreakIterator *bi,
           __swift_int32_t offset);

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t __swift_stdlib_ubrk_following(__swift_stdlib_UBreakIterator *bi,
           __swift_int32_t offset);
SWIFT_RUNTIME_STDLIB_INTERFACE
void
__swift_stdlib_ubrk_setUText(__swift_stdlib_UBreakIterator* bi,
             __swift_stdlib_UText*          text,
             __swift_stdlib_UErrorCode*     status);


typedef enum {
    
    __swift_stdlib_UNORM2_COMPOSE,
    
    __swift_stdlib_UNORM2_DECOMPOSE,
    
    __swift_stdlib_UNORM2_FCD,
    
    __swift_stdlib_UNORM2_COMPOSE_CONTIGUOUS
} __swift_stdlib_UNormalization2Mode;


typedef enum __swift_stdlib_UNormalizationCheckResult {
  
  __swift_stdlib_UNORM_NO,
  
  __swift_stdlib_UNORM_YES,
  
  __swift_stdlib_UNORM_MAYBE
} __swift_stdlib_UNormalizationCheckResult;


struct __swift_stdlib_UNormalizer2;
typedef struct __swift_stdlib_UNormalizer2 __swift_stdlib_UNormalizer2;  

SWIFT_RUNTIME_STDLIB_INTERFACE
const __swift_stdlib_UNormalizer2 *__null_unspecified
__swift_stdlib_unorm2_getNFCInstance(__swift_stdlib_UErrorCode *pErrorCode);


SWIFT_RUNTIME_STDLIB_INTERFACE
const __swift_stdlib_UNormalizer2 *__null_unspecified
__swift_stdlib_unorm2_getNFDInstance(__swift_stdlib_UErrorCode *pErrorCode);


SWIFT_RUNTIME_STDLIB_INTERFACE
const __swift_stdlib_UNormalizer2 *__null_unspecified
__swift_stdlib_unorm2_getNFKCInstance(__swift_stdlib_UErrorCode *pErrorCode);


SWIFT_RUNTIME_STDLIB_INTERFACE
const __swift_stdlib_UNormalizer2 *__null_unspecified
__swift_stdlib_unorm2_getNFKDInstance(__swift_stdlib_UErrorCode *pErrorCode);


SWIFT_RUNTIME_STDLIB_INTERFACE
const __swift_stdlib_UNormalizer2 *__null_unspecified
__swift_stdlib_unorm2_getNFKCCasefoldInstance(__swift_stdlib_UErrorCode *pErrorCode);


SWIFT_RUNTIME_STDLIB_INTERFACE
const __swift_stdlib_UNormalizer2 *__null_unspecified
__swift_stdlib_unorm2_getInstance(const char *__null_unspecified packageName,
                   const char *name,
                   __swift_stdlib_UNormalization2Mode mode,
                   __swift_stdlib_UErrorCode *pErrorCode);


// We don't want to expose USet, at least not yet
#if 0
SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_stdlib_UNormalizer2 *
__swift_stdlib_unorm2_openFiltered(const __swift_stdlib_UNormalizer2 *norm2, const
                                   __swift_stdlib_USet *filterSet, __swift_stdlib_UErrorCode *pErrorCode);
#endif 

SWIFT_RUNTIME_STDLIB_INTERFACE
void
__swift_stdlib_unorm2_close(__swift_stdlib_UNormalizer2 *norm2);


SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t
__swift_stdlib_unorm2_normalize(const __swift_stdlib_UNormalizer2 *norm2,
                 const __swift_stdlib_UChar *src, __swift_int32_t length,
                 __swift_stdlib_UChar *dest, __swift_int32_t capacity,
                 __swift_stdlib_UErrorCode *pErrorCode);

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t
__swift_stdlib_unorm2_normalizeSecondAndAppend(const __swift_stdlib_UNormalizer2 *norm2,
                                __swift_stdlib_UChar *first, __swift_int32_t firstLength, __swift_int32_t firstCapacity,
                                const __swift_stdlib_UChar *second, __swift_int32_t secondLength,
                                __swift_stdlib_UErrorCode *pErrorCode);

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t
__swift_stdlib_unorm2_append(const __swift_stdlib_UNormalizer2 *norm2,
              __swift_stdlib_UChar *first, __swift_int32_t firstLength, __swift_int32_t firstCapacity,
              const __swift_stdlib_UChar *second, __swift_int32_t secondLength,
              __swift_stdlib_UErrorCode *pErrorCode);


SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t
__swift_stdlib_unorm2_getDecomposition(const __swift_stdlib_UNormalizer2 *norm2,
                        __swift_stdlib_UChar32 c, __swift_stdlib_UChar *decomposition, __swift_int32_t capacity,
                        __swift_stdlib_UErrorCode *pErrorCode);


SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t
__swift_stdlib_unorm2_getRawDecomposition(const __swift_stdlib_UNormalizer2 *norm2,
                           __swift_stdlib_UChar32 c, __swift_stdlib_UChar *decomposition, __swift_int32_t capacity,
                           __swift_stdlib_UErrorCode *pErrorCode);


SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_stdlib_UChar32
__swift_stdlib_unorm2_composePair(const __swift_stdlib_UNormalizer2 *norm2, __swift_stdlib_UChar32 a, __swift_stdlib_UChar32 b);


SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_uint8_t
__swift_stdlib_unorm2_getCombiningClass(const __swift_stdlib_UNormalizer2 *norm2, __swift_stdlib_UChar32 c);


SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_stdlib_UBool
__swift_stdlib_unorm2_isNormalized(const __swift_stdlib_UNormalizer2 *norm2,
                    const __swift_stdlib_UChar *s, __swift_int32_t length,
                    __swift_stdlib_UErrorCode *pErrorCode);


SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_stdlib_UNormalizationCheckResult
__swift_stdlib_unorm2_quickCheck(const __swift_stdlib_UNormalizer2 *norm2,
                  const __swift_stdlib_UChar *s, __swift_int32_t length,
                  __swift_stdlib_UErrorCode *pErrorCode);


SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_int32_t
__swift_stdlib_unorm2_spanQuickCheckYes(const __swift_stdlib_UNormalizer2 *norm2,
                         const __swift_stdlib_UChar *s, __swift_int32_t length,
                         __swift_stdlib_UErrorCode *pErrorCode);


SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_stdlib_UBool
__swift_stdlib_unorm2_hasBoundaryBefore(const __swift_stdlib_UNormalizer2 *norm2, __swift_stdlib_UChar32 c);


SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_stdlib_UBool
__swift_stdlib_unorm2_hasBoundaryAfter(const __swift_stdlib_UNormalizer2 *norm2, __swift_stdlib_UChar32 c);

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_stdlib_UBool
__swift_stdlib_unorm2_isInert(const __swift_stdlib_UNormalizer2 *norm2, __swift_stdlib_UChar32 c);
    
#ifdef __cplusplus
}} // extern "C", namespace swift
#endif


#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif


#endif
