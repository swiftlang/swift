/*
******************************************************************************
* Copyright (C) 2006-2008 Apple Inc. All Rights Reserved.
******************************************************************************
*/

#ifndef URBTOK_H
#define URBTOK_H

#include "unicode/utypes.h"

#if !UCONFIG_NO_BREAK_ITERATION

#include "unicode/utext.h"
#include "unicode/ubrk.h"
#include "unicode/parseerr.h"


typedef struct RuleBasedTokenRange {
    signed long location;
    signed long length;
} RuleBasedTokenRange;

/**
 * Open a new UBreakIterator for tokenizing text using specified breaking rules.
 * The rule syntax is ... (TBD)
 * @param rules A set of rules specifying the text breaking conventions.
 * @param rulesLength The number of characters in rules, or -1 if null-terminated.
 * @param parseErr   Receives position and context information for any syntax errors
 *                   detected while parsing the rules.
 * @param status A UErrorCode to receive any errors.
 * @return A UBreakIterator for the specified rules.
 * @see ubrk_open
 * @internal
 */
U_INTERNAL UBreakIterator* U_EXPORT2
urbtok_openRules(const UChar     *rules,
               int32_t         rulesLength,
               UParseError     *parseErr,
               UErrorCode      *status);

/**
 * Open a new UBreakIterator for tokenizing text using specified breaking rules.
 * @param rules A set of rules specifying the text breaking conventions. The binary rules
 *              must be at least 32-bit aligned. Note: This version makes a copy of the
 *				rules, so after calling this function the caller can close or release
 *				the rules that were passed to this function. The copy created by this
 *				call will be freed when ubrk_close() is called on the UBreakIterator*.
 * @param status A UErrorCode to receive any errors.
 * @return A UBreakIterator for the specified rules.
 * @see ubrk_open
 * @internal
 */
U_INTERNAL UBreakIterator* U_EXPORT2
urbtok_openBinaryRules(const uint8_t *rules,
               UErrorCode      *status);

/**
 * Open a new UBreakIterator for tokenizing text using specified breaking rules.
 * @param rules A set of rules specifying the text breaking conventions. The binary rules
 *              must be at least 32-bit aligned. Note: This version does NOT make a copy
 *				of the rules, so after calling this function the caller must not close or
 *				release the rules passed to this function until after they are finished
 *				with this UBreakIterator* (and any others created using the same rules)
  *				and have called ubrk_close() to close the UBreakIterator* (and any others
 *				using the same rules).
 * @param status A UErrorCode to receive any errors.
 * @return A UBreakIterator for the specified rules.
 * @see ubrk_open
 * @internal
 */
U_INTERNAL UBreakIterator* U_EXPORT2
urbtok_openBinaryRulesNoCopy(const uint8_t *rules,
               UErrorCode      *status);

/**
 * Get the (native-endian) binary break rules for this tokenizer.
 * @param bi The tokenizer to use.
 * @param buffer The output buffer for the rules. You can pass 0 to get the required size.
 * @param buffSize The size of the output buffer.
 * @param status A UErrorCode to receive any errors.
 * @return The actual size of the binary rules, whether they fit the buffer or not.
 * @internal
 */
U_INTERNAL uint32_t U_EXPORT2
urbtok_getBinaryRules(UBreakIterator      *bi,
                uint8_t             *buffer,
                uint32_t            buffSize,
                UErrorCode          *status);

/**
 * Tokenize text using a rule-based tokenizer.
 * @param bi The tokenizer to use.
 * @param maxTokens The maximum number of tokens to return.
 * @param outTokens An array of RuleBasedTokenRange to fill in with the tokens.
 * @param outTokenFlags An (optional) array of uint32_t to fill in with token flags.
 * @return The number of tokens returned, 0 if done.
 * @internal
 */
U_INTERNAL int32_t U_EXPORT2
urbtok_tokenize(UBreakIterator      *bi,
               int32_t              maxTokens,
               RuleBasedTokenRange  *outTokens,
               unsigned long        *outTokenFlags);

/**
 * Swap the endianness of a set of binary break rules.
 * @param rules A set of rules which need swapping.
 * @param buffer The output buffer for the swapped rules, which must be the same
 *               size as the input rules buffer.
 * @param inIsBigEndian UBool indicating whether the input is big-endian
 * @param outIsBigEndian UBool indicating whether the output should be big-endian
 * @param status A UErrorCode to receive any errors.
 * @internal
 */
U_INTERNAL void U_EXPORT2
urbtok_swapBinaryRules(const uint8_t *rules,
               uint8_t          *buffer,
               UBool            inIsBigEndian,
               UBool            outIsBigEndian,
               UErrorCode       *status);


#endif /* #if !UCONFIG_NO_BREAK_ITERATION */

#endif
