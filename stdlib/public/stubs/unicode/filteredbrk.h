/*
********************************************************************************
*   Copyright (C) 1997-2015, International Business Machines
*   Corporation and others.  All Rights Reserved.
********************************************************************************
*/

#ifndef FILTEREDBRK_H
#define FILTEREDBRK_H

#include "unicode/utypes.h"
#include "unicode/brkiter.h"

#if !UCONFIG_NO_BREAK_ITERATION && !UCONFIG_NO_FILTERED_BREAK_ITERATION
#ifndef U_HIDE_INTERNAL_API

U_NAMESPACE_BEGIN

/**
 * \file
 * \brief C++ API: FilteredBreakIteratorBuilder
 */

/**
 * The BreakIteratorFilter is used to modify the behavior of a BreakIterator
 *  by constructing a new BreakIterator which suppresses certain segment boundaries.
 *  See  http://www.unicode.org/reports/tr35/tr35-general.html#Segmentation_Exceptions .
 *  For example, a typical English Sentence Break Iterator would break on the space
 *  in the string "Mr. Smith" (resulting in two segments),
 *  but with "Mr." as an exception, a filtered break iterator
 *  would consider the string "Mr. Smith" to be a single segment.
 *
 * <p><b>Note:</b> An instance of {@link BreakIterator} returned by this builder
 * class currently does not support following operations in this technology preview
 * version:
 * <ul>
 *   <li>{@link BreakIterator#next(int32_t) next(int32_t n)}</li>
 *   <li>{@link BreakIterator#previous(void) previous(void)}</li>
 *   <li>{@link BreakIterator#following(int32_t) following(int32_t offset)}</li>
 *   <li>{@link BreakIterator#preceding(int32_t) preceding(int32_t offset)}</li>
 * </ul>
 * When one of above methods is called, the corresponding method of the adopted
 * BreakIterator will be invoked (i.e. no segment suppressions will be used).
 * Note: This fallback behavior undoes r36410 in which these methods were changed
 * to just return BreakIterator.DONE immediately without updating the internal state.
 *
 * @internal technology preview
 */
class U_COMMON_API FilteredBreakIteratorBuilder : public UObject {
 public:
  /**
   *  destructor.
   * @internal technology preview
   */
  virtual ~FilteredBreakIteratorBuilder();

  /**
   * Construct a FilteredBreakIteratorBuilder based on rules in a locale.
   * The rules are taken from CLDR exception data for the locale,
   *  see http://www.unicode.org/reports/tr35/tr35-general.html#Segmentation_Exceptions
   *  This is the equivalent of calling createInstance(UErrorCode&)
   *    and then repeatedly calling addNoBreakAfter(...) with the contents
   *    of the CLDR exception data.
   * @param where the locale.
   * @param status The error code.
   * @return the new builder
   * @internal technology preview
   */
  static FilteredBreakIteratorBuilder *createInstance(const Locale& where, UErrorCode& status);

  /**
   * Construct an empty FilteredBreakIteratorBuilder.
   * In this state, it will not suppress any segment boundaries.
   * @param status The error code.
   * @return the new builder
   * @internal technology preview
   */
  static FilteredBreakIteratorBuilder *createInstance(UErrorCode &status);

  /**
   * Suppress a certain string from being the end of a segment.
   * For example, suppressing "Mr.", then segments ending in "Mr." will not be returned
   * by the iterator.
   * @param string the string to suppress, such as "Mr."
   * @param status error code
   * @return returns TRUE if the string was not present and now added,
   * FALSE if the call was a no-op because the string was already being suppressed.
   * @internal technology preview
   */
  virtual UBool suppressBreakAfter(const UnicodeString& string, UErrorCode& status) = 0;

  /**
   * Stop suppressing a certain string from being the end of the segment.
   * This function does not create any new segment boundaries, but only serves to un-do
   * the effect of earlier calls to suppressBreakAfter, or to un-do the effect of
   * locale data which may be suppressing certain strings.
   * @param exception the exception to remove
   * @param status error code
   * @return returns TRUE if the string was present and now removed,
   * FALSE if the call was a no-op because the string was not being suppressed.
   * @internal technology preview
   */
  virtual UBool unsuppressBreakAfter(const UnicodeString& string, UErrorCode& status) = 0;

  /**
   * Wrap (adopt) an existing break iterator in a new filtered instance.
   * The resulting BreakIterator is owned by the caller.
   * The BreakIteratorFilter may be destroyed before the BreakIterator is destroyed.
   * Note that the adoptBreakIterator is adopted by the new BreakIterator
   * and should no longer be used by the caller.
   * The FilteredBreakIteratorBuilder may be reused.
   * @param adoptBreakIterator the break iterator to adopt
   * @param status error code
   * @return the new BreakIterator, owned by the caller.
   * @internal technology preview
   */
  virtual BreakIterator *build(BreakIterator* adoptBreakIterator, UErrorCode& status) = 0;

 protected:
  /**
   * For subclass use
   * @internal technology preview
   */
  FilteredBreakIteratorBuilder();
};


U_NAMESPACE_END

#endif  /* U_HIDE_INTERNAL_API */
#endif // #if !UCONFIG_NO_BREAK_ITERATION && !UCONFIG_NO_FILTERED_BREAK_ITERATION

#endif // #ifndef FILTEREDBRK_H
