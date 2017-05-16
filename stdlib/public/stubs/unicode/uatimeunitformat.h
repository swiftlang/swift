/*
*****************************************************************************************
* Copyright (C) 2014 Apple Inc. All Rights Reserved.
*****************************************************************************************
*/

#ifndef UATIMEUNITFORMAT_H
#define UATIMEUNITFORMAT_H

#include "unicode/utypes.h"

#if !UCONFIG_NO_FORMATTING
#ifndef U_HIDE_DRAFT_API

#include "unicode/localpointer.h"
#include "unicode/unum.h"

/**
 * \file
 * \brief C API: Support functions for formatting time units or portions thereof.
 *
 * These are somewhat temporary Apple-only functions to support NSDateComponentsFormatter.
 */

/**
 * Opaque UATimeUnitFormat object for use in C programs.
 * @draft ICU 53
 */
struct UATimeUnitFormat;
typedef struct UATimeUnitFormat UATimeUnitFormat;  /**< C typedef for struct UATimeUnitFormat. @draft ICU 53 */

/**
 * TimeUnit format styles
 * @draft ICU 53
 */
typedef enum UATimeUnitStyle {
    /**
     * full style, e.g. "1.0 minutes"
     * @draft ICU 53 */
    UATIMEUNITSTYLE_FULL,
    /**
     * abbreviated/short style, e.g. "1.0 min"
     * @draft ICU 53 */
    UATIMEUNITSTYLE_ABBREVIATED,
    /**
     * narrow style, e.g. "1.0 m"
     * @draft ICU 53 */
    UATIMEUNITSTYLE_NARROW,
    /** @draft ICU 53 */
    UATIMEUNITSTYLE_COUNT
} UATimeUnitStyle;

/**
 * TimeUnit fields
 * @draft ICU 53
 */
typedef enum UATimeUnitField {
    /** @draft ICU 53 */
    UATIMEUNITFIELD_YEAR,
    /** @draft ICU 53 */
    UATIMEUNITFIELD_MONTH,
    /** @draft ICU 53 */
    UATIMEUNITFIELD_DAY,
    /** @draft ICU 53 */
    UATIMEUNITFIELD_WEEK,
    /** @draft ICU 53 */
    UATIMEUNITFIELD_HOUR,
    /** @draft ICU 53 */
    UATIMEUNITFIELD_MINUTE,
    /** @draft ICU 53 */
    UATIMEUNITFIELD_SECOND,
    /** @draft ICU 53 */
    UATIMEUNITFIELD_COUNT
} UATimeUnitField;

/**
 * Open a new UATimeUnitFormat object for a given locale using the specified style,
 * using the default decimal formatter.
 * @param locale
 *            The locale
 * @param style
 *            The style (width) - full, abbreviated, etc.
 * @param status
 *            A pointer to a UErrorCode to receive any errors.
 * @return
 *            A pointer to a UATimeUnitFormat object for the specified locale,
 *            or NULL if an error occurred.
 * @draft ICU 53
 */
U_DRAFT UATimeUnitFormat* U_EXPORT2
uatmufmt_open(const char*  locale,
              UATimeUnitStyle style,
              UErrorCode*  status);

/**
 * Open a new UATimeUnitFormat object for a given locale using the specified style,
 * along with the number formatter to use for the numeric part of the time unit,
 * e.g. "1 min", "1.0 min", etc.
* @param locale
 *            The locale
 * @param style
 *            The style (width) - full, abbreviated, etc.
 * @param nfToAdopt
 *            A number formatter to set for this UATimeUnitFormat object (instead of
 *            the default decimal formatter). Ownership of this UNumberFormat object
 *            will pass to the UATimeUnitFormat object (the UATimeUnitFormat adopts the
 *            UNumberFormat), which becomes responsible for closing it. If the caller
 *            wishes to retain ownership of the UNumberFormat object, the caller must
 *            clone it (with unum_clone) and pass the clone to
 *            uatmufmt_openWithNumberFormat. May be NULL to use the default decimal
 *            formatter.
 * @param status
 *            A pointer to a UErrorCode to receive any errors.
 * @return
 *            A pointer to a UATimeUnitFormat object for the specified locale,
 *            or NULL if an error occurred.
 * @draft ICU 53
 */
U_DRAFT UATimeUnitFormat* U_EXPORT2
uatmufmt_openWithNumberFormat(const char*  locale,
                            UATimeUnitStyle style,
                            UNumberFormat*  nfToAdopt,
                            UErrorCode*  status);

/**
 * Close a UATimeUnitFormat object. Once closed it may no longer be used.
 * @param tufmt
 *            The UATimeUnitFormat object to close.
 * @draft ICU 53
 */
U_DRAFT void U_EXPORT2
uatmufmt_close(UATimeUnitFormat *tufmt);


#if U_SHOW_CPLUSPLUS_API

U_NAMESPACE_BEGIN

/**
 * \class LocalUDateIntervalFormatPointer
 * "Smart pointer" class, closes a UATimeUnitFormat via uatmufmt_close().
 * For most methods see the LocalPointerBase base class.
 *
 * @see LocalPointerBase
 * @see LocalPointer
 * @draft ICU 53
 */
U_DEFINE_LOCAL_OPEN_POINTER(LocalUATimeUnitFormatPointer, UATimeUnitFormat, uatmufmt_close);

U_NAMESPACE_END

#endif


/**
 * Set the number formatter to use for the numeric part of the time unit,
 * e.g. "1 min", "1.0 min", etc.
 * DO NOT USE - use uatmufmt_openWithNumberFormat instead, this will be
 * removed soon.
 * @param tufmt
 *            The UATimeUnitFormat object specifying the format conventions.
 * @param numfmt
 *            The number formatter to set for this UATimeUnitFormat object;
 *            uatmufmt_setNumberFormat clones this for its own use, so the
 *            caller retains ownership of the passed-in UNumberFormat object.
 * @param status
 *            A pointer to a UErrorCode to receive any errors.
 * @deprecated ICU 53, use uatmufmt_openWithNumberFormat
 */
U_DEPRECATED void U_EXPORT2
uatmufmt_setNumberFormat(UATimeUnitFormat* tufmt,
                        UNumberFormat*  numfmt,
                        UErrorCode*     status);

/**
 * Format a value like 1.0 and a field like UATIMEUNIT_MINUTE to e.g. "1.0 minutes".
 * @param tufmt
 *            The UATimeUnitFormat object specifying the format conventions.
 * @param value
 *            The numeric value to format
 * @param field
 *            The time field to format with the specified numeric value
 * @param result
 *            A pointer to a buffer to receive the formatted result.
 * @param resultCapacity
 *            The maximum size of result.
 * @param status
 *            A pointer to a UErrorCode to receive any errors. In case of error status,
 *            the contents of result are undefined.
 * @return
 *            The length of the formatted result; may be greater than resultCapacity,
 *            in which case an error is returned.
 * @draft ICU 53
 */
U_DRAFT int32_t U_EXPORT2
uatmufmt_format(const UATimeUnitFormat* tufmt,
                double          value,
                UATimeUnitField field,
                UChar*          result,
                int32_t         resultCapacity,
                UErrorCode*     status);


/**
 * Parse a single formatted time unit like "1.0 minutes" into a numeric value and unit type.
 * NOT CURRENTLY SUPPORTED, sets status to U_UNSUPPORTED_ERROR and returns 0.0. 
 * @param tufmt
 *            The UATimeUnitFormat object specifying the format conventions.
 * @param text
 *            The text to parse
 * @param textLength
 *            The length of text, or -1 if null-terminated
 * @param parsePos
 *            A pointer to an offset index into text at which to begin parsing. On output,
 *            *parsePos will point after the last parsed character. This parameter may be
 *            NULL, in which case parsing begins at offset 0.
 * @param field
 *            A pointer to a UATimeUnitField to be set to the parsed firled type.
 * @param status
 *            A pointer to a UErrorCode to receive any errors.
 * @return
 *            The parsed double value.
 * @draft ICU 53
 */
U_DRAFT double U_EXPORT2
uatmufmt_parse( const UATimeUnitFormat* tufmt,
                const UChar*    text,
                int32_t         textLength,
                int32_t*        parsePos,
                UATimeUnitField* field,
                UErrorCode*     status);


/**
 * TimeUnit time duration positional pattern types
 * @draft ICU 53
 */
typedef enum UATimeUnitTimePattern {
    /**
     * e.g. "h:mm"
     * @draft ICU 53 */
    UATIMEUNITTIMEPAT_HM,
    /**
     * e.g. "h:mm:ss"
     * @draft ICU 53 */
    UATIMEUNITTIMEPAT_HMS,
    /**
     * e.g. "m:ss"
     * @draft ICU 53 */
    UATIMEUNITTIMEPAT_MS,
    /** @draft ICU 53 */
    UATIMEUNITTIMEPAT_COUNT
} UATimeUnitTimePattern;

/**
 * Get a localized pattern for positional duration style, e.g. "h:mm:ss".
 * This comes from the durationUnits CLDR data in ICU, e.g.
 *  durationUnits{
 *      hm{"h:mm"}
 *      hms{"h:mm:ss"}
 *      ms{"m:ss"}
 *  }
 * For usage see CLDR documentation on durationUnit under
 * <a href="http://www.unicode.org/reports/tr35/tr35-general.html#Unit_Elements">Unit Elements</a>.
 *
 * @param locale
 *            The locale
 * @param type
 *            The type of time pattern to get (hm, hms, ms)
 * @param result
 *            A pointer to a buffer to receive the formatted result.
 * @param resultCapacity
 *            The maximum size of result.
 * @param status
 *            A pointer to a UErrorCode to receive any errors. In case of error status,
 *            the contents of result are undefined.
 * @return
 *            The length of the formatted result; may be greater than resultCapacity,
 *            in which case an error is returned.
 * @draft ICU 53
 */
U_DRAFT int32_t U_EXPORT2
uatmufmt_getTimePattern(const char*     locale,
                        UATimeUnitTimePattern type,
                        UChar*          result,
                        int32_t         resultCapacity,
                        UErrorCode*     status);


/**
 * TimeUnit list pattern types
 * @draft ICU 53
 */
typedef enum UATimeUnitListPattern {
    /**
     * for two only, e.g. "{0} and {1}"
     * @draft ICU 53 */
    UATIMEUNITLISTPAT_TWO_ONLY,
    /**
     * for end of list with 3 or more, e.g. "{0}, and {1}"
     * @draft ICU 53 */
    UATIMEUNITLISTPAT_END_PIECE,
    /**
     * for middle of list with 3 or more, e.g. "{0}, {1}"
     * @draft ICU 53 */
    UATIMEUNITLISTPAT_MIDDLE_PIECE,
    /**
     * for start of list with 3 or more, e.g. "{0}, {1}"
     * @draft ICU 53 */
    UATIMEUNITLISTPAT_START_PIECE,
    /** @draft ICU 53 */
    UATIMEUNITLISTPAT_COUNT
} UATimeUnitListPattern;

/**
 * Get a localized pattern for combining units in a list, e.g. "3 min, 42 sec".
 * This comes from the listPattern/unit* CLDR data in ICU, e.g.
 *  listPattern{
 *      unit{
 *          .. use short if not present
 *      }
 *      unit-short{
 *          2{"{0}, {1}"}
 *          end{"{0}, {1}"}
 *          middle{"{0}, {1}"}
 *          start{"{0}, {1}"}
 *      }
 *      unit-narrow{
 *          .. use short if not present
 *      }
 *  }
 * For usage see CLDR documentation on
 * <a href="http://www.unicode.org/reports/tr35/tr35-general.html#ListPatterns">List Patterns</a>.
 *
 * @param locale
 *            The locale
 * @param style
 *            The style (width) - full, abbreviated, etc.
 * @param type
 *            The type of list pattern to get (for two items, end part for >= 3 items, etc.)
 * @param result
 *            A pointer to a buffer to receive the formatted result.
 * @param resultCapacity
 *            The maximum size of result.
 * @param status
 *            A pointer to a UErrorCode to receive any errors. In case of error status,
 *            the contents of result are undefined.
 * @return
 *            The length of the formatted result; may be greater than resultCapacity,
 *            in which case an error is returned.
 * @draft ICU 53
 */
U_DRAFT int32_t U_EXPORT2
uatmufmt_getListPattern(const char*     locale,
                        UATimeUnitStyle style,
                        UATimeUnitListPattern type,
                        UChar*          result,
                        int32_t         resultCapacity,
                        UErrorCode*     status);


#endif /* U_HIDE_DRAFT_API */
#endif /* #if !UCONFIG_NO_FORMATTING */

#endif /* #ifndef UATIMEUNITFORMAT_H */
