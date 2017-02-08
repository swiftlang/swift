/*
******************************************************************************
* Copyright (C) 2010-2011 Apple Inc. All Rights Reserved.
******************************************************************************
*/

#ifndef UDATINTV_H
#define UDATINTV_H

#include "unicode/utypes.h"

#if !UCONFIG_NO_FORMATTING

#include "unicode/umisc.h"
#include "unicode/udateintervalformat.h"

/**
 * NOTE - THE TEMPORARY APPLE INTERFACES DECLARED HERE ARE OBSOLETE, PLEASE USE
 * THE REAL ICU EQUIVALENTS IN udateintervalformat.h
 *
 * A UDateIntervalFormat is used to format the range between two UDate values
 * in a locale-sensitive way, using a skeleton that specifies the precision and
 * completeness of the information to show. If the range smaller than the resolution
 * specified by the skeleton, a single date format will be produced. If the range
 * is larger than the format specified by the skeleton, a locale-specific fallback
 * will be used to format the items missing from the skeleton.
 *
 * For example, if the range is 2010-03-04 07:56 - 2010-03-04 19:56 (12 hours)
 * - The skeleton jm will produce
 *   for en_US, "7:56 AM – 7:56 PM"
 *   for en_GB, "7:56 – 19:56"
 * - The skeleton MMMd will produce
 *   for en_US, "Mar 4"
 *   for en_GB, "4 Mar"
 * If the range is 2010-03-04 07:56 - 2010-03-08 16:11 (4 days, 8 hours, 15 minutes)
 * - The skeleton jm will produce
 *   for en_US, "3/4/2010 7:56 AM – 3/8/2010 4:11 PM"
 *   for en_GB, "4/3/2010 7:56 – 8/3/2010 16:11"
 * - The skeleton MMMd will produce
 *   for en_US, "Mar 4–8"
 *   for en_GB, "4-8 Mar"
 * 
 * Note, in ICU 4.4 the standard skeletons for which date interval format data
 * is usually available are as follows; best results will be obtained by using
 * skeletons from this set, or those formed by combining these standard skeletons
 * (note that for these skeletons, the length of digit field such as d, y, or
 * M vs MM is irrelevant (but for non-digit fields such as MMM vs MMMM it is
 * relevant). Note that a skeleton involving h or H generally explicitly requests
 * that time style (12- or 24-hour time respectively). For a skeleton that
 * requests the locale's default time style (h or H), use 'j' instead of h or H.
 *   h, H, hm, Hm,
 *   hv, Hv, hmv, Hmv,
 *   d,
 *   M, MMM, MMMM,
 *   Md, MMMd,
 *   MEd, MMMEd,
 *   y,
 *   yM, yMMM, yMMMM,
 *   yMd, yMMMd,
 *   yMEd, yMMMEd
 *
 * Locales for which ICU 4.4 seems to have a reasonable amount of this data
 * include:
 *   af, am, ar, be, bg, bn, ca, cs, da, de (_AT), el, en (_AU,_CA,_GB,_IE,_IN...),
 *   eo, es (_AR,_CL,_CO,...,_US) et, fa, fi, fo, fr (_BE,_CH,_CA), fur, gsw, he,
 *   hr, hu, hy, is, it (_CH), ja, kk, km, ko, lt, lv, mk, ml, mt, nb, nl )_BE),
 *   nn, pl, pt (_PT), rm, ro, ru (_UA), sk, sl, so, sq, sr, sr_Latn, sv, th, to,
 *   tr, uk, ur, vi, zh (_SG), zh_Hant (_HK,_MO)
 */

/**
 * A UDateIntervalFormat object for use in C programs.
 * struct UDateIntervalFormat; defined in udateintervalformat.h
 */

/**
 * Open a new UDateIntervalFormat object using the predefined rules for a
 * given locale plus a specified skeleton.
 * @param locale
 *            The locale for whose rules should be used; may be NULL for
 *            default locale.
 * @param skeleton
 *            A pattern containing only the fields desired for the interval
 *            format, for example "Hm", "yMMMd", or "yMMMEdHm".
 * @param length
 *            The length of skeleton; may be -1 if the skeleton is zero-terminated.
 * @param status
 *            A pointer to a UErrorCode to receive any errors.
 * @return
 *            A UDateIntervalFormat object for the specified locale, or NULL
 *            if an error occurred.
 * @internal/obsolete, use udtitvfmt_open in udateintervalformat.h
 */
U_INTERNAL UDateIntervalFormat* U_EXPORT2
udatintv_open(const char*  locale,
              const UChar* skeleton,
              int32_t      skeletonLength,
              UErrorCode*  status);

/**
 * Close a UDateIntervalFormat object. Once closed it may no longer be used.
 * @param datintv
 *            The UDateIntervalFormat object to close.
 * @internal/obsolete, use udtitvfmt_close in udateintervalformat.h
 */
U_INTERNAL void U_EXPORT2
udatintv_close(UDateIntervalFormat *datintv);

/**
 * Formats a date/time range using the conventions established for the
 * UDateIntervalFormat object.
 * @param datintv
 *            The UDateIntervalFormat object specifying the format conventions.
 * @param fromDate
 *            The starting point of the range.
 * @param fromDate
 *            The ending point of the range.
 * @param result
 *            A pointer to a buffer to receive the formatted range.
 * @param resultCapacity
 *            The maximum size of result.
 * @param position
 *            A pointer to a UFieldPosition. On input, position->field is read.
 *            On output, position->beginIndex and position->endIndex indicate
 *            the beginning and ending indices of field number position->field,
 *            if such a field exists. This parameter may be NULL, in which case
 *            no field position data is returned.
 * @param status
 *            A pointer to a UErrorCode to receive any errors.
 * @return
 *            The total buffer size needed; if greater than resultLength, the
 *            output was truncated.
 * @internal/obsolete, use udtitvfmt_format in udateintervalformat.h
 */
U_INTERNAL int32_t U_EXPORT2
udatintv_format(const UDateIntervalFormat* datintv,
                UDate           fromDate,
                UDate           toDate,
                UChar*          result,
                int32_t         resultCapacity,
                UFieldPosition* position,
                UErrorCode*     status);

#endif /* #if !UCONFIG_NO_FORMATTING */

#endif
