/*
*****************************************************************************************
* Copyright (C) 2014-2015 Apple Inc. All Rights Reserved.
*****************************************************************************************
*/

#ifndef UALOC_H
#define UALOC_H

#include "unicode/utypes.h"

#ifndef U_HIDE_DRAFT_API

/**
 * Codes for language status in a country or region.
 * @draft ICU 54
 */
typedef enum {
    /**
     * The status is unknown, or the language has no special status.
     * @draft ICU 54 */
    UALANGSTATUS_UNSPECIFIED       = 0,
    /**
     * The language is official in a region of the specified country,
     * e.g. Hawaiian for U.S.
     * @draft ICU 54 */
    UALANGSTATUS_REGIONAL_OFFICIAL = 4,
    /**
     * The language is de-facto official for the specified country or region,
     * e.g. English for U.S.
     * @draft ICU 54 */
    UALANGSTATUS_DEFACTO_OFFICIAL  = 8,
    /**
     * The language is explicitly official for the specified country or region.
     * @draft ICU 54 */
    UALANGSTATUS_OFFICIAL          = 12
} UALanguageStatus;

/**
 * UALANGDATA_CODELEN is the maximum length of a language code
 * (language subtag, possible extension, possible script subtag)
 * in the UALanguageEntry struct.
 * @draft ICU 54
 */
#define UALANGDATA_CODELEN 23

/**
 * The UALanguageEntry structure provides information about
 * one of the languages for a specified region.
 * @draft ICU 54
 */
typedef struct {
    char languageCode[UALANGDATA_CODELEN + 1];  /**< language code, may include script or
                                                   other subtags after primary language.
                                                   This may be "und" (undetermined) for
                                                   some regions such as AQ Antarctica.
                                                   @draft ICU 54 */
    double userFraction;      /**< fraction of region's population (from 0.0 to 1.0) that
                                uses this language (not necessarily as a first language).
                                This may be 0.0 if the fraction is known to be very small.
                                @draft ICU 54 */
    UALanguageStatus status;  /**< status of the language, if any.
                                @draft ICU 54 */
} UALanguageEntry;

/**
 * Fills out a provided UALanguageEntry entry with information about the languages
 * used in a specified region.
 *
 * @param regionID
 *          The specified regionID (currently only ISO-3166-style IDs are supported).
 * @param minimumFraction
 *          The minimum fraction of language users for a language to be included
 *          in the UALanguageEntry array. Must be in the range 0.0 to 1.0; set to
 *          0.0 if no minimum threshold is desired. As an example, as of March 2014
 *          ICU lists 74 languages for India; setting minimumFraction to 0.001 (0.1%)
 *          skips 27, keeping the top 47 languages for inclusion in the entries array
 *          (depending on entriesCapacity); setting minimumFraction to 0.01 (1%)
 *          skips 54, keeping the top 20 for inclusion.
 * @param entries
 *          Caller-provided array of UALanguageEntry elements. This will be filled
 *          out with information for languages of the specified region that meet
 *          the minimumFraction threshold, sorted in descending order by
 *          userFraction, up to the number of elements specified by entriesCapacity
 *          (so the number of languages for which data is provided can be limited by
 *          total count, by userFraction, or both).
 *          Preflight option: You may set this to NULL (and entriesCapacity to 0)
 *          to just obtain a count of all of the languages that meet the specified
 *          minimumFraction, without actually getting data for any of them.
 * @param entriesCapacity
 *          The number of elements in the provided entries array. Must be 0 if
 *          entries is NULL (preflight option).
 * @param status
 *          A pointer to a UErrorCode to receive any errors, for example
 *          U_MISSING_RESOURCE_ERROR if there is no data available for the
 *          specified region.
 * @return
 *          The number of elements in entries filled out with data, or if
 *          entries is NULL and entriesCapacity is 0 (preflight option ), the total
 *          number of languages for the specified region that meet the minimumFraction
 *          threshold.
 * @draft ICU 54
 */
U_DRAFT int32_t U_EXPORT2
ualoc_getLanguagesForRegion(const char *regionID, double minimumFraction,
                            UALanguageEntry *entries, int32_t entriesCapacity,
                            UErrorCode *err);


/**
 * Gets the desired lproj parent locale ID for the specified locale,
 * using ICU inheritance chain plus Apple additions (for zh*). For
 * example, provided any ID in the following chains it would return
 * the next one to the right:
 *
 *                                 en_US → en → root;
 *                en_HK → en_GB → en_001 → en → root;
 *                en_IN → en_GB → en_001 → en → root;
 *                                 es_ES → es → root;
 *                        es_MX → es_419 → es → root;
 *                                        haw → root;
 *                                 pt_BR → pt → root;
 *                                 pt_PT → pt → root;
 *                               sr_Cyrl → sr → root;
 *                                    sr_Latn → root;
 *                       zh_Hans → zh → zh_CN → root;
 *  zh_Hant_MO → zh_Hant_HK → zh_Hant → zh_TW → root;
 *       zh_HK → zh_Hant_HK → zh_Hant → zh_TW → root;
 *                                       root → root;
 *                                        tlh → root;
 *
 * @param localeID  The locale whose parent to get. This can use either '-' or '_' for separator.
 * @param parent    Buffer into which the parent localeID will be copied.
 *                  This will always use '_' for separator.
 * @param parentCapacity  The size of the buffer for parent localeID (including room for null terminator).
 * @param err       Pointer to UErrorCode. If on input it indicates failure, function will return 0.
 *                  If on output it indicates an error the contents of parent buffer are undefined.
 * @return          The actual buffer length of the parent localeID. If it is greater than parentCapacity,
 *                  an overflow error will be set and the contents of parent buffer are undefined.
 * @draft ICU 53
 */
U_DRAFT int32_t U_EXPORT2
ualoc_getAppleParent(const char* localeID,
                     char * parent,
                     int32_t parentCapacity,
                     UErrorCode* err);

/**
 * ualoc_localizationsToUse - map preferred languages to
 * available localizations.
 * =========================
 *  BEHAVIOR EXAMPLES
 *  Each block gives up to 6 sets of available lprojs, and then shows how various
 *  preferred language requests would be mapped into one of the lprojs in each set.
 *  The en entriy marked * is currently not working as intended (get just "en"
 *  instead of the indicated values)
 *
 *  --------
 *  lproj sets →    list1       list2       list3       list4       list5        list6
 *                  zh_CN       zh_CN       zh_CN       zh_Hans     zh_CN        zh_CN
 *                  zh_TW       zh_TW       zh_TW       zh_Hant     zh_TW        zh_TW
 *                              zh_HK       zh_HK       zh_Hant_HK  zh_Hans      zh_HK
 *                                          zh_MO                   zh_Hant      zh_Hans
 *                                                                               zh_Hant
 *                                                                               zh_Hant_HK
 *  language ↓
 *  zh              zh_CN       zh_CN       zh_CN       zh_Hans     zh_Hans      zh_Hans
 *  zh-Hans         zh_CN       zh_CN       zh_CN       zh_Hans     zh_Hans      zh_Hans
 *  zh-Hant         zh_TW       zh_TW       zh_TW       zh_Hant     zh_Hant      zh_Hant
 *  zh-Hans-CN      zh_CN       zh_CN       zh_CN       zh_Hans     zh_CN,       zh_CN,
 *                                                                  zh_Hans      zh_Hans
 *  zh-Hans-SG      zh_CN       zh_CN       zh_CN       zh_Hans     zh_Hans      zh_Hans
 *  zh-Hant-TW      zh_TW       zh_TW       zh_TW       zh_Hant     zh_TW,       zh_TW,
 *                                                                  zh_Hant      zh_Hant
 *  zh-Hant-HK      zh_TW       zh_HK,      zh_HK,      zh_Hant_HK, zh_Hant      zh_Hant_HK,
 *                              zh_TW       zh_TW       zh_Hant                  zh_Hant
 *  zh-Hant-MO      zh_TW       zh_HK,      zh_MO,      zh_Hant_HK, zh_Hant      zh_Hant_HK,
 *                              zh_TW       zh_HK,zh_TW zh_Hant                  zh_Hant
 *  zh-Hans-HK      zh_CN       zh_CN       zh_CN       zh_Hans     zh_Hans      zh_Hans
 *  zh-CN           zh_CN       zh_CN       zh_CN       zh_Hans     zh_CN,       zh_CN,
 *                                                                  zh_Hans      zh_Hans
 *  zh-SG           zh_CN       zh_CN       zh_CN       zh_Hans     zh_Hans      zh_Hans
 *  zh-TW           zh_TW       zh_TW       zh_TW       zh_Hant     zh_TW,       zh_TW,
 *                                                                  zh_Hant      zh_Hant
 *  zh-HK           zh_TW       zh_HK       zh_HK,      zh_Hant_HK, zh_Hant      zh_HK,
 *                                          zh_TW       zh_Hant                  zh_Hant_HK,zh_Hant
 *  zh-MO           zh_TW       zh_HK       zh_MO,      zh_Hant_HK, zh_Hant      zh_Hant_HK,
 *                                          zh_HK,zh_TW zh_Hant                  zh_Hant_HK,zh_Hant
 *  --------
 *  lproj sets →    list1       list2       list3       list4       list5        list6
 *                  English     en          en          en          en           en
 *                              en_AU       en_AU       en_AU       en_AU        en_AU
 *                              en_GB       en_GB       en_GB       en_GB        en_GB
 *                                          en_CA       en_CA       en_001       en_001
 *                                                      en_IN                    en_150
 *                                                      en_US
 *  language ↓
 *  en              English     en          en          en          en           en
 *  en-US           English     en          en          en_US,      en           en
 *                                                      en
 *  en-AU           English     en_AU,      en_AU,      en_AU,      en_AU,en_GB, en_AU,en_GB,
 *                              en_GB,en    en_GB,en    en_GB,en    en_001,en    en_001,en
 *  en-CA           English     en          en_CA,      en_CA,      en_001,      en_001,
 *                                          en          en          en           en
 *  en-GB           English     en_GB,      en_GB,      en_GB,      en_GB,       en_GB,
 *                              en          en          en          en_001,en    en_001,en
 *  en-IN           English     en_GB,      en_GB,      en_IN,      en_GB,       en_GB,
 *                              en          en          en_GB,en    en_001,en    en_001,en
 *  en-US           English     en          en          en_US,      en           en
 *                                                      en
 *  en-FR           English     en_GB,      en_GB,      en_GB,      en_GB,       en_150,en_GB,
 *                              en          en          en          en_001,en    en_001,en
 *  en-IL           English     en          en          en          en_001,      en_001,
 *                                                                  en           en
 *  en-001          English     en          en          en          en_001,      en_001,
 *                                                                  en           en
 *  en-150          English     en_GB,      en_GB,      en_GB,      en_GB,       en_150,en_GB,
 *                              en          en          en          en_001,en    en_001,en
 *  en-Latn         English     en          en          en          en           en
 *  en-Latn_US      English     en          en          en_US,*     en           en
 *                                                      en
 *  --------
 *  lproj sets →    list1       list2       list3       list4       list5        list6
 *                  Spanish     es          es          es          es           es
 *                              es_MX       es_419      es_419      es_ES        es_ES
 *                                                      es_MX       es_MX        es_419
 *                                                                               es_MX
 *  language ↓
 *  es              Spanish     es          es          es          es           es
 *  es-ES           Spanish     es          es          es          es_ES,       es_ES,
 *                                                                  es           es
 *  es-419          Spanish     es          es_419,     es_419,     es           es_419,
 *                                          es          es                       es
 *  es-MX           Spanish     es_MX,      es_419,     es_MX,      es_MX,       es_MX,
 *                              es          es          es_419,es   es           es_419,es
 *  es-AR           Spanish     es          es_419,     es_419,     es           es_419,
 *                                          es          es                       es
 *  --------
 *  lproj sets →    list1       list2       list3       list4       list5        list6
 *                  Portuguese  pt          pt          pt
 *                              pt_PT       pt_BR       pt_BR
 *                                          pt_PT
 *  language ↓
 *  pt              Portuguese  pt          pt          pt
 *  pt-BR           Portuguese  pt          pt_BR,      pt_BR,
 *                                          pt          pt
 *  pt-PT           Portuguese  pt_PT,      pt_PT,      pt
 *                              pt          pt
 *  pt-MO           Portuguese  pt_PT,      pt_PT,      pt
 *                              pt          pt
 * =========================
 *
 * @param preferredLanguages
 *          Ordered array of pointers to user's preferred language
 *          codes (BCP47 style null-terminated ASCII strings), in
 *          order from most preferred; intended to accept the
 *          contents of AppleLanguages. Must not be NULL.
 *          Entries with the following values will be ignored:
 *          NULL, "", "root", any entry beginning with '-' or '_'.
 * @param preferredLanguagesCount
 *          Count of entries in preferredLanguages.
 * @param availableLocalizations
 *          Unordered array of pointers to identifiers for available
 *          localizations (lprojs); handles old Apple-style
 *          identifiers such as "English", as well as currently-
 *          superseded identifiers such as "no", "tl". Must not
 *          be NULL.
 * @param availableLocalizationsCount
 *          Count of entries in availableLocalizations.
 * @param localizationsToUse
 *          Caller-provided array to be filled with pointers to
 *          localizations to use in the availableLocalizations
 *          array; these are entries from availableLocalizations
 *          that correspond to the first language in
 *          preferredLanguages for which there is any entry in
 *          availableLocalizations that is a reasonable match,
 *          with the best-matching localization first in
 *          localizationsToUse, followed by any other possible
 *          fallback localizations (for example, "en_IN" in
 *          preferredLanguages might produce { "en_GB, "en" } in
 *          localizationsToUse). Must not be NULL. This need not
 *          large enough to accomodate all of the localizations
 *          that might be returned; it is perfectly reasonable
 *          (and more efficient) to only provide an array with
 *          one entry, if that is all that you are planning to use.
 * @param localizationsToUseCapacity
 *          The capacity of localizationsToUse.
 * @param status
 *          A pointer to a UErrorCode to receive any errors.
 * @return
 *          The number of entries filled out in localizationsToUse.
 * @draft ICU 55
  */
U_DRAFT int32_t U_EXPORT2
ualoc_localizationsToUse( const char* const *preferredLanguages,
                          int32_t preferredLanguagesCount,
                          const char* const *availableLocalizations,
                          int32_t availableLocalizationsCount,
                          const char* *localizationsToUse,
                          int32_t localizationsToUseCapacity,
                          UErrorCode *status );

#endif /* U_HIDE_DRAFT_API */
#endif /*UALOC_H*/
