# Copyright 2021 Saleem Abdulrasool.  All Rights Reserved.
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.12.3)

project(ICU LANGUAGES C CXX VERSION 69.1)

option(BUILD_SHARED_LIBS "build shared libaries" YES)
option(BUILD_TOOLS "build tools" NO)
option(ICU_TOOLS_DIR "Path to prebuilt tools" "")

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_EXTENSIONS OFF)

include(GNUInstallDirs)

find_package(Python3 COMPONENTS Interpreter REQUIRED)

if(NOT BUILD_SHARED_LIBS)
  add_compile_definitions(U_STATIC_IMPLEMENTATION)
  if(CMAKE_SYSTEM_NAME STREQUAL Windows)
    set(CMAKE_STATIC_LIBRARY_PREFIX s)
  endif()
endif()

if(CMAKE_SYSTEM_NAME STREQUAL Windows)
  # NOTE(compnerd) we alter the user-controlled flags here as we cannot override
  # this as a setting otherwise.
  string(REPLACE "/EHsc" "/EHs-c-" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})

  add_compile_definitions(_CRT_SECURE_NO_DEPRECATE)
  add_compile_definitions(_HAS_EXCEPTIONS=0)
  add_compile_definitions(U_PLATFORM_USES_ONLY_WIN32_API=1)

  add_compile_options(/Zc:wchar_t)
  add_compile_options(/utf-8)

  add_compile_options(/GF)
  add_compile_options(/Gy)
elseif(CMAKE_SYSTEM_NAME STREQUAL Linux OR CMAKE_SYSTEM_NAME STREQUAL Android)
  add_compile_options(-Wno-invalid-constexpr)
  add_compile_options(-ffunction-sections -fdata-sections)

  include(CheckIncludeFiles)
  check_include_files(elf.h HAVE_ELF_H)
  if(HAVE_ELF_H)
    add_compile_definitions(U_HAVE_ELF_H=1)
  endif()
endif()

set(THREADS_PREFER_PTHREAD_FLAG YES)
find_package(Threads)

add_compile_definitions(U_ATTRIBUTE_DEPRECATED=)
add_compile_definitions(U_DISABLE_RENAMING=0)
# add_compile_definitions(U_HIDE_DRAFT_API=1)
# add_compile_definitions(U_HIDE_DEPRECATED_API=1)
add_compile_definitions(U_ENABLE_DYLOAD=0)

include_directories(source/common)

# icu stub data
add_library(icudt
  source/stubdata/stubdata.cpp)
if(CMAKE_SYSTEM_NAME STREQUAL Windows)
  target_sources(icudt PRIVATE
    source/data/misc/icudata.rc)
endif()
target_compile_definitions(icudt PRIVATE
  STUBDATA_BUILD)
set_target_properties(icudt PROPERTIES
  OUTPUT_NAME icudt${PROJECT_VERSION_MAJOR})

# icu common (unicode)
add_library(icuuc
  source/common/filteredbrk.cpp
  source/common/ubidi.cpp
  source/common/ubiditransform.cpp
  source/common/ubidi_props.cpp
  source/common/ubidiln.cpp
  source/common/ubidiwrt.cpp
  source/common/uloc_keytype.cpp
  source/common/ushape.cpp
  source/common/brkeng.cpp
  source/common/brkiter.cpp
  source/common/dictbe.cpp
  source/common/pluralmap.cpp
  source/common/rbbi.cpp
  source/common/rbbidata.cpp
  source/common/rbbinode.cpp
  source/common/rbbirb.cpp
  source/common/rbbiscan.cpp
  source/common/rbbisetb.cpp
  source/common/rbbistbl.cpp
  source/common/rbbitblb.cpp
  source/common/rbbi_cache.cpp
  source/common/dictionarydata.cpp
  source/common/ubrk.cpp
  source/common/ucol_swp.cpp
  source/common/propsvec.cpp
  source/common/uarrsort.cpp
  source/common/uenum.cpp
  source/common/uhash.cpp
  source/common/uhash_us.cpp
  source/common/ulist.cpp
  source/common/ustack.cpp
  source/common/ustrenum.cpp
  source/common/utrie.cpp
  source/common/utrie_swap.cpp
  source/common/utrie2.cpp
  source/common/utrie2_builder.cpp
  source/common/uvector.cpp
  source/common/uvectr32.cpp
  source/common/uvectr64.cpp
  source/common/errorcode.cpp
  source/common/icudataver.cpp
  source/common/locmap.cpp
  source/common/putil.cpp
  source/common/umath.cpp
  source/common/umutex.cpp
  source/common/utrace.cpp
  source/common/utypes.cpp
  source/common/wintz.cpp
  source/common/ucnv.cpp
  source/common/ucnv2022.cpp
  source/common/ucnv_bld.cpp
  source/common/ucnv_cb.cpp
  source/common/ucnv_cnv.cpp
  source/common/ucnv_ct.cpp
  source/common/ucnv_err.cpp
  source/common/ucnv_ext.cpp
  source/common/ucnv_io.cpp
  source/common/ucnv_lmb.cpp
  source/common/ucnv_set.cpp
  source/common/ucnv_u16.cpp
  source/common/ucnv_u32.cpp
  source/common/ucnv_u7.cpp
  source/common/ucnv_u8.cpp
  source/common/ucnvbocu.cpp
  source/common/ucnvdisp.cpp
  source/common/ucnvhz.cpp
  source/common/ucnvisci.cpp
  source/common/ucnvlat1.cpp
  source/common/ucnvmbcs.cpp
  source/common/ucnvscsu.cpp
  source/common/ucnvsel.cpp
  source/common/cmemory.cpp
  source/common/ucln_cmn.cpp
  source/common/ucmndata.cpp
  source/common/udata.cpp
  source/common/udatamem.cpp
  source/common/udataswp.cpp
  source/common/uinit.cpp
  source/common/umapfile.cpp
  source/common/uobject.cpp
  source/common/dtintrv.cpp
  source/common/parsepos.cpp
  source/common/ustrfmt.cpp
  source/common/util.cpp
  source/common/util_props.cpp
  source/common/punycode.cpp
  source/common/uidna.cpp
  source/common/uts46.cpp
  source/common/localebuilder.cpp
  source/common/localematcher.cpp
  source/common/localeprioritylist.cpp
  source/common/locavailable.cpp
  source/common/locbased.cpp
  source/common/locdispnames.cpp
  source/common/locdistance.cpp
  source/common/locdspnm.cpp
  source/common/locid.cpp
  source/common/loclikely.cpp
  source/common/loclikelysubtags.cpp
  source/common/locresdata.cpp
  source/common/locutil.cpp
  source/common/lsr.cpp
  source/common/resbund.cpp
  source/common/resbund_cnv.cpp
  source/common/ucat.cpp
  source/common/uloc.cpp
  source/common/uloc_tag.cpp
  source/common/ures_cnv.cpp
  source/common/uresbund.cpp
  source/common/uresdata.cpp
  source/common/resource.cpp
  source/common/ucurr.cpp
  source/common/caniter.cpp
  source/common/filterednormalizer2.cpp
  source/common/loadednormalizer2impl.cpp
  source/common/normalizer2.cpp
  source/common/normalizer2impl.cpp
  source/common/normlzr.cpp
  source/common/unorm.cpp
  source/common/unormcmp.cpp
  source/common/bmpset.cpp
  source/common/patternprops.cpp
  source/common/propname.cpp
  source/common/ruleiter.cpp
  source/common/ucase.cpp
  source/common/uchar.cpp
  source/common/characterproperties.cpp
  source/common/unames.cpp
  source/common/unifiedcache.cpp
  source/common/unifilt.cpp
  source/common/unifunct.cpp
  source/common/uniset.cpp
  source/common/uniset_closure.cpp
  source/common/uniset_props.cpp
  source/common/unisetspan.cpp
  source/common/uprops.cpp
  source/common/usc_impl.cpp
  source/common/uscript.cpp
  source/common/uscript_props.cpp
  source/common/uset.cpp
  source/common/uset_props.cpp
  source/common/usetiter.cpp
  source/common/icuplug.cpp
  source/common/serv.cpp
  source/common/servlk.cpp
  source/common/servlkf.cpp
  source/common/servls.cpp
  source/common/servnotf.cpp
  source/common/servrbf.cpp
  source/common/servslkf.cpp
  source/common/usprep.cpp
  source/common/appendable.cpp
  source/common/bytesinkutil.cpp
  source/common/bytestream.cpp
  source/common/bytestrie.cpp
  source/common/bytestriebuilder.cpp
  source/common/bytestrieiterator.cpp
  source/common/chariter.cpp
  source/common/charstr.cpp
  source/common/cstring.cpp
  source/common/cstr.cpp
  source/common/cwchar.cpp
  source/common/edits.cpp
  source/common/messagepattern.cpp
  source/common/schriter.cpp
  source/common/stringpiece.cpp
  source/common/stringtriebuilder.cpp
  source/common/simpleformatter.cpp
  source/common/ucasemap.cpp
  source/common/ucasemap_titlecase_brkiter.cpp
  source/common/ucharstrie.cpp
  source/common/ucharstriebuilder.cpp
  source/common/ucharstrieiterator.cpp
  source/common/uchriter.cpp
  source/common/ucptrie.cpp
  source/common/uinvchar.cpp
  source/common/uiter.cpp
  source/common/umutablecptrie.cpp
  source/common/unistr.cpp
  source/common/unistr_case.cpp
  source/common/unistr_case_locale.cpp
  source/common/unistr_cnv.cpp
  source/common/unistr_props.cpp
  source/common/unistr_titlecase_brkiter.cpp
  source/common/ustr_cnv.cpp
  source/common/ustr_titlecase_brkiter.cpp
  source/common/ustr_wcs.cpp
  source/common/ustrcase.cpp
  source/common/ustrcase_locale.cpp
  source/common/ustring.cpp
  source/common/ustrtrns.cpp
  source/common/utext.cpp
  source/common/utf_impl.cpp
  source/common/static_unicode_sets.cpp
  source/common/restrace.cpp
  source/common/sharedobject.cpp)
if(CMAKE_SYSTEM_NAME STREQUAL Windows)
  target_sources(icuuc PRIVATE
    source/common/common.rc)
endif()
target_compile_definitions(icuuc PRIVATE
  U_COMMON_IMPLEMENTATION)
target_include_directories(icuuc PUBLIC
  source/common)
target_link_libraries(icuuc PRIVATE
  icudt
  Threads::Threads)
set_target_properties(icuuc PROPERTIES
  OUTPUT_NAME icuuc${PROJECT_VERSION_MAJOR})

# icu internationalization (i18n)
add_library(icuin
  source/i18n/erarules.cpp
  source/i18n/region.cpp
  source/i18n/uregion.cpp
  source/i18n/alphaindex.cpp
  source/i18n/bocsu.cpp
  source/i18n/coleitr.cpp
  source/i18n/coll.cpp
  source/i18n/collation.cpp
  source/i18n/collationbuilder.cpp
  source/i18n/collationcompare.cpp
  source/i18n/collationdata.cpp
  source/i18n/collationdatabuilder.cpp
  source/i18n/collationdatareader.cpp
  source/i18n/collationdatawriter.cpp
  source/i18n/collationfastlatin.cpp
  source/i18n/collationfastlatinbuilder.cpp
  source/i18n/collationfcd.cpp
  source/i18n/collationiterator.cpp
  source/i18n/collationkeys.cpp
  source/i18n/collationroot.cpp
  source/i18n/collationrootelements.cpp
  source/i18n/collationruleparser.cpp
  source/i18n/collationsets.cpp
  source/i18n/collationsettings.cpp
  source/i18n/collationtailoring.cpp
  source/i18n/collationweights.cpp
  source/i18n/rulebasedcollator.cpp
  source/i18n/search.cpp
  source/i18n/sortkey.cpp
  source/i18n/stsearch.cpp
  source/i18n/tzfmt.cpp
  source/i18n/tzgnames.cpp
  source/i18n/tznames.cpp
  source/i18n/tznames_impl.cpp
  source/i18n/ucol.cpp
  source/i18n/ucol_res.cpp
  source/i18n/ucol_sit.cpp
  source/i18n/ucoleitr.cpp
  source/i18n/uitercollationiterator.cpp
  source/i18n/usearch.cpp
  source/i18n/astro.cpp
  source/i18n/basictz.cpp
  source/i18n/buddhcal.cpp
  source/i18n/calendar.cpp
  source/i18n/cecal.cpp
  source/i18n/chnsecal.cpp
  source/i18n/choicfmt.cpp
  source/i18n/compactdecimalformat.cpp
  source/i18n/coptccal.cpp
  source/i18n/curramt.cpp
  source/i18n/currfmt.cpp
  source/i18n/currpinf.cpp
  source/i18n/currunit.cpp
  source/i18n/dangical.cpp
  source/i18n/datefmt.cpp
  source/i18n/dayperiodrules.cpp
  source/i18n/dcfmtsym.cpp
  source/i18n/decContext.cpp
  source/i18n/decimfmt.cpp
  source/i18n/decNumber.cpp
  source/i18n/double-conversion-bignum-dtoa.cpp
  source/i18n/double-conversion-bignum.cpp
  source/i18n/double-conversion-cached-powers.cpp
  source/i18n/double-conversion-double-to-string.cpp
  source/i18n/double-conversion-fast-dtoa.cpp
  source/i18n/double-conversion-string-to-double.cpp
  source/i18n/double-conversion-strtod.cpp
  source/i18n/dtfmtsym.cpp
  source/i18n/dtitvfmt.cpp
  source/i18n/dtitvinf.cpp
  source/i18n/dtptngen.cpp
  source/i18n/dtrule.cpp
  source/i18n/ethpccal.cpp
  source/i18n/fmtable.cpp
  source/i18n/fmtable_cnv.cpp
  source/i18n/format.cpp
  source/i18n/formattedval_iterimpl.cpp
  source/i18n/formattedval_sbimpl.cpp
  source/i18n/formattedvalue.cpp
  source/i18n/fphdlimp.cpp
  source/i18n/fpositer.cpp
  source/i18n/gender.cpp
  source/i18n/gregocal.cpp
  source/i18n/gregoimp.cpp
  source/i18n/hebrwcal.cpp
  source/i18n/indiancal.cpp
  source/i18n/islamcal.cpp
  source/i18n/japancal.cpp
  source/i18n/listformatter.cpp
  source/i18n/ulistformatter.cpp
  source/i18n/measfmt.cpp
  source/i18n/measunit.cpp
  source/i18n/measunit_extra.cpp
  source/i18n/measure.cpp
  source/i18n/msgfmt.cpp
  source/i18n/nfrs.cpp
  source/i18n/nfrule.cpp
  source/i18n/nfsubs.cpp
  source/i18n/number_affixutils.cpp
  source/i18n/number_asformat.cpp
  source/i18n/number_compact.cpp
  source/i18n/number_decimalquantity.cpp
  source/i18n/number_decimfmtprops.cpp
  source/i18n/number_fluent.cpp
  source/i18n/number_formatimpl.cpp
  source/i18n/number_grouping.cpp
  source/i18n/number_integerwidth.cpp
  source/i18n/number_longnames.cpp
  source/i18n/number_modifiers.cpp
  source/i18n/number_notation.cpp
  source/i18n/number_output.cpp
  source/i18n/number_padding.cpp
  source/i18n/number_patternmodifier.cpp
  source/i18n/number_patternstring.cpp
  source/i18n/number_rounding.cpp
  source/i18n/number_scientific.cpp
  source/i18n/formatted_string_builder.cpp
  source/i18n/number_usageprefs.cpp
  source/i18n/number_utils.cpp
  source/i18n/number_mapper.cpp
  source/i18n/number_multiplier.cpp
  source/i18n/number_currencysymbols.cpp
  source/i18n/number_skeletons.cpp
  source/i18n/number_symbolswrapper.cpp
  source/i18n/number_capi.cpp
  source/i18n/string_segment.cpp
  source/i18n/numparse_parsednumber.cpp
  source/i18n/numparse_impl.cpp
  source/i18n/numparse_symbols.cpp
  source/i18n/numparse_decimal.cpp
  source/i18n/numparse_scientific.cpp
  source/i18n/numparse_currency.cpp
  source/i18n/numparse_affixes.cpp
  source/i18n/numparse_compositions.cpp
  source/i18n/numparse_validators.cpp
  source/i18n/numrange_capi.cpp
  source/i18n/numrange_fluent.cpp
  source/i18n/numrange_impl.cpp
  source/i18n/numfmt.cpp
  source/i18n/numsys.cpp
  source/i18n/olsontz.cpp
  source/i18n/persncal.cpp
  source/i18n/pluralranges.cpp
  source/i18n/plurfmt.cpp
  source/i18n/plurrule.cpp
  source/i18n/quantityformatter.cpp
  source/i18n/rbnf.cpp
  source/i18n/rbtz.cpp
  source/i18n/reldatefmt.cpp
  source/i18n/reldtfmt.cpp
  source/i18n/scientificnumberformatter.cpp
  source/i18n/sharedbreakiterator.cpp
  source/i18n/selfmt.cpp
  source/i18n/simpletz.cpp
  source/i18n/scriptset.cpp
  source/i18n/smpdtfmt.cpp
  source/i18n/smpdtfst.cpp
  source/i18n/standardplural.cpp
  source/i18n/taiwncal.cpp
  source/i18n/timezone.cpp
  source/i18n/tmunit.cpp
  source/i18n/tmutamt.cpp
  source/i18n/tmutfmt.cpp
  source/i18n/tzrule.cpp
  source/i18n/tztrans.cpp
  source/i18n/ucal.cpp
  source/i18n/udat.cpp
  source/i18n/udateintervalformat.cpp
  source/i18n/udatpg.cpp
  source/i18n/ufieldpositer.cpp
  source/i18n/ulocdata.cpp
  source/i18n/umsg.cpp
  source/i18n/units_complexconverter.cpp
  source/i18n/units_converter.cpp
  source/i18n/units_data.cpp
  source/i18n/units_router.cpp
  source/i18n/unum.cpp
  source/i18n/unumsys.cpp
  source/i18n/upluralrules.cpp
  source/i18n/utf16collationiterator.cpp
  source/i18n/utf8collationiterator.cpp
  source/i18n/utmscale.cpp
  source/i18n/vtzone.cpp
  source/i18n/vzone.cpp
  source/i18n/windtfmt.cpp
  source/i18n/winnmfmt.cpp
  source/i18n/wintzimpl.cpp
  source/i18n/zonemeta.cpp
  source/i18n/zrule.cpp
  source/i18n/ztrans.cpp
  source/i18n/ucln_in.cpp
  source/i18n/regexcmp.cpp
  source/i18n/regeximp.cpp
  source/i18n/regexst.cpp
  source/i18n/regextxt.cpp
  source/i18n/rematch.cpp
  source/i18n/repattrn.cpp
  source/i18n/uregex.cpp
  source/i18n/uregexc.cpp
  source/i18n/anytrans.cpp
  source/i18n/brktrans.cpp
  source/i18n/casetrn.cpp
  source/i18n/cpdtrans.cpp
  source/i18n/esctrn.cpp
  source/i18n/funcrepl.cpp
  source/i18n/name2uni.cpp
  source/i18n/nortrans.cpp
  source/i18n/nultrans.cpp
  source/i18n/quant.cpp
  source/i18n/rbt.cpp
  source/i18n/rbt_data.cpp
  source/i18n/rbt_pars.cpp
  source/i18n/rbt_rule.cpp
  source/i18n/rbt_set.cpp
  source/i18n/remtrans.cpp
  source/i18n/strmatch.cpp
  source/i18n/strrepl.cpp
  source/i18n/titletrn.cpp
  source/i18n/tolowtrn.cpp
  source/i18n/toupptrn.cpp
  source/i18n/translit.cpp
  source/i18n/transreg.cpp
  source/i18n/tridpars.cpp
  source/i18n/unesctrn.cpp
  source/i18n/uni2name.cpp
  source/i18n/utrans.cpp
  source/i18n/csdetect.cpp
  source/i18n/csmatch.cpp
  source/i18n/csr2022.cpp
  source/i18n/csrecog.cpp
  source/i18n/csrmbcs.cpp
  source/i18n/csrsbcs.cpp
  source/i18n/csrucode.cpp
  source/i18n/csrutf8.cpp
  source/i18n/inputext.cpp
  source/i18n/ucsdet.cpp
  source/i18n/uspoof.cpp
  source/i18n/uspoof_build.cpp
  source/i18n/uspoof_conf.cpp
  source/i18n/uspoof_impl.cpp)
if(CMAKE_SYSTEM_NAME STREQUAL Windows)
  target_sources(icuin PRIVATE
    source/i18n/i18n.rc)
endif()
target_include_directories(icuin PUBLIC
  source/i18n)
target_compile_definitions(icuin PRIVATE
  U_I18N_IMPLEMENTATION)
target_link_libraries(icuin PRIVATE
  icuuc)
set_target_properties(icuin PROPERTIES
  OUTPUT_NAME icuin${PROJECT_VERSION_MAJOR})

if(BUILD_TOOLS)
  add_library(icutu
    source/tools/toolutil/collationinfo.cpp
    source/tools/toolutil/dbgutil.cpp
    source/tools/toolutil/denseranges.cpp
    source/tools/toolutil/filestrm.cpp
    source/tools/toolutil/filetools.cpp
    source/tools/toolutil/flagparser.cpp
    source/tools/toolutil/package.cpp
    source/tools/toolutil/pkg_genc.cpp
    source/tools/toolutil/pkg_gencmn.cpp
    source/tools/toolutil/pkg_icu.cpp
    source/tools/toolutil/pkgitems.cpp
    source/tools/toolutil/ppucd.cpp
    source/tools/toolutil/swapimpl.cpp
    source/tools/toolutil/toolutil.cpp
    source/tools/toolutil/ucbuf.cpp
    source/tools/toolutil/ucln_tu.cpp
    source/tools/toolutil/ucm.cpp
    source/tools/toolutil/ucmstate.cpp
    source/tools/toolutil/udbgutil.cpp
    source/tools/toolutil/unewdata.cpp
    source/tools/toolutil/uoptions.cpp
    source/tools/toolutil/uparse.cpp
    source/tools/toolutil/writesrc.cpp
    source/tools/toolutil/xmlparser.cpp)
  target_compile_definitions(icutu PRIVATE
    U_TOOLUTIL_IMPLEMENTATION)
  target_include_directories(icutu PUBLIC
    source/tools/toolutil)
  target_link_libraries(icutu PRIVATE
    icuuc icuin)

  add_executable(gencnval
    source/tools/gencnval/gencnval.c)
  target_link_libraries(gencnval PRIVATE
    icuuc icutu)

  add_executable(gencfu
    source/tools/gencfu/gencfu.cpp)
  target_link_libraries(gencfu PRIVATE
    icuuc icuin icutu)

  add_executable(makeconv
    source/tools/makeconv/gencnvex.c
    source/tools/makeconv/genmbcs.cpp
    source/tools/makeconv/makeconv.cpp
    source/tools/makeconv/ucnvstat.c)
  target_link_libraries(makeconv PRIVATE
    icuuc icutu)

  add_executable(genbrk
    source/tools/genbrk/genbrk.cpp)
  target_link_libraries(genbrk PRIVATE
    icuuc icutu)

  add_executable(gensprep
    source/tools/gensprep/gensprep.c
    source/tools/gensprep/store.c)
  target_link_libraries(gensprep PRIVATE
    icuuc icutu)

  add_executable(gendict
    source/tools/gendict/gendict.cpp)
  target_link_libraries(gendict PRIVATE
    icuuc icutu)

  add_executable(icupkg
    source/tools/icupkg/icupkg.cpp)
  target_link_libraries(icupkg PRIVATE
    icuuc icutu)

  add_executable(genrb
    source/tools/genrb/errmsg.c
    source/tools/genrb/filterrb.cpp
    source/tools/genrb/genrb.cpp
    source/tools/genrb/parse.cpp
    source/tools/genrb/prscmnts.cpp
    source/tools/genrb/rbutil.c
    source/tools/genrb/read.c
    source/tools/genrb/reslist.cpp
    source/tools/genrb/rle.c
    source/tools/genrb/ustr.c
    source/tools/genrb/wrtjava.cpp
    source/tools/genrb/wrtxml.cpp)
  target_link_libraries(genrb PRIVATE
    icuuc icuin icutu)

  add_executable(pkgdata
    source/tools/pkgdata/pkgdata.cpp
    source/tools/pkgdata/pkgtypes.c)
  target_link_libraries(pkgdata PRIVATE
    icuuc icutu)

  set(ICU_TOOLS_DIR ${CMAKE_CURRENT_BINARY_DIR})
elseif(ICU_TOOLS_DIR)
  foreach(tool gencnval;gencfu;makeconv;genbrk;gensprep;gendict;icupkg;genrb;pkgdata)
    add_executable(${tool} IMPORTED)
    set_target_properties(${tool} PROPERTIES
      IMPORTED_LOCATION ${ICU_TOOLS_DIR}/${tool}${CMAKE_EXECUTABLE_SUFFIX})
  endforeach()
else()
  include(ExternalProject)
  ExternalProject_Add(Tools
    SOURCE_DIR ${PROJECT_SOURCE_DIR}
    BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/Tools
    CMAKE_ARGS
      -DBUILD_TOOLS=YES
      -DCMAKE_BUILD_TYPE:STRING=Release
      -DCMAKE_MAKE_PROGRAM=${CMAKE_MAKE_PROGRAM}
      -DCMAKE_SYSTEM_NAME:STRING=${CMAKE_HOST_SYSTEM_NAME}
      -DCMAKE_SYSTEM_PROCESSOR:STRING=${CMAKE_HOST_SYSTEM_PROCESSOR}
    BUILD_COMMAND ${CMAKE_COMMAND} --build <BINARY_DIR>
    INSTALL_COMMAND ""
    BUILD_BYPRODUCTS
      <BINARY_DIR>/gencnval${CMAKE_EXECUTABLE_SUFFIX}
      <BINARY_DIR>/gencfu${CMAKE_EXECUTABLE_SUFFIX}
      <BINARY_DIR>/makeconv${CMAKE_EXECUTABLE_SUFFIX}
      <BINARY_DIR>/genbrk${CMAKE_EXECUTABLE_SUFFIX}
      <BINARY_DIR>/gensprep${CMAKE_EXECUTABLE_SUFFIX}
      <BINARY_DIR>/gendict${CMAKE_EXECUTABLE_SUFFIX}
      <BINARY_DIR>/icupkg${CMAKE_EXECUTABLE_SUFFIX}
      <BINARY_DIR>/genrb${CMAKE_EXECUTABLE_SUFFIX}
      <BINARY_DIR>/pkgdata${CMAKE_EXECUTABLE_SUFFIX})
    ExternalProject_Get_Property(Tools BINARY_DIR)

    foreach(tool gencnval;gencfu;makeconv;genbrk;gensprep;gendict;icupkg;genrb;pkgdata)
      add_executable(${tool} IMPORTED)
      set_target_properties(${tool} PROPERTIES
        IMPORTED_LOCATION ${BINARY_DIR}/${tool}${CMAKE_EXECUTABLE_SUFFIX})
    endforeach()

    set(ICU_TOOLS_DIR ${BINARY_DIR})
endif()

include(TestBigEndian)
test_big_endian(IS_BIG_ENDIAN_HOST)

set(U_ICUDATA_NAME icudt${PROJECT_VERSION_MAJOR})
if(IS_BIG_ENDIAN_HOST)
  set(U_ICUDATA_PKGN icudt${PROJECT_VERSION_MAJOR}b)
else()
  set(U_ICUDATA_PKGN icudt${PROJECT_VERSION_MAJOR}l)
endif()

add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/data/tmp/icudata.lst
  COMMAND ${CMAKE_COMMAND} -E env PYTHONPATH=${PROJECT_SOURCE_DIR}/source/python
    ${Python3_EXECUTABLE} -B -m icutools.databuilder
    --mode $<IF:$<STREQUAL:${CMAKE_HOST_SYSTEM_NAME},Windows>,windows-exec,unix-exec>
    --src_dir ${PROJECT_SOURCE_DIR}/source/data
    --tool_dir ${ICU_TOOLS_DIR}
    # NOTE(compnerd) setting the configuration allows us to move back into the
    # binary directory as we emit all the tools into the binary directory.
    --tool_cfg ..
    --out_dir ${CMAKE_CURRENT_BINARY_DIR}/data/${U_ICUDATA_PKGN}
    --tmp_dir ${CMAKE_CURRENT_BINARY_DIR}/data/tmp
  DEPENDS gencnval gencfu makeconv genbrk gensprep gendict icupkg genrb
  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/source/data)

if(CMAKE_SYSTEM_NAME STREQUAL Windows)
  if(BUILD_SHARED_LIBS)
    set(_U_ICUDATA_LIB ${CMAKE_CURRENT_BINARY_DIR}/data/${U_ICUDATA_PKGN}/${CMAKE_SHARED_LIBRARY_PREFIX}${U_ICUDATA_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX})
  else()
    set(_U_ICUDATA_LIB ${CMAKE_CURRENT_BINARY_DIR}/data/${U_ICUDATA_PKGN}/${CMAKE_STATIC_LIBRARY_PREFIX}${U_ICUDATA_NAME}${CMAKE_STATIC_LIBRARY_SUFFIX})
  endif()

  add_custom_command(OUTPUT ${_U_ICUDATA_LIB}
    COMMAND $<TARGET_FILE:pkgdata> -a ${MSVC_C_ARCHITECTURE_ID} -f -e ${U_ICUDATA_NAME} -v -m $<IF:$<BOOL:${BUILD_SHARED_LIBS}>,dll,static> -c -p ${U_ICUDATA_PKGN} -T ${CMAKE_CURRENT_BINARY_DIR}/data/tmp -L ${U_ICUDATA_NAME} -d ${CMAKE_CURRENT_BINARY_DIR}/data/${U_ICUDATA_PKGN} -s ${CMAKE_CURRENT_BINARY_DIR}/data/${U_ICUDATA_PKGN} ${CMAKE_CURRENT_BINARY_DIR}/data/tmp/icudata.lst
    DEPENDS pkgdata ${CMAKE_CURRENT_BINARY_DIR}/data/tmp/icudata.lst
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/data/tmp/${U_ICUDATA_PKGN}.dat ${CMAKE_CURRENT_BINARY_DIR}/data/tmp/${U_ICUDATA_PKGN}_dat.obj)
  add_custom_target(icudata ALL
    DEPENDS ${_U_ICUDATA_LIB})

  if(BUILD_SHARED_LIBS)
    install(FILES ${_U_ICUDATA_LIB} DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
  else()
    install(FILES ${_U_ICUDATA_LIB} DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR})
  endif()
elseif(CMAKE_SYSTEM_NAME STREQUAL Darwin)
  message(FATAL_ERROR "cannot build ICU data library")
else()
  enable_language(ASM)

  configure_file(icupkg.inc.cmake ${CMAKE_BINARY_DIR}/icupkg.inc)

  add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/data/tmp/${U_ICUDATA_PKGN}_dat.S
    COMMAND $<TARGET_FILE:pkgdata> -f -e ${U_ICUDATA_NAME} -v -m $<IF:$<BOOL:${BUILD_SHARED_LIBS}>,dll,static> -c -p ${U_ICUDATA_PKGN} -T ${CMAKE_CURRENT_BINARY_DIR}/data/tmp -L ${U_ICUDATA_NAME} -d ${CMAKE_CURRENT_BINARY_DIR}/data/${U_ICUDATA_PKGN} -s ${CMAKE_CURRENT_BINARY_DIR}/data/${U_ICUDATA_PKGN} ${CMAKE_CURRENT_BINARY_DIR}/data/tmp/icudata.lst -O ${CMAKE_BINARY_DIR}/icupkg.inc
    DEPENDS pkgdata ${CMAKE_CURRENT_BINARY_DIR}/data/tmp/icudata.lst
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/data/tmp/${U_ICUDATA_PKGN}.dat ${CMAKE_CURRENT_BINARY_DIR}/data/tmp/${U_ICUDATA_PKGN}_dat.S)

  add_library(${U_ICUDATA_NAME}
    ${CMAKE_CURRENT_BINARY_DIR}/data/tmp/${U_ICUDATA_PKGN}_dat.S)
  set_target_properties(${U_ICUDATA_NAME} PROPERTIES
    LINKER_LANGUAGE C
    LINK_OPTIONS "-nodefaultlibs;-nostdlib;-Bsymbolic"
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/data/${U_ICUDATA_PKGN})

  install(TARGETS ${U_ICUDATA_NAME}
    DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR})
endif()

install(TARGETS icuuc icuin
  ARCHIVE DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
install(DIRECTORY source/common/unicode
  DESTINATION ${CMAKE_INSTALL_FULL_INCLUDEDIR})
install(DIRECTORY source/i18n/unicode
  DESTINATION ${CMAKE_INSTALL_FULL_INCLUDEDIR})

