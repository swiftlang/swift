#//===--- MirrorCommon.py -------------------------------------*- python -*-===//
#//
#// This source file is part of the Swift.org open source project
#//
#// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
#// Licensed under Apache License v2.0 with Runtime Library Exception
#//
#// See http://swift.org/LICENSE.txt for license information
#// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#//
#//===----------------------------------------------------------------------===//
# This file contains utility functions that are used by the gyb template files
# that generate Mirrors for the Swift Standard Library.
# If you edit this, make sure to also accordingly tweak the actual template files.

def getDisposition(disp=None):
  if disp == None:
    return '.Aggregate'
  if len(disp) == 0 or disp[0] != '.':
    disp = '.' + disp
  return disp

def _getGenericArgStrings(genericArgs=None,genericConstraints=None):
  if genericArgs == None:
    return ('','')
  genericArgString = ''
  first = True
  for arg in genericArgs:
    if not first:
      genericArgString  = genericArgString + ','
    first = False
    genericArgString = genericArgString + arg
  if genericConstraints == None:
    genericConstraintString = genericArgString
  else:
    genericConstraintString = ''
    first = True
    for arg in genericArgs:
      if not first:
        genericConstraintString = genericConstraintString + ','
      first = False
      genericConstraintString = genericConstraintString + arg
      if arg in genericConstraints:
        cons = genericConstraints[arg]
        genericConstraintString = genericConstraintString + ' : ' + cons
  genericArgString = '<' + genericArgString + '>'
  genericConstraintString = '<' + genericConstraintString + '>'
  return (genericArgString,genericConstraintString)

def getGenericArgString(genericArgs=None,genericConstraints=None):
  return _getGenericArgStrings(genericArgs,genericConstraints)[0]

def getGenericConstraintString(genericArgs=None,genericConstraints=None):
  return _getGenericArgStrings(genericArgs,genericConstraints)[1]

