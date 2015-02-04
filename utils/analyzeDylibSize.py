#!/usr/bin/env python
import subprocess
import sys

if len(sys.argv) < 2:
  print "Usage: %s file.dylib" % sys.argv[0]
  sys.exit(0)

class BinaryFunction:
  """
  This class represents a disassembled binary function.
  """
  def __init__(self, symbolName):
    self.name = symbolName
    self.start = None
    self.end = None

  def addLine(self, opcodeLine):
    """
    Parse a single disassembly line that is not the header.
    Example input:
    0000000000002b0a  callq 0x1aef4c
    """
    # Ignore the second part of the multi-line instruction.
    if not opcodeLine.startswith("000"): return
    # Normalize the spaces in the line.
    opcodeLine = opcodeLine.replace("\t"," ")
    # Find the address of the instruction.
    addr = int(opcodeLine.split(" ")[0], 16)
    # Initialize the start address if we haven't done this before.
    if not self.start: self.start = addr
    # notice that we don't have the size of the last instruction
    # and assume it is zero.
    self.end = addr

  def getSize(self):
    """
    Return the size of the function in the binary.
    """
    if not self.end: return 0
    return self.end - self.start

  def __str__(self): return self.name + ", " +  str(self.getSize())

#Disassemble the dylib.
content = subprocess.check_output(["otool", "-v", "-t", sys.argv[1]]).split("\n")

# parse the disassembled test:
Funcs = []
CurrF = None
for line in content:
  # Parse the function header.
  if line.endswith(":"):
    CurrF = BinaryFunction(line[:-1])
    Funcs.append(CurrF)
    continue
  # Parse the instructions.
  CurrF.addLine(line)

Prefix = {
  # Cpp
  "__Z" : "CPP",
  "_swift" : "CPP",
  "__swift" : "CPP",

  # Objective-C
  "+[" : "CPP",
  "-[" : "CPP",

  # Swift
  "__TP"  : "Partial Apply",
  "__TTW" : "Protocol Witness",
  "__Tw"  : "Value Witness",
  "__TM"  : "Type Metadata",
  "__TF"  : "Swift Function",
  "__TTS" : "Specialization",

  # Default
  ""      : "Unknown",
}

# initialize an empty score board for the prefixes.
ScoreBoard = {}
for key in Prefix:
  ScoreBoard[Prefix[key]] = (0,0)

# Sort the functions by size (in case we want to print them)
SortedFunctions = sorted(Funcs, key = lambda x: x.getSize())

# Calculate the size of each kind of function.
for BinFunc in SortedFunctions:
  for prefix in reversed(sorted(Prefix)):
    if BinFunc.name.startswith(prefix):
      count, size = ScoreBoard[Prefix[prefix]]
      ScoreBoard[Prefix[prefix]] = (count + 1, size + BinFunc.getSize())
      break

# Print the different prefixes.
print "Function Kind, Count, Size,"
for Entry in reversed(sorted(ScoreBoard)):
  print Entry, ", ", ScoreBoard[Entry][0], ",", ScoreBoard[Entry][1],","


