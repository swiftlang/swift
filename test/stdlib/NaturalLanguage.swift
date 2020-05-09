// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest

import NaturalLanguage


var tests = TestSuite("NaturalLanguage")

if #available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *) {
  tests.test("recognizer") {
    let recognizer = NLLanguageRecognizer()
    let str = "This is a test mein Freund"
    recognizer.processString(str)
    recognizer.languageHints = [.english: 0.9, .german: 0.1]
    let lang = recognizer.dominantLanguage
    expectEqual(NLLanguage.english, lang)
    let hypotheses = recognizer.languageHypotheses(withMaximum: 2)
    expectEqual(hypotheses.count, 2)
    let enProb = hypotheses[.english] ?? 0.0
    let deProb = hypotheses[.german] ?? 0.0
    let frProb = hypotheses[.french] ?? 0.0
    expectNotEqual(0.0, enProb)
    expectNotEqual(0.0, deProb)
    expectEqual(0.0, frProb)
  }

  tests.test("tokenizer") {
    let tokenizer = NLTokenizer(unit:.word)
    let str = "This is a test. 😀"
    let strRange = Range(NSMakeRange(0, 18), in: str)!
    tokenizer.string = str
    tokenizer.setLanguage(.english)
    let tokenRange1 = tokenizer.tokenRange(at: str.startIndex)
    let tokenArray = tokenizer.tokens(for: strRange)
    let tokenRange2 = tokenArray[0]
    expectEqual(tokenRange1, tokenRange2)
    expectEqual("This", str[tokenRange1])
    expectEqual(5, tokenArray.count)
    var numTokens = 0
    tokenizer.enumerateTokens(in: strRange) { (tokenRange, attrs) -> Bool in
      if (numTokens == 0) {
        expectEqual(tokenRange, tokenRange1)
      }
      numTokens = numTokens + 1
      return true
    }
    expectEqual(5, numTokens)
    expectEqual("😀", str[tokenArray[4]])
  }


  tests.test("tagger") {
    let tagger = NLTagger(tagSchemes: [.tokenType])
    let str = "This is a test. 😀"
    let strRange = Range(NSMakeRange(0, 18), in: str)!
    tagger.string = str
    tagger.setLanguage(.english, range: strRange)
    let ortho = NSOrthography.defaultOrthography(forLanguage: "en")
    tagger.setOrthography(ortho, range: strRange)
    let (tag1, tokenRange1) = tagger.tag(at: str.startIndex, unit: .word, scheme: .tokenType)
    let tags = tagger.tags(in: strRange, unit: .word, scheme: .tokenType, options: .omitWhitespace)
    let (tag2, tokenRange2) = tags[0]
    let tokenRange3 = tagger.tokenRange(at: str.startIndex, unit: .word)
    expectEqual(NLTag.word, tag1)
    expectEqual(NLTag.word, tag2)
    expectEqual(tokenRange1, tokenRange2)
    expectEqual(tokenRange2, tokenRange3)
    expectEqual("This", str[tokenRange1])
    expectEqual(6, tags.count)
    var numTokens = 0
    tagger.enumerateTags(in: strRange, unit: .word, scheme: .tokenType, options: .omitWhitespace) { (tag, tokenRange) -> Bool in
      let (tagAt, tokenRangeAt) = tagger.tag(at: tokenRange.lowerBound, unit: .word, scheme: .tokenType)
      expectEqual(tag, tagAt)
      expectEqual(tokenRange, tokenRangeAt)
      if (numTokens == 0) {
        expectEqual(NLTag.word, tag)
        expectEqual(tokenRange, tokenRange1)
      }
      numTokens += 1
      return true
    }
    expectEqual(6, numTokens)
    let (_, tokenRange4) = tags[5]
    expectEqual("😀", str[tokenRange4])
  }
}

runAllTests()
