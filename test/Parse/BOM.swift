// RUN: %swift %s -parse -verify
ÿþ // expected-error{{UTF-16 BOM marker is not valid UTF-8}}
asdfadf() // no error, the BOM marker causes us to skip to the end
