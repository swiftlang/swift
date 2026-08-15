# Usage: perl tpqa.pl word1 word2 ...
foreach (@ARGV) {
  while (s/p(a+)p/p\1/ ||
         s/q(a+)q/q\1/ ||
         s/t(a*)p/t\1/ ||
         s/t(a*)q/t\1/) {}
  print $_, "\n";
}
