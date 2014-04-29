# Linear function shift register.
#
# This is just to drive benchmarks. I don't make any claim about its
# strength. According to Wikipedia, it has the maximal period for a
# 32-bit register.
class LFSR:
    def __init__(self):
        # set the register to some seed
        self.lfsr = 0xb78978e7

    def shift(self):
        self.lfsr = (self.lfsr >> 1) ^ (-(self.lfsr & 1) & 0xD0000001)

    def randInt(self):
        result = 0
        for i in range(0,32):
            result = (result << 1) | self.lfsr & 1
            self.shift()
        return result

if __name__ == "__main__":
    import sys, sets
    lfsr = LFSR()
    rands = sets.Set()
    for i in range (0,int(sys.argv[1])):
        r = lfsr.randInt()
        assert r not in rands
        rands.add(r)
        print r
