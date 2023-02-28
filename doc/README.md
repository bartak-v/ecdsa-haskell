# ecdsa-haskell
ECDSA implemented in Haskell - Functional project for the FLP course at BUT FIT 2023.

The project was implemented in Haskell using GHC 9.2.5.

## Input format

For readability purposes, the format of the input file should be as such (secp256k1 curve):

```json
Curve {
p: 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
a: 0
b: 7
g: Point {
x: 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
y: 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
}
n: 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
h: 1
}
<-- ADD Key SECTION IF YOU ARE CALLING THE -s / Sign option -->
Key {
d: 0xc9dcda39c4d7ab9d854484dbed2963da9c0cf3c6e9333528b4422ef00dd0b28e
Q: 0x040e411e56210f20bf5172cbefab02135421b1eb58f6918d28c1b848be5eee42...
}
<-- OR ADD Signature IF YOU ARE CALLING THE -v / Verify option -->
Signature {
r: 0xb21ff64650be40610ba9324bc6bd273eafa87ac1bbc075be425c0f422f53196f
s: 0x3b4d090468eddbea8a53c565d19a24c56377786c49a7f114459c43bc7d59615a
}
```

While similar, the input format is not adhering to JSON in any way - it's pretty loose and the parser will parse properly even if the format is such as:

```json
p: 0xF...
a: 0xF...
b: 0xF...
x: 0xF...
y: 0xF...
n: 0xF...
h: 0xF...
```

The semantics and names of the ECDSA parameters (e.g. "c: ...") must be preserved. The parser is (as of now) case sensitive ("c:" =/= "C:")!. The parser does not care about the order of the parameters, but each parameter must be on it's own line. The parameters must be unique in the input. You can put random comments in the input files if they do not contain any of the keywords - anything else (if the other rules are fulfilled) is a comment.

The list of reserved keywords is:

```json
  ["p:", "a:", "b:", "x:", "y:", "n:", "h:", "d:", "Q:", "Hash:", "r:", "s:"]
```

You can use these keywords only once in the input file / stream!

See `test/` directory for working examples and examples of "misformatted" inputs that the parser understands correctly. If no input file is specified, the program reads STDIN.

Public Key is saved as `0x04<PADDING>PUB_KEY_X<PADDING>CONVERTED_PUB_KEY_Y`, which is 4+2\*n bytes long, where 2 bytes are "0x" hex prefix, "04" is uncompressed pubkey prefix and 2*nlen is the (Xpub,Ypub) point with padding. nlen is the key-length, which is based on how long the prime n () is.

This program is able to parse arbitrary curve (such as SECP256K1, SECP384R1, etc.) # TODO TEST THIS

### References

Implemented according to <https://secg.org/sec1-v2.pdf>
<http://learnyouahaskell.com>
<https://www.cs.miami.edu/home/burt/learning/Csc609.142/ecdsa-cert.pdf>
<https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm>
<https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm>
<https://www.secg.org/sec1-v2.pdf>