#!/bin/bash
set -e
#  Description : Testing suite for ECDSA implementation in Haskell.
#  Author:     : Bc. Vít Barták (xbarta47)
#  Maintainer  : xbarta47@fit.vutbr.cz
#  Year        : 2023

# --- You must have standard Ubuntu packages installed, such as OpenSSL. ---
# This script runs tests on arbitrary Elliptical Curve.
# The test is successfull if and only if it outputs "True".
# The test.sh takes 2 arguments, first is the file with the correct format
# of Elliptic Curve (see README.md).
# Currently, it only supports 128, 256 and 384 bits long hashes, for testing
# random curve parameters, do the tests manually.

# The test first takes an input of Elliptical Curve specified in a file $1.
# [PARSING]
# It then loads the Elliptical Curve parameters into the inner representation
#   of the Haskell ECDSA program and outputs it to STDOUT (pipe).
# [KEY GENERATION]
# The ECDSA program is then called again with the EC Parameters on STDIN and
#   generates a random KeyPair (private and public key).
# [MESSAGE DIGEST SIGNING]
# The ECDSA program is then called again with the EC Parameters + KeyPair 
#   Message Hash on STDIN and outputs an ECDSA signature of the hash with the
#   private key.
# The ECDSA program is then called again with EC Parameters + Signature 
# and tests, whether or not the signature is correct.

CURVE_DEFINITION=$1
KEY_LENGTH=$2
echo "[INFO] Compiling the ECDSA-Haskell program."
make 1>/dev/null
echo "[SUCCESS] Compilation successful."
mkdir -p tmp
touch tmp/key_mode_output.txt
echo "[INFO] Testing the ECDSA-Haskell program for parsing and key generation modes."
./flp22-fun -i $1 | ./flp22-fun -k > tmp/key_mode_output.txt
echo "[SUCCESS] Key generation successful."

# Generate random message hash.
case $KEY_LENGTH in
  "128")
    MD=" -md5 " 
    ;;
  "256")
    MD=" -sha256 " 
    ;;
  "384")
    MD=" -sha384 "
    ;;
  *)
    echo -e "[ERROR] test.sh currently supports only hashes with 128, 256, 384 bits long.\n For testing arbitrary ECDSA key (and hash) length, run the ECDSA program manually."
    exit 1
    ;;
esac

echo "[INFO] Testing the ECDSA-Haskell program for digital signature mode."
echo "[INFO] Generating random ${KEY_LENGTH} bit long Hash for signing."
printf "\nHash: 0x" >> tmp/key_mode_output.txt
MESSAGE=$(tr -dc '[:print:]' </dev/urandom | head -c 10) # Get 10 random ascii characters
HASH=$(echo $MESSAGE | openssl dgst $MD | awk '{print $2}') # Create KEY_LENGTH long hash.
printf $HASH >> tmp/key_mode_output.txt
touch tmp/sign_mode_output.txt
./flp22-fun -s tmp/key_mode_output.txt > tmp/sign_mode_output.txt
echo "[SUCCESS] Digital Signing successful."

echo "[INFO] Testing the ECDSA-Haskell program for parsing and key generation."
./flp22-fun -v tmp/sign_mode_output.txt
echo "[SUCCESS] Signature verification (NOT IMPLEMENTED YET) successful."

# Delete the temporary directory
rm -rf tmp/