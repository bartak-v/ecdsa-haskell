#!/bin/bash
# For more info, read the comments in test.sh
set -e
echo "[INFO] Testing the ECDSA-Haskell program with secp128r1, secp256k1 and secp384r1 curves."

echo -e "\n------ [TESTING] secp128r1 ------"
sleep 2
./test.sh curves/secp128r1 128
sleep 1
echo -e "\n------ [TESTING] secp192r1 ------"
sleep 2
./test.sh curves/secp192r1 128 # todo test, that the hash lenght can be mismatched with the key length (it probably can i guess)...
sleep 1
echo -e "\n------ [TESTING] secp256k1 ------"
sleep 2
./test.sh curves/secp256k1 256
sleep 1
echo -e "\n------ [TESTING] secp384r1 ------"
sleep 2
./test.sh curves/secp384r1 384
sleep 1

echo -e "\n[SUCCESS] All tests successful."